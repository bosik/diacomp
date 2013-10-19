unit DiaryRoutines;

{ Разные утилиты }

{$R+ O+}

interface

uses
  SysUtils,
  Classes,
  Windows {GetThreadLocale};

type
  TIndexList = class
  private
    FData: array of integer;
  protected
    function GetItem(Index: integer): integer;
    procedure SetItem(Index, Value: integer);
  public
    function Count: integer;
    procedure Init(Count: integer);
    procedure RemoveValue(Value: integer);
    procedure Swap(Index1, Index2: integer);

    property Items[Index: integer]: integer read GetItem write SetItem; default;
  end;

  TStringPair = class
    Key, Value: string;
    constructor Create(const Key, Value: string);
  end;

  TStringMap = class
  private
    FData: array of TStringPair;
  protected
    function GetItem(Index: integer): TStringPair;
  public
    procedure Add(const Key, Value: string; Overwrite: boolean = False);
    procedure Clear;
    function Count: integer;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    property Items[Index: integer]: TStringPair read GetItem; default;
  end;

  //TDate = type Double;
  TDate = type integer; // yao ming ;D

  TStringArray = array of string;
  TSwapProcedure = procedure(Index1, Index2: integer) of object;
  TMoreFunction = function(Index1,Index2: integer): boolean of object;
  TGetterFunction = function(Index: integer): Variant of object;
  TCallbackProgress = procedure(Progress: integer);

  { файловые }
  function ReadFile(const FileName: string): string;
  procedure WriteFile(const FileName: string; const Content: string = '');

  { форматирование строк : общее }
  function CheckDot(const S: string): string;

  { форматирование строк : числа }
  function RealToStr(const X: real): string;
  function RealToStrZero(const X: real): string;
  function FloatToStrP(const X: real): string;

  { форматирование строк : дата, время }
  function GetCurrentTime: string;
  function GetCurrentMinutes: integer;
  function TimeToStr(Time: integer; Sign: char): string; overload;
  function TimeToStr(Time: integer): string; overload;
  function TimeToStrColon(Time: integer): string;
  function TimeSaveSecToStr(Time: integer): string;
  function TimeTrncSecToStr(Time: integer): string;

  { парсинг }
  function StrToTimeQuick(const Str: string): integer;
  function TryStrToTime(Str: string; var T: integer): boolean;
  function TryToCalculate(const S: string; var R: Extended): boolean;

  { проверки }
  function CorrectTime(Time: integer): boolean;
  function CheckPositiveFloat(const Value: string): boolean;
  function TestStrToFloat(const S: string): boolean;

  { чистая работа со строками }
  function EqStarts(const s1,s2: string; Count: integer): boolean;
  function ReadStrTo(const S: string; Terminal: char; var Caret: integer): string;
  function ReadFloatTo(const S: string; Terminal: char; var Caret: integer): real;
  function ReadIntTo(const S: string; Terminal: char; var Caret: integer): integer;
  function FmtArray(const S: string): TStringArray;
  function TextBefore(const S: string; Terminal: char): string;
  function TextAfter(const S: string; Terminal: char): string;
  function MatchStr(const S1, S2: string; IgnoreCase: boolean): boolean;
  procedure Separate(const S: string; out Before: string; Separator: Char; out After: string);
  procedure SeparateBack(const S: string; out Before: string; Separator: Char; out After: string);
  function StartsWith(const S: string; C: char): boolean;
  function UppercaseFirst(const S: string): string;

  { быстрая сортировка }
  procedure QuickSort(Left, Right: integer; Swap: TSwapProcedure; More: TMoreFunction);
  { быстрый поиск }
  function BinarySearch(Value: Variant; Left, Right: integer; Getter: TGetterFunction): integer;

var
  Decimal: char;
  LocalFmt: TFormatSettings;

const
  SecPerMin     = 60; // TODO: хочу капс-константы. Все.
  MinPerHour    = 60;
  HourPerDay    = 24;
  MinPerDay     = MinPerHour * HourPerDay;
  HalfMinPerDay = MinPerDay div 2;
  SecPerHour    = SecPerMin * MinPerHour;
  SecPerDay     = SecPerMin * MinPerHour * HourPerDay;

  // TODO: move to localization
  ERROR_CANT_CREATE_DIARY = 'Невозможно создать файл дневника';
  ERROR_LOADINGDIARY      = 'Ошибка загрузки дневника';
  ERROR_LOADINGFOODBASE   = 'Ошибка загрузки базы продуктов';
  ERROR_LOADINGDISHBASE   = 'Ошибка загрузки базы блюд';

implementation

{ TIndexList }

{==============================================================================}
function TIndexList.Count: integer;
{==============================================================================}
begin
  Result := Length(FData);
end;

{==============================================================================}
function TIndexList.GetItem(Index: integer): integer;
{==============================================================================}
begin
  Result := FData[Index];
end;

{==============================================================================}
procedure TIndexList.Init(Count: integer);
{==============================================================================}
var
  i: integer;
begin
  // инициализирует список числами 0..Count-1
  if (Count >= 0) then
  begin
    SetLength(FData, Count);
    for i := 0 to Count - 1 do
      FData[i] := i;
  end else
    raise Exception.CreateFmt('TIndexList().Init: illegal count (%d)', [Count]);
end;

{==============================================================================}
procedure TIndexList.RemoveValue(Value: integer);
{==============================================================================}
var
  i, w: integer;
begin
  { удаление (предполагаем многократное вхождение) }
  w := 0;
  for i := 0 to High(FData) do
  if (FData[i] <> Value) then
  begin
    FData[w] := FData[i];
    inc(w);
  end;
  SetLength(FData, w);

  { корректировка }
  for i := 0 to High(FData) do
  if (FData[i] > Value) then
    dec(FData[i]);
end;

{==============================================================================}
procedure TIndexList.SetItem(Index, Value: integer);
{==============================================================================}
begin
  FData[Index] := Value;
end;

{==============================================================================}
procedure TIndexList.Swap(Index1, Index2: integer);
{==============================================================================}
begin
  FData[Index1] := FData[Index1] xor FData[Index2];
  FData[Index2] := FData[Index1] xor FData[Index2];
  FData[Index1] := FData[Index1] xor FData[Index2];
end;

{ TStringPair }

constructor TStringPair.Create(const Key, Value: string);
begin
  Self.Key := Key;
  Self.Value := Value;
end;

{ TStringMap }

{==============================================================================}
procedure TStringMap.Add(const Key, Value: string; Overwrite: boolean);
{==============================================================================}
var
  i: integer;
begin
  if (Overwrite) then
  for i := 0 to High(FData) do
  if (FData[i].Key = Key) then
  begin
    FData[i].Value := Value;
    Exit;
  end;

  SetLength(FData, Length(FData) + 1);
  FData[High(FData)] := TStringPair.Create(Key, Value);
end;

{==============================================================================}
procedure TStringMap.Clear;
{==============================================================================}
begin
  SetLength(FData, 0);
end;

{==============================================================================}
function TStringMap.Count: integer;
{==============================================================================}
begin
  Result := Length(FData);
end;

{==============================================================================}
function TStringMap.GetItem(Index: integer): TStringPair;
{==============================================================================}
begin
  Result := FData[Index];
end;

{==============================================================================}
procedure TStringMap.LoadFromFile(const FileName: string);
{==============================================================================}
var
  i: integer;
  Key, Value: string;
begin
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    for i := 0 to Count - 1 do
    begin
      Key := TextBefore(Strings[i], '=');
      Value := TextAfter(Strings[i], '=');
      Self.Add(Key, Value, False);
    end;
    Free;
  end;
end;

{==============================================================================}
procedure TStringMap.SaveToFile(const FileName: string);
{==============================================================================}
var
  i: integer;
begin
  with TStringList.Create do
  begin
    for i := 0 to Count - 1 do
      Add(Items[i].Key + '=' + Items[i].Value);
    SaveToFile(FileName);
    Free;
  end;
end;

{==============================================================================}
function ReadFile(const FileName: string): string;
{==============================================================================}
begin
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    Result := Text;
    Free;
  end;
end;

{==============================================================================}
procedure WriteFile(const FileName: string; const Content: string = '');
{==============================================================================}
(*var
  f: File;
begin
  {$I-}
  AssignFile(f, FileName);
  Rewrite(f);
  Write(f{, Content});
  CloseFile(f);
  {$I+}
  Result := (IOResult = 0) and FileExists(FileName);
end;   *)
begin
  with TStringList.Create do
  begin
    Text := Content;
    SaveToFile(FileName);
    Free;
  end;
end;

{==============================================================================}
function CheckDot(const S: string): string;
{==============================================================================}
var
  n: integer;
begin
  Result := S;
  for n := 1 to length(Result) do
  if Result[n] in ['.', ','] then
    Result[n] := Decimal;
end;

{==============================================================================}
function RealToStr(const X: real): string;
{==============================================================================}
begin
  // НЕ БОЛЕЕ одного ноля после запятой
  Result := FloatToStr(Round(x * 10) / 10);
end;

{==============================================================================}
function RealToStrZero(const X: real): string;
{==============================================================================}
begin
  // РОВНО один ноль после запятой
  Result := Format('%.1f', [X]);
end;

{==============================================================================}
function FloatToStrP(const X: real): string;
{==============================================================================}
begin
  if X > 0 then
    Result := '+' + RealToStr(X)
  else
    Result := RealToStr(X);
end;

{==============================================================================}
function TimeToStr(Time: integer; Sign: char): string;
{==============================================================================}
begin
  {if time<600 then Result := '0' else Result := '';
  Result := Result+IntToStr(time div 60)+Sign;
  if (time mod 60)<10 then Result := Result+'0';
  Result := Result+IntToStr(time mod 60); }
  Result := Format('%.2d' + Sign + '%.2d', [Time div 60, Time mod 60]);
end;

{==============================================================================}
function TimeToStr(Time: integer): string;
{==============================================================================}
begin
  Result := TimeToStr(Time, '.');
end;

{==============================================================================}
function TimeToStrColon(Time: integer): string;
{==============================================================================}
begin
  Result := TimeToStr(Time, ':');
end;

{==============================================================================}
function StrToTimeQuick(const Str: string): integer;
{==============================================================================}
begin
  Result := 
    StrToInt(str[1]+str[2])*60+
    StrToInt(str[4]+str[5]);
end;

{==============================================================================}
function TryStrToTime(Str: string; var T: integer): boolean;
{==============================================================================}
var
  Sh,Sm: string;
  h,m: integer;
begin
  try
    { спокойно, граммар-наци: здесь время, а не число с точкой }
    if pos(',',str)>0 then str[pos(',',str)] := '.';
    if pos(':',str)>0 then str[pos(':',str)] := '.';

    Sh := Copy(str, 1, pos('.', str) - 1);
    Sm := Copy(str, pos('.', str) + 1, Length(str) - pos('.', str));
    Result :=
      TryStrToInt(Sh, h) and
      TryStrToInt(Sm, m) and
      (h >= 0)and(h < 24)and(m >= 0)and(m < 60);
    if Result then
      T := h * 60 + m
    else
      T := -500;
  except
    Result := false;
  end;
end;

{==============================================================================}
function TryToCalculate(const S: string; var R: Extended): boolean;
{==============================================================================}
var
  Op1, Op2: string;
  Val1, Val2: Extended;
begin
 // Result := False;

  // правильный порядок вычислений (правая ассоциативность(?)) поддерживается
  // с помощью метода разбиения SeparateBack 

  if TryStrToFloat(S, R) then Result := True else

  if (pos('+', S) > 0) then
  begin
    SeparateBack(S, Op1, '+', Op2);
    Op1 := Trim(Op1);
    Op2 := Trim(Op2);

    if (Op1 <> '') and (Op2 = '') then Result := TryStrToFloat(Op1, R) else
    if (Op1 = '') and (Op2 <> '') then Result := TryStrToFloat(Op2, R) else
    if (Op1 <> '') and (Op2 <> '') and
       TryToCalculate(Op1, Val1) and
       TryToCalculate(Op2, Val2) then
    begin
      R := Val1 + Val2;
      Result := True;
    end else
      Result := False;
  end else

  if (pos('-', S) > 0) then
  begin
    SeparateBack(S, Op1, '-', Op2);
    Op1 := Trim(Op1);
    Op2 := Trim(Op2);

    if (Op1 <> '') and (Op2 = '') then Result := TryStrToFloat(Op1, R) else
    if (Op1 = '') and (Op2 <> '') then Result := TryStrToFloat('-'+Op2, R) else     
    if (Op1 <> '') and (Op2 <> '') and
       TryToCalculate(Op1, Val1) and
       TryToCalculate(Op2, Val2) then
    begin
      R := Val1 - Val2;
      Result := True;
    end else
      Result := False;
  end else
    Result := False;
end;

{==============================================================================}
function GetCurrentTime: string;
{==============================================================================}
var
  h,m,s,ms: word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := Format('%.2d.%.2d', [h, m]);
end;

{==============================================================================}
function GetCurrentMinutes: integer;
{==============================================================================}
var
  h,m,s,ms: word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := h * 60 + m;
end;

(*{==============================================================================}
function FormatDate(Date: TDate): string;
{==============================================================================}
const
  Days: array[1..7]of string =
  ('вс','пн','вт','ср','чт','пт','сб');
begin
  Result := DateToStr(date);
  Result := Copy(Result,1,length(Result)-5);
  Result := Result + ', ' + Days[DayOfWeek(date)];
end;  *)

{==============================================================================}
function TimeSaveSecToStr(Time: integer): string;
{==============================================================================}
begin
  if time < 0 then
  begin
    Result := '-';
    time := -1*time;
  end else
    Result := '';

  if time < 3600 then Result := Result + '0';
  Result := Result+IntToStr(time div 3600)+':';
  time := time mod 3600;

  if time<60 then Result := Result+'0';
  Result := Result+IntToStr(time div 60)+':';
  time := time mod 60;

  if time < 10 then Result := Result+'0';
  Result := Result+IntToStr(time);
end;

{==============================================================================}
function TimeTrncSecToStr(Time: integer): string;
{==============================================================================}
var
  h,m: integer;
begin
  {if time < 0 then
  begin
    Result := '-';
    time := -time;
  end else
    Result := '';  }

  h := Time div SecPerHour;
  m := (Time - h * SecPerHour) div SecPerMin;
  //s := Time mod SecPerMin;

  Result := Format('%.2d:%.2d', [h, m]);
            {
  if time < 10*3600 then Result := Result+'0';
  Result := Result+IntToStr(time div 3600)+':';
  time := (time mod 3600)div 60;

  if time < 10 then Result := Result+'0';
  Result := Result+IntToStr(time);}
end;

(*
{==============================================================================}
function Srv_StrToDateTime(const S: string): TDateTime;
{==============================================================================}
{var
  F: TFormatSettings;}
begin
  try
    if S <> '0' then
    begin
      {GetLocaleFormatSettings(GetThreadLocale, F);
      F.DateSeparator := '-';
      F.TimeSeparator := ':';
      F.ShortDateFormat := 'yyyy-mm-dd';
      Result := StrToDateTime(S, F); }
      Result := StrToDateTime(S, WebFmt);
    end else
      Result := 0;
  except
    raise Exception.Create('Ошибка выполнения Srv_StrToDateTime');
  end;
end;

{==============================================================================}
function Srv_DateToStr(Date: TDate): string;
{==============================================================================}
begin
  try
    Result := DateToStr(Date, WebFmt);
  except
    raise Exception.Create('Ошибка выполнения Srv_DateToStr');
  end;
end;
*)

function CorrectTime(Time: integer): boolean;
begin
  Result := (Time >= 0)and(Time < MinPerDay);
end;

{==============================================================================}
function CheckPositiveFloat(const Value: string): boolean;
{==============================================================================}
var
  Temp: double;
begin
  Result :=
    TryStrToFloat(CheckDot(Value), Temp) and
    (Temp > 0);
end;

function EqStarts(const s1,s2: string; Count: integer): boolean;
begin
  if (Count > Length(s1)) then Count := Length(s1);
  if (Count > Length(s2)) then Count := Length(s2);
  Result := Copy(s1,1,Count) = Copy(s2,1,Count);
end;

function ReadStrTo(const S: string; Terminal: char; var Caret: integer): string;
var
  n: integer;
begin
  n := Caret;
  while (n<=length(S))and(S[n]<>Terminal) do inc(n);
  Result := Copy(S, Caret, n-Caret);
  Caret := n + 1;
end;

function ReadFloatTo(const S: string; Terminal: char; var Caret: integer): real;
begin
  Result := StrToFloat( CheckDot(ReadStrTo(S, Terminal, Caret)) );
end;

function ReadIntTo(const S: string; Terminal: char; var Caret: integer): integer;
begin
  Result := StrToInt( ReadStrTo(S, Terminal, Caret) );
end;

function TestStrToFloat(const S: string): boolean;
var
  val: double;
begin
  Result := TryStrToFloat(S, val);
end;

function FmtArray(const S: string): TStringArray;
begin
  SetLength(Result,1);
  Result[0] := s;
end;

function TextBefore(const S: string; Terminal: char): string;
begin
  Result := Copy(S, 1, pos(Terminal, S)-1);
end;

function TextAfter(const S: string; Terminal: char): string;
var
  n: integer;
begin
  n := pos(Terminal, S);
  Result := Copy(S, n + 1, Length(S) - n);
end;

function MatchStr(const S1, S2: string; IgnoreCase: boolean): boolean;
begin
  if IgnoreCase then
    Result := (AnsiLowerCase(s1) = AnsiLowerCase(s2))
  else
    Result := (s1 = s2);
end;

procedure Separate(const S: string; out Before: string; Separator: Char; out After: string);
var
  k: integer;
begin
  k := pos(Separator, S);
  if (k = 0) then
  begin
    Before := S;
    After := '';
  end else
  begin
    Before := Copy(S, 1, k - 1);
    After := Copy(S, k + 1, Length(S) - k);
  end;
end;

procedure SeparateBack(const S: string; out Before: string; Separator: Char; out After: string);

  function PosBack(C: Char; const Str: string): integer;
  var
    i: integer;
  begin
    for i := Length(Str) downto 1 do
    if (Str[i] = C) then
    begin
      Result := i;
      Exit;
    end;
    Result := 0;
  end;

var
  k: integer;
begin
  k := PosBack(Separator, S);
  if (k = 0) then
  begin
    Before := S;
    After := '';
  end else
  begin
    Before := Copy(S, 1, k - 1);
    After := Copy(S, k + 1, Length(S) - k);
  end;
end;

{==============================================================================}
function StartsWith(const S: string; C: char): boolean;
{==============================================================================}
begin
  Result := (length(S) > 0) and (S[1] = C);
end;

{==============================================================================}
function UppercaseFirst(const S: string): string;
{==============================================================================}
begin
  Result := S;
  if (Result <> '') then
    Result[1] := AnsiUppercase(Result[1])[1];
end;

procedure QuickSort(Left, Right: integer; Swap: TSwapProcedure; More: TMoreFunction);

  procedure qsort(l,r: integer);
  var
    i, j: integer;
    x: integer;
  begin
    i := l;
    j := r;
    x := (l+r) div 2;
    repeat
      while More(x, i) do inc(i);
      while More(j, x) do dec(j);
      if (i <= j) then
      begin
        if More(i, j) or More(j, i) then
        begin
          Swap(i, j);
          { необходимо, если X - это индекс }
          if x = i then x := j else
          if x = j then x := i;
        end;
        inc(i);
        dec(j);
      end;
    until i > j;
    if l < j then qsort(l, j);
    if i < r then qsort(i, r);
  end;

begin
  if Left <= Right then
    qsort(Left, Right);
end;

function BinarySearch(Value: Variant; Left, Right: integer; Getter: TGetterFunction): integer;
begin
  while (Left <= Right) do
  begin
    Result := (Left + Right) div 2;
    if (Getter(Result) < Value) then Left := Result + 1 else
    if (Getter(Result) > Value) then Right := Result - 1 else
      Exit;
  end;
  Result := -1;
end;

initialization
  // DateToStr(Date, Fmt);          - ShortDateFormat
  // DateTimeToStr(DateTime, Fmt);  - ShortDateFormat + LongTimeFormat
  // StrToDate(S, Fmt);             - ShortDateFormat + DateSeparator
  // StrToDateTime(S, Fmt);         - ShortDateFormat + TimeSeparator

  // 02.04.1992 09:45:00
  GetLocaleFormatSettings(GetThreadLocale, LocalFmt);
  LocalFmt.DateSeparator := '.';
  LocalFmt.TimeSeparator := ':';
  LocalFmt.ShortDateFormat := 'dd.mm.yyyy';
  LocalFmt.LongTimeFormat := 'hh:nn:ss';

  Decimal := LocalFmt.DecimalSeparator;
end.

unit DiaryRoutines;

{ ������ ������� }

{$R+ O+}

interface

uses
  SysUtils,
  Classes,
  Forms {ProcessMessages},
  Windows {GetThreadLocale},
  uLkJSON;

type
  TStringArray = array of string;

  TIndexList = class
  private
    FData: array of integer;
  protected
    function GetItem(Index: integer): integer;
    procedure SetItem(Index, Value: integer);
  public
    procedure Add(Value: integer);
    procedure Clear;
    function Count: integer;
    procedure Init(Count: integer);
    procedure RemoveValue(Value: integer);
    procedure Swap(Index1, Index2: integer);

    property Items[Index: integer]: integer read GetItem write SetItem; default;
  end;

  TStringPair = record
    Key, Value: string;
  end;

  EKeyNotFoundException = class (Exception);

  TKeyValues = class
  protected
    FData: array of TStringPair;
    FCount: integer;
    function GetItem(Index: integer): TStringPair;
  public
    procedure Add(const Key, Value: string); virtual;
    procedure Clear();
    constructor Create;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property Count: integer read FCount;
    property Item[Index: integer]: TStringPair read GetItem;
  end;

  TStringMap = class (TKeyValues)
  protected
    function GetValue(Key: string): string; virtual; // TODO: introduce GetOrDefault() instead
    function IndexOf(const Key: string): integer;
  public
    procedure Add(const Key, Value: string); override;
    procedure AddAll(Map: TStringMap);
    function Contains(Key: string): boolean;
    function Values(): TStringArray;

    property Items[Key: string]: string read GetValue; default;
  end;

  TDate = type integer; // yao ming ;D

  TSwapClassProcedure = procedure(Index1, Index2: integer) of object;
  TMoreClassFunction = function(Index1, Index2: integer): boolean of object;
  TSwapSimpleProcedure = procedure(Index1, Index2: integer);
  TMoreSimpleFunction = function(Index1, Index2: integer): boolean;

  TGetterFunction = function(Index: integer): Variant of object;
  TCallbackProgress = procedure(Progress: integer);

  function ParseStringMap(const S: string): TStringMap;
  function StringPair(const Key, Value: string): TStringPair;

  { �������� }
  function ReadFile(const FileName: string): string;
  procedure WriteFile(const FileName: string; const Content: string = '');
  function FileSize(const FileName: string): Int64;

  { �������������� ����� : ����� }
  function CheckDot(const S: string): string;

  { �������������� ����� : boolean }
  function ReadBoolean(const S: string): boolean;
  function WriteBoolean(f: boolean): string;

  { �������������� ����� : ����� }
  function RealToStr(const X: real): string;
  function RealToStrZero(const X: real): string;
  function FloatToStrP(const X: real): string;

  { �������������� ����� : ����, ����� }
  function GetTimeUTC: TDateTime;
  function LocalToUTC(Time: TDateTime): TDateTime;
  function UTCToLocal(Time: TDateTime): TDateTime;
  function GetCurrentTime: string;
  function GetCurrentMinutes: integer; deprecated;
  function ExtractMinutes(Time: TDateTime): integer;
  function MTimeToStr(Time: integer; Sign: char): string; overload;
  function MTimeToStr(Time: integer): string; overload;
  function MTimeToStrColon(Time: integer): string;
  function TimeToMStr(Time: TDateTime): string; overload;
  function TimeSaveSecToStr(Time: integer): string;
  function TimeTrncSecToStr(Time: integer): string;
  function FormatDateTime(const Time: TDateTime): string;

  { ������� }
  function StrToTimeQuick(const Str: string): integer;
  function TryStrToTime(Str: string; var T: integer): boolean;
  function TryToCalculate(const S: string; var R: Extended): boolean;
  function ParseDateTime(const S: string): TDateTime;

  { �������� }
  function CorrectTime(Time: integer): boolean;
  function CheckPositiveFloat(const Value: string): boolean;
  function TestStrToFloat(const S: string): boolean;

  { ������ ������ �� �������� }
  function ChkSpace(const S: string): string;
  function EqStarts(const s1,s2: string; Count: integer): boolean;
  function ReadStrTo(const S: string; Terminal: char; var Caret: integer): string;
  function ReadFloatTo(const S: string; Terminal: char; var Caret: integer): real;
  function ReadIntTo(const S: string; Terminal: char; var Caret: integer): integer;
  function FmtArray(const S: string): TStringArray;
  function TextBefore(const S: string; Terminal: char): string;
  function TextAfter(const S: string; Terminal: char): string;
  function MakeSureJsonList(const S: string): string;
  function MatchStr(const S1, S2: string; IgnoreCase: boolean): boolean;
  procedure RemoveAll(var S: string; c: char);
  function ReplaceAll(const S, Find, Replace: string): string;
  procedure Separate(const S: string; out Before: string; Separator: Char; out After: string);
  procedure SeparateBack(const S: string; out Before: string; Separator: Char; out After: string);
  function StartsWith(const S: string; C: char): boolean; overload;
  function StartsWith(const S: string; Prefix: string): boolean; overload;
  function UppercaseFirst(const S: string): string;
  function Split(const S: string; Delimiter: char): TStringArray;
  function SwitchLanguage(s: String): String;
  function EscapeHtml(const S: String): String;

  { ������� ���������� }
  procedure QuickSort(Left, Right: integer; Swap: TSwapClassProcedure; More: TMoreClassFunction); overload;
  procedure QuickSort(Left, Right: integer; Swap: TSwapSimpleProcedure; More: TMoreSimpleFunction); overload;
  { ������� ����� }
  function BinarySearch(Value: Variant; Left, Right: integer; Getter: TGetterFunction): integer;

  procedure Wait(Time: cardinal);

var
  Decimal: char;

const
  SecPerMin     = 60; // TODO: ���� ����-���������. ���.
  MinPerHour    = 60;
  HourPerDay    = 24;
  MinPerDay     = MinPerHour * HourPerDay;
  HalfMinPerDay = MinPerDay div 2;
  SecPerHour    = SecPerMin * MinPerHour;
  SecPerDay     = SecPerMin * MinPerHour * HourPerDay;
  EPS           = 0.0000001;

  { ������� }
  ENERGY_PROTS = 4.1;
  ENERGY_FATS  = 9.3;
  ENERGY_CARBS = 4.1;

implementation

{ TIndexList }

{======================================================================================================================}
procedure TIndexList.Add(Value: integer);
{======================================================================================================================}
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)] := Value;
end;

{======================================================================================================================}
procedure TIndexList.Clear;
{======================================================================================================================}
begin
  SetLength(FData, 0);
end;

{======================================================================================================================}
function TIndexList.Count: integer;
{======================================================================================================================}
begin
  Result := Length(FData);
end;

{======================================================================================================================}
function TIndexList.GetItem(Index: integer): integer;
{======================================================================================================================}
begin
  Result := FData[Index];
end;

{======================================================================================================================}
procedure TIndexList.Init(Count: integer);
{======================================================================================================================}
var
  i: integer;
begin
  // �������������� ������ ������� 0..Count-1
  if (Count >= 0) then
  begin
    SetLength(FData, Count);
    for i := 0 to Count - 1 do
      FData[i] := i;
  end else
    raise Exception.CreateFmt('TIndexList().Init: illegal count (%d)', [Count]);
end;

{======================================================================================================================}
procedure TIndexList.RemoveValue(Value: integer);
{======================================================================================================================}
var
  i, w: integer;
begin
  { �������� (������������ ������������ ���������) }
  w := 0;
  for i := 0 to High(FData) do
  if (FData[i] <> Value) then
  begin
    FData[w] := FData[i];
    inc(w);
  end;
  SetLength(FData, w);

  { ������������� }
  for i := 0 to High(FData) do
  if (FData[i] > Value) then
    dec(FData[i]);
end;

{======================================================================================================================}
procedure TIndexList.SetItem(Index, Value: integer);
{======================================================================================================================}
begin
  FData[Index] := Value;
end;

{======================================================================================================================}
procedure TIndexList.Swap(Index1, Index2: integer);
{======================================================================================================================}
begin
  FData[Index1] := FData[Index1] xor FData[Index2];
  FData[Index2] := FData[Index1] xor FData[Index2];
  FData[Index1] := FData[Index1] xor FData[Index2];
end;

{ TStringPair }

{======================================================================================================================}
function StringPair(const Key, Value: string): TStringPair;
{======================================================================================================================}
begin
  Result.Key := Key;
  Result.Value := Value;
end;

{ TKeyValues }

{======================================================================================================================}
procedure TKeyValues.Add(const Key, Value: string);
{======================================================================================================================}
begin
  if (FCount = Length(FData)) then
  begin
    SetLength(FData, Length(FData) * 2 + 1);
  end;

  FData[FCount] := StringPair(Key, Value);
  inc(FCount);
end;

{======================================================================================================================}
procedure TKeyValues.Clear;
{======================================================================================================================}
begin
  SetLength(FData, 16);
  FCount := 0;
end;

{======================================================================================================================}
constructor TKeyValues.Create();
{======================================================================================================================}
begin
  Clear();
end;

{======================================================================================================================}
function TKeyValues.GetItem(Index: integer): TStringPair;
{======================================================================================================================}
begin
  Result := FData[Index];
end;

{======================================================================================================================}
procedure TKeyValues.LoadFromFile(const FileName: string);
{======================================================================================================================}
var
  i: integer;
  Key, Value: string;
begin
  Clear();

  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    for i := 0 to Count - 1 do
    begin
      Key := TrimRight(TextBefore(Strings[i], '='));
      Value := TrimLeft(TextAfter(Strings[i], '='));
      Self.Add(Key, Value);
    end;
    Free();
  end;
end;

{======================================================================================================================}
procedure TKeyValues.SaveToFile(const FileName: string);
{======================================================================================================================}
var
  i: integer;
begin
  with TStringList.Create do
  begin
    for i := 0 to FCount - 1 do
    begin
      Add(FData[i].Key + '=' + FData[i].Value);
    end;
    SaveToFile(FileName);
    Free();
  end;
end;

{ TStringMap }

{======================================================================================================================}
procedure TStringMap.Add(const Key, Value: string);
{======================================================================================================================}
var
  k: integer;
begin
  k := IndexOf(Key);
  if (k > -1) then
    FData[k].Value := Value
  else
    inherited Add(Key, Value);
end;

{======================================================================================================================}
procedure TStringMap.AddAll(Map: TStringMap);
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to Map.Count - 1 do
    Add(Map.FData[i].Key, Map.FData[i].Value);
end;

{======================================================================================================================}
function TStringMap.Contains(Key: string): boolean;
{======================================================================================================================}
begin
  Result := IndexOf(Key) > -1;
end;

{======================================================================================================================}
function TStringMap.GetValue(Key: string): string;
{======================================================================================================================}
var
  k: integer;
begin
  k := IndexOf(Key);
  if (k > -1) then
    Result := FData[k].Value
  else
    Result := '';
//    raise EKeyNotFoundException.CreateFmt('Key %s not found', [Key]);
end;

{======================================================================================================================}
function TStringMap.IndexOf(const Key: string): integer;
{======================================================================================================================}
var
  i: integer;
begin
  // TODO: make it binary
  for i := 0 to FCount - 1 do
  if (FData[i].Key = Key) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

{======================================================================================================================}
function TStringMap.Values(): TStringArray;
{======================================================================================================================}
var
  i: integer;
begin
  SetLength(Result, Length(FData));
  for i := 0 to FCount - 1 do
    Result[i] := FData[i].Value;
end;

{======================================================================================================================}
function ParseStringMap(const S: string): TStringMap;
{======================================================================================================================}
var
  Json: TlkJSONlist;
  Item: TlkJSONobject;
  Key, Value: string;
  i: integer;
begin
  Json := TlkJSON.ParseText(MakeSureJsonList(S)) as TlkJSONlist;
  try
    Result := TStringMap.Create;
    for i := 0 to json.Count - 1 do
    begin
      Item := json.Child[i] as TlkJSONobject;
      Key := Item.getString('key');
      Value := Item.GetString('value');
      Result.Add(Key, Value);
    end;
  finally
    Json.Free;
  end;
end;

{======================================================================================================================}
function ReadFile(const FileName: string): string;
{======================================================================================================================}
begin
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    Result := Text;
    Free;
  end;
end;

{======================================================================================================================}
procedure WriteFile(const FileName: string; const Content: string = '');
{======================================================================================================================}
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

{======================================================================================================================}
function FileSize(const FileName: string): Int64;
{======================================================================================================================}
var
  f: TFileStream;
begin
  try
    f := TFileStream.Create(FileName, fmOpenRead);
    Result := f.Size;
    f.Free;
  except
    Result := 0;
  end;
end;

{======================================================================================================================}
function CheckDot(const S: string): string;
{======================================================================================================================}
var
  n: integer;
begin
  Result := S;
  for n := 1 to length(Result) do
  if Result[n] in ['.', ','] then
    Result[n] := Decimal;
end;

{======================================================================================================================}
function ReadBoolean(const S: string): boolean;
{======================================================================================================================}
begin
  Result := (S = 'true');
end;

{======================================================================================================================}
function WriteBoolean(f: boolean): string;
{======================================================================================================================}
begin
  if (f) then
    Result := 'true'
  else
    Result := 'false';
end;

{======================================================================================================================}
function RealToStr(const X: real): string;
{======================================================================================================================}
begin
  // �� ����� ������ ���� ����� �������
  Result := FloatToStr(Round(x * 10) / 10);
end;

{======================================================================================================================}
function RealToStrZero(const X: real): string;
{======================================================================================================================}
begin
  // ����� ���� ���� ����� �������
  Result := Format('%.1f', [X]);
end;

{======================================================================================================================}
function FloatToStrP(const X: real): string;
{======================================================================================================================}
begin
  if X > 0 then
    Result := '+' + RealToStr(X)
  else
    Result := RealToStr(X);
end;

{======================================================================================================================}
function MTimeToStr(Time: integer; Sign: char): string;
{======================================================================================================================}
begin
  {if time<600 then Result := '0' else Result := '';
  Result := Result+IntToStr(time div 60)+Sign;
  if (time mod 60)<10 then Result := Result+'0';
  Result := Result+IntToStr(time mod 60); }
  Result := Format('%.2d' + Sign + '%.2d', [Time div 60, Time mod 60]);
end;

{======================================================================================================================}
function MTimeToStr(Time: integer): string;
{======================================================================================================================}
begin
  Result := MTimeToStr(Time, '.');
end;

{======================================================================================================================}
function MTimeToStrColon(Time: integer): string;
{======================================================================================================================}
begin
  Result := MTimeToStr(Time, ':');
end;

{======================================================================================================================}
function TimeToMStr(Time: TDateTime): string; overload;
{======================================================================================================================}
var
  Minutes: integer;
begin
  Minutes := ExtractMinutes(Time);
  Result := MTimeToStr(Minutes);
end;

{======================================================================================================================}
function StrToTimeQuick(const Str: string): integer;
{======================================================================================================================}
begin
  Result := 
    StrToInt(str[1]+str[2])*60+
    StrToInt(str[4]+str[5]);
end;

{======================================================================================================================}
function TryStrToTime(Str: string; var T: integer): boolean;
{======================================================================================================================}
var
  Sh,Sm: string;
  h,m: integer;
begin
  try
    { ��������, �������-����: ����� �����, � �� ����� � ������ }
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

{======================================================================================================================}
function TryToCalculate(const S: string; var R: Extended): boolean;
{======================================================================================================================}
var
  Op1, Op2: string;
  Val1, Val2: Extended;
begin
 // Result := False;

  // ���������� ������� ���������� (������ ���������������(?)) ��������������
  // � ������� ������ ��������� SeparateBack 

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

{======================================================================================================================}
function GetTimeUTC(): TDateTime;
{======================================================================================================================}
begin
  Result := LocalToUTC(Now());
end;

{======================================================================================================================}
function LocalToUTC(Time: TDateTime): TDateTime;
{======================================================================================================================}
var
  TimeZone: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TimeZone);
  Result := Time + TimeZone.Bias / MinPerDay;
end;

{======================================================================================================================}
function UTCToLocal(Time: TDateTime): TDateTime;
{======================================================================================================================}
var
  TimeZone: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TimeZone);
  Result := Time - TimeZone.Bias / MinPerDay;
end;

{======================================================================================================================}
function GetCurrentTime: string;
{======================================================================================================================}
var
  h,m,s,ms: word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := Format('%.2d.%.2d', [h, m]);
end;

{======================================================================================================================}
function GetCurrentMinutes: integer;
{======================================================================================================================}
var
  h,m,s,ms: word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := h * 60 + m;
end;

{======================================================================================================================}
function ExtractMinutes(Time: TDateTime): integer;
{======================================================================================================================}
begin
  Result := Round(Time * MinPerDay) mod MinPerDay;
end;

(*
{======================================================================================================================}
function FormatDate(Date: TDate): string;
{======================================================================================================================}
const
  Days: array[1..7]of string =
  ('��','��','��','��','��','��','��');
begin
  Result := DateToStr(date);
  Result := Copy(Result,1,length(Result)-5);
  Result := Result + ', ' + Days[DayOfWeek(date)];
end;  *)

{======================================================================================================================}
function TimeSaveSecToStr(Time: integer): string;
{======================================================================================================================}
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

{======================================================================================================================}
function TimeTrncSecToStr(Time: integer): string;
{======================================================================================================================}
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
{======================================================================================================================}
function Srv_StrToDateTime(const S: string): TDateTime;
{======================================================================================================================}
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
    raise Exception.Create('������ ���������� Srv_StrToDateTime');
  end;
end;

{======================================================================================================================}
function Srv_DateToStr(Date: TDate): string;
{======================================================================================================================}
begin
  try
    Result := DateToStr(Date, WebFmt);
  except
    raise Exception.Create('������ ���������� Srv_DateToStr');
  end;
end;
*)

function CorrectTime(Time: integer): boolean;
begin
  Result := (Time >= 0)and(Time < MinPerDay);
end;

{======================================================================================================================}
function CheckPositiveFloat(const Value: string): boolean;
{======================================================================================================================}
var
  Temp: double;
begin
  Result :=
    TryStrToFloat(CheckDot(Value), Temp) and
    (Temp > 0);
end;

{======================================================================================================================}
function ChkSpace(const S: string): string;
{======================================================================================================================}
const
  SPACE_CODE = '%20';
var
  k: integer;
begin
  Result := S;
  k := Pos(' ', Result);
  while (k > 0) do
  begin
    Result := Copy(Result, 1, k - 1) + SPACE_CODE + Copy(Result, k + 1, Length(Result) - k);
    k := Pos(' ', Result);
  end;
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
var
  n: integer;
begin
  n := pos(Terminal, S);
  if (n > 0) then
    Result := Copy(S, 1, n - 1)
  else
    Result := S;
end;

function TextAfter(const S: string; Terminal: char): string;
var
  n: integer;
begin
  n := pos(Terminal, S);
  if (n > 0) then
    Result := Copy(S, n + 1, Length(S) - n)
  else
    Result := S;
end;

{======================================================================================================================}
function MakeSureJsonList(const S: string): string;
{======================================================================================================================}
begin
  if (S <> '') and (S[1] = '{') and (S[Length(S)] = '}') then
    Result := '[' + S + ']'
  else
    Result := S;
end;

function MatchStr(const S1, S2: string; IgnoreCase: boolean): boolean;
begin
  if IgnoreCase then
    Result := (AnsiLowerCase(s1) = AnsiLowerCase(s2))
  else
    Result := (s1 = s2);
end;

procedure RemoveAll(var S: string; c: char);
var
  k: integer;
begin
  k := pos(c, S);
  while (k > 0) do
  begin
    Delete(S, k, 1);
    k := pos(c, S);
  end;
end;

function ReplaceAll(const S, Find, Replace: string): string;
var
  k: integer;
begin
  k := pos(Find, S);
  if (k > 0) then
  begin
    Result :=
      Copy(S, 1, k - 1) +
      Replace +
      ReplaceAll(Copy(S, k + Length(Find), Length(S) - Length(Find) - k + 1), Find, Replace);
  end else
  begin
    Result := S;
  end;
end;

procedure Separate(const S: string; out Before: string; Separator: Char; out After: string);
var
  k: integer;
  ResBefore, ResAfter: string;
begin
  k := pos(Separator, S);
  if (k = 0) then
  begin
    Before := S;
    After := '';
  end else
  begin
    // in case S == Before/After
    ResBefore := Copy(S, 1, k - 1);
    ResAfter := Copy(S, k + 1, Length(S) - k);
    Before := ResBefore;
    After := ResAfter;
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

{======================================================================================================================}
function StartsWith(const S: string; C: char): boolean;
{======================================================================================================================}
begin
  Result := (length(S) > 0) and (S[1] = C);
end;

{======================================================================================================================}
function StartsWith(const S: string; Prefix: string): boolean; overload;
{======================================================================================================================}
begin
  Result := (Length(S) >= Length(Prefix)) and (Copy(S, 1, Length(Prefix)) = Prefix);
end;

{======================================================================================================================}
function UppercaseFirst(const S: string): string;
{======================================================================================================================}
begin
  Result := S;
  if (Result <> '') then
    Result[1] := AnsiUppercase(Result[1])[1];
end;

{======================================================================================================================}
function Split(const S: string; Delimiter: char): TStringArray;
{======================================================================================================================}
var
  i, Start: integer;
begin
  Start := 1;
  SetLength(Result, 0);

  for i := 1 to Length(S) do
  begin
    if (S[i] = Delimiter) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Copy(S, Start, i - start);
      Start := i + 1;
    end;
  end;

  SetLength(Result, Length(Result) + 1);
  if (Start <= Length(S)) then
    Result[High(Result)] := Copy(S, Start, Length(S) - Start + 1)
  else
    Result[High(Result)] := '';
end;

{======================================================================================================================}
function SwitchLanguage(s: String): String;
{======================================================================================================================}
const
  CHARS_EN = 'f,dult`;pbqrkvyjghcnea[wxio]sm''.zF<DULT~:PBQRKVYJGHCNEA{WXIO}SM">Z';
  CHARS_RU = '�������������������������������������Ũ��������������������������';
var
  i, k: integer;
begin
  Result := '';

  for i := 1 to Length(S) do
  begin
    k := pos(S[i], CHARS_EN);
    if (k > 0) then
      Result := Result + CHARS_RU[k] else
    begin
      k := pos(S[i], CHARS_RU);
      if (k > 0) then
        Result := Result + CHARS_EN[k]
      else
        Result := Result + S[i];
    end;
  end;
end;

{======================================================================================================================}
function EscapeHtml(const S: String): String;
{======================================================================================================================}
begin
  Result := S;
  Result := ReplaceAll(Result, '%', '%25');
  Result := ReplaceAll(Result, '&', '%26');
end;

{======================================================================================================================}
procedure QuickSort(Left, Right: integer; Swap: TSwapClassProcedure; More: TMoreClassFunction); overload;
{======================================================================================================================}

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
          { ����������, ���� X - ��� ������ }
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

{======================================================================================================================}
procedure QuickSort(Left, Right: integer; Swap: TSwapSimpleProcedure; More: TMoreSimpleFunction); overload;
{======================================================================================================================}

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
          { ����������, ���� X - ��� ������ }
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

procedure Wait(Time: cardinal);
begin
  Time := GetTickCount() + Time;
  while (GetTickCount() < Time) do
  begin
    sleep(20);
    Application.ProcessMessages;
  end;
end;

var
  Temp: TFormatSettings;
  STD_DATETIME_FMT: TFormatSettings;
  
{======================================================================================================================}
function FormatDateTime(const Time: TDateTime): string;
{======================================================================================================================}
begin
  DateTimeToString(Result, 'yyyy-mm-dd hh:nn:ss', Time);
end;

{======================================================================================================================}
function ParseDateTime(const S: string): TDateTime;
{======================================================================================================================}
begin
  Result := StrToDateTime(S, STD_DATETIME_FMT);
end;

initialization
  // DateToStr(Date, Fmt);          - ShortDateFormat
  // DateTimeToStr(DateTime, Fmt);  - ShortDateFormat + LongTimeFormat
  // StrToDate(S, Fmt);             - ShortDateFormat + DateSeparator
  // StrToDateTime(S, Fmt);         - ShortDateFormat + TimeSeparator

  GetLocaleFormatSettings(GetThreadLocale, Temp);
  Decimal := Temp.DecimalSeparator;

  // 1992-04-02 09:45:00
  GetLocaleFormatSettings(GetThreadLocale, STD_DATETIME_FMT);
  STD_DATETIME_FMT.DateSeparator := '-';
  STD_DATETIME_FMT.TimeSeparator := ':';
  STD_DATETIME_FMT.ShortDateFormat := 'yyyy-mm-dd';
  STD_DATETIME_FMT.LongTimeFormat := 'hh:nn:ss';
end.

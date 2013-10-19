unit DiaryPage;

interface

uses 
  SysUtils, // Exception
  Classes, // TStrings
  DiaryRoutines, // TDate
  DiaryRecords,
  BusinessObjects,
  DiaryPageSerializer {TODO: remove dependency};

type
  { 2. ДНЕВНИК }

  TRecordsList = array of TCustomRecord; // слава полиморфизму!

  TDiaryPage = class;

  TEventPageChanged = procedure(EventType: TPageEventType; Page: TDiaryPage; RecClass: TClassCustomRecord; RecInstance: TCustomRecord) of object;

  TDiaryPage = class
  private
    FDate: TDate;
    {+}FRecs: TRecordsList;
    {+}FSilentChange: boolean;
    FTimeStamp: TDateTime;
    FVersion: integer;

    { события }
    FOnChange: array of TEventPageChanged;

    {+}procedure CheckIndex(Index: integer);
    {+}function GetRecord(Index: integer): TCustomRecord;
    {+}function GetStat(Index: integer): real;
    {#}procedure ProcessRecordChanged(RecInstance: TCustomRecord); overload;
    {#}procedure ProcessRecordChanged(EventType: TPageEventType; RecType: TClassCustomRecord; RecInstance: TCustomRecord = nil); overload;
    {+}function Trace(Index: integer): integer;
    {+}function TraceLast: integer;
  public
    {+}function Add(Rec: TCustomRecord): integer;
    {+}procedure Clear;
    {+}function Count: integer;
    constructor Create(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer); overload;
    constructor Create(PageData: TPageData); overload;
    destructor Destroy; override;
    {+}procedure Remove(Index: integer; AutoFree: boolean = True);

    // сахар
    {+}function FindRecordFirst(RecType: TClassCustomRecord): TCustomRecord;
    {+}function FindRecordLast(RecType: TClassCustomRecord): TCustomRecord;
    {+}function FindRecord(Rec: TCustomRecord): integer; overload;

    // I/O

    procedure ReadFrom(S: TStrings); overload;
    procedure WriteTo(S: TStrings); overload;

    procedure ReadFrom(const S: string); overload;
    procedure WriteTo(out S: string); overload;

    procedure ReadFrom(S: TPageData); overload;
    procedure WriteTo(S: TPageData); overload;

    // Listeners

    procedure AddChangeListener(Listener: TEventPageChanged);

    // свойства

    property Date: TDate read FDate;
    property TimeStamp: TDateTime read FTimeStamp;
    property Version: integer read FVersion;

    {+}property Recs[Index: integer]: TCustomRecord read GetRecord; default;
    {+}property DayProts: real index 1 read GetStat;
    {+}property DayFats:  real index 2 read GetStat;
    {+}property DayCarbs: real index 3 read GetStat;
    {+}property DayValue: real index 4 read GetStat;
    {+}property DayMass:  real index 5 read GetStat;
    {+}property DayIns:   real index 6 read GetStat;
  end;

implementation

{ TDiaryPage }

{==============================================================================}
function TDiaryPage.Add(Rec: TCustomRecord): integer;
{==============================================================================}
begin
  if (Rec = nil) then
    raise Exception.Create('Record can''t be nil');

  Result := Length(FRecs);
  SetLength(FRecs, Result + 1);
  FRecs[Result] := Rec;
  Rec.OnChange := ProcessRecordChanged;
  Result := TraceLast;

  //{#}Changed({FRecs[Result]}Rec);
  ProcessRecordChanged(etAdd, Rec.RecType, Rec);
end;

{==============================================================================}
procedure TDiaryPage.AddChangeListener(Listener: TEventPageChanged);
{==============================================================================}

  function GetListenerIndex(L: TEventPageChanged): integer;
  var
    i: integer;
  begin
    for i := 0 to High(FOnChange) do
    if (@FOnChange[i] = @L) then
    begin
      Result := i;
      Exit;
    end;
    Result := -1;
  end;

begin
  if (Assigned(Listener)) then
  begin
    if (GetListenerIndex(Listener) = -1) then
    begin
      SetLength(FOnChange, Length(FOnChange) + 1);
      FOnChange[High(FOnChange)] := Listener;
    end;
  end;
end;

{==============================================================================}
procedure TDiaryPage.CheckIndex(Index: integer);
{==============================================================================}
begin
  if (Index < Low(FRecs)) or (Index > High(FRecs)) then
    raise ERangeError.CreateFmt('TDiaryPage: недопустимый индекс (%d)', [Index]);
end;

// используется при загрузке и при уничтожении
{==============================================================================}
procedure TDiaryPage.Clear;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FRecs) do
    FRecs[i].Free;
  SetLength(FRecs, 0);

  //{#}Changed(nil);
  //Changed(etRemove, nil);
  // TODO: нужен ли Changed? Когда вызывается Clear?
end;

{==============================================================================}
function TDiaryPage.Count: integer;
{==============================================================================}
begin
  Result := Length(FRecs);
end;

{==============================================================================}
constructor TDiaryPage.Create(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer);
{==============================================================================}
begin
  FOnChange := nil;

  FDate := ADate;
  FTimeStamp := ATimeStamp;
  FVersion := AVersion;

  FSilentChange := False;
end;

{==============================================================================}
constructor TDiaryPage.Create(PageData: TPageData);
{==============================================================================}
begin
  FOnChange := nil;

  //FSilentChange := True;
  ReadFrom(PageData);

  FSilentChange := False;
end;

{==============================================================================}
destructor TDiaryPage.Destroy;
{==============================================================================}
begin
  FOnChange := nil;
  FSilentChange := True;
  Clear;
end;

{==============================================================================}
function TDiaryPage.FindRecord(Rec: TCustomRecord): integer;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FRecs) do
  if (FRecs[i] = Rec) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

{==============================================================================}
function TDiaryPage.FindRecordFirst(RecType: TClassCustomRecord): TCustomRecord;
{==============================================================================}
var
  i: integer;
begin
  Result := nil;
  for i := 0 to High(FRecs) do
  if (FRecs[i].RecType = RecType) then
  begin
    Result := FRecs[i];
    Exit;
  end;
end;

{==============================================================================}
function TDiaryPage.GetRecord(Index: integer): TCustomRecord;
{==============================================================================}
begin
  // TODO: just debug
  if (Index < Low(FRecs)) or (Index > High(FRecs)) then
    raise ERangeError.Create('GetRecord: index "' + IntToStr(index) + '" is out of bounds [0; ' + IntToStr(High(FRecs)) + ']')
  else
    Result := FRecs[Index];
end;

{==============================================================================}
function TDiaryPage.GetStat(Index: integer): real;
{==============================================================================}
var
  i: integer;
begin
  {
    1 FDayProts: real;
    2 FDayFats: real;
    3 FDayCarbs: real;
    4 FDayValue: real;
    5 FDayMass: real;
    6 FDayIns: real;
  }
  Result := 0;
  case Index of
    1: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Prots;
    2: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Fats;
    3: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Carbs;
    4: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Value;
    5: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Mass;
    6: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TInsRecord)  then Result := Result + TInsRecord(FRecs[i]).Value;
  end;
end;

{==============================================================================}
function TDiaryPage.FindRecordLast(RecType: TClassCustomRecord): TCustomRecord;
{==============================================================================}
var
  i: integer;
begin
  Result := nil;
  for i := High(FRecs) downto 0 do
  if (FRecs[i].RecType = RecType) then
  begin
    Result := FRecs[i];
    Exit;
  end;
end;

// TODO: эта процедура должна вызываться только мелкими изменениями пользователя
// избегать вызова её при загрузке данных
{==============================================================================}
procedure TDiaryPage.ProcessRecordChanged(EventType: TPageEventType; RecType: TClassCustomRecord;
  RecInstance: TCustomRecord = nil);
{==============================================================================}
var
  Index: integer;
  i: integer;
begin
  if FSilentChange then Exit;

  // обновляем печатьку
  FTimeStamp := Now;
  inc(FVersion);

  // принимаем коррекционные меры, если нужно
  if (RecInstance <> nil) then
  begin
    Index := FindRecord(RecInstance);
    if (Index <> -1) then
    begin
      Trace(Index);
      // TODO: etc.
    end;
  end;

  // информируем слушателей (ПОСЛЕ коррекций)

  for i := 0 to High(FOnChange) do
   FOnChange[i](EventType, Self, RecType, RecInstance);
end;

{==============================================================================}
procedure TDiaryPage.ProcessRecordChanged(RecInstance: TCustomRecord);
{==============================================================================}
begin
  ProcessRecordChanged(etModify, RecInstance.RecType, RecInstance);
end;

{==============================================================================}
procedure TDiaryPage.ReadFrom(S: TStrings);
{==============================================================================}
var
  i,k: integer;
  CurStr: string;
  TempStr: string;
  TempTime: integer;
  TempValue: real;
  TempFinger: integer;
  TempShort: boolean;
  Meal: TMealRecord;
  TempFood: TFoodMassed;
begin
  with Self do
  begin
    // TODO: протестировать скорость загрузки дневника из XML
    //Log('TDiaryPage.ReadFrom() started');

    if S = nil then
    begin
      //Log('TDiaryPage.ReadFrom() error: S=nil');
      raise Exception.Create('TDiaryPage.ReadFrom(): поток для чтения не может быть nil');
    end;

    FSilentChange := True;

    try
      Clear;
      Meal := nil;

      for i := 0 to S.Count - 1 do
      if (S[i] <> '') then
      begin
        CurStr := S[i];
        case CurStr[1] of
          '*':
          begin
            TempTime := StrToTimeQuick(Copy(CurStr,2,5));

            k := pos('|', curStr);
            if k > 0 then
            begin
              TempValue := StrToFloat(CheckDot( Copy(CurStr, 8, k-8) ));
              TempFinger := StrToInt( Copy(CurStr, k+1, Length(CurStr)-k) );
            end else
            begin
              TempValue := StrToFloat(CheckDot(Copy(CurStr,8,Length(CurStr)-7)));
              TempFinger := -1;
            end;                                                            
            Add(TBloodRecord.Create(TempTime, TempValue, TempFinger));
          end;
          '-':
          begin
            TempTime := StrToTimeQuick(Copy(CurStr,2,5));
            TempValue := StrToFloat(CheckDot(Copy(CurStr,8,Length(CurStr)-7)));
            Add(TInsRecord.Create(TempTime, TempValue));
          end;
          ' ':
          begin
            TempTime := StrToTimeQuick(Copy(CurStr,2,5));
            TempShort := (CurStr[Length(CurStr)] = 's');
            Meal := TMealRecord.Create(TempTime, TempShort); // save it for further modifications
            Add(Meal);
          end;
          '#':
          begin
            if (Meal <> nil) then
            begin
              TempFood := TFoodMassed.Create();
              TempFood.Read(Copy(CurStr, 2, Length(CurStr) - 1));
              Meal.Add(TempFood);
            end;
          end;
          '%':
          begin
            TempTime := StrToTimeQuick(Copy(CurStr, 2, 5));
            TempStr := Copy(CurStr, 8, Length(CurStr) - 7);
            Add(TNoteRecord.Create(TempTime, TempStr));
          end;
          {else
            // и что, из-за одного символа вся база полетит?
            raise ELoadingError.Create('TDiaryPage.ReadFrom: Некорректные данные'#13+
              'Строка :'+#13+
              CurStr);   }
        end;
      end;
    finally
      //Log('TDiaryPage.ReadFrom() finished');
      //FUpdateStampOnChange := True;
    end;
    //Log('TDiaryPage.ReadFrom() done ok');

    FSilentChange := False;
  end;
end;

{==============================================================================}
procedure TDiaryPage.ReadFrom(const S: string);
{==============================================================================}
var
  Temp: TStringList;
begin
  Temp := TStringList.Create;
  try
    Temp.Text := S;
    ReadFrom(Temp);
  finally
    Temp.Free;
  end;
end;

{==============================================================================}
procedure TDiaryPage.ReadFrom(S: TPageData);
{==============================================================================}
begin
  FDate := S.Date;
  FTimeStamp := S.TimeStamp;
  FVersion := S.Version;
  ReadFrom(S.Page);


  {

  Switch: boolean;

  ------

  Switch := Switch and (not SilentMode);
  SilentMode := SilentMode or Switch;

  ...

  SilentMode := SilentMode and (not Switch);

  ------

  OldMode := SilentMode;
  if Switch then SilentMode := True;

  ...

  SilentMode := OldMode;

  }
end;

{==============================================================================}
procedure TDiaryPage.Remove(Index: integer; AutoFree: boolean = True);
{==============================================================================}
var
  i: integer;
  T: TClassCustomRecord;
begin
  CheckIndex(Index);
  T := TClassCustomRecord(FRecs[Index].ClassType);

  if AutoFree then
    FRecs[Index].Free;

  for i := Index to High(FRecs) - 1 do
    FRecs[i] := FRecs[i + 1];
  SetLength(FRecs, Length(FRecs) - 1);

  {#}ProcessRecordChanged(etRemove, T);
end;

{==============================================================================}
function TDiaryPage.Trace(Index: integer): integer;
{==============================================================================}
var
  Temp: TCustomRecord;
  Changed: boolean;
begin
  Result := Index;
  if (Index >= 0)and(Index <= High(FRecs)) then
  begin
    Temp := Recs[Result];
    Changed := False;

    { прогон вверх }
    while (Result > 0)and(FRecs[Result - 1].Time > Temp.Time) do
    begin
      FRecs[Result] := FRecs[Result - 1];
      dec(Result);
      Changed := True;
    end;

    { прогон вниз }
    while (Result < High(FRecs))and(FRecs[Result + 1].Time < Temp.Time) do
    begin
      FRecs[Result] := FRecs[Result + 1];
      inc(Result);
      Changed := True;
    end;

    { запись }
    if Changed then
      FRecs[Result] := Temp;
  end;
end;

{==============================================================================}
function TDiaryPage.TraceLast: integer;
{==============================================================================}
{var
  temp: TCustomRecord;
  Changed: boolean; }
begin
  (*
  Result := High(FRecs);
  if (Index >= 0)and(Index <= High(FRecs)) then
  begin
    Temp := Recs[Result];
    Changed := False;

    { прогон вверх }
    while (Result > 0)and(FRecs[Result-1].Time > Temp.Time) do
    begin
      FRecs[Result] := FRecs[Result-1];
      dec(Result);
      Changed := True;
    end;

    { запись }
    if Changed then
      FRecs[Result] := Temp;
  end;   *)

  Result := Trace(High(FRecs));
end;

{==============================================================================}
procedure TDiaryPage.WriteTo(S: TStrings);
{==============================================================================}
var
  j, n: integer;
begin
  if (S = nil) then
    raise Exception.Create('TDiaryPage.WriteTo(): поток для записи не может быть nil');

  for j := 0 to Count - 1 do
  begin
    if (FRecs[j].RecType = TBloodRecord) then
    begin
      // TODO: use Format() instead
      s.Add(
        '*' + TimeToStr(FRecs[j].Time) +
        ' ' + FloatToStr(TBloodRecord(FRecs[j]).Value) +
        '|' + IntToStr(TBloodRecord(FRecs[j]).Finger)
      )
    end else

    if (FRecs[j].RecType = TInsRecord) then
    begin
      s.Add(
        '-' + TimeToStr(FRecs[j].Time) +
        ' ' + FloatToStr(TInsRecord(FRecs[j]).Value)
      );
    end else

    if (FRecs[j].RecType = TMealRecord) then
    begin
      if TMealRecord(FRecs[j]).ShortMeal then
        s.Add(' ' + TimeToStr(FRecs[j].Time) + 's')
      else
        s.Add(' ' + TimeToStr(FRecs[j].Time));

      for n := 0 to TMealRecord(FRecs[j]).Count - 1 do
        s.Add('#' + TMealRecord(FRecs[j])[n].Write);
    end else

    if (FRecs[j].RecType = TNoteRecord) then
    begin
      s.Add(
        '%' + TimeToStr(FRecs[j].Time) +
        ' ' + TNoteRecord(FRecs[j]).Text
      );
    end;
  end;
end;

{==============================================================================}
procedure TDiaryPage.WriteTo(out S: string);
{==============================================================================}
var
  Temp: TStringList;
begin
  Temp := TStringList.Create;
  try
    WriteTo(Temp);
    S := Temp.Text;

   { if (S <> '') and (S[Length(S)] = #13) then
      Delete(S, Length(S), 1);}
  finally
    Temp.Free;
  end;
end;

{==============================================================================}
procedure TDiaryPage.WriteTo(S: TPageData);
{==============================================================================}
begin
  S.Date := FDate;
  S.TimeStamp := FTimeStamp;
  S.Version := FVersion;
  WriteTo(S.Page);
end;

end.

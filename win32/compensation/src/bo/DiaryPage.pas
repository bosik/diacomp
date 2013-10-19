unit DiaryPage;

interface

uses 
  SysUtils, // Exception
  DiaryRoutines, // TDate
  DiaryRecords;

type
  { 2. ДНЕВНИК }

  TRecordsList = array of TCustomRecord; // слава полиморфизму!

  TDiaryPage = class;

  TEventPageChanged = procedure(EventType: TPageEventType; Page: TDiaryPage; RecClass: TClassCustomRecord; RecInstance: TCustomRecord) of object;

  TDiaryPage = class
  private
    FDate: TDate;
    FTimeStamp: TDateTime;
    FVersion: integer;
    {+}FRecs: TRecordsList;
    {+}FSilentChange: boolean;

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
    constructor Create;
    destructor Destroy; override;
    {+}procedure Remove(Index: integer; AutoFree: boolean = True);

    // сахар
    {+}function FindRecordFirst(RecType: TClassCustomRecord): TCustomRecord;
    {+}function FindRecordLast(RecType: TClassCustomRecord): TCustomRecord;
    {+}function FindRecord(Rec: TCustomRecord): integer; overload;
    
    // Listeners
    procedure AddChangeListener(Listener: TEventPageChanged);

    // свойства
    property Date: TDate read FDate write FDate;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property Version: integer read FVersion write FVersion;
    property Recs[Index: integer]: TCustomRecord read GetRecord; default;
    property DayProts: real index 1 read GetStat;
    property DayFats:  real index 2 read GetStat;
    property DayCarbs: real index 3 read GetStat;
    property DayValue: real index 4 read GetStat;
    property DayMass:  real index 5 read GetStat;
    property DayIns:   real index 6 read GetStat;
    property SilentChange: boolean read FSilentChange write FSilentChange;
  end;

  TDiaryPageList = array of TDiaryPage;

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
constructor TDiaryPage.Create();
{==============================================================================}
begin
  FOnChange := nil;
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

end.

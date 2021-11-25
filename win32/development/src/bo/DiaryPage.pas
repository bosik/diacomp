unit DiaryPage;

interface

uses 
  SysUtils, // Exception
  Math, // Max
  DiaryRoutines, // TDate
  DiaryRecords,
  BusinessObjects;

type
  { 2. ДНЕВНИК }

  TDiaryPage = class;

  TEventPageChanged = procedure(EventType: TPageEventType; Page: TDiaryPage; RecClass: TClassCustomRecord; RecInstance: TVersioned) of object;

  TDiaryPage = class
  private
    FDate: TDate;
    FTimeStamp: TDateTime;
    FVersion: integer;
    {+}FRecs: TVersionedList;
    {+}FSilentChange: boolean;

    FFreeTime: integer;

    FStdMealPeriod: integer;
    FShortMealPeriod: integer;
    FInsPeriod: integer;

    { события }
    FOnChange: array of TEventPageChanged;

    {+}procedure CheckIndex(Index: integer);
    {+}function GetRecord(Index: integer): TVersioned;
    {+}function GetStat(Index: integer): real;
    {#}procedure ProcessRecordChanged(RecInstance: TVersioned); overload;
    {#}procedure ProcessRecordChanged(EventType: TPageEventType; RecType: TClassCustomRecord; RecInstance: TVersioned = nil); overload;
    {+}function Trace(Index: integer): integer;
    {+}function TraceLast: integer;
  public
    {+}function Add(Rec: TVersioned): integer;
    {+}procedure Clear;
    {+}function Count: integer;
    constructor Create; overload;
    constructor Create(Copy: TDiaryPage); overload; deprecated;
    destructor Destroy; override;
    {+}procedure Remove(Index: integer; AutoFree: boolean = True);

    // сахар
    {+}function FindRecordFirst(RecType: TClassCustomRecord): TVersioned;
    {+}function FindRecordLast(RecType: TClassCustomRecord): TVersioned;
    {+}function FindRecord(Rec: TVersioned): integer; overload; // TODO: does direct comparison, check if it's intended
    
    // Listeners
    procedure AddChangeListener(Listener: TEventPageChanged);
    
    // свойства
    property Date: TDate read FDate write FDate;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property Version: integer read FVersion write FVersion;
    property Recs[Index: integer]: TVersioned read GetRecord; default;
    property DayProts: real index 1 read GetStat;
    property DayFats:  real index 2 read GetStat;
    property DayCarbs: real index 3 read GetStat;
    property DayValue: real index 4 read GetStat;
    property DayMass:  real index 5 read GetStat;
    property DayIns:   real index 6 read GetStat;
    property SilentChange: boolean read FSilentChange write FSilentChange;

    property FreeTime: integer read FFreeTime write FFreeTime; // время, когда замеры перестают быть постпрандиальными
    property PostPrandMealStd: integer   read FStdMealPeriod   write FStdMealPeriod;
    property PostPrandMealShort: integer read FShortMealPeriod write FShortMealPeriod;
    property PostPrandIns: integer       read FInsPeriod       write FInsPeriod;
  end;

implementation

{ TDiaryPage }

const
  INITIAL_PAGE_VERSION = 0;

{======================================================================================================================}
function TDiaryPage.Add(Rec: TVersioned): integer;
{======================================================================================================================}
var
  R: TCustomRecord;
begin
  if (Rec = nil) then
    raise Exception.Create('Record can''t be nil');

  R := Rec.Data as TCustomRecord;

  Result := Length(FRecs);
  SetLength(FRecs, Result + 1);
  FRecs[Result] := Rec;
  Result := TraceLast;

  //{#}Changed({FRecs[Result]}Rec);
  ProcessRecordChanged(etAdd, R.RecType, Rec);
end;

{======================================================================================================================}
procedure TDiaryPage.AddChangeListener(Listener: TEventPageChanged);
{======================================================================================================================}

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

{======================================================================================================================}
procedure TDiaryPage.CheckIndex(Index: integer);
{======================================================================================================================}
begin
  if (Index < Low(FRecs)) or (Index > High(FRecs)) then
    raise ERangeError.CreateFmt('TDiaryPage: недопустимый индекс (%d)', [Index]);
end;

// используется при загрузке и при уничтожении
{======================================================================================================================}
procedure TDiaryPage.Clear;
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to High(FRecs) do
    FreeAndNil(FRecs[i]);
  SetLength(FRecs, 0);

  //{#}Changed(nil);
  //Changed(etRemove, nil);
  // TODO: нужен ли Changed? Когда вызывается Clear?
end;

{======================================================================================================================}
function TDiaryPage.Count: integer;
{======================================================================================================================}
begin
  Result := Length(FRecs);
end;

{======================================================================================================================}
constructor TDiaryPage.Create();
{======================================================================================================================}
begin
  FOnChange := nil;
  FSilentChange := False;
  FVersion := INITIAL_PAGE_VERSION;
end;

{======================================================================================================================}
constructor TDiaryPage.Create(Copy: TDiaryPage);
{======================================================================================================================}
//var
//  i: integer;
begin
  raise Exception.Create('Method TDiaryPage.Create(Copy) is deprecated');

  {FOnChange := nil;
  FSilentChange := False;

  if (Copy = nil) then
    raise Exception.Create('Base diary page can''t be nil');

  // TODO: deep copy content here

  for i := 0 to Copy.Count - 1 do
  begin
    TBloodRecord
  end;

  Date := Copy.Date;
  TimeStamp := Copy.TimeStamp;
  Version := Copy.Version;  }
end;

{======================================================================================================================}
destructor TDiaryPage.Destroy;
{======================================================================================================================}
begin
  FOnChange := nil;
  FSilentChange := True;
  Clear;
end;

{======================================================================================================================}
function TDiaryPage.FindRecord(Rec: TVersioned): integer;
{======================================================================================================================}
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

{======================================================================================================================}
function TDiaryPage.FindRecordFirst(RecType: TClassCustomRecord): TVersioned;
{======================================================================================================================}
var
  i: integer;
begin
  Result := nil;
  for i := 0 to High(FRecs) do
  if (TCustomRecord(FRecs[i].Data).RecType = RecType) then
  begin
    Result := FRecs[i];
    Exit;
  end;
end;

{======================================================================================================================}
function TDiaryPage.GetRecord(Index: integer): TVersioned;
{======================================================================================================================}
begin
  // TODO: just debug
  if (Index < Low(FRecs)) or (Index > High(FRecs)) then
    raise ERangeError.Create('GetRecord: index "' + IntToStr(index) + '" is out of bounds [0; ' + IntToStr(High(FRecs)) + ']')
  else
    Result := FRecs[Index];
end;

{======================================================================================================================}
function TDiaryPage.GetStat(Index: integer): real;
{======================================================================================================================}
var
  Item: TCustomRecord;
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

  for i := 0 to High(FRecs) do
  begin
    Item := TCustomRecord(FRecs[i].Data);

    case Index of
      1: if (Item.RecType = TMealRecord) then Result := Result + TMealRecord(Item).Prots;
      2: if (Item.RecType = TMealRecord) then Result := Result + TMealRecord(Item).Fats;
      3: if (Item.RecType = TMealRecord) then Result := Result + TMealRecord(Item).Carbs;
      4: if (Item.RecType = TMealRecord) then Result := Result + TMealRecord(Item).Value;
      5: if (Item.RecType = TMealRecord) then Result := Result + TMealRecord(Item).Mass;
      6: if (Item.RecType = TInsRecord)  then Result := Result + TInsRecord(Item).Value;
    end;
  end;
end;

{======================================================================================================================}
function TDiaryPage.FindRecordLast(RecType: TClassCustomRecord): TVersioned;
{======================================================================================================================}
var
  i: integer;
begin
  Result := nil;
  for i := High(FRecs) downto 0 do
  if (TCustomRecord(FRecs[i].Data).RecType = RecType) then
  begin
    Result := FRecs[i];
    Exit;
  end;
end;

// TODO: эта процедура должна вызываться только мелкими изменениями пользователя
// избегать вызова её при загрузке данных
{======================================================================================================================}
procedure TDiaryPage.ProcessRecordChanged(EventType: TPageEventType; RecType: TClassCustomRecord;
  RecInstance: TVersioned = nil);
{======================================================================================================================}
var
  Index: integer;
  i: integer;
begin
  if FSilentChange then Exit;

  // обновляем печатьку
  FTimeStamp := GetTimeUTC();  
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

  UpdatePostprand(FRecs, FInsPeriod, FStdMealPeriod, FShortMealPeriod);

  // информируем слушателей (ПОСЛЕ коррекций)
  for i := 0 to High(FOnChange) do
    FOnChange[i](EventType, Self, RecType, RecInstance);
end;

{======================================================================================================================}
procedure TDiaryPage.ProcessRecordChanged(RecInstance: TVersioned);
{======================================================================================================================}
begin
  ProcessRecordChanged(etModify, TCustomRecord(RecInstance.Data).RecType, RecInstance);
end;

{======================================================================================================================}
procedure TDiaryPage.Remove(Index: integer; AutoFree: boolean = True);
{======================================================================================================================}
var
  i: integer;
  T: TClassCustomRecord;
begin
  CheckIndex(Index);
  T := TClassCustomRecord(FRecs[Index].ClassType);

  if AutoFree then
    FreeAndNil(FRecs[Index]);

  for i := Index to High(FRecs) - 1 do
    FRecs[i] := FRecs[i + 1];
  SetLength(FRecs, Length(FRecs) - 1);

  {#}ProcessRecordChanged(etRemove, T);
end;

{======================================================================================================================}
function TDiaryPage.Trace(Index: integer): integer;
{======================================================================================================================}

  procedure Swap(var A, B: TVersioned);
  var
    C: TVersioned;
  begin
    C := A;
    A := B;
    B := C;
  end;

  function GetData(i: integer): TCustomRecord;
  begin
    Result := TCustomRecord(FRecs[i].Data);
  end;

begin
  Result := Index;

  { прогон вверх }
  while (Result > 0) and (GetData(Result - 1).Time > GetData(Result).Time) do
  begin
    Swap(FRecs[Result], FRecs[Result - 1]);
    dec(Result);
  end;

  { прогон вниз }
  while (Result < High(FRecs)) and (GetData(Result + 1).Time < GetData(Result).Time) do
  begin
    Swap(FRecs[Result], FRecs[Result + 1]);
    inc(Result);
  end;
end;

{======================================================================================================================}
function TDiaryPage.TraceLast: integer;
{======================================================================================================================}
begin
  Result := Trace(High(FRecs));
end;

end.

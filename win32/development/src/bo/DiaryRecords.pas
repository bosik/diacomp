unit DiaryRecords;

interface

uses
  Math,
  BusinessObjects,
  DiaryRoutines,
  SysUtils,
  Bases;

type
  TCustomRecord = class;

  TClassCustomRecord = class of TCustomRecord;

  TEventRecordChanged = procedure(Sender: TCustomRecord) of object;

  // #entity
  TCustomRecord = class (TVersioned)
  private
    FTime: TDateTime;               // UTC
  protected
    function GetTime(): integer;
    procedure SetNativeTime(Value: TDateTime);
  public
    function RecType: TClassCustomRecord;

    property Time: integer read GetTime;
    property NativeTime: TDateTime read FTime write SetNativeTime;
  end;

  TRecordList = array of TCustomRecord;

  TPageEventType = (etAdd, etModify, etRemove);

  // #entity
  TBloodRecord = class(TCustomRecord)
  private
    FValue: real;
    FFinger: integer;
    FPostPrand: boolean;  // transient
  protected
    procedure SetFinger(NewFinger: integer);
    procedure SetValue(const NewValue: real);
    class function CheckFinger(TestFinger: integer): boolean;
    class function CheckValue(const TestValue: real): boolean;
  public
    constructor Create(ATime: TDateTime = 0; AValue: real = -1; AFinger: integer = -1);
    property Finger: integer read FFinger write SetFinger;
    property Value: real read FValue write SetValue;
    property PostPrand: boolean read FPostPrand write FPostPrand;
  end;

  // #entity
  TInsRecord = class(TCustomRecord)
  private
    FValue: real;
  protected
    procedure SetValue(const NewValue: real);
    class function CheckValue(const Value: real): boolean;
  public
    constructor Create(ATime: TDateTime = 0; AValue: real = -1);
    property Value: real read FValue write SetValue;
  end;

  // #entity
  TMealRecord = class(TCustomRecord)
  private
    FFood: array of TFoodMassed;
    FShortMeal: boolean;
    procedure CheckIndex(Index: integer);
    function GetFood(Index: integer): TFoodMassed;
    function GetProp(Index: integer): real;
    procedure SetShortMeal(Value: boolean);
  public
    function Add(Food: TFoodMassed): integer;
    constructor Create(ATime: TDateTime = 0; AShortMeal: boolean = False);
    procedure Clear;
    destructor Destroy; override;
    procedure Exchange(Index1, Index2: integer);
    procedure Remove(Index: integer);
    function Count: integer;

    property Food[Index: integer]: TFoodMassed read GetFood; default;
    property Mass:  real index 0 read GetProp;
    property Prots: real index 1 read GetProp;
    property Fats:  real index 2 read GetProp;
    property Carbs: real index 3 read GetProp;
    property Value: real index 4 read GetProp;
    property ShortMeal: boolean read FShortMeal write SetShortMeal;
  end;

  // #entity
  TNoteRecord = class(TCustomRecord)
  private
    FText: string;
  protected
    procedure SetText(const Value: string);
  public
    constructor Create(ATime: TDateTime = 0; AText: string = '');
    property Text: string read FText write SetText;
  end;

  procedure Statistics(const R: TRecordList; out TotalProts, TotalFats, TotalCarbs,
    TotalValue, TotalMass, TotalIns: Real);
  function RecordToVersioned(const List: TRecordList): TVersionedList;
  function VersionedToRecord(const Item: TVersioned): TCustomRecord; overload;
  function VersionedToRecord(const List: TVersionedList): TRecordList; overload;

  procedure UpdatePostprand(const Recs: TRecordList; InsPeriod, StdMealPeriod, ShortMealPeriod: Real);
  procedure FreeRecords(var Recs: TRecordList);

implementation

{==============================================================================}
function RecordToVersioned(const List: TRecordList): TVersionedList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, Length(List));
  for i := 0 to High(Result) do
    Result[i] := List[i];
end;

{==============================================================================}
function VersionedToRecord(const Item: TVersioned): TCustomRecord;
{==============================================================================}
begin
  Result := Item as TCustomRecord;
end;

{==============================================================================}
function VersionedToRecord(const List: TVersionedList): TRecordList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, Length(List));
  for i := 0 to High(Result) do
    Result[i] := List[i] as TCustomRecord;
end;

{ TCustomRecord }

{==============================================================================}
function TCustomRecord.GetTime: integer;
{==============================================================================}
begin
  Result := Round(UTCToLocal(FTime) * MinPerDay) mod MinPerDay;
end;

{==============================================================================}
function TCustomRecord.RecType: TClassCustomRecord;
{==============================================================================}
begin
  Result := TClassCustomRecord(Self.ClassType);
end;

{==============================================================================}
procedure TCustomRecord.SetNativeTime(Value: TDateTime);
{==============================================================================}
begin
  FTime := Value;
end;

{ TBloodRecord }

{==============================================================================}
class function TBloodRecord.CheckFinger(TestFinger: integer): boolean;
{==============================================================================}
begin
  Result := (TestFinger >= -1) and (TestFinger <= 9);
end;

{==============================================================================}
class function TBloodRecord.CheckValue(const TestValue: real): boolean;
{==============================================================================}
begin
  Result := (TestValue > 0){and ? - единицы бывают разные};
end;

{==============================================================================}
constructor TBloodRecord.Create(ATime: TDateTime = 0; AValue: real = -1; AFinger: integer = -1);
{==============================================================================}
begin
  inherited Create;

  FPostPrand := False;
  SetNativeTime(ATime);
  SetValue(AValue);
  SetFinger(AFinger);
end;

{==============================================================================}
procedure TBloodRecord.SetFinger(NewFinger: integer);
{==============================================================================}
begin
  if (FFinger <> NewFinger) and (CheckFinger(NewFinger)) then
  begin
    FFinger := NewFinger;
  end;
end;

{==============================================================================}
procedure TBloodRecord.SetValue(const NewValue: real);
{==============================================================================}
begin
  if (FValue <> NewValue) and (CheckValue(NewValue)) then
  begin
    FValue := NewValue;
  end;
end;

{ TInsRecord }

{==============================================================================}
class function TInsRecord.CheckValue(const Value: real): boolean;
{==============================================================================}
begin
  Result := (Value > 0);
end;

{==============================================================================}
constructor TInsRecord.Create(ATime: TDateTime; AValue: real);
{==============================================================================}
begin
  inherited Create;
  SetNativeTime(ATime);
  SetValue(AValue);
end;

{==============================================================================}
procedure TInsRecord.SetValue(const NewValue: real);
{==============================================================================}
begin
  if (FValue <> NewValue) and (CheckValue(NewValue)) then
  begin
    FValue := NewValue;
  end;
end;

{ TMealRecord }

{==============================================================================}
function TMealRecord.Add(Food: TFoodMassed): integer;
{==============================================================================}
begin
  if Food <> nil then
  begin
    Result := Length(FFood);
    SetLength(FFood, Result + 1);
    FFood[Result] := Food;
  end else
    Result := -1;
end;

{==============================================================================}
procedure TMealRecord.CheckIndex(Index: integer);
{==============================================================================}
begin
  if (Index < Low(FFood)) or (Index > High(FFood)) then
    raise ERangeError.CreateFmt('TMealRecord: недопустимый индекс (%d)', [Index]);
end;

{==============================================================================}
procedure TMealRecord.Clear;
{==============================================================================}
var
  i: integer;
begin
  if Length(FFood) > 0 then
  begin
    for i := 0 to High(FFood) do
      FFood[i].Free;
    SetLength(FFood, 0);
  end;
end;

{==============================================================================}
constructor TMealRecord.Create(ATime: TDateTime; AShortMeal: boolean);
{==============================================================================}
begin
  inherited Create;
  ShortMeal := AShortMeal;
  SetNativeTime(ATime);
end;

{==============================================================================}
destructor TMealRecord.Destroy;
{==============================================================================}
begin
  Clear;
end;

{==============================================================================}
procedure TMealRecord.Exchange(Index1, Index2: integer);
{==============================================================================}
var
  Temp: TFoodMassed;
begin
  if Index1 <> Index2 then
  begin
    CheckIndex(Index1);
    CheckIndex(Index2);

    Temp := FFood[Index1];
    FFood[Index1] := FFood[Index2];
    FFood[Index2] := Temp;
  end;
end;

{==============================================================================}
function TMealRecord.Count: integer;
{==============================================================================}
begin
  Result := Length(FFood);
end;

{==============================================================================}
function TMealRecord.GetFood(Index: integer): TFoodMassed;
{==============================================================================}
begin
  CheckIndex(Index);
  Result := FFood[Index];
end;

{==============================================================================}
function TMealRecord.GetProp(Index: integer): real;
{==============================================================================}
var
  i: integer;
begin
  Result := 0;
  case Index of
    0: for i := 0 to High(FFood) do Result := Result + FFood[i].Mass;
    1: for i := 0 to High(FFood) do Result := Result + FFood[i].Prots;
    2: for i := 0 to High(FFood) do Result := Result + FFood[i].Fats;
    3: for i := 0 to High(FFood) do Result := Result + FFood[i].Carbs;
    4: for i := 0 to High(FFood) do Result := Result + FFood[i].Value;
  end;
end;

{==============================================================================}
procedure TMealRecord.SetShortMeal(Value: boolean);
{==============================================================================}
begin
  FShortMeal := Value;
end;

{==============================================================================}
procedure TMealRecord.Remove(Index: integer);
{==============================================================================}
var
  i: integer;
begin
  CheckIndex(Index);
  FFood[Index].Free;
  for i := Index to High(FFood) - 1 do
    FFood[i] := FFood[i + 1];
  SetLength(FFood, Length(FFood) - 1);
end;

{ TNoteRecord }

{==============================================================================}
constructor TNoteRecord.Create(ATime: TDateTime; AText: string);
{==============================================================================}
begin
  inherited Create;
  SetNativeTime(ATime);
  Text := AText;   
end;

{==============================================================================}
procedure TNoteRecord.SetText(const Value: string);
{==============================================================================}

  function Check(const S: string): string;
  const
    Reserved = [#10, #13];
  var
    i,j: integer;
  begin
    Result := S;
    j := 0;
    for i := 1 to length(S) do
    if not (S[i] in Reserved) then
    begin
      inc(j);
      Result[j] := S[i];
    end;
    SetLength(Result, j);
  end;

begin
  FText := Check(Value);
end;

{==============================================================================}
procedure Statistics(const R: TRecordList; out TotalProts, TotalFats, TotalCarbs,
  TotalValue, TotalMass, TotalIns: Real);
{==============================================================================}
var
  i: integer;
begin
  TotalProts := 0.0;
  TotalFats := 0.0;
  TotalCarbs := 0.0;
  TotalValue := 0.0;
  TotalMass := 0.0;
  TotalIns := 0.0;
  for i := Low(R) to High(R) do
  if (R[i].RecType = TMealRecord) then
  begin
    TotalProts := TotalProts + TMealRecord(R[i]).Prots;
    TotalFats := TotalFats + TMealRecord(R[i]).Fats;
    TotalCarbs := TotalCarbs + TMealRecord(R[i]).Carbs;
    TotalValue := TotalValue + TMealRecord(R[i]).Value;
    TotalMass := TotalMass + TMealRecord(R[i]).Mass;
  end else
  if (R[i].REcType = TInsRecord) then
  begin
    TotalIns := TotalIns + TInsRecord(R[i]).Value;
  end;
end;

{==============================================================================}
procedure UpdatePostprand(const Recs: TRecordList; InsPeriod, StdMealPeriod, ShortMealPeriod: Real);
{==============================================================================}
var
  i: integer;
  CurFreeTime: TDateTime;
begin
  //Log('TDiaryPage.UpdatePostPrand()');
  // TODO: дублирующийся код (GetNextDayFreeTime)

  CurFreeTime := 0.0;

  for i := Low(Recs) to High(Recs) do
  begin
    if (Recs[i].RecType = TInsRecord) then
    begin
      CurFreeTime := Max(CurFreeTime, Recs[i].NativeTime + InsPeriod);
    end else

    if (Recs[i].RecType = TMealRecord) then
    begin
      if TMealRecord(Recs[i]).Carbs > 0 then
         if TMealRecord(Recs[i]).ShortMeal then
           CurFreeTime := Max(CurFreeTime, Recs[i].NativeTime + ShortMealPeriod)
         else
           CurFreeTime := Max(CurFreeTime, Recs[i].NativeTime + StdMealPeriod);
    end else

    if (Recs[i].RecType = TBloodRecord) then
    begin
      TBloodRecord(Recs[i]).PostPrand := (Recs[i].NativeTime < CurFreeTime);
    end;
  end;
end;

{==============================================================================}
procedure FreeRecords(var Recs: TRecordList);
{==============================================================================}
var
  i: integer;
begin
  for i := Low(Recs) to High(Recs) do
    Recs[i].Free;
  SetLength(Recs, 0);
end;

end.

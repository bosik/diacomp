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
  TCustomRecord = class
  private
    FTime: TDateTime;               // UTC
  public
    function TimeInMinutes(): integer;
    function RecType: TClassCustomRecord;

    property Time: TDateTime read FTime write FTime;
  end;

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

  procedure Statistics(const R: TVersionedList; out TotalProts, TotalFats, TotalCarbs, TotalValue, TotalMass, TotalIns: Real);
  function VersionedToRecord(const Item: TVersioned): TCustomRecord;

  procedure UpdatePostprand(const Recs: TVersionedList; InsPeriod, StdMealPeriod, ShortMealPeriod: Real);
  procedure FreeRecords(var Recs: TVersionedList);

implementation

{======================================================================================================================}
function VersionedToRecord(const Item: TVersioned): TCustomRecord;
{======================================================================================================================}
begin
  Result := Item.Data as TCustomRecord;
end;

{ TCustomRecord }

{======================================================================================================================}
function TCustomRecord.TimeInMinutes: integer;
{======================================================================================================================}
begin
  Result := Round(UTCToLocal(FTime) * MinPerDay) mod MinPerDay;
end;

{======================================================================================================================}
function TCustomRecord.RecType: TClassCustomRecord;
{======================================================================================================================}
begin
  Result := TClassCustomRecord(Self.ClassType);
end;

{ TBloodRecord }

{======================================================================================================================}
class function TBloodRecord.CheckFinger(TestFinger: integer): boolean;
{======================================================================================================================}
begin
  Result := (TestFinger >= -1) and (TestFinger <= 9);
end;

{======================================================================================================================}
class function TBloodRecord.CheckValue(const TestValue: real): boolean;
{======================================================================================================================}
begin
  Result := (TestValue > 0){and ? - единицы бывают разные};
end;

{======================================================================================================================}
constructor TBloodRecord.Create(ATime: TDateTime = 0; AValue: real = -1; AFinger: integer = -1);
{======================================================================================================================}
begin
  inherited Create;

  FPostPrand := False;
  FTime := ATime;
  SetValue(AValue);
  SetFinger(AFinger);
end;

{======================================================================================================================}
procedure TBloodRecord.SetFinger(NewFinger: integer);
{======================================================================================================================}
begin
  if (FFinger <> NewFinger) and (CheckFinger(NewFinger)) then
  begin
    FFinger := NewFinger;
  end;
end;

{======================================================================================================================}
procedure TBloodRecord.SetValue(const NewValue: real);
{======================================================================================================================}
begin
  if (FValue <> NewValue) and (CheckValue(NewValue)) then
  begin
    FValue := NewValue;
  end;
end;

{ TInsRecord }

{======================================================================================================================}
class function TInsRecord.CheckValue(const Value: real): boolean;
{======================================================================================================================}
begin
  Result := (Value > 0);
end;

{======================================================================================================================}
constructor TInsRecord.Create(ATime: TDateTime; AValue: real);
{======================================================================================================================}
begin
  inherited Create;
  FTime := ATime;
  SetValue(AValue);
end;

{======================================================================================================================}
procedure TInsRecord.SetValue(const NewValue: real);
{======================================================================================================================}
begin
  if (FValue <> NewValue) and (CheckValue(NewValue)) then
  begin
    FValue := NewValue;
  end;
end;

{ TMealRecord }

{======================================================================================================================}
function TMealRecord.Add(Food: TFoodMassed): integer;
{======================================================================================================================}
begin
  if Food <> nil then
  begin
    Result := Length(FFood);
    SetLength(FFood, Result + 1);
    FFood[Result] := Food;
  end else
    Result := -1;
end;

{======================================================================================================================}
procedure TMealRecord.CheckIndex(Index: integer);
{======================================================================================================================}
begin
  if (Index < Low(FFood)) or (Index > High(FFood)) then
    raise ERangeError.CreateFmt('TMealRecord: недопустимый индекс (%d)', [Index]);
end;

{======================================================================================================================}
procedure TMealRecord.Clear;
{======================================================================================================================}
var
  i: integer;
begin
  if Length(FFood) > 0 then
  begin
    for i := 0 to High(FFood) do
      FreeAndNil(FFood[i]);
    SetLength(FFood, 0);
  end;
end;

{======================================================================================================================}
constructor TMealRecord.Create(ATime: TDateTime; AShortMeal: boolean);
{======================================================================================================================}
begin
  inherited Create;
  ShortMeal := AShortMeal;
  FTime := ATime;
end;

{======================================================================================================================}
destructor TMealRecord.Destroy;
{======================================================================================================================}
begin
  Clear;
end;

{======================================================================================================================}
procedure TMealRecord.Exchange(Index1, Index2: integer);
{======================================================================================================================}
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

{======================================================================================================================}
function TMealRecord.Count: integer;
{======================================================================================================================}
begin
  Result := Length(FFood);
end;

{======================================================================================================================}
function TMealRecord.GetFood(Index: integer): TFoodMassed;
{======================================================================================================================}
begin
  CheckIndex(Index);
  Result := FFood[Index];
end;

{======================================================================================================================}
function TMealRecord.GetProp(Index: integer): real;
{======================================================================================================================}
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

{======================================================================================================================}
procedure TMealRecord.SetShortMeal(Value: boolean);
{======================================================================================================================}
begin
  FShortMeal := Value;
end;

{======================================================================================================================}
procedure TMealRecord.Remove(Index: integer);
{======================================================================================================================}
var
  i: integer;
begin
  CheckIndex(Index);
  FreeAndNil(FFood[Index]);
  for i := Index to High(FFood) - 1 do
    FFood[i] := FFood[i + 1];
  SetLength(FFood, Length(FFood) - 1);
end;

{ TNoteRecord }

{======================================================================================================================}
constructor TNoteRecord.Create(ATime: TDateTime; AText: string);
{======================================================================================================================}
begin
  inherited Create;
  FTime := ATime;
  Text := AText;   
end;

{======================================================================================================================}
procedure TNoteRecord.SetText(const Value: string);
{======================================================================================================================}

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

{======================================================================================================================}
procedure Statistics(const R: TVersionedList; out TotalProts, TotalFats, TotalCarbs, TotalValue, TotalMass, TotalIns: Real);
{======================================================================================================================}
var
  i: integer;
  Rec: TCustomRecord;
  Meal: TMealRecord;
begin
  TotalProts := 0.0;
  TotalFats := 0.0;
  TotalCarbs := 0.0;
  TotalValue := 0.0;
  TotalMass := 0.0;
  TotalIns := 0.0;

  for i := Low(R) to High(R) do
  begin
    Rec := TCustomRecord(R[i].Data);

    if (Rec.RecType = TMealRecord) then
    begin
      Meal := TMealRecord(Rec);

      TotalProts := TotalProts + Meal.Prots;
      TotalFats := TotalFats + Meal.Fats;
      TotalCarbs := TotalCarbs + Meal.Carbs;
      TotalValue := TotalValue + Meal.Value;
      TotalMass := TotalMass + Meal.Mass;
    end else
    if (Rec.RecType = TInsRecord) then
    begin
      TotalIns := TotalIns + TInsRecord(Rec).Value;
    end;
  end;
end;

{======================================================================================================================}
procedure UpdatePostprand(const Recs: TVersionedList; InsPeriod, StdMealPeriod, ShortMealPeriod: Real);
{======================================================================================================================}
var
  i: integer;
  CurFreeTime: TDateTime;
  Rec: TCustomRecord;
begin
  //Log('TDiaryPage.UpdatePostPrand()');
  // TODO: дублирующийся код (GetNextDayFreeTime)

  CurFreeTime := 0.0;

  for i := Low(Recs) to High(Recs) do
  begin
    Rec := Recs[i].Data as TCustomRecord;

    if (Rec.RecType = TInsRecord) then
    begin
      CurFreeTime := Max(CurFreeTime, Rec.Time + InsPeriod);
    end else

    if (Rec.RecType = TMealRecord) then
    begin
      if TMealRecord(Rec).Carbs > 0 then
         if TMealRecord(Rec).ShortMeal then
           CurFreeTime := Max(CurFreeTime, Rec.Time + ShortMealPeriod)
         else
           CurFreeTime := Max(CurFreeTime, Rec.Time + StdMealPeriod);
    end else

    if (Rec.RecType = TBloodRecord) then
    begin
      TBloodRecord(Rec).PostPrand := (Rec.Time < CurFreeTime);
    end;
  end;
end;

{======================================================================================================================}
procedure FreeRecords(var Recs: TVersionedList);
{======================================================================================================================}
var
  i: integer;
begin
  for i := Low(Recs) to High(Recs) do
    FreeAndNil(Recs[i]);
  SetLength(Recs, 0);
end;

end.

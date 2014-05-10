unit DiaryRecords;

interface

uses
  BusinessObjects,
  DiaryRoutines,
  SysUtils;

type
  TCustomRecord = class;

  TClassCustomRecord = class of TCustomRecord;

  TEventRecordChanged = procedure(Sender: TCustomRecord) of object;

  // #entity
  TCustomRecord = class
  private
    FID: TCompactGUID;
    FTimeStamp: TDateTime;
    FVersion: integer;
    FDeleted: boolean;

    FTime: TDateTime;               // UTC

    FSilentMode: boolean;           // transient
    FSilentlyModified: boolean;     // transient
    FOnChange: TEventRecordChanged; // transient

    procedure NotifyPage;
    function GetTime(): integer;
  public
    procedure BeginUpdate;
    procedure CopyFrom(Org: TCustomRecord); virtual; deprecated;
    constructor Create; overload;
    constructor Create(Org: TCustomRecord); overload; deprecated;
    procedure EndUpdate;
    function RecType: TClassCustomRecord;

    procedure SetNativeTime(Value: TDateTime);
    property GetNativeTime: TDateTime read FTime;

    property Time: integer read GetTime;
    property OnChange: TEventRecordChanged read FOnChange write FOnChange;
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
    procedure CopyFrom(Org: TCustomRecord); override; deprecated;
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
    //procedure CopyFrom(Org: TCustomRecord); override; deprecated;
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

    procedure MealChangeHandler(Sender: TObject);
  public
    function Add(Food: TFoodMassed): integer;
    // procedure CopyFrom(Org: TCustomRecord); override; deprecated;
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
    // procedure CopyFrom(Org: TCustomRecord); override; deprecated;
    constructor Create(ATime: TDateTime = 0; AText: string = '');
    property Text: string read FText write SetText;
  end;

implementation

{ TCustomRecord }

{==============================================================================}
procedure TCustomRecord.BeginUpdate;
{==============================================================================}
begin
  FSilentMode := True;
end;

{==============================================================================}
procedure TCustomRecord.CopyFrom(Org: TCustomRecord);
{==============================================================================}
begin
  if (Org = nil) then
    raise Exception.Create('Org record can''t be nil');
  if (not (Org is Self.ClassType)) then
    raise Exception.Create('Unsupported org type');

  Self.SetNativeTime(Org.Time);

  // NO EVENT COPY HERE, DON'T KNOW IF IT'S OK...
end;

{==============================================================================}
constructor TCustomRecord.Create;
{==============================================================================}
begin
  FOnChange := nil;
  FSilentMode := False;
  FSilentlyModified := False;
end;

{==============================================================================}
constructor TCustomRecord.Create(Org: TCustomRecord);
{==============================================================================}
begin
  FOnChange := nil;
  FSilentMode := False;
  FSilentlyModified := False;
  
  CopyFrom(Org);
end;

{==============================================================================}
procedure TCustomRecord.EndUpdate;
{==============================================================================}
begin
  { Идея в том, чтобы множественные изменения записи оборачивать блоком }
  { BeginUpdate...EndUpdate и оповещать страницу об изменениях только   }
  { один раз при вызове EndUpdate. }
  FSilentMode := False;
  if FSilentlyModified then
    NotifyPage;
end;

{==============================================================================}
function TCustomRecord.GetTime: integer;
{==============================================================================}
begin
  Result := Round(UTCToLocal(FTime) * MinPerDay) mod MinPerDay;
end;

{==============================================================================}
procedure TCustomRecord.NotifyPage;
{==============================================================================}
begin
  begin
    if (FSilentMode) then
      FSilentlyModified := True
    else
    if (Assigned(FOnChange)) then
    begin
      FOnChange(Self);
      FSilentlyModified := False;
    end;
  end;
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
  if (FTime <> Value) //and (CheckTime(Value))
  then
  begin
    FTime := Value;
    NotifyPage;
  end;
  // TODO: throw exception!
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
procedure TBloodRecord.CopyFrom(Org: TCustomRecord);
{==============================================================================}
var
  Temp: TBloodRecord;
begin
  inherited;
  Temp := TBloodRecord(Org);
  Self.SetFinger(Temp.Finger);
  Self.SetValue(Temp.Value);
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
    NotifyPage;
  end;
end;

{==============================================================================}
procedure TBloodRecord.SetValue(const NewValue: real);
{==============================================================================}
begin
  if (FValue <> NewValue) and (CheckValue(NewValue)) then
  begin
    FValue := NewValue;
    NotifyPage;
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
    NotifyPage;
  end;
end;

{ TMealRecord }

{==============================================================================}
function TMealRecord.Add(Food: TFoodMassed): integer;
{==============================================================================}
begin
  if Food <> nil then
  begin
    Food.OnChange := MealChangeHandler;

    Result := Length(FFood);
    SetLength(FFood,Result+1);
    FFood[Result] := Food;
    NotifyPage;
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
    SetLength(FFood,0);
    NotifyPage;
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
  FOnChange := nil; { чтобы при очистке родительская страница ничего не увидела }
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

    NotifyPage;
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
  if FShortMeal <> Value then
  begin
    FShortMeal := Value;
    NotifyPage;
  end;
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

  NotifyPage;
end;

{==============================================================================}
procedure TMealRecord.MealChangeHandler(Sender: TObject);
{==============================================================================}
begin
  NotifyPage();
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
    Reserved = [#10,#13];
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

var
  NewValue: string;
begin
  NewValue := Check(Value);
  if NewValue <> FText then
  begin
    FText := NewValue;
    NotifyPage;
  end;
end;

end.

unit BusinessObjects;

interface

uses
  Classes,
  SysUtils,
  DiaryRoutines;

type
  TVersioned = class
  private
    FID: TCompactGUID;
    FTimeStamp: TDateTime;
    FVersion: integer;
    FDeleted: boolean;
  public
    procedure Modified();

    property ID: TCompactGUID read FID write FID;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property Version: integer read FVersion write FVersion;
    property Deleted: boolean read FDeleted write FDeleted;
  end;

  TVersionedList = array of TVersioned;

  // Уведомляет внешнюю среду о своём изменении через событие OnChange
  // TODO: DEPRECATED
  TMutableItem = class (TVersioned)
  private
    FOnChange: TNotifyEvent;
  protected
    //FID: TCompactGUID;
    procedure Modified; virtual;
  public
    constructor Create;
    //property ID: TCompactGUID read FID write FID;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // хранит: название, БЖУ
  // #entity
  TFoodRelative = class (TMutableItem)
  private
    FName: string;
    FRelProts: real;
    FRelFats: real;
    FRelCarbs: real;
    FRelValue: real;
    procedure SetName(const Value: string);
    procedure SetRel(Index: integer; const Value: real);
  public
    procedure CopyFrom(Food: TFoodRelative);
    constructor Create;
    class function IsCorrectRel(const Value: real): boolean;

    property Name:     string          read FName     write SetName;
    property RelProts: real    index 1 read FRelProts write SetRel;
    property RelFats:  real    index 2 read FRelFats  write SetRel;
    property RelCarbs: real    index 3 read FRelCarbs write SetRel;
    property RelValue: real    index 4 read FRelValue write SetRel;
  end;

  // хранит: название, БЖУ, массу
  // имеет методы для получения абсолютных БЖУ
  // используется для дневника и блюд
  // #entity
  TFoodMassed = class (TFoodRelative)
  private
    FMass: real;
    procedure SetMass(const Value: real);
  public
    constructor Create; overload;
    constructor Create(const Name: string; const RelProts, RelFats, RelCarbs, RelValue, Mass: real); overload;
    procedure CopyFrom(Food: TFoodMassed);
    class function IsCorrectMass(const Value: real): boolean;

    {*}procedure Read(const S: string);
    {*}function Write: string;

    property Mass: real read FMass write SetMass;
    function Prots(): Real;
    function Fats(): Real;
    function Carbs(): Real;
    function Value(): Real;
  end;

  // для базы продуктов (TFoodRelative + свойство FromTable)
  // #entity
  // TODO: rename to TFoodItem
  TFood = class (TFoodRelative)
  private
    FFromTable: boolean;
    FTag: integer;  { для хранения частоты использования (transient) }
    procedure SetFromTable(Value: boolean);
  public
    function AsFoodMassed(Mass: real): TFoodMassed;
    procedure CopyFrom(Food: TFood);
    constructor Create;

    property FromTable: boolean read FFromTable write SetFromTable;
    property Tag: integer       read FTag       write FTag;
  end;

  TFoodItemList = array of TFood;

  // #entity
  TDish = class (TMutableItem)
  private
    FContent: array of TFoodMassed;
    FFixedMass: boolean;
    FName: string;
    FResultMass: real;
    FTag: integer;
    function GetItem(Index: integer): TFoodMassed;
    function GetProp(Index: integer): real;
    function GetRel(Index: integer): real;
    {#}procedure SetName(Value: string);
  public
    {#}function Add(Food: TFoodMassed): integer;
    function AsFoodMassed(Mass: real): TFoodMassed;
    function AsFoodRelative(): TFoodRelative;
    {#}procedure Clear();
    procedure CopyFrom(Dish: TDish);
    function Count(): integer;
    constructor Create;
    {#}procedure Delete(Index: integer);
    destructor Destroy; override;
    procedure EraseResultMass;                    
    function RealMass(): real;
    {#}procedure SetResultMass(const Value: real);

    property Name: string read FName write SetName;
    property SummMass: real index 0 read GetProp;
    property Prots:    real index 1 read GetProp;
    property Fats:     real index 2 read GetProp;
    property Carbs:    real index 3 read GetProp;
    property Value:    real index 4 read GetProp;
    property RelProts: real index 1 read GetRel;
    property RelFats:  real index 2 read GetRel;
    property RelCarbs: real index 3 read GetRel;
    property RelValue: real index 4 read GetRel; 
    property ResultMass: real read FResultMass write SetResultMass;
    property FixedMass: boolean read FFixedMass;
    property Content[Index: integer]: TFoodMassed read GetItem; //default;
    property Tag: integer read FTag write FTag;
    //property SilentMode: boolean read FSilentMode write FSilentMode;
  end;

  TDishItemList = array of TDish; // TODO: rename

const
  FOOD_SEP          = '|';
  FOOD_RESERVED     = ['[', FOOD_SEP, ']', ':'];
  FOODBASE_RESERVED = ['#'] + FOOD_RESERVED;
  DISHBASE_RESERVED = ['#', '%', '='] + FOOD_RESERVED;
  SYSTEM_CHARS      = FOODBASE_RESERVED + DISHBASE_RESERVED;

implementation

{ TVersioned }

{==============================================================================}
procedure TVersioned.Modified;
{==============================================================================}
begin
  inc(FVersion);
  FTimeStamp := GetTimeUTC();
end;

{ TMutableItem }

{==============================================================================}
constructor TMutableItem.Create;
{==============================================================================}
begin
  FID := CreateCompactGUID();
end;

{==============================================================================}
procedure TMutableItem.Modified;
{==============================================================================}
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

{ TFoodRelative }

{==============================================================================}
procedure TFoodRelative.CopyFrom(Food: TFoodRelative);
{==============================================================================}
begin
  if (Food = nil) then raise Exception.Create('TFoodRelative.CopyFrom(): Food is nil');

  Name     := Food.Name;
  RelProts := Food.RelProts;
  RelFats  := Food.RelFats;
  RelCarbs := Food.RelCarbs;
  RelValue := Food.RelValue;
  ID       := Food.ID;
end;

{==============================================================================}
constructor TFoodRelative.Create;
{==============================================================================}
begin
  inherited Create();
  FName     := '';
  FRelProts := 0;
  FRelFats  := 0;
  FRelCarbs := 0;
  FRelValue := 0;
end;

{==============================================================================}
class function TFoodRelative.IsCorrectRel(const Value: real): boolean;
{==============================================================================}
begin
  Result := (Value >= 0) and (Value <= 100);
end;

{==============================================================================}
procedure TFoodRelative.SetName(const Value: string);
{==============================================================================}
begin
  if (FName <> Value) then
  begin
    FName := Value;
    Modified();
  end;
end;

{==============================================================================}
procedure TFoodRelative.SetRel(Index: integer; const Value: real);
{==============================================================================}

  procedure MicroSet(var ARelXXX: real);
  begin
    if (ARelXXX <> Value) then
    begin
      ARelXXX := Value;
      Modified();
    end;
  end;

begin
  case Index of
    1: if IsCorrectRel(Value) then MicroSet(FRelProts) else raise Exception.CreateFmt('TFoodRelative.SetRel(): illegal RelProts value (%f)', [Value]);
    2: if IsCorrectRel(Value) then MicroSet(FRelFats)  else raise Exception.CreateFmt('TFoodRelative.SetRel(): illegal RelFats value (%f)', [Value]);
    3: if IsCorrectRel(Value) then MicroSet(FRelCarbs) else raise Exception.CreateFmt('TFoodRelative.SetRel(): illegal RelCarbs value (%f)', [Value]);
    4: if (Value >= 0)        then MicroSet(FRelValue) else raise Exception.CreateFmt('TFoodRelative.SetRel(): illegal RelValue value (%f)', [Value]);
    else raise ERangeError.CreateFmt('TFoodRelative.SetRel(): illegal index (%d)', [Index]);
  end;
end;

{ TFoodMassed }

{==============================================================================}
procedure TFoodMassed.CopyFrom(Food: TFoodMassed);
{==============================================================================}
begin
  inherited CopyFrom(Food);
  Mass := TFoodMassed(Food).Mass;
end;

{==============================================================================}
constructor TFoodMassed.Create;
{==============================================================================}
begin
  inherited;
  FMass := 0;
end;

{==============================================================================}
constructor TFoodMassed.Create(const Name: string; const RelProts, RelFats,
  RelCarbs, RelValue, Mass: real);
{==============================================================================}
begin
  Self.Name     := Name;
  Self.RelProts := RelProts;
  Self.RelFats  := RelFats;
  Self.RelCarbs := RelCarbs;
  Self.RelValue := RelValue;
  Self.Mass     := Mass;
end;

{==============================================================================}
class function TFoodMassed.IsCorrectMass(const Value: real): boolean;
{==============================================================================}
begin
  Result := (Value >= 0);
end;

{==============================================================================}
procedure TFoodMassed.SetMass(const Value: real);
{==============================================================================}
begin
  if (not IsCorrectMass(Value)) then raise Exception.CreateFmt('TFoodMassed.SetMass(): illegal mass (%f)', [Value]);

  if (Mass <> Value) then
  begin
    FMass := Value;
    Modified();
  end;
end;

{==============================================================================}
function TFoodMassed.Prots: Real; begin Result := FMass * FRelProts / 100; end;
function TFoodMassed.Fats:  Real; begin Result := FMass * FRelFats  / 100; end;
function TFoodMassed.Carbs: Real; begin Result := FMass * FRelCarbs / 100; end;
function TFoodMassed.Value: Real; begin Result := FMass * FRelValue / 100; end;
{==============================================================================}

{==============================================================================}
procedure TFoodMassed.Read(const S: string);
{==============================================================================}
var
  n: integer;
begin
  n := 1;
  Name := ReadStrTo(S, '[', n);
  RelProts := ReadFloatTo(S, FOOD_SEP, n);
  RelFats  := ReadFloatTo(S, FOOD_SEP, n);
  RelCarbs := ReadFloatTo(S, FOOD_SEP, n);
  RelValue := ReadFloatTo(S, ']', n);
  Mass     := StrToFloat(CheckDot(Copy(S, n + 1, Length(S) - n)));
end;

{==============================================================================}
function TFoodMassed.Write(): string;
{==============================================================================}
begin
  Result := Format(
    '%s[%s' + FOOD_SEP + '%s'  + FOOD_SEP + '%s' + FOOD_SEP + '%s' + ']:%s',
    [Name, RealToStr(RelProts), RealToStr(RelFats), RealToStr(RelCarbs), RealToStr(RelValue), RealToStr(Mass)]
  );


  {Result :=
    Name + '[' +
    RealToStr(RelProts) + FOOD_SEP +
    RealToStr(RelFats)  + FOOD_SEP +
    RealToStr(RelCarbs) + FOOD_SEP +
    RealToStr(RelValue) + ']:'+
    RealToStr(Mass);}
end;

{ TFood }

{==============================================================================}
function TFood.AsFoodMassed(Mass: real): TFoodMassed;
{==============================================================================}
begin
  Result := TFoodMassed.Create();
  Result.Name := Name;
  Result.RelProts := RelProts;
  Result.RelFats := RelFats;
  Result.RelCarbs := RelCarbs;
  Result.RelValue := RelValue;
  Result.Mass := Mass;
end;

{==============================================================================}
procedure TFood.CopyFrom(Food: TFood);
{==============================================================================}
begin
  inherited CopyFrom(Food);

  FFromTable := Food.FFromTable;
  FTag := Food.FTag;
end;

{==============================================================================}
constructor TFood.Create;
{==============================================================================}
begin
  inherited Create();
  FFromTable := True;
  FTag := 0;
end;

{==============================================================================}
procedure TFood.SetFromTable(Value: boolean);
{==============================================================================}
begin
  if (Value <> FFromTable) then
  begin
    FFromTable := Value;
    Modified();
  end;
end;

{ TDish }

{==============================================================================}
function TDish.Add(Food: TFoodMassed): integer;
{==============================================================================}
begin
  Food.OnChange := OnChange; 

  Result := Count;
  SetLength(FContent, Result + 1);
  FContent[Result] := Food;

  Modified();
end;

{==============================================================================}
function TDish.AsFoodMassed(Mass: real): TFoodMassed;
{==============================================================================}
begin
  Result := TFoodMassed.Create();
  Result.Name := Name;
  Result.RelProts := RelProts;
  Result.RelFats := RelFats;
  Result.RelCarbs := RelCarbs;
  Result.RelValue := RelValue;
  Result.Mass := Mass;
end;

{==============================================================================}
function TDish.AsFoodRelative: TFoodRelative;
{==============================================================================}
begin
  Result := TFoodRelative.Create;
  Result.Name := Name;
  Result.RelProts := RelProts;
  Result.RelFats := RelFats;
  Result.RelCarbs := RelCarbs;
  Result.RelValue := RelValue;
end;

{==============================================================================}
procedure TDish.Clear();
{==============================================================================}
var
  i: integer;
begin
  if (Length(FContent) > 0) then
  begin
    for i := 0 to High(FContent) do
      FContent[i].Free;
    SetLength(FContent, 0);

    Modified();
  end;
end;

{==============================================================================}
constructor TDish.Create();
{==============================================================================}
begin
  inherited;
  FFixedMass := False;
  FName := '';
  FResultMass := 0;
  FTag := 0;
end;

{==============================================================================}
procedure TDish.CopyFrom(Dish: TDish);
{==============================================================================}
var
  i: integer;
  Temp: TFoodMassed;
begin
  if (Dish = nil) then raise Exception.Create('TDish.CopyFrom(): Dish is nil');

  Name := Dish.Name;
  Tag := Dish.Tag;
  FID := Dish.ID; 

  if Dish.FixedMass then
    SetResultMass(Dish.RealMass)
  else
    EraseResultMass;

  Clear;
  for i := 0 to Dish.Count - 1 do
  begin
    { разрываем связку (копируем) }
    Temp := TFoodMassed.Create;
    Temp.CopyFrom(Dish.Content[i]);
    Add(Temp);
  end;
end;

{==============================================================================}
function TDish.Count(): integer;
{==============================================================================}
begin
  Result := Length(FContent);
end;

{==============================================================================}
procedure TDish.Delete(Index: integer);
{==============================================================================}
var
  i: integer;
begin
  if (Index < 0) or (Index > High(FContent)) then
    raise ERangeError.CreateFmt('TDish().Delete(): index out of bounds (%d)', [Index]);

  FContent[Index].Free;

  for i := Index to High(FContent) - 1 do
    FContent[i] := FContent[i + 1];
  SetLength(FContent, Length(FContent) - 1);

  Modified();
end;

{==============================================================================}
destructor TDish.Destroy;
{==============================================================================}
begin
  OnChange := nil; // TODO: "до Clear(), чтобы не оповещать об очистке" - обновить
  Clear;
  inherited;
end;

{==============================================================================}
procedure TDish.EraseResultMass;
{==============================================================================}
begin
  FFixedMass := False;
end;

{==============================================================================}
function TDish.GetItem(Index: integer): TFoodMassed;
{==============================================================================}
begin
  Result := FContent[Index];
end;

{==============================================================================}
function TDish.GetProp(Index: integer): real;
{==============================================================================}
var
  i: integer;
begin
  Result := 0;
  case Index of
    0: for i := 0 to High(FContent) do Result := Result + FContent[i].Mass;
    1: for i := 0 to High(FContent) do Result := Result + FContent[i].Prots;
    2: for i := 0 to High(FContent) do Result := Result + FContent[i].Fats;
    3: for i := 0 to High(FContent) do Result := Result + FContent[i].Carbs;
    4: for i := 0 to High(FContent) do Result := Result + FContent[i].Value;
    else raise ERangeError.CreateFmt('TDish.GetProp(): illegal index (%d)', [Index]);
  end;
end;

{==============================================================================}
function TDish.GetRel(Index: integer): real;
{==============================================================================}
var
  RM: Real;
begin
  RM := RealMass();
  if (abs(RM) > EPS) then
    Result := GetProp(Index) / RM * 100
  else
    Result := 0;
end;

{==============================================================================}
function TDish.RealMass(): real;
{==============================================================================}
begin
  if FixedMass then
    Result := FResultMass
  else
    Result := SummMass;
end;

{==============================================================================}
procedure TDish.SetName(Value: string);
{==============================================================================}
begin
  if (Value <> FName) then
  begin
    FName := Value;
    Modified();
  end;
end;

{==============================================================================}
procedure TDish.SetResultMass(const Value: real);
{==============================================================================}
var
  MinResultMass: Real;
begin
  MinResultMass := Prots + Fats + Carbs;
  if (Value <= 0) then raise Exception.CreateFmt('TDish.SetResultMass(): Value (%f) must be positive :)', [Value]);
  if (Value < MinResultMass) then raise Exception.CreateFmt('TDish.SetResultMass(): Value (%f) must be larger than %f', [Value, MinResultMass]);

  if (not FFixedMass) or (FResultMass <> Value) then
  begin
    FResultMass := Value;
    FFixedMass := True;
    Modified();
  end;
end;

initialization
  Randomize;
end.

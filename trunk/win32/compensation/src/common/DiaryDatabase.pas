unit DiaryDatabase;

{$R+ O+}

interface

uses
  SysUtils,
  Classes,
  Math,
  DiaryRoutines,
  DiarySources,
  AutoLog, // debug only
  ActiveX,
  XMLDoc,
  XMLIntf,
  BusinessObjects,
  Variants;

type
  { 1.2 БАЗЫ }

  TSortType = (stName, stTag);

  // Имеет методы для бинарного поиска
  // Имеет методы для сортировки
  // Имеет номер версии и метод для её инкремента
  TCustomBase = class
  private
    TempIndexList: TIndexList;
    FVersion: integer;
  protected
    function GetName(Index: integer): string; virtual; abstract;
    function GetTag(Index: integer): integer; virtual; abstract;
    procedure SetTag(Index, Value: integer); virtual; abstract;
    procedure Swap(Index1, Index2: integer); virtual; abstract;

    procedure Changed;
    function MoreName(Index1, Index2: integer): boolean;
    function MoreTag(Index1, Index2: integer): boolean;
    function MoreIndName(Index1, Index2: integer): boolean;
    function MoreIndTag(Index1, Index2: integer): boolean;
    procedure Sort();
    procedure SwapInd(Index1, Index2: integer);
  public
    function Count: integer; virtual; abstract;
    constructor Create;
    function Find(const ItemName: string): integer;
    procedure SortIndexes(IndexList: TIndexList; SortType: TSortType);

    property Version: integer read FVersion;
  end;

  TFoodBase = class(TCustomBase)
  private
    FBase: array of TFood;
    function GetFood(Index: integer): TFood;
    procedure FoodChangeHandler(Sender: TObject);
  protected // like in a base class
    function GetName(Index: integer): string; override;
    function GetTag(Index: integer): integer; override;
    procedure SetTag(Index, Value: integer); override;
    procedure Swap(Index1, Index2: integer); override;
  public
    {#}function Add(Food: TFood): integer;
    function Count: integer; override;
    {#}procedure Delete(Index: integer);
    destructor Destroy; override;
    procedure LoadFromFile_Old(const FileName: string);
    procedure LoadFromFile_XML(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property Items[Index: integer]: TFood read GetFood; default;
  end;

  TDishBase = class(TCustomBase)
  private
    FBase: array of TDish;
    function GetDish(Index: integer): TDish;
    procedure DishChangeHandler(Sender: TObject);
  protected // like in a base class
    function GetName(Index: integer): string; override;
    function GetTag(Index: integer): integer; override;
    procedure SetTag(Index, Value: integer); override;
    procedure Swap(Index1, Index2: integer); override;
  public
    {#}function Add(Dish: TDish): integer;
    function Count: integer; override;
    {#}procedure Delete(Index: integer);
    destructor Destroy; override;
    procedure LoadFromFile_Old(const FileName: string; FoodBase: TFoodBase = nil);
    procedure LoadFromFile_XML(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure AnalyzeUsing(Base: TCustomBase);
    {#}function RenameFood(const OldName, NewName: string): boolean;
    function UsedFood(const FoodName: string): integer;

    property Items[Index: integer]: TDish read GetDish; default;
  end;

  { 2. КЛАССЫ ДНЕВНИКА }

  TRecType = (rtUnknown, rtBlood, rtIns, rtMeal, rtNote);

  TDiaryPage = class; // forward declaration
  //TNo

  TCustomRecord = class
  private
    FTime: integer; // в мин
    FPage: TDiaryPage; // TODO: abstract it
    FSilentMode: boolean;
    FSilentlyModified: boolean;
    procedure NotifyPage;
    procedure SetTime(Value: integer);
    class function CheckTime(TestTime: integer): boolean;
  public
    procedure BeginUpdate;
    constructor Create;
    procedure EndUpdate;
    function RecType: TRecType;

    // через Page страница привязывает запись к себе при добавлении
    property Page: TDiaryPage read FPage write FPage;
    property Time: integer read FTime write SetTime;
  end;

  TClassCustomRecord = class of TCustomRecord;

  // #entity
  TBloodRecord = class(TCustomRecord)
  private
    FValue: real;
    FFinger: integer;
    FPostPrand: boolean;  // просто маркер, НЕ данные
  protected
    procedure SetFinger(NewFinger: integer);
    procedure SetValue(const NewValue: real);
    class function CheckFinger(TestFinger: integer): boolean;
    class function CheckValue(const TestValue: real): boolean;
  public
    constructor Create(ATime: integer = 0; AValue: real = -1; AFinger: integer = -1);
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
    constructor Create(ATime: integer = 0; AValue: real = -1);
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
    constructor Create(ATime: integer = 0; AShortMeal: boolean = False);
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
    constructor Create(ATime: integer = 0; AText: string = '');
    property Text: string read FText write SetText;
  end; 

  //=======================================================================

  TRecordsList = array of TCustomRecord; // слава полиморфизму!

  TPageEventType = (etAdd, etModify, etRemove);

  TDiary = class;

  TEventPageChanged = procedure(EventType: TPageEventType; Page: TDiaryPage; RecClass: TClassCustomRecord; RecInstance: TCustomRecord) of object;

  TDiaryPage = class
  private
    FDate: TDate;
    {+}FRecs: TRecordsList;

    {+}FSilentChange: boolean;
    FTimeStamp: TDateTime;
    FVersion: integer;

    FDiaryBase: TDiary;

    { события }
    FOnChange: TEventPageChanged;

    {+}procedure CheckIndex(Index: integer);
    {+}function GetRecord(Index: integer): TCustomRecord;
    {+}function GetStat(Index: integer): real;

    {#}procedure Changed(EventType: TPageEventType; RecClass: TClassCustomRecord; RecInstance: TCustomRecord = nil);

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

    {+}function FirstRec(RecType: TRecType): TCustomRecord;
    {+}function LastRec(RecType: TRecType): TCustomRecord;
    {+}function FindRecord(Rec: TCustomRecord): integer; overload;
    {+}function FirstBloodRec: TBloodRecord;
    {+}function FirstInsRec: TInsRecord;
    {+}function FirstMealRec: TMealRecord;
    {+}function LastBloodRec: TBloodRecord;
    {+}function LastInsRec: TInsRecord;
    {+}function LastMealRec: TMealRecord;

    // I/O

    procedure ReadFrom(S: TStringList); overload;
    procedure WriteTo(S: TStringList); overload;

    procedure ReadFrom(const S: string); overload;
    procedure WriteTo(out S: string); overload;

    procedure ReadFrom(S: TPageData); overload;
    procedure WriteTo(S: TPageData); overload;

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

    property OnChange: TEventPageChanged read FOnChange write FOnChange;
  end;

  TSearchDirection = (sdBack, sdForward, sdAround);

  // {*} - устаревшее
  // {!} - трудности

  TPageCache = class (TDiaryPage)
  private
    FCalculatedPostprand: boolean;
  public
    constructor Create(PageData: TPageData);
    function GetNextDayFreeTime(StdMealPeriod, ShortMealPeriod, InsPeriod: integer): integer;
    procedure UpdatePostprand(FreeTime, StdMealPeriod, ShortMealPeriod, InsPeriod: integer);

    property CalculatedPostprand: boolean read FCalculatedPostprand; 
  end;

  TDiary = class
  private
    FSource: IDiarySource;
    FCache: array of TPageCache;
    FModified: boolean;

    FPostPrandStd: integer;
    FPostPrandShort: integer;

    { события }
    FOnChange: TEventPageChanged;

    { оповещатели }
    procedure NotifyChanged(EventType: TPageEventType; Page: TDiaryPage; RecClass: TClassCustomRecord;
      RecInstance: TCustomRecord);                                      

    function GetPage(Date: TDate): TDiaryPage;
    function GetPageIndex(Date: TDate; CalculatePostprand: boolean): integer;
    function TraceLastPage: integer;

    procedure UpdateCached_Postprand;
    procedure SetPostPrand(Index, Value: integer);
  public
    constructor Create(Source: IDiarySource);
    destructor Destroy; override;
    function FindRecord(RecType: TRecType; ADate: TDate; ATime: integer;
      DeltaTime: integer; Direction: TSearchDirection): TCustomRecord;
    function GetLastBloodRecord: TBloodRecord;
    function GetNextFinger: integer;
    procedure Post;
    procedure PrepareCache(FromDate, ToDate: TDate);
    procedure ReloadCache;
    procedure ResetCache;
    procedure SaveToXML(const FileName: string);
    procedure SaveToJSON(const FileName: string);

    // свойства
    property Pages[Index: TDate]: TDiaryPage read GetPage; default;
    property Modified: boolean read FModified write FModified;
    property PostPrandStd: integer index 1 read FPostPrandStd write SetPostPrand default 210;
    property PostPrandShort: integer index 2 read FPostPrandShort write SetPostPrand default 30;
    property OnChange: TEventPageChanged read FOnChange write FOnChange;
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
class function TCustomRecord.CheckTime(TestTime: integer): boolean;
{==============================================================================}
begin
  Result := (TestTime >= 0) and (TestTime < SecPerDay);
end;

{==============================================================================}
constructor TCustomRecord.Create;
{==============================================================================}
begin
  FPage := nil;
  FSilentMode := False;
  FSilentlyModified := False;
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
procedure TCustomRecord.NotifyPage;
{==============================================================================}
begin
  if (FPage <> nil) then
  begin
    if (FSilentMode) then
      FSilentlyModified := True
    else
    begin
      FPage.Changed(etModify, TClassCustomRecord(Self.ClassType), Self);
      FSilentlyModified := False;
    end;
  end;
end;

{==============================================================================}
procedure TCustomRecord.SetTime(Value: integer);
{==============================================================================}
begin
  if (FTime <> Value) and (CheckTime(Value)) then
  begin
    FTime := Value;
    NotifyPage;
  end;
end;

{==============================================================================}
function TCustomRecord.RecType: TRecType;
{==============================================================================}
begin
  // TODO: предок знает о потомках, плохо, да
  if (Self is TBloodRecord) then Result := rtBlood else
  if (Self is TInsRecord) then Result := rtIns else
  if (Self is TMealRecord) then Result := rtMeal else
  if (Self is TNoteRecord) then Result := rtNote else
    Result := rtUnknown;
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
constructor TBloodRecord.Create(ATime: integer = 0; AValue: real = -1; AFinger: integer = -1);
{==============================================================================}
begin
  inherited Create;

  FPostPrand := False;
  SetTime(ATime);
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

class function TInsRecord.CheckValue(const Value: real): boolean;
begin
  Result := (Value > 0);
end;

constructor TInsRecord.Create(ATime: integer; AValue: real);
begin
  inherited Create;
  SetTime(ATime);
  SetValue(AValue);
end;

procedure TInsRecord.SetValue(const NewValue: real);
begin
  if (FValue <> NewValue) and (CheckValue(NewValue)) then
  begin
    FValue := NewValue;
    NotifyPage;
  end;
end;

{ TMealRecord }

function TMealRecord.Add(Food: TFoodMassed): integer;
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

procedure TMealRecord.CheckIndex(Index: integer);
begin
  if (Index < Low(FFood)) or (Index > high(FFood)) then
    raise ERangeError.CreateFmt('TMealRecord: недопустимый индекс (%d)', [Index]);
end;

procedure TMealRecord.Clear;
var
  i: integer;
begin
  if Length(FFood) > 0 then
  begin
    for i := 0 to high(FFood) do
      FFood[i].Free;
    SetLength(FFood,0);
    NotifyPage;
  end;
end;

constructor TMealRecord.Create(ATime: integer; AShortMeal: boolean);
begin
  inherited Create;
  ShortMeal := AShortMeal;
  SetTime(ATime);
end;

destructor TMealRecord.Destroy;
begin
  FPage := nil; { чтобы при очистке родительская страница ничего не увидела }
  Clear;
end;

procedure TMealRecord.Exchange(Index1, Index2: integer);
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

function TMealRecord.Count: integer;
begin
  Result := Length(FFood);
end;

function TMealRecord.GetFood(Index: integer): TFoodMassed;
begin
  CheckIndex(Index);
  Result := FFood[Index];
end;

function TMealRecord.GetProp(Index: integer): real;
var
  i: integer;
begin
  Result := 0;
  case Index of
    0: for i := 0 to high(FFood) do Result := Result + FFood[i].Mass;
    1: for i := 0 to high(FFood) do Result := Result + FFood[i].Prots;
    2: for i := 0 to high(FFood) do Result := Result + FFood[i].Fats;
    3: for i := 0 to high(FFood) do Result := Result + FFood[i].Carbs;
    4: for i := 0 to high(FFood) do Result := Result + FFood[i].Value;
  end;
end;

procedure TMealRecord.SetShortMeal(Value: boolean);
begin
  if FShortMeal <> Value then
  begin
    FShortMeal := Value;
    NotifyPage;
  end;
end;

procedure TMealRecord.Remove(Index: integer);
var
  i: integer;
begin
  CheckIndex(Index);
  FFood[Index].Free;
  for i := Index to high(FFood)-1 do
    FFood[i] := FFood[i+1];
  SetLength(FFood, Length(FFood)-1);

  NotifyPage;
end;

procedure TMealRecord.MealChangeHandler(Sender: TObject);
begin
  NotifyPage();
end;

{ TNoteRecord }

constructor TNoteRecord.Create(ATime: integer; AText: string);
begin
  inherited Create;
  SetTime(ATime);
  Text := AText;   
end;

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

procedure TNoteRecord.SetText(const Value: string);
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

{ TDiaryPage }

(*function TDiaryPage.AddBlood(Time: integer; const Value: real; Finger: integer): integer;
begin
  if (TCustomRecord.CheckTime(Time)) and
     (TBloodRecord.CheckValue(Value)) and
     (TBloodRecord.CheckFinger(Finger)) then // TODO: fix it
  begin
    Result := length(FRecs);
    SetLength(FRecs,Result+1);
    FRecs[Result] := TBloodRecord.Create(Self);
    TBloodRecord(FRecs[Result]).Time := Time;
    TBloodRecord(FRecs[Result]).Value := Value;
    TBloodRecord(FRecs[Result]).Finger := Finger;
    Result := TraceLast;
    {#}Changed(FRecs[Result]);
  end else
    Result := -1;
end;

function TDiaryPage.AddFoodToMeal(Index: integer;
  Food: TFoodMassed): integer;
begin
  CheckIndex(Index);

  if (FRecs[Index].TagType = rtMeal)and
     (Food <> nil) then
  begin
    Result := TMealRecord(FRecs[Index]).Add(Food);
    {#}Changed(FRecs[Index]);
  end else
    Result := -1;
end;

function TDiaryPage.AddIns(Time: integer; const Value: real): integer;
begin
  if (TCustomRecord.CheckTime(Time)) and
     (TInsRecord.CheckValue(Value)) then
  begin
    Result := length(FRecs);
    SetLength(FRecs,Result+1);
    FRecs[Result] := TInsRecord.Create(Self);
    TInsRecord(FRecs[Result]).Time := Time;
    TInsRecord(FRecs[Result]).Value := Value;
    Result := TraceLast;
    {#}Changed(FRecs[Result]);
  end else
    Result := -1;
end;

function TDiaryPage.AddNote(Time: integer; const Value: string): integer;
begin
  if (TCustomRecord.CheckTime(Time))then
  begin
    Result := length(FRecs);
    SetLength(FRecs,Result+1);
    FRecs[Result] := TNoteRecord.Create(Self);
    TNoteRecord(FRecs[Result]).Time := Time;
    TNoteRecord(FRecs[Result]).Text := Value;
    Result := TraceLast;
    {#}Changed(FRecs[Result]);
  end else
    Result := -1;
end;

function TDiaryPage.AddMealBlank(Time: integer; ShortMeal: boolean): integer;
begin
  if TCustomRecord.CheckTime(Time) then
  begin
    Result := FindRecord(Time,rtMeal);
    if Result = -1 then
    begin
      Result := length(FRecs);
      SetLength(FRecs,Result+1);
      FRecs[Result] := TMealRecord.Create(Self);
      TMealRecord(FRecs[Result]).Time := Time;
      TMealRecord(FRecs[Result]).ShortMeal := ShortMeal;
      Result := TraceLast;
      {#}Changed(FRecs[Result]);
    end;
  end else
    Result := -1;
end;  *)

{==============================================================================}
function TDiaryPage.Add(Rec: TCustomRecord): integer;
{==============================================================================}
begin
  if (Rec = nil) then
    raise Exception.Create('Record can''t be nil');

  Result := Length(FRecs);
  SetLength(FRecs, Result + 1);
  FRecs[Result] := Rec;
  Rec.Page := Self;
  Result := TraceLast;

  //{#}Changed({FRecs[Result]}Rec);
  Changed(etAdd, TClassCustomRecord(Rec.ClassType), Rec);
end;


// TODO: эта процедура должна вызываться только мелкими изменениями пользователя
// избегать вызова её при загрузке данных
{==============================================================================}
procedure TDiaryPage.Changed(EventType: TPageEventType; RecClass: TClassCustomRecord;
  RecInstance: TCustomRecord = nil);
{==============================================================================}
var
  Index: integer;
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

  // пересылаем событие базе (ПОСЛЕ коррекций)

  if Assigned(FOnChange) then FOnChange(EventType, Self, RecClass, RecInstance);

  if (FDiaryBase <> nil) then
    FDiaryBase.NotifyChanged(EventType, Self, RecClass, RecInstance);
end;

{==============================================================================}
procedure TDiaryPage.CheckIndex(Index: integer);
{==============================================================================}
begin
  if (Index < Low(FRecs)) or (Index > high(FRecs)) then
    raise ERangeError.CreateFmt('TDiaryPage: недопустимый индекс (%d)', [Index]);
end;

// используется при загрузке и при уничтожении
{==============================================================================}
procedure TDiaryPage.Clear;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to high(FRecs) do
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
  FDiaryBase := nil;

  FDate := ADate;
  FTimeStamp := ATimeStamp;
  FVersion := AVersion;

  FSilentChange := False;
end;

{==============================================================================}
constructor TDiaryPage.Create(PageData: TPageData);
{==============================================================================}
begin
  FDiaryBase := nil;

  //FSilentChange := True;
  ReadFrom(PageData);

  FSilentChange := False;
end;

{==============================================================================}
destructor TDiaryPage.Destroy;
{==============================================================================}
begin
  FDiaryBase := nil;
  FSilentChange := True;
  Clear;
end;

(*
function TDiaryPage.FindRecord(Time: integer; AType: TRecType): integer;
var
  i: integer;
begin
  { !!! поиск можно улучшить, т.к. массив отсортирован }
  Result := -1;
  for i := 0 to high(FRecs) do
  if (FRecs[i].Time = Time)and
     (FRecs[i].TagType = AType) then
  begin
    Result := i;
    Exit;
  end;
end;

function TDiaryPage.FindRecord(Time: integer): integer;
var
  i: integer;
begin
  { !!! поиск можно улучшить, т.к. массив отсортирован }
  Result := -1;
  for i := 0 to high(FRecs) do
  if FRecs[i].Time = Time then
  begin
    Result := i;
    Exit;
  end;
end;    *)

{==============================================================================}
function TDiaryPage.FindRecord(Rec: TCustomRecord): integer;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to high(FRecs) do
  if (FRecs[i] = Rec) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

{==============================================================================}
function TDiaryPage.FirstRec(RecType: TRecType): TCustomRecord;
{==============================================================================}
var
  i: integer;
begin
  Result := nil;
  for i := 0 to high(FRecs) do
  if (FRecs[i].RecType = RecType) then
  begin
    Result := FRecs[i];
    Exit;
  end;
end;

{==============================================================================}
function TDiaryPage.FirstBloodRec: TBloodRecord;
{==============================================================================}
begin
  Result := TBloodRecord(FirstRec(rtBlood));
end;

{==============================================================================}
function TDiaryPage.FirstInsRec: TInsRecord;
{==============================================================================}
begin
  Result := TInsRecord(FirstRec(rtIns));
end;

{==============================================================================}
function TDiaryPage.FirstMealRec: TMealRecord;
{==============================================================================}
begin
  Result := TMealRecord(FirstRec(rtMeal));
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
    1: for i := 0 to high(FRecs) do if FRecs[i].RecType = rtMeal then Result := Result + TMealRecord(FRecs[i]).Prots;
    2: for i := 0 to high(FRecs) do if FRecs[i].RecType = rtMeal then Result := Result + TMealRecord(FRecs[i]).Fats;
    3: for i := 0 to high(FRecs) do if FRecs[i].RecType = rtMeal then Result := Result + TMealRecord(FRecs[i]).Carbs;
    4: for i := 0 to high(FRecs) do if FRecs[i].RecType = rtMeal then Result := Result + TMealRecord(FRecs[i]).Value;
    5: for i := 0 to high(FRecs) do if FRecs[i].RecType = rtMeal then
         Result := Result + TMealRecord(FRecs[i]).Mass;
    6: for i := 0 to high(FRecs) do if FRecs[i].RecType = rtIns  then Result := Result + TInsRecord(FRecs[i]).Value;
  end;
end;

{==============================================================================}
function TDiaryPage.LastRec(RecType: TRecType): TCustomRecord;
{==============================================================================}
var
  i: integer;
begin
  Result := nil;
  for i := high(FRecs) downto 0 do
  if (FRecs[i].RecType = RecType) then
  begin
    Result := FRecs[i];
    Exit;
  end;
end;

{==============================================================================}
function TDiaryPage.LastBloodRec: TBloodRecord;
{==============================================================================}
begin
  Result := TBloodRecord(LastRec(rtBlood));
end;

{==============================================================================}
function TDiaryPage.LastInsRec: TInsRecord;
{==============================================================================}
begin
  Result := TInsRecord(LastRec(rtIns));
end;

{==============================================================================}
function TDiaryPage.LastMealRec: TMealRecord;
{==============================================================================}
begin
  Result := TMealRecord(LastRec(rtMeal));
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

  for i := Index to high(FRecs)-1 do
    FRecs[i] := FRecs[i+1];
  SetLength(FRecs,length(FRecs)-1);

  {#}Changed(etRemove, T);
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
procedure TDiaryPage.ReadFrom(S: TStringList);
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
          Meal := TMealRecord.Create(TempTime, TempShort);
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
          rai se ELoadingError.Create('TDiaryPage.ReadFrom: Некорректные данные'#13+
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
procedure TDiaryPage.WriteTo(S: TStringList);
{==============================================================================}
var
  j, n: integer;
begin
  if (S = nil) then
    raise Exception.Create('TDiaryPage.WriteTo(): поток для записи не может быть nil');

  for j := 0 to Count - 1 do
  case FRecs[j].RecType of
    rtBlood: s.Add(
               '*' + TimeToStr(FRecs[j].Time) +
               ' ' + FloatToStr(TBloodRecord(FRecs[j]).Value) +
               '|' + IntToStr(TBloodRecord(FRecs[j]).Finger)
             );

    rtIns:   s.Add(
               '-' + TimeToStr(FRecs[j].Time)+
               ' ' + FloatToStr(TInsRecord(FRecs[j]).Value)
             );

    rtMeal:  begin
               if TMealRecord(FRecs[j]).ShortMeal then
                 s.Add(' ' + TimeToStr(FRecs[j].Time)+'s')
               else
                 s.Add(' ' + TimeToStr(FRecs[j].Time));

               for n := 0 to TMealRecord(FRecs[j]).Count-1 do
                 s.Add('#' + TMealRecord(FRecs[j])[n].Write);
             end;
    rtNote:  s.Add(
               '%' + TimeToStr(FRecs[j].Time)+
               ' ' + TNoteRecord(FRecs[j]).Text
             );
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

{ TDiary }

constructor TDiary.Create(Source: IDiarySource);
begin
  if (Source = nil) then
    raise Exception.Create('Source can''t be nil')
  else
    FSource := Source;

  FModified := False;
  FPostPrandStd := 210;
  FPostPrandShort := 30;
end;

function TDiary.GetPage(Date: TDate): TDiaryPage;
begin
  Result := FCache[GetPageIndex(Date, True)];
end;

function TDiary.GetPageIndex(Date: TDate; CalculatePostprand: boolean): integer;

  function FindInCache(Date: TDate): integer;
  var
    L, R: integer;
  begin
    L := 0;
    R := high(FCache);
    while (L <= R) do
    begin
      Result := (L + R) div 2;
      if (FCache[Result].Date < Date) then L := Result + 1 else
      if (FCache[Result].Date > Date) then R := Result - 1 else
        Exit;
    end;
    Result := -1;
  end;

  function LoadFromSource(Date: TDate): integer;
  var
    PageData: TPageData;
  begin
    PageData := TPageData.Create;
    FSource.GetPage(Date, PageData);

    Result := Length(FCache);
    SetLength(FCache, Result + 1);
    FCache[Result] := TPageCache.Create(PageData);
    FCache[Result].FDiaryBase := Self;
    Result := TraceLastPage();

    PageData.Free;
  end;

  function MakeSureExists(Date: TDate): integer;
  begin
    Result := FindInCache(Date);
    if Result = -1 then
      Result := LoadFromSource(Date);
  end;

var
  Index: integer;
  FreeTime: integer;
begin
  Result := MakeSureExists(Date);

  if CalculatePostprand and (not FCache[Result].CalculatedPostprand) then
  begin
    Index := MakeSureExists(Date - 1);
    FreeTime := FCache[Index].GetNextDayFreeTime(PostPrandStd, PostPrandShort, PostPrandStd);
    Result := MakeSureExists(Date); // !!!
    FCache[Result].UpdatePostprand(FreeTime, PostPrandStd, PostPrandShort, PostPrandStd)
    // TODO: INS time, not meal
    // TODO: выделить эти параметры в рекорд/класс и сделать его отдельным полем
  end;
end;

destructor TDiary.Destroy;
begin
  ResetCache;
end;

function TDiary.FindRecord(RecType: TRecType;
  ADate: TDate; ATime: integer; DeltaTime: integer;
  Direction: TSearchDirection): TCustomRecord;

  {

  08.30                           ||
  08.40                           ||
    09.00   <--  FirstMoreEq  --- \/ ---
    09.00
    09.00
    09.00   <--  LastLessEq   --- /\ ---
  09.20                           ||
  09.30                           ||
  }

var
  StartDate: TDate;
  StartTime: integer;
  FinishDate: TDate;
  FinishTime: integer;
  Date: TDate;

  Page: TDiaryPage;
  CurDist, MinDist: integer;
  i: integer;
begin
  { проверяем аргументы }
  if not CorrectTime(ATime) then
    raise Exception.Create('FindRecord: некорректное значение Time (' + IntToStr(ATime) + ')');
  if (DeltaTime <= 0) then
    raise Exception.Create('FindRecord: некорректное значение DeltaTime (' + IntToStr(DeltaTime) + ')');

  { определяем границы }
  if (Direction = sdForward) then
  begin
    StartDate := ADate;
    StartTime := ATime;
  end else
  begin
    StartDate := ((ADate * MinPerDay + ATime) - DeltaTime) div MinPerDay;
    StartTime := ((ADate * MinPerDay + ATime) - DeltaTime) mod MinPerDay;
  end;

  if (Direction = sdBack) then
  begin
    FinishDate := ADate;
    FinishTime := ATime;
  end else
  begin
    FinishDate := ((ADate * MinPerDay + ATime) + DeltaTime) div MinPerDay;
    FinishTime := ((ADate * MinPerDay + ATime) + DeltaTime) mod MinPerDay;
  end;

  { ищем }
  MinDist := (FinishDate - StartDate + 1) * 2 * MinPerDay;
  Result := nil;

  for Date := StartDate to FinishDate do
  begin
    Page := GetPage(Date);
    for i := 0 to Page.Count - 1 do
    if ((Date > StartDate) or (Page[i].Time >= StartTime)) and
       ((Date < FinishDate) or (Page[i].Time <= FinishTime)) and
       ((RecType = rtUnknown) or (Page[i].RecType = RecType))
    then
    begin
      CurDist := abs((Date * MinPerDay + Page[i].Time) - (ADate * MinPerDay + ATime));
      if (CurDist < MinDist) then
      begin
        MinDist := CurDist;
        Result := Page[i];
      end;
    end;
  end;  

  (*
  PageIndex := GetPageIndex(Date);

  if (PageIndex < 0) or
     (PageIndex > high(FPages))or // ???
     (not CorrectTime(AroundTime))or
     (DeltaTime <= 0) then
    Result := nil
  else

  begin
    { поиск }

    if Direction = sdBack then
    begin
      p := PageIndex;
      r := FPages[p].Count;
      while (r = 0) and (p > 0) do
      begin
        dec(p);
        r := FPages[p].Count;
      end;
      founded := False;
      Changed := r > 0;
      dec(r);

      while (Changed)and(not founded) do
      begin
        Time := abs((FPages[p].Date-FPages[PageIndex].Date)*MinPerDay+FPages[p][r].Time - AroundTime);
        if (FPages[p][r].TagType = RecType)and
           ((FPages[p][r].Time<=AroundTime)or(p<PageIndex))and
           (Time <= DeltaTime) then
          founded := True
        else
          Changed := Prev(p,r);
      end;

      if founded then
        Result := FPages[p][r]
      else
        Result := nil;
    end else

    if Direction = sdForward then
    begin
      p := PageIndex;
      if FPages[p].Count>0 then
        r := 0
      else
        r := -1;
      while (r=-1)and(p<high(FPages)) do
      begin
        inc(p);
        if FPages[p].Count>0 then
          r := 0
        else
          r := -1;
      end;
      founded := False;
      Changed := r>-1;

      while (Changed)and(not founded) do
      begin
        Time := abs(
          Trunc(FPages[p].Date-FPages[PageIndex].Date)*MinPerDay+
          FPages[p][r].Time-AroundTime);
        if (FPages[p][r].TagType = RecType)and
           ((FPages[p][r].Time>=AroundTime)or(p>PageIndex)) and
           (Time <= DeltaTime) then
          founded := True
        else
          Changed := Next(p,r);
      end;

      if founded then
        Result := FPages[p][r]
      else
        Result := nil;
    end else

    //if Direction = sdAround then
    begin
      p := PageIndex;
      r := FPages[p].Count;
      while (r = 0) and (p > 0) do
      begin
        dec(p);
        r := FPages[p].Count;
      end;
      founded := False;
      Changed := r > 0;
      dec(r);

      while (Changed)and(not founded) do
      begin
        Time := abs((FPages[p].Date-FPages[PageIndex].Date)*MinPerDay+FPages[p][r].Time - AroundTime);
         if (FPages[p][r].TagType = RecType)and
           (Time <= DeltaTime) then
          founded := True
        else
          Changed := Prev(p,r);
      end;

      if founded then
        Result := FPages[p][r]
      else
        Result := nil;
    end;
  end;
  *)
end;

(*function TDiary.FindRecordFrom(RecType: TRecType;
  Date: TDate; StartRecord: integer; DeltaTime: integer;
  Direction: TSearchDirection): TCustomRecord;
var
  p,r: integer;
  Founded,Changed: boolean;
  Time: integer;

  RecPrev,RecNext: TCustomRecord;
  TimePrev,TimeNext: integer;
  PageIndex: integer;
begin
  PageIndex := GetPageIndex(Date);

  if (PageIndex<0)or(PageIndex>high(FPages))or
     (StartRecord<0)or(StartRecord>FPages[PageIndex].Count-1)or
     (DeltaTime<=0) then
    Result := nil else
  begin
    { поиск }

    if Direction = sdBack then
    begin
      p := PageIndex;
      r := StartRecord;
      founded := False;
      Changed := True;

      while (Changed)and(not founded) do
      begin
        Time := abs(
          Trunc(FPages[p].Date-FPages[PageIndex].Date)*MinPerDay+
          FPages[p][r].Time-FPages[PageIndex][StartRecord].Time);
        if (FPages[p][r].TagType = RecType)and
           ((FPages[p][r].Time<=FPages[PageIndex][StartRecord].Time)or(p<PageIndex))and
           (Time <= DeltaTime) then
          founded := True
        else
          Changed := Prev(p,r);
      end;

      if founded then
        Result := FPages[p][r]
      else
        Result := nil;
    end else

    if Direction = sdForward then
    begin
      p := PageIndex;
      r := StartRecord;
      founded := False;
      Changed := True;

      while (Changed)and(not founded) do
      begin
        Time := abs(
          Trunc(FPages[p].Date-FPages[PageIndex].Date)*MinPerDay+
          FPages[p][r].Time-FPages[PageIndex][StartRecord].Time);
        if (FPages[p][r].TagType = RecType)and
           ((FPages[p][r].Time>=FPages[PageIndex][StartRecord].Time)or(p>PageIndex)) and
           (Time <= DeltaTime) then
          founded := True
        else
          Changed := Next(p,r);
      end;

      if founded then
        Result := FPages[p][r]
      else
        Result := nil;
    end else

    //if Direction = sdAround then
    begin
      p := PageIndex;
      r := StartRecord;
      Changed := True;
      RecPrev := nil;
      TimePrev := High(Integer);
      Time := 0;
      while (Changed)and(RecPrev=nil)and(Time <= DeltaTime) do
      begin
        Time := abs(
          Trunc(FPages[p].Date-FPages[PageIndex].Date)*MinPerDay+
          FPages[p][r].Time-FPages[PageIndex][StartRecord].Time);
        if (FPages[p][r].TagType = RecType)and
           (Time <= DeltaTime) then
        begin
          RecPrev := FPages[p][r];
          TimePrev := Time;
        end else
          Changed := Prev(p,r);
      end;

      p := PageIndex;
      r := StartRecord;
      Changed := True;
      RecNext := nil;
      TimeNext := High(Integer);
      Time := 0;
      while (Changed)and(RecNext=nil)and(Time <= DeltaTime) do
      begin
        Time := abs(
          Trunc(FPages[p].Date-FPages[PageIndex].Date)*MinPerDay+
          FPages[p][r].Time-FPages[PageIndex][StartRecord].Time);
        if (FPages[p][r].TagType = RecType)and
           (Time <= DeltaTime) then
        begin
          RecNext := FPages[p][r];
          TimeNext := Time;
        end else
          Changed := Next(p,r);
      end;

      if TimePrev<=TimeNext then
        Result := RecPrev else
        Result := RecNext;
    end;
  end;
end;

function TDiary.GetFirstDate: TDate;
var
  i: integer;
begin
  for i := 0 to high(FPages) do
  if FPages[i].Count > 0 then
  begin
    Result := FPages[i].FDate;
    Exit;
  end;
  raise Exception.Create('Вызван метод GetFirstDate для пустого дневника');
end;

function TDiary.GetLastDate: TDate;
var
  i: integer;
begin
  for i := high(FPages) downto 0 do
  if FPages[i].Count > 0 then
  begin
    Result := FPages[i].FDate;
    Exit;
  end;
  raise Exception.Create('Вызван метод GetLastDate для пустого дневника');
end;

function TDiary.IsEmpty: boolean;
var
  i: integer;
begin
  for i := 0 to high(FPages) do
  if FPages[i].Count > 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TDiary.LoadFromFile(const FileName: string; CanCreate: boolean): boolean;
var
  s: TStrings;
  i,n: integer;
  tmp,s1,s2: string;

  StartLine: integer;
  buf: TStringList;

  Date: TDateTime; // с TDate не работает функция StrToDate
  TimeStamp: TDateTime;
  Version: integer;

  procedure FlushBuffer;
  begin
    if (buf.Count > 0) and (n <> -1) then
    begin
      FPages[n].ReadFrom(buf);
      buf.Clear;
    end;
  end;

begin
  Clear;
  Result := False;

  if not FileExists(FileName) then
  begin
    if CanCreate then
    begin
      try
        with TStringList.Create do
        begin
          SaveToFile(FileName);
          Free;
        end;
        Result := True;
      except
        raise ELoadingError.Create(ERROR_CANT_CREATE_DIARY);
      end;
    end;
    Exit;
  end;

  s := TStringList.Create;
  buf := TStringList.Create;
  try
    try
      s.LoadFromFile(FileName);
      n := -1;

      if (s.Count > 1)and(s[0] = DIARY_FORMAT) then
      begin
        FTimeStamp := StrToDateTime(s[1]);
        StartLine := 2;
      end else
      begin
        FTimeStamp := 0;
        StartLine := 0;
      end;

      for i := StartLine to s.Count-1 do
      if (s[i] <> '') then
      begin
        if s[i][1] = '=' then
        begin
          FlushBuffer;

          Date := StrToDate(Copy(s[i], 5, 10));

          tmp := s[i];
          Separate(tmp, s1, '|', s2);

          // только дата
          if s2 = '' then
          begin
            TimeStamp := Date + 23/24;  // модификация в 23:00
            Version := 1;
          end else
          begin
            tmp := s2;
            Separate(tmp, s1, '|', s2);

            TimeStamp := StrToDateTime(s1);
            // Дата | TimeStamp
            if s2 = '' then
              Version := 1
            else
              // Дата | TimeStamp | Версия
              Version := StrToInt(s2);
          end;

          n := CreatePage(Trunc(Date), TimeStamp, Version);
        end else
          buf.Add(s[i]);
      end;

      FlushBuffer;
    finally
      s.Free;
      buf.Free;
    end;

    UpdatePostprand();
    FModified := false;  // да)
    Result := True;
  except
    raise ELoadingError.Create(ERROR_LOADINGDIARY);
  end;
end;

function TDiary.Next(var APage, ARec: integer): boolean;
begin
  inc(ARec);
  while (ARec = FPages[APage].Count) and (APage < high(FPages)) do
  begin
    inc(APage);
    if FPages[APage].Count > 0 then
      ARec := 0
    else
      ARec := -1;
  end;
  Result := (ARec <> -1) and (ARec < FPages[APage].Count);
end;

function TDiary.Prev(var APage, ARec: integer): boolean;
begin
  dec(ARec);
  while (ARec = -1) and (APage > 0) do
  begin
    dec(APage);
    ARec := FPages[APage].Count-1;
  end;
  Result := ARec >- 1;
end;

function TDiary.SaveToFile(const FileName: string): boolean;
var
  s: TStringList;
  i: integer;
begin
  Result := True;
  try
    s := TStringList.Create;
    try
      { Time Stamp }
      s.Add(DIARY_FORMAT);
      s.Add(DateTimeToStr(FTimeStamp));

      { Data }
      for i := 0 to high(FPages) do
      // пустые страницы могут появляться после интенсивного тыкания по календарю,
      // поэтому сохраняем только страницы с записями - МОЛОДЕЦ! :-)
      if FPages[i].Count > 0 then
      begin
        s.Add(
          '=== ' + DateToStr(FPages[i].Date) + ' ===|' +
          DateTimeToStr(FPages[i].TimeStamp) + '|' +
          IntToStr(FPages[i].FVersion));
        FPages[i].WriteTo(s);
      end;
      s.SaveToFile(FileName);
    finally
      s.Free;
    end;
    FModified := false;
  except
    Result := False;
  end;
end; *)

procedure TDiary.NotifyChanged(EventType: TPageEventType; Page: TDiaryPage;
  RecClass: TClassCustomRecord; RecInstance: TCustomRecord);
begin
  //Log('TDiary.Changed()');

  FModified := True;

  // TODO: проверить, что не вызывается слишком часто (например, при загрузке)
  // TODO: [Trunc(Now) + 1] — не лучшее решение
  if Page <> nil then
    UpdateCached_Postprand; // TODO: optimize
    //UpdatePostprand(Page.Date-1, Trunc(Now) + 1);

  if Assigned(FOnChange) then FOnChange(EventType, Page, RecClass, RecInstance);
end;

procedure TDiary.SetPostPrand(Index, Value: integer);
begin
  case Index of
    1: // std
    if (Value <> FPostPrandStd) and
       (Value >= 0) and
       (Value <= 5*60) then
    begin
      FPostPrandStd := Value;
    end;

    2: // short
    if (Value <> FPostPrandShort) and
       (Value >= 0) and
       (Value <= 60) then
    begin
      FPostPrandShort := Value;
    end;
  end;

  UpdateCached_Postprand;
end;

function TDiary.TraceLastPage: integer;
var
  temp: TPageCache;
begin
  Result := High(FCache);
  if (Result > -1) then
  begin
    Temp := FCache[Result];
    while (Result > 0) and (FCache[Result - 1].Date > Temp.Date) do
    begin
      FCache[Result] := FCache[Result-1];
      dec(Result);
    end;
    FCache[Result] := temp;
  end;
end;

(*procedure TDiary.UpdatePostPrand(FromDate, ToDate: TDate);
var
  j: integer;
  FreeTime: integer; // время, когда всё "устаканится"
  CurTime: integer;
  d: TDate;

  InsActionTime: integer;
begin
  Log('TDiary.UpdatePostPrand()');

  //if (FromDate = 0) then
  //  FromDate := GetFirstDate();

  {LastEventTime := 0;
  for d := FromDate to GetLastDate do
  for j := 0 to Pages[d].Count-1 do
  begin
    CurTime := d * MinPerDay + Pages[d][j].Time;

    case Pages[d][j].TagType of
      rtBlood: TBloodRecord(Pages[d][j]).PostPrand := (CurTime - LastEventTime) < PostPrandStd;
      rtIns:   LastEventTime := CurTime;
      rtMeal:  if TMealRecord(Pages[d][j]).Count > 0 then
               if TMealRecord(Pages[d][j]).ShortMeal then
                 LastEventTime := Max(LastEventTime, CurTime + (PostPrandShort-PostPrandStd))
               else
                 LastEventTime := CurTime;
    end;
  end;  }

  InsActionTime := PostPrandStd; { !!! по-хорошему - время действия инсулина }
  FreeTime := 0;

 { if Pages[FromDate-1].Count > 0 then
  begin
    d := FromDate-1;  
    for j := 0 to Pages[d].Count-1 do
    begin
      CurTime := d * MinPerDay + Pages[d][j].Time;
      case Pages[d][j].TagType of
        rtIns:   FreeTime := Max(FreeTime, CurTime + InsActionTime);
        rtMeal:  if TMealRecord(Pages[d][j]).Count > 0 then
                 if TMealRecord(Pages[d][j]).ShortMeal then
                   FreeTime := Max(FreeTime, CurTime + PostPrandShort)
                 else
                   FreeTime := Max(FreeTime, CurTime + PostPrandStd);
      end;
    end;
  end;    }

  //ToDate := GetLastDate;

  for d := FromDate to ToDate do
  for j := 0 to Pages[d].Count-1 do
  begin
    CurTime := d * MinPerDay + Pages[d][j].Time;

    case Pages[d][j].TagType of
      rtIns:   FreeTime := Max(FreeTime, CurTime + InsActionTime);
      rtMeal:  if TMealRecord(Pages[d][j]).Count > 0 then
               if TMealRecord(Pages[d][j]).ShortMeal then
                 FreeTime := Max(FreeTime, CurTime + PostPrandShort)
               else
                 FreeTime := Max(FreeTime, CurTime + PostPrandStd);

      rtBlood: if FreeTime > 0 then
                 TBloodRecord(Pages[d][j]).PostPrand := (CurTime < FreeTime);
    end;
  end;
end;   *)

function TDiary.GetLastBloodRecord: TBloodRecord;
const
  INTERVAL = 7;
var
  Date, Today: integer;
  Index, i: integer;
begin
  Today := Trunc(Now);
  for Date := Today downto (Today - INTERVAL + 1) do
  begin
    Index := GetPageIndex(Date, False);

    if (Index > -1) then
    for i := FCache[Index].Count-1 downto 0 do
    if (FCache[Index][i] is TBloodRecord) then
    begin
      Result := TBloodRecord(FCache[Index][i]);
      Exit;
    end;
  end;

  Result := nil;

  {for i := high(FPages) downto 0 do
  for j := FPages[i].Count-1 downto 0 do
  if FPages[i][j] is TBloodRecord then
  begin
   Result := TBloodRecord(FPages[i][j]);
   Exit;
  end;
  Result := nil;  }
end;

procedure TDiary.PrepareCache(FromDate, ToDate: TDate);
var
  Date: TDate;
begin
  //Log('TDiary.PrepareCache()');

  // TODO: оптимизировать
  for Date := FromDate to ToDate do
    GetPageIndex(Date, True);
end;

procedure TDiary.ResetCache;
var
  i: integer;
begin
  for i := 0 to high(FCache) do
    FCache[i].Free;
  SetLength(FCache, 0);
end;

procedure TDiary.UpdateCached_Postprand;
var
  i: integer;
  FreeTime: integer;
begin
  //Log('TDiary.UpdateCachedPostprand()');

  FreeTime := 0; // just to avoid compiler's warning

  for i := 0 to high(FCache) do
  begin
    // если убрать проверку, то кэш будет разрастаться при повторном вызове этой процедуры
    if FCache[i].CalculatedPostprand then
      FCache[i].UpdatePostprand(FreeTime - FCache[i].Date * MinPerDay, PostPrandStd, PostPrandShort, PostPrandStd);

    FreeTime := FCache[i].GetNextDayFreeTime(PostPrandStd, PostPrandShort, PostPrandStd);
    FreeTime := FreeTime + (FCache[i].Date + 1) * MinPerDay
  end;
end;

{==============================================================================}
procedure TDiary.Post;
{==============================================================================}
var
  Pages: TPageList;
  i: integer;
begin
  // выгружаем в список
  SetLength(Pages, Length(FCache));
  for i := 0 to High(Pages) do
  begin
    Pages[i] := TPageData.Create();
    FCache[i].WriteTo(Pages[i]);
  end;

  // отправляем источнику
  FSource.PostPages(Pages);

  // освобождаем память
  for i := 0 to High(Pages) do
    Pages[i].Free;

  FModified := False;
end;

procedure TDiary.ReloadCache;
var
  i: integer;
  PageData: TPageData;
begin
  PageData := TPageData.Create;

  for i := 0 to high(FCache) do
  begin
    FSource.GetPage(FCache[i].Date, PageData);
    FCache[i].ReadFrom(PageData);
    //FCache[i].FCalculatedPostprand := False; - нужно сохранить состояния для UpdateCached_Postprand
  end;

  PageData.Free;

  UpdateCached_Postprand;
end;

{==============================================================================}
procedure TDiary.SaveToXML(const FileName: string);
{==============================================================================}
var
  XML: IXMLDocument;
  Page: TDiaryPage;
  Meal: TMealRecord;
  Root, PageNode, RecNode, ItemNode: IXMLNODE;
  i, j, k: integer;
  DS: char;
begin
  DS := SysUtils.DecimalSeparator;
  //SysUtils.DecimalSeparator := '.';

  try
    XML := NewXMLDocument();
    XML.Encoding := 'utf-8';
    XML.Version := '1.0';
    XML.NodeIndentStr := #9;
    XML.Options := [doNodeAutoIndent];

    Root := XML.AddChild('diary');

    //DiaryLocalSource.TDiaryLocalSource(FSource).

    // TODO: hardcoded date
    for i := Trunc(EncodeDate(2009, 12, 01)) to Trunc(Now) do
    begin
      Page := GetPage(i);

      PageNode := Root.AddChild('page');
      PageNode.Attributes['date'] := DateToStr(Page.Date);
      PageNode.Attributes['mod'] := Page.TimeStamp;
      PageNode.Attributes['ver'] := Page.Version;

      for j := 0 to Page.Count - 1 do
      begin
        RecNode := PageNode.AddChild('rec');
        RecNode.Attributes['time']  := TimeToStr(Page[j].Time/MinPerDay);

        case (Page[j].RecType) of
          rtBlood:
          begin
            RecNode.Attributes['type']  := 'blood';
            RecNode.Attributes['value']  := TBloodRecord(Page[j]).Value;
            if (TBloodRecord(Page[j]).Finger > -1) then
              RecNode.Attributes['finger']  := TBloodRecord(Page[j]).Finger;
          end;

          rtIns:
          begin
            RecNode.Attributes['type']  := 'insulin';
            RecNode.Attributes['value']  := TInsRecord(Page[j]).Value;
          end;

          rtNote:
          begin
            RecNode.Attributes['type']  := 'note';
            RecNode.Attributes['text']  := TNoteRecord(Page[j]).Text;
          end;

          rtMeal:
          begin
            Meal := TMealRecord(Page[j]);     

            RecNode.Attributes['type']  := 'meal';
            if (Meal.ShortMeal) then
              RecNode.Attributes['short']  := Meal.ShortMeal;

            for k := 0 to Meal.Count - 1 do
            begin
              ItemNode := RecNode.AddChild('item');
              ItemNode.Attributes['name']  := Meal[k].Name;
              ItemNode.Attributes['prots'] := Meal[k].RelProts;
              ItemNode.Attributes['fats']  := Meal[k].RelFats;
              ItemNode.Attributes['carbs'] := Meal[k].RelCarbs;
              ItemNode.Attributes['val']   := Meal[k].RelValue;
              ItemNode.Attributes['mass']  := Meal[k].Mass;
            end;
          end;
        end;
      end;
    end;

    XML.SaveToFile(FileName);
  finally
    SysUtils.DecimalSeparator := DS;
  end;
end;

{==============================================================================}
procedure TDiary.SaveToJSON(const FileName: string);
{==============================================================================}

  function BlockBlood(R: TBloodRecord): string;
  begin
    Result := Format('{type: "blood", time: %d, value: %.1f, finger: %d}', [R.Time, R.Value, R.Finger]);
  end;

  function BlockIns(R: TInsRecord): string;
  begin
    Result := Format('{type: "insulin", time: %d, value: %.1f}', [R.Time, R.Value]);
  end;

  function BlockFood(R: TFoodMassed): string;
  begin
    Result := Format('{name: "%s", prots: %.1f, fats: %.1f, carbs: %.1f, value: %.1f, mass: %.1f}', [R.Name, R.RelProts, R.RelFats, R.RelCarbs, R.RelValue, R.Mass]);
  end;

  function BlockMeal(R: TMealRecord): string;
  var
    i: integer;
  begin
    Result := Format('{type: "meal", time: %d, content: [',
      [R.Time]);
    for i := 0 to R.Count - 1 do
    begin
      Result := Result + BlockFood(R[i]);
      if (i < R.Count - 1) then
        Result := Result + ', ';
    end;
    Result := Result + ']}';
  end;

  function BlockNote(R: TNoteRecord): string;
  begin
    Result := Format('{type: "note", time: %d, text: "%s"}', [R.Time, R.Text]);
  end;

  function BlockPage(P: TDiaryPage): string;
  var
    i: integer;
  begin
    Result := Format('{date: "%s", stamp: "%s", version: %d, content: [',
      [DateToStr(P.Date),
      DateTimeToStr(P.TimeStamp),
      P.Version]);
    for i := 0 to P.Count - 1 do
    begin
      case P[i].RecType of
        rtBlood: Result := Result + BlockBlood(TBloodRecord(P[i]));
        rtIns:   Result := Result + BlockIns(TInsRecord(P[i]));
        rtMeal:  Result := Result + BlockMeal(TMealRecord(P[i]));
        rtNote:  Result := Result + BlockNote(TNoteRecord(P[i]));
      end;
      if (i < P.Count - 1) then
        Result := Result + ', ';
    end;
    Result := Result + ']}';
  end;

var
  JSON: string;
  Page: TDiaryPage;
  i: integer;
  //DS: char;
  S: TStrings;
begin
  //DS := SysUtils.DecimalSeparator;
  //SysUtils.DecimalSeparator := '.';
  JSON := '[';

  S := TStringList.Create;
  try
    // TODO: hardcoded date
    for i := Trunc(EncodeDate(2009, 12, 04)) to Trunc(Now) do
    begin
      Page := GetPage(i);

      JSON := JSON + BlockPage(Page);
      if (i < Page.Count - 1) then
        JSON := JSON + ', ';

      S.Add(json);
      json := '';
    end;

    JSON := JSON + ']';
    S.Add(json);
    json := '';

    S.SaveToFile(FileName);
  finally
    S.Free;
    //SysUtils.DecimalSeparator := DS;
  end;
end;

{==============================================================================}
function TDiary.GetNextFinger: integer;
{==============================================================================}
var
  Blood: TBloodRecord;
begin
  Blood := GetLastBloodRecord();
  if (Blood <> nil) then
    Result := (Blood.Finger + 1) mod 10
  else
    Result := -1;
end;

{ TCustomBase }

{==============================================================================}
procedure TCustomBase.Changed;
{==============================================================================}
begin
  inc(FVersion);
end;

{==============================================================================}
constructor TCustomBase.Create;
{==============================================================================}
begin
  FVersion := 0;
end;

{==============================================================================}
function TCustomBase.Find(const ItemName: string): integer;
{==============================================================================}

// поиск на полное соответствие (без учёта регистра)

// линейный поиск
{var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
  if MatchStr(GetName(i), ItemName, True) then
  begin
    Result := i;
    break;
  end;
end; }

// бинарный поиск
var
  l,r,k: integer;
  Upper, Cur: string;
begin
  l := 0;
  r := Count - 1;

  Upper := AnsiUpperCase(ItemName);

  while (l <= r) do
  begin
    k := (l + r) div 2;
    {*}Cur := AnsiUpperCase(GetName(k));
    if (Cur < Upper) then l := k + 1 else
    if (Cur > Upper) then r := k - 1 else
    begin
      Result := k;
      Exit;
    end;
  end;

  Result := -1;
end;

{==============================================================================}
function TCustomBase.MoreIndName(Index1, Index2: integer): boolean;
{==============================================================================}
begin
  Index1 := TempIndexList[Index1];
  Index2 := TempIndexList[Index2];
  Result := MoreName(Index1, Index2);
end;

{==============================================================================}
function TCustomBase.MoreIndTag(Index1, Index2: integer): boolean;
{==============================================================================}
begin
  Index1 := TempIndexList[Index1];
  Index2 := TempIndexList[Index2];
  Result := MoreTag(Index1, Index2);
end;

{==============================================================================}
function TCustomBase.MoreName(Index1, Index2: integer): boolean;
{==============================================================================}
begin
  Result :=
    AnsiUpperCase(GetName(Index1)) >
    AnsiUpperCase(GetName(Index2));
end;

{==============================================================================}
function TCustomBase.MoreTag(Index1, Index2: integer): boolean;
{==============================================================================}
var
  Tag1, Tag2: integer;
begin
  Tag1 := GetTag(Index1);
  Tag2 := GetTag(Index2);
  if (Tag1 <> Tag2) then
    Result := (Tag1 < Tag2)
  else
    Result := MoreName(Index1, Index2);
end;

{==============================================================================}
procedure TCustomBase.Sort();
{==============================================================================}
begin
  QuickSort(0, Count() - 1, Swap, MoreName);
end;

{==============================================================================}
procedure TCustomBase.SortIndexes(IndexList: TIndexList; SortType: TSortType);
{==============================================================================}
begin
  IndexList.Init(Count());
  TempIndexList := IndexList;

  case SortType of
    stName: QuickSort(0, Count() - 1, SwapInd, MoreIndName);
    stTag:  QuickSort(0, Count() - 1, SwapInd, MoreIndTag);
  end;

  TempIndexList := nil;
end;

{==============================================================================}
procedure TCustomBase.SwapInd(Index1, Index2: integer);
{==============================================================================}
begin
  TempIndexList.Swap(Index1, Index2);
end;

{ TFoodBase }

{==============================================================================}
function TFoodBase.Add(Food: TFood): integer;
{==============================================================================}
begin
  if (Food = nil) then
    raise Exception.Create('TFoodBase.Add(): Food is nil');

  { добавление в отсортированный список }
  Result := Length(FBase);
  SetLength(FBase, Result + 1);
  while (Result > 0) and (FBase[Result - 1].Name > Food.Name) do
  begin
    FBase[Result] := FBase[Result - 1];
    dec(Result);
  end;
  FBase[Result] := Food;
  Food.OnChange := FoodChangeHandler;

  {#}Changed;
end;

{==============================================================================}
function TFoodBase.Count(): integer;
{==============================================================================}
begin
  Result := Length(FBase);
end;

{==============================================================================}
procedure TFoodBase.Delete(Index: integer);
{==============================================================================}
var
  i: integer;
begin
  if (Index < 0) and (Index > High(FBase)) then
    raise ERangeError.Create(Format('TFoodBase.Delete(): Index out of bounds (%d)', [Index]));

  FBase[Index].Free;
  for i := Index to High(FBase) - 1 do
    FBase[i] := FBase[i + 1];
  SetLength(FBase, Length(FBase) - 1);

  {#}Changed;
end;

{==============================================================================}
destructor TFoodBase.Destroy;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to high(FBase) do
    FBase[i].Free;
  SetLength(FBase, 0);
end;

{==============================================================================}
procedure TFoodBase.FoodChangeHandler(Sender: TObject);
{==============================================================================}
begin
  Changed();
end;

{==============================================================================}
function TFoodBase.GetFood(Index: integer): TFood;
{==============================================================================}
begin
  if (Index < Low(FBase)) and (Index > High(FBase)) then
    raise ERangeError.Create(Format('TFoodBase.GetFood(): Index out of bounds (%d)', [Index]));

  Result := FBase[Index]
end;

{==============================================================================}
function TFoodBase.GetName(Index: integer): string;
{==============================================================================}
begin
  Result := GetFood(Index).Name;
end;

{==============================================================================}
function TFoodBase.GetTag(Index: integer): integer;
{==============================================================================}
begin
  Result := GetFood(Index).Tag;
end;

{==============================================================================}
procedure TFoodBase.LoadFromFile_Old(const FileName: string);
{==============================================================================}
const
  FOODBASE_FORMAT = 'FBASEFMT';
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;

  try
    s.LoadFromFile(FileName);

    if (s.Count > 1) and (s[0] = FOODBASE_FORMAT) then
    begin
      //FTimeStamp := StrToDateTime(s[1]);
      s.Delete(0); { 'FBASEFMT' }
      s.Delete(0); { '02.02.2011 23:29:23' }
    end else
    begin
      //FTimeStamp := 0;
    end;

    FVersion := 1;
    SetLength(FBase, s.Count);

    for i := 0 to s.Count - 1 do
    begin
      FBase[i] := TFood.Create();
      FBase[i].Read(S[i]);
      FBase[i].OnChange := FoodChangeHandler;
    end;
  finally
    s.Free;
  end;

  Sort();
end;

{==============================================================================}
procedure TFoodBase.LoadFromFile_XML(const FileName: string);
{==============================================================================}
const
  DEBUG = False;

  procedure CheckNode(Node: IXMLNODE; Index: integer);
  const
    A: array[1..6] of string = ('name', 'prots', 'fats', 'carbs', 'val', 'table');
  var
    i: integer;
  begin
    for i := Low(A) to High(A) do
      if (not Node.HasAttribute(A[i])) then
        raise Exception.CreateFmt('Food node #%d has no %s attribute', [Index, A[i]]);
  end;

var
  XML: IXMLDocument;
  Root, FoodNode: IXMLNODE;
  i: integer;
  DS: char;
begin
  (*
  { если файл не существует, база пуста }
  if not FileExists(FileName) then
  begin
    if CanCreate then
      Result := CreateFile(FileName)
    else
      Result := False;
    SetLength(FBase, 0);
    Exit;
  end;
  *)

  Log('TFoodBase.LoadFromFile_XML(): filename is "' + FileName + '"', DEBUG);

  DS := SysUtils.DecimalSeparator;

  try
    Log('TFoodBase.LoadFromFile_XML(): loading XML...', DEBUG);

    XML := LoadXMLDocument(FileName);

    SysUtils.DecimalSeparator := '.';
    Log('TFoodBase.LoadFromFile_XML(): decimal separator is setted to "' + SysUtils.DecimalSeparator + '"', DEBUG);
    Log('TFoodBase.LoadFromFile_XML(): XML.AsyncLoadState = ' + IntToStr(XML.AsyncLoadState), DEBUG);

    if (XML.Active) then
      Log('TFoodBase.LoadFromFile_XML(): XML.Active = True', DEBUG)
    else
      Log('TFoodBase.LoadFromFile_XML(): XML.Active = False', DEBUG);

    Log('TFoodBase.LoadFromFile_XML(): XML version is ' + XML.Version, DEBUG);
    Log('TFoodBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', DEBUG);

    if (XML.Version = '1.0') then
    begin
      Log('TFoodBase.LoadFromFile_XML(): XML version is supported', DEBUG);

      Root := XML.DocumentElement;
      FVersion := Root.Attributes['version'];
      SetLength(FBase, Root.ChildNodes.Count);

      Log('TFoodBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', DEBUG);
      Log('TFoodBase.LoadFromFile_XML(): count = ' + IntToStr(Root.ChildNodes.Count), DEBUG);

      {=========================================================}
      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        FoodNode := Root.ChildNodes[i];
        // TODO: debug only
        //CheckNode(FoodNode, i);

        FBase[i]           := TFood.Create();
        FBase[i].Name      := FoodNode.Attributes['name'];

        {FBase[i].RelProts  := FoodNode.Attributes['prots'];
        FBase[i].RelFats   := FoodNode.Attributes['fats'];
        FBase[i].RelCarbs  := FoodNode.Attributes['carbs'];
        FBase[i].RelValue  := FoodNode.Attributes['val'];  }
        FBase[i].RelProts  := StrToFloat(VarAsType(FoodNode.Attributes['prots'], varOleStr));
        FBase[i].RelFats   := StrToFloat(VarAsType(FoodNode.Attributes['fats'], varOleStr));
        FBase[i].RelCarbs  := StrToFloat(VarAsType(FoodNode.Attributes['carbs'], varOleStr));
        FBase[i].RelValue  := StrToFloat(VarAsType(FoodNode.Attributes['val'], varOleStr));

        FBase[i].FromTable := FoodNode.Attributes['table'];
        FBase[i].OnChange  := FoodChangeHandler;
      end;
      {=========================================================}

      Log('TFoodBase.LoadFromFile_XML(): data fetched OK', DEBUG);
    end else
    begin
      Log('TFoodBase.LoadFromFile_XML(): XML version is not supported, raising exception...', True);
      raise Exception.Create('FoodBase version ' + XML.Version + ' is not supported');
    end;

    Log('TFoodBase.LoadFromFile_XML(): Sorting data...', DEBUG);
    Sort();
  finally
    Log('TFoodBase.LoadFromFile_XML(): decimal separator "' + SysUtils.DecimalSeparator + '" is returned back to "' + DS + '"', DEBUG);
    SysUtils.DecimalSeparator := DS;
  end;

  Log('TFoodBase.LoadFromFile_XML(): DONE', DEBUG);
end;

{==============================================================================}
procedure TFoodBase.SaveToFile(const FileName: string);
{==============================================================================}
  (*try
    s := TStringList.Create;
    try
      { Time Stamp }
      FTimeStamp := now;
      s.Add(FOODBASE_FORMAT);
      s.Add(DateTimeToStr(FTimeStamp));

      { Data }
      for i := 0 to high(FBase) do
        s.Add(FBase[i].Write);

      s.SaveToFile(FileName);
    finally
      s.Free;
    end;
    Result := True;
  except
    Result := False;
  end;     *)

var
  XML: IXMLDocument;
  Root, FoodNode: IXMLNODE;
  i: integer;
  DS: char;
begin
  DS := SysUtils.DecimalSeparator;
  SysUtils.DecimalSeparator := '.';

  try
    XML := NewXMLDocument('1.0');
    XML.Encoding := 'utf-8';
    XML.NodeIndentStr := #9;
    XML.Options := [doNodeAutoIndent]; // looks better in Editor ;)

    Root := XML.AddChild('foods');
    Root.Attributes['version'] := FVersion;

    for i := 0 to High(FBase) do
    //if (FBase[i].FromTable) then
    begin
      FoodNode := Root.AddChild('food');
      FoodNode.Attributes['name']  := FBase[i].Name;
      FoodNode.Attributes['prots'] := FBase[i].RelProts;
      FoodNode.Attributes['fats']  := FBase[i].RelFats;
      FoodNode.Attributes['carbs'] := FBase[i].RelCarbs;
      FoodNode.Attributes['val']   := FBase[i].RelValue;
      FoodNode.Attributes['table'] := FBase[i].FromTable;
    end;

    XML.SaveToFile(FileName);
  finally
    SysUtils.DecimalSeparator := DS;
  end;
end;

{==============================================================================}
procedure TFoodBase.SetTag(Index, Value: integer);
{==============================================================================}
begin
  FBase[Index].Tag := Value;
end;

{==============================================================================}
procedure TFoodBase.Swap(Index1, Index2: integer);
{==============================================================================}
var
  Temp: TFood;
begin
  Temp := FBase[Index1];
  FBase[Index1] := FBase[Index2];
  FBase[Index2] := Temp;
end;

{ TDishBase }

{==============================================================================}
function TDishBase.Add(Dish: TDish): integer;
{==============================================================================}
begin
  if (Dish = nil) then
    raise Exception.Create('TDishBase.Add(): Dish is nil');

  { добавление в отсортированный список }
  Result := Length(FBase);
  SetLength(FBase, Result + 1);
  while (Result > 0) and (FBase[Result - 1].Name > Dish.Name) do
  begin
    FBase[Result] := FBase[Result - 1];
    dec(Result);
  end;
  FBase[Result] := Dish;
  Dish.OnChange := DishChangeHandler;

  {#}Changed;
end;

{==============================================================================}
procedure TDishBase.AnalyzeUsing(Base: TCustomBase);
{==============================================================================}
var
  i,j,n: integer;
begin
  for i := 0 to Base.Count - 1 do
    Base.SetTag(i, 0);

  for i := 0 to High(FBase) do
  for j := 0 to FBase[i].Count - 1 do
  begin
    n := Base.Find(FBase[i].Content[j].Name);
    if (n <> -1) then
      Base.SetTag(n, Base.GetTag(n) + 1);
  end;
end;

{==============================================================================}
function TDishBase.Count(): integer;
{==============================================================================}
begin
  Result := Length(FBase);
end;

{==============================================================================}
procedure TDishBase.Delete(Index: integer);
{==============================================================================}
var
  i: integer;
begin
  if (Index < 0) or (Index > High(FBase)) then
    raise ERangeError.CreateFmt('TDishBase.Delete(): index out of bounds (%d)', [Index]);

  FBase[Index].Free;
  for i := Index to High(FBase) - 1 do
    FBase[i] := FBase[i + 1];
  SetLength(FBase, Length(FBase) - 1);
  {#}Changed;
end;

{==============================================================================}
destructor TDishBase.Destroy;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to high(FBase) do
    FBase[i].Free;
  SetLength(FBase, 0);
end;

{==============================================================================}
procedure TDishBase.DishChangeHandler(Sender: TObject);
{==============================================================================}
begin
  {#}Changed();
end;

{==============================================================================}
function TDishBase.GetDish(Index: integer): TDish;
{==============================================================================}
begin
  if (Index < 0) or (Index > high(FBase)) then
    raise ERangeError.CreateFmt('TDishBase.GetDish(): index out of bounds (%d)', [Index]);

  Result := FBase[Index]
end;

{==============================================================================}
function TDishBase.GetName(Index: integer): string;
{==============================================================================}
begin
  Result := GetDish(Index).Name;
end;

{==============================================================================}
function TDishBase.GetTag(Index: integer): integer;
{==============================================================================}
begin
  Result := GetDish(Index).Tag;
end;

{==============================================================================}
procedure TDishBase.LoadFromFile_Old(const FileName: string; FoodBase: TFoodBase = nil);
{==============================================================================}
const
  DISHBASE_FORMAT = 'DBASEFMT';
var
  s: TStrings;
  buf: string;
  i,j,n,w: integer;
  StartLine: integer;

  TempFood: TFoodMassed;
begin
  {if not FileExists(FileName) then
  begin
    if CanCreate then
      Result := CreateFile(FileName)
    else
      Result := False;
    SetLength(FBase,0);
    FTimeStamp := Now;
    Exit;
  end;}

  s := TStringList.Create;

  try
    s.LoadFromFile(FileName);
    SetLength(FBase, 0);
    n := -1;

    if (s.Count > 1) and (s[0] = DISHBASE_FORMAT) then
    begin
      //FTimeStamp := StrToDateTime(s[1]);
      StartLine := 2;
    end else
    begin
      //FTimeStamp := 0;
      StartLine := 0;
    end;

    FVersion := 1;

    for i := StartLine to s.Count - 1 do
    if s[i] <> '' then
    if s[i][1] = '#' then
    begin
      inc(n);
      SetLength(FBase, n + 1);
      FBase[n] := TDish.Create;
      FBase[n].OnChange := DishChangeHandler;
      ///FBase[n].SilentMode := True;
      w := pos(':', s[i]);
      if w = 0 then
      begin
        FBase[n].Name := Copy(s[i], 2, length(s[i])-1);
      end else              //  123456789
      begin                 //  #name:123
        FBase[n].Name := Copy(s[i], 2, w - 2);
        FBase[n].ResultMass := StrToFloat(CheckDot(Copy(s[i], w + 1, length(s[i]) - w)));
      end;
    end else
    if s[i][1] = '%' then
    begin
      buf := Copy(s[i], 4, length(s[i]) - 3);
      FBase[n].ModifiedTime := StrToDateTime(buf);
    end else
    if s[i][1] <> '=' then
    begin
      { а вот тут самое интересное }
      { смотрим совместимость версий:  }

      TempFood := TFoodMassed.Create();

      { старый формат }
      if (pos('[', s[i]) = 0) then
      begin
        buf := TextBefore(s[i], ':');

        if (FoodBase <> nil) then
        begin
          j := FoodBase.Find(buf);
          if (j <> -1) then
            TempFood.CopyFrom(FoodBase[j])
          else
            TempFood.Name := buf + ' (не найдено)';
        end else
          TempFood.Name := buf + ' (не найдено)';

        TempFood.Mass := StrToFloat(CheckDot(TextAfter(s[i], ':')));
      end else

      { новый формат }
      begin
        TempFood.Read(s[i]);
      end;

      //FBase[n].SilentMode := False;
      FBase[n].Add(TempFood);
    end;
  finally
    s.Free;
  end;
  Sort();
end;

{==============================================================================}
procedure TDishBase.LoadFromFile_XML(const FileName: string);
{==============================================================================}
const
  DEBUG = False;

var
  XML: IXMLDocument;
  Root, DishNode, ItemNode: IXMLNODE;
  i, j: integer;
  Food: TFoodMassed;
  DS: char;
begin
  (*
  { если файл не существует, база пуста }
  if not FileExists(FileName) then
  begin
    if CanCreate then
      Result := CreateFile(FileName)
    else
      Result := False;
    SetLength(FBase, 0);
    Exit;
  end;
  *)

  Log('TDishBase.LoadFromFile_XML(): filename is "' + FileName + '"', DEBUG);

  DS := SysUtils.DecimalSeparator;

  try
    Log('TDishBase.LoadFromFile_XML(): loading XML...', DEBUG);

    XML := LoadXMLDocument(FileName);

    SysUtils.DecimalSeparator := '.';
    Log('TDishBase.LoadFromFile_XML(): decimal separator is setted to "' + SysUtils.DecimalSeparator + '"', DEBUG);
    Log('TDishBase.LoadFromFile_XML(): XML.AsyncLoadState = ' + IntToStr(XML.AsyncLoadState), DEBUG);

    if (XML.Active) then
      Log('TDishBase.LoadFromFile_XML(): XML.Active = True', DEBUG)
    else
      Log('TDishBase.LoadFromFile_XML(): XML.Active = False', DEBUG);

    Log('TDishBase.LoadFromFile_XML(): XML version is ' + XML.Version, DEBUG);
    Log('TDishBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', DEBUG);

    if (XML.Version = '1.0') then
    begin
      Log('TDishBase.LoadFromFile_XML(): XML version is supported', DEBUG);

      Root := XML.DocumentElement;
      FVersion := Root.Attributes['version'];
      SetLength(FBase, Root.ChildNodes.Count);

      Log('TDishBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', DEBUG);
      Log('TDishBase.LoadFromFile_XML(): count = ' + IntToStr(Root.ChildNodes.Count), DEBUG);

      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        DishNode := Root.ChildNodes[i];

        FBase[i] := TDish.Create();
        FBase[i].Name := DishNode.Attributes['name'];
        if DishNode.HasAttribute('mass') then
          FBase[i].SetResultMass(DishNode.Attributes['mass']);

        //SetLength(FBase[i].FContent, DishNode.ChildNodes.Count);

        for j := 0 to DishNode.ChildNodes.Count - 1 do
        begin
          ItemNode := DishNode.ChildNodes[j];

          Food := TFoodMassed.Create();
          Food.Name     := ItemNode.Attributes['name'];

          {Food.RelProts := ItemNode.Attributes['prots'];
          Food.RelFats  := ItemNode.Attributes['fats'];
          Food.RelCarbs := ItemNode.Attributes['carbs'];
          Food.RelValue := ItemNode.Attributes['val'];
          Food.Mass     := ItemNode.Attributes['mass'];}
          Food.RelProts := StrToFloat(VarAsType(ItemNode.Attributes['prots'], varOleStr));
          Food.RelFats  := StrToFloat(VarAsType(ItemNode.Attributes['fats'], varOleStr));
          Food.RelCarbs := StrToFloat(VarAsType(ItemNode.Attributes['carbs'], varOleStr));
          Food.RelValue := StrToFloat(VarAsType(ItemNode.Attributes['val'], varOleStr));
          Food.Mass     := StrToFloat(VarAsType(ItemNode.Attributes['mass'], varOleStr));

          FBase[i].Add(Food);
        end;

        FBase[i].OnChange := DishChangeHandler;
        if DishNode.HasAttribute('time') then
          FBase[i].ModifiedTime := StrToDateTime(DishNode.Attributes['time'])
        else
          FBase[i].ModifiedTime := 0;
      end;

      Log('TDishBase.LoadFromFile_XML(): data fetched OK', DEBUG);
    end else
    begin
      Log('TDishBase.LoadFromFile_XML(): XML version is not supported, raising exception...', True);
      raise Exception.Create('DishBase version ' + XML.Version + ' is not supported');
    end;

    Log('TDishBase.LoadFromFile_XML(): Sorting data...', DEBUG);
    Sort();
  finally
    Log('TDishBase.LoadFromFile_XML(): decimal separator "' + SysUtils.DecimalSeparator + '" is returned back to "' + DS + '"', DEBUG);
    SysUtils.DecimalSeparator := DS;
  end;

  Log('TDishBase.LoadFromFile_XML(): DONE', DEBUG);
end;

{==============================================================================}
function TDishBase.RenameFood(const OldName, NewName: string): boolean;
{==============================================================================}
var
  i, j: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  for j := 0 to FBase[i].Count - 1 do
  if (FBase[i].Content[j].Name = OldName) then
  begin
    FBase[i].Content[j].Name := NewName;
    Result := True;
  end;

  if (Result) then Changed();
end;

{==============================================================================}
procedure TDishBase.SaveToFile(const FileName: string);
{==============================================================================}
 (*try
    s := TStringList.Create;
    try
      { Time Stamp }
      FTimeStamp := now;
      s.Add(DISHBASE_FORMAT);
      s.Add(DateTimeToStr(FTimeStamp));

      { Data }
      for i := 0 to high(FBase) do
      begin
        if FBase[i].FixedMass then
          s.Add('#'+FBase[i].Name+':'+RealToStr(FBase[i].RealMass))
        else
          s.Add('#'+FBase[i].Name);

        if FBase[i].ModifiedTime <> 0 then
          s.Add('%m '+DateTimeToStr(FBase[i].ModifiedTime));

        for j := 0 to high(FBase[i].FContent) do
          {s.Add(FFoodBase[FBase[i][j].FoodType].Name+
          ':'+RealToStr(FBase[i][j].AMass)); }
          s.Add(FBase[i][j].Write);
        s.Add('==============================');
      end;
      s.SaveToFile(FileName);
    finally
      s.Free;
    end;
    Result := True;
  except
    Result := False;
  end;  *)
var
  XML: IXMLDocument;
  Root, DishNode, ItemNode: IXMLNODE;
  i, j: integer;
  DS: char;
begin
  DS := SysUtils.DecimalSeparator;
  SysUtils.DecimalSeparator := '.';

  try
    XML := NewXMLDocument();
    XML.Encoding := 'utf-8';
    XML.Version := '1.0';
    XML.NodeIndentStr := #9;
    XML.Options := [doNodeAutoIndent];

    Root := XML.AddChild('dishes');
    Root.Attributes['version'] := FVersion;

    for i := 0 to High(FBase) do
    begin
      DishNode := Root.AddChild('dish');
      DishNode.Attributes['name'] := FBase[i].Name;
      DishNode.Attributes['time'] := FBase[i].ModifiedTime;
      if (FBase[i].FixedMass) then
        DishNode.Attributes['mass'] := FBase[i].ResultMass;

      for j := 0 to FBase[i].Count - 1 do
      begin
        ItemNode := DishNode.AddChild('item');

        ItemNode.Attributes['name']  := FBase[i].Content[j].Name;
        ItemNode.Attributes['prots'] := FBase[i].Content[j].RelProts;
        ItemNode.Attributes['fats']  := FBase[i].Content[j].RelFats;
        ItemNode.Attributes['carbs'] := FBase[i].Content[j].RelCarbs;
        ItemNode.Attributes['val']   := FBase[i].Content[j].RelValue;
        ItemNode.Attributes['mass']  := FBase[i].Content[j].Mass;
      end;
    end;

    XML.SaveToFile(FileName);
  finally
    SysUtils.DecimalSeparator := DS;
  end;
end;

{==============================================================================}
procedure TDishBase.SetTag(Index, Value: integer);
{==============================================================================}
begin
  FBase[Index].Tag := Value;
end;

{==============================================================================}
procedure TDishBase.Swap(Index1, Index2: integer);
{==============================================================================}
var
  Temp: TDish;
begin
  Temp := FBase[Index1];
  FBase[Index1] := FBase[Index2];
  FBase[Index2] := Temp;
end;

{==============================================================================}
function TDishBase.UsedFood(const FoodName: string): integer;
{==============================================================================}
var
  i,j: integer;
begin
  Result := -1;
  for i := 0 to high(FBase) do
  for j := 0 to FBase[i].Count-1 do
  if (FBase[i].Content[j].Name = FoodName) then
  begin
    Result := i;
    Exit;
  end;
end;

{ TPageCache }

{==============================================================================}
constructor TPageCache.Create(PageData: TPageData);
{==============================================================================}
begin
  inherited Create(PageData);
  FCalculatedPostprand := False;
end;

{==============================================================================}
function TPageCache.GetNextDayFreeTime(StdMealPeriod, ShortMealPeriod,
  InsPeriod: integer): integer;
{==============================================================================}
var
  CurTime, i: integer;
begin
  //Log('TPageCache.GetNextDayFreeTime()');

  Result := 0;

  // TODO: дублирующийся код (UpdatePostprand)
  for i := 0 to Count-1 do
  begin
    CurTime := Recs[i].Time;

    case Recs[i].RecType of
      rtIns:   Result := Max(Result, CurTime + InsPeriod);
      rtMeal:  if TMealRecord(Recs[i]).Carbs > 0 then
               if TMealRecord(Recs[i]).ShortMeal then
                 Result := Max(Result, CurTime + ShortMealPeriod)
               else
                 Result := Max(Result, CurTime + StdMealPeriod);
    end;
  end;

  Result := Result - MinPerDay;
end;

{==============================================================================}
procedure TPageCache.UpdatePostprand(FreeTime, StdMealPeriod,
  ShortMealPeriod, InsPeriod: integer);
{==============================================================================}
var
  CurTime, i: integer;
begin
  //Log('TPageCache.UpdatePostPrand()');

  // TODO: дублирующийся код (GetNextDayFreeTime)
  for i := 0 to Count-1 do
  begin
    CurTime := Recs[i].Time;

    case Recs[i].RecType of
      rtIns:   FreeTime := Max(FreeTime, CurTime + InsPeriod);
      rtMeal:  if TMealRecord(Recs[i]).Carbs > 0 then
               if TMealRecord(Recs[i]).ShortMeal then
                 FreeTime := Max(FreeTime, CurTime + ShortMealPeriod)
               else
                 FreeTime := Max(FreeTime, CurTime + StdMealPeriod);
      rtBlood: TBloodRecord(Recs[i]).PostPrand := (CurTime < FreeTime);
    end;
  end;
  FCalculatedPostprand := True;
end;

initialization
  ActiveX.CoInitialize(nil)
finalization
  ActiveX.CoUninitialize;
end.


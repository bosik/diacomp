unit Bases;

interface

uses
  BusinessObjects, DiaryRoutines, SysUtils, Classes, XMLDoc,
  XMLIntf, Autolog, Variants;

type
  // TODO 1: сделать оповещение базы (Changed()) при изменении любого поля

  { 1. БАЗЫ }

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

implementation

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

  procedure Read(Food: TFood; const S: string);
  var
    j: integer;
  begin
    j := 1;
    with Food do
    begin
      Name := ReadStrTo(S, '[', j);
      RelProts := ReadFloatTo(S, FOOD_SEP, j);
      RelFats  := ReadFloatTo(S, FOOD_SEP, j);
      RelCarbs := ReadFloatTo(S, FOOD_SEP, j);
      RelValue := ReadFloatTo(S, FOOD_SEP, j);
      FromTable := S[Length(S)] = '#';
    end;

    // TODO: log it outside
  end;

{
  function TFoodItem.Write_(): string;
  begin
    Result :=
      Name + '[' +
      RealToStr(RelProts) + FOOD_SEP +
      RealToStr(RelFats)  + FOOD_SEP +
      RealToStr(RelCarbs) + FOOD_SEP +
      RealToStr(RelValue) + ']';
    if FromTable then
      Result := Result + '#';
  end;
  }

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
      Read(FBase[i], S[i]);
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
  buf, mass: string;
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
    if (s[i][1] = '%') then
    begin
      buf := Copy(s[i], 4, length(s[i]) - 3);
      FBase[n].ModifiedTime := StrToDateTime(buf);
    end else
    if (s[i][1] <> '=') then
    begin
      { а вот тут самое интересное }
      { смотрим совместимость версий:  }

      TempFood := TFoodMassed.Create();

      { старый формат - со ссылкой на базу продуктов}
      if (pos('[', s[i]) = 0) then
      begin
        buf := TextBefore(s[i], ':');
        mass := TextAfter(s[i], ':');

        if (FoodBase <> nil) then
        begin
          j := FoodBase.Find(buf);
          if (j <> -1) then
            TempFood.CopyFrom(FoodBase[j].AsFoodMassed(StrToFloat(CheckDot(mass))))
          else
            TempFood.Name := buf + ' (не найдено)';
        end else
          TempFood.Name := buf + ' (не найдено)';
      end else

      { новый формат - независимый }
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

end.

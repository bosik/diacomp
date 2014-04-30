unit Bases;

interface

uses
  BusinessObjects, DiaryRoutines, SysUtils, Classes, XMLDoc,
  XMLIntf, Autolog, Variants, ActiveX;

type
  // TODO 1: сделать оповещение базы (Changed()) при изменении любого пол€

  TSortType = (stName, stTag);

  TVersioned = class
  private
    FID: string;
    FTimeStamp: TDateTime;
    FVersion: integer;
    FDeleted: boolean;
    FData: TObject;
  public
    property ID: string read FID write FID;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property Version: integer read FVersion write FVersion;
    property Deleted: boolean read FDeleted write FDeleted;
    property Data: TObject read FData write FData;
  end;

  // »меет методы дл€ бинарного поиска
  // »меет методы дл€ сортировки
  // »меет номер версии и метод дл€ еЄ инкремента
  TAbstractBase = class
  private
    TempIndexList: TIndexList;
    FVersion: integer;
    procedure ItemChangeHandler(Sender: TObject);
    function MoreName(Index1, Index2: integer): boolean;
    function MoreTag(Index1, Index2: integer): boolean;
    function MoreIndName(Index1, Index2: integer): boolean;
    function MoreIndTag(Index1, Index2: integer): boolean;
    procedure Swap(Index1, Index2: integer); virtual; abstract;
    procedure SwapInd(Index1, Index2: integer);
    function TraceLast(): integer;
  protected
    function GetName(Index: integer): string; virtual; abstract;
    function GetTag(Index: integer): integer; virtual; abstract;
    procedure SetTag(Index, Value: integer); virtual; abstract;
    procedure Changed;
  public
    function Count: integer; virtual; abstract;
    constructor Create;
    procedure Delete(Index: integer); virtual; abstract;
    function Find(const ItemName: string): integer;
    procedure Sort(); // вызываетс€ потомками после загрузки
    procedure SortIndexes(IndexList: TIndexList; SortType: TSortType);

    property Version: integer read FVersion write FVersion;
  end;

  TArrayBase = class(TAbstractBase)
  private
    FBase: array of TMutableItem;
  protected
    function GetItem(Index: integer): TMutableItem;
    procedure Swap(Index1, Index2: integer); override;
  public
    function Add(Item: TMutableItem): integer; virtual;
    procedure Clear;
    function Count: integer; override;
    procedure Delete(Index: integer); override;
    destructor Destroy; override;
    function GetIndex(ID: TCompactGUID): integer; 
  end;

  TFoodBase = class(TArrayBase)
  private
    function GetFood(Index: integer): TFood;
  protected
    function GetName(Index: integer): string; override;
    function GetTag(Index: integer): integer; override;
    procedure SetTag(Index, Value: integer); override;
  public
    function Add(Food: TFood): integer; reintroduce;
    procedure LoadFromFile_Old(const FileName: string);
    procedure LoadFromFile_XML(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property Items[Index: integer]: TFood read GetFood; default;
  end;

  TDishBase = class(TArrayBase)
  private
    function GetDish(Index: integer): TDish;
  protected
    function GetName(Index: integer): string; override;
    function GetTag(Index: integer): integer; override;
    procedure SetTag(Index, Value: integer); override;
  public
    function Add(Dish: TDish): integer; reintroduce;
    procedure LoadFromFile_Old(const FileName: string);
    procedure LoadFromFile_XML(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure AnalyzeUsing(Base: TAbstractBase);
    {#}function RenameFood(const OldName, NewName: string): boolean;
    function UsedFood(const FoodName: string): integer;

    property Items[Index: integer]: TDish read GetDish; default;
  end;

implementation

{ TAbstractBase }

{==============================================================================}
procedure TAbstractBase.Changed;
{==============================================================================}
begin
  inc(FVersion);
end;

{==============================================================================}
constructor TAbstractBase.Create;
{==============================================================================}
begin
  FVersion := 0;
end;

{==============================================================================}
function TAbstractBase.Find(const ItemName: string): integer;
{==============================================================================}

// поиск на полное соответствие (без учЄта регистра)

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
procedure TAbstractBase.ItemChangeHandler(Sender: TObject);
{==============================================================================}
begin
  Changed();
end;

{==============================================================================}
function TAbstractBase.MoreIndName(Index1, Index2: integer): boolean;
{==============================================================================}
begin
  Index1 := TempIndexList[Index1];
  Index2 := TempIndexList[Index2];
  Result := MoreName(Index1, Index2);
end;

{==============================================================================}
function TAbstractBase.MoreIndTag(Index1, Index2: integer): boolean;
{==============================================================================}
begin
  Index1 := TempIndexList[Index1];
  Index2 := TempIndexList[Index2];
  Result := MoreTag(Index1, Index2);
end;

{==============================================================================}
function TAbstractBase.MoreName(Index1, Index2: integer): boolean;
{==============================================================================}
begin
  Result :=
    AnsiUpperCase(GetName(Index1)) >
    AnsiUpperCase(GetName(Index2));
end;

{==============================================================================}
function TAbstractBase.MoreTag(Index1, Index2: integer): boolean;
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
procedure TAbstractBase.Sort();
{==============================================================================}
begin
  QuickSort(0, Count() - 1, Swap, MoreName);
end;

{==============================================================================}
procedure TAbstractBase.SortIndexes(IndexList: TIndexList; SortType: TSortType);
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
procedure TAbstractBase.SwapInd(Index1, Index2: integer);
{==============================================================================}
begin
  TempIndexList.Swap(Index1, Index2);
end;

{==============================================================================}
function TAbstractBase.TraceLast: integer;
{==============================================================================}
begin
  Result := Count - 1;
  while (Result > 0) and (MoreName(Result - 1, Result)) do
  begin
    Swap(Result, Result - 1);
    dec(Result);
  end;
end;

{ TArrayBase }

{==============================================================================}
function TArrayBase.Add(Item: TMutableItem): integer;
{==============================================================================}
begin
  if (Item = nil) then
    raise EInvalidPointer.Create('Can''t add nil object into the list');

  SetLength(FBase, Count + 1);
  FBase[Count - 1] := Item;
  Result := TraceLast;

  Item.OnChange := ItemChangeHandler;

  {#}Changed;
end;

{==============================================================================}
procedure TArrayBase.Clear;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FBase) do
    FBase[i].Free;
  SetLength(FBase, 0);

  Changed;
end;

{==============================================================================}
function TArrayBase.Count: integer;
{==============================================================================}
begin
  Result := Length(FBase);
end;

{==============================================================================}
procedure TArrayBase.Delete(Index: integer);
{==============================================================================}
var
  i: integer;
begin
  if (Index < Low(FBase)) and (Index > High(FBase)) then
    raise ERangeError.Create(Format('TArrayBase.Delete(): Index out of bounds (%d)', [Index]));

  FBase[Index].Free;
  for i := Index to High(FBase) - 1 do
    FBase[i] := FBase[i + 1];
  SetLength(FBase, Length(FBase) - 1);

  Changed;
end;

{==============================================================================}
destructor TArrayBase.Destroy;
{==============================================================================}
begin
  Clear;
end;

{==============================================================================}
function TArrayBase.GetIndex(ID: TCompactGUID): integer;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FBase) do
  if (FBase[i].ID = ID) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

{==============================================================================}
function TArrayBase.GetItem(Index: integer): TMutableItem;
{==============================================================================}
begin
  if (Index < Low(FBase)) and (Index > High(FBase)) then
    raise ERangeError.Create(Format('TArrayBase.GetItem(): Index out of bounds (%d)', [Index]));

  Result := FBase[Index];
end;

{==============================================================================}
procedure TArrayBase.Swap(Index1, Index2: integer);
{==============================================================================}
var
  Temp: TMutableItem;
begin
  if (Index1 < Low(FBase)) and (Index1 > High(FBase)) then
    raise ERangeError.Create(Format('TArrayBase.GetItem(): Index out of bounds (%d)', [Index1]));
  if (Index2 < Low(FBase)) and (Index2 > High(FBase)) then
    raise ERangeError.Create(Format('TArrayBase.GetItem(): Index out of bounds (%d)', [Index2]));

  Temp := FBase[Index1];
  FBase[Index1] := FBase[Index2];
  FBase[Index2] := Temp;
end;

{ TFoodBase }

{==============================================================================}
function TFoodBase.Add(Food: TFood): integer;
{==============================================================================}
begin
  Result := inherited Add(Food);
end;

{==============================================================================}
function TFoodBase.GetFood(Index: integer): TFood;
{==============================================================================}
begin
  Result := TFood(GetItem(Index));
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
  Temp: TFood;
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
      Temp := TFood.Create();
      Read(Temp, S[i]);
      Temp.OnChange := ItemChangeHandler;
      FBase[i] := Temp;
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
  LOGGING = False;

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

  Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): filename is "' + FileName + '"', LOGGING);

  DS := SysUtils.DecimalSeparator;

  try
    Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): loading XML...', LOGGING);

    XML := LoadXMLDocument(FileName);

    SysUtils.DecimalSeparator := '.';
    Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): decimal separator is setted to "' + SysUtils.DecimalSeparator + '"', LOGGING);
    Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): XML.AsyncLoadState = ' + IntToStr(XML.AsyncLoadState), LOGGING);

    if (XML.Active) then
      Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): XML.Active = True', LOGGING)
    else
      Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): XML.Active = False', LOGGING);

    Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): XML version is ' + XML.Version, LOGGING);
    Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', LOGGING);

    if (XML.Version = '1.0') then
    begin
      Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): XML version is supported', LOGGING);

      Root := XML.DocumentElement;
      FVersion := Root.Attributes['version'];
      SetLength(FBase, Root.ChildNodes.Count);

      Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', LOGGING);
      Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): count = ' + IntToStr(Root.ChildNodes.Count), LOGGING);

      {=========================================================}
      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        FoodNode := Root.ChildNodes[i];
        // TODO: debug only
        //CheckNode(FoodNode, i);

        FBase[i]           := TFood.Create();
        if (FoodNode.HasAttribute('id')) then
          Items[i].ID := FoodNode.Attributes['id']
        else
          Items[i].ID := CreateCompactGUID();
        Items[i].Name      := FoodNode.Attributes['name'];
        {Items[i].RelProts := FoodNode.Attributes['prots'];
        Items[i].RelFats   := FoodNode.Attributes['fats'];
        Items[i].RelCarbs  := FoodNode.Attributes['carbs'];
        Items[i].RelValue  := FoodNode.Attributes['val'];  }
        Items[i].RelProts  := StrToFloat(VarAsType(FoodNode.Attributes['prots'], varOleStr));
        Items[i].RelFats   := StrToFloat(VarAsType(FoodNode.Attributes['fats'], varOleStr));
        Items[i].RelCarbs  := StrToFloat(VarAsType(FoodNode.Attributes['carbs'], varOleStr));
        Items[i].RelValue  := StrToFloat(VarAsType(FoodNode.Attributes['val'], varOleStr));

        Items[i].FromTable := FoodNode.Attributes['table'];
        if (FoodNode.HasAttribute('tag')) then
          Items[i].Tag := FoodNode.Attributes['tag']
        else
          Items[i].Tag := 0;

        Items[i].OnChange  := ItemChangeHandler;
      end;
      {=========================================================}

      Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): data fetched OK', LOGGING);
    end else
    begin
      Log(ERROR, 'TFoodBase.LoadFromFile_XML(): XML version ' + XML.Version + ' is not supported', LOGGING);
      raise Exception.Create('FoodBase version ' + XML.Version + ' is not supported');
    end;

    Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): Sorting data...', LOGGING);
    Sort();
  finally
    Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): decimal separator "' + SysUtils.DecimalSeparator + '" is returned back to "' + DS + '"', LOGGING);
    SysUtils.DecimalSeparator := DS;
  end;

  Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): DONE', LOGGING);
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
      for i := 0 to High(FBase) do
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
    begin
      FoodNode := Root.AddChild('food');
      FoodNode.Attributes['id']    := Items[i].ID;
      FoodNode.Attributes['name']  := Items[i].Name;
      FoodNode.Attributes['prots'] := Items[i].RelProts;
      FoodNode.Attributes['fats']  := Items[i].RelFats;
      FoodNode.Attributes['carbs'] := Items[i].RelCarbs;
      FoodNode.Attributes['val']   := Items[i].RelValue;
      FoodNode.Attributes['table'] := Items[i].FromTable;
      FoodNode.Attributes['tag']   := Items[i].Tag;
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
  Items[Index].Tag := Value;
end;

{ TDishBase }

{==============================================================================}
function TDishBase.Add(Dish: TDish): integer;
{==============================================================================}
begin
  Result := inherited Add(Dish);
end;

{==============================================================================}
procedure TDishBase.AnalyzeUsing(Base: TAbstractBase);
{==============================================================================}
var
  i,j,n: integer;
begin
  for i := 0 to Base.Count - 1 do
    Base.SetTag(i, 0);

  for i := 0 to Count - 1 do
  for j := 0 to Items[i].Count - 1 do
  begin
    n := Base.Find(Items[i].Content[j].Name);
    if (n <> -1) then
      Base.SetTag(n, Base.GetTag(n) + 1);
  end;
end;

{==============================================================================}
function TDishBase.GetDish(Index: integer): TDish;
{==============================================================================}
begin
  if (Index < 0) or (Index > High(FBase)) then
    raise ERangeError.CreateFmt('TDishBase.GetDish(): index out of bounds (%d)', [Index]);

  Result := TDish(FBase[Index]);
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
procedure TDishBase.LoadFromFile_Old(const FileName: string);
{==============================================================================}
const
  DISHBASE_FORMAT = 'DBASEFMT';
var
  s: TStrings;
  buf, mass: string;
  i,n,w: integer;
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
      Items[n].OnChange := ItemChangeHandler;
      ///FBase[n].SilentMode := True;
      w := pos(':', s[i]);
      if w = 0 then
      begin
        Items[n].Name := Copy(s[i], 2, length(s[i])-1);
      end else              //  123456789
      begin                 //  #name:123
        Items[n].Name := Copy(s[i], 2, w - 2);
        Items[n].ResultMass := StrToFloat(CheckDot(Copy(s[i], w + 1, length(s[i]) - w)));
      end;
    end else
    if (s[i][1] = '%') then
    begin
      buf := Copy(s[i], 4, length(s[i]) - 3);
      Items[n].ModifiedTime := StrToDateTime(buf);
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

        {if (FoodBase <> nil) then
        begin
          j := FoodBase.Find(buf);
          if (j <> -1) then
            TempFood.CopyFrom(FoodBase[j].AsFoodMassed(StrToFloat(CheckDot(mass))))
          else
            TempFood.Name := buf + ' (не найдено)';
        end else }
          TempFood.Name := buf + ' (не найдено)';
      end else

      { новый формат - независимый }
      begin
        TempFood.Read(s[i]);
      end;

      //Items[n].SilentMode := False;
      Items[n].Add(TempFood);
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
  LOGGING = False;

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

  Log(DEBUG, 'TDishBase.LoadFromFile_XML(): filename is "' + FileName + '"', LOGGING);

  DS := SysUtils.DecimalSeparator;

  try
    Log(DEBUG, 'TDishBase.LoadFromFile_XML(): loading XML...', LOGGING);

    XML := LoadXMLDocument(FileName);

    SysUtils.DecimalSeparator := '.';
    Log(DEBUG, 'TDishBase.LoadFromFile_XML(): decimal separator is setted to "' + SysUtils.DecimalSeparator + '"', LOGGING);
    Log(DEBUG, 'TDishBase.LoadFromFile_XML(): XML.AsyncLoadState = ' + IntToStr(XML.AsyncLoadState), LOGGING);

    if (XML.Active) then
      Log(DEBUG, 'TDishBase.LoadFromFile_XML(): XML.Active = True', LOGGING)
    else
      Log(DEBUG, 'TDishBase.LoadFromFile_XML(): XML.Active = False', LOGGING);

    Log(DEBUG, 'TDishBase.LoadFromFile_XML(): XML version is ' + XML.Version, LOGGING);
    Log(DEBUG, 'TDishBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', LOGGING);

    if (XML.Version = '1.0') then
    begin
      Log(DEBUG, 'TDishBase.LoadFromFile_XML(): XML version is supported', LOGGING);

      Root := XML.DocumentElement;
      FVersion := Root.Attributes['version'];
      SetLength(FBase, Root.ChildNodes.Count);

      Log(DEBUG, 'TDishBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', LOGGING);
      Log(DEBUG, 'TDishBase.LoadFromFile_XML(): count = ' + IntToStr(Root.ChildNodes.Count), LOGGING);

      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        DishNode := Root.ChildNodes[i];

        FBase[i] := TDish.Create();
        Items[i].Name := DishNode.Attributes['name'];
        if DishNode.HasAttribute('mass') then
          Items[i].SetResultMass(DishNode.Attributes['mass']);
        if DishNode.HasAttribute('tag') then
          Items[i].Tag := DishNode.Attributes['tag']
        else
          Items[i].Tag := 0;

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

          Items[i].Add(Food);
        end;

        Items[i].OnChange := ItemChangeHandler;
        if DishNode.HasAttribute('time') then
          Items[i].ModifiedTime := StrToDateTime(DishNode.Attributes['time'])
        else
          Items[i].ModifiedTime := 0;
      end;

      Log(DEBUG, 'TDishBase.LoadFromFile_XML(): data fetched OK', LOGGING);
    end else
    begin
      Log(ERROR, 'TDishBase.LoadFromFile_XML(): XML version ' + XML.Version + ' is not supported', True);
      raise Exception.Create('DishBase version ' + XML.Version + ' is not supported');
    end;

    Log(DEBUG, 'TDishBase.LoadFromFile_XML(): Sorting data...', LOGGING);
    Sort();
  finally
    Log(DEBUG, 'TDishBase.LoadFromFile_XML(): decimal separator "' + SysUtils.DecimalSeparator + '" is returned back to "' + DS + '"', LOGGING);
    SysUtils.DecimalSeparator := DS;
  end;

  Log(DEBUG, 'TDishBase.LoadFromFile_XML(): DONE', LOGGING);
end;

{==============================================================================}
function TDishBase.RenameFood(const OldName, NewName: string): boolean;
{==============================================================================}
var
  i, j: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  for j := 0 to Items[i].Count - 1 do
  if (Items[i].Content[j].Name = OldName) then
  begin
    Items[i].Content[j].Name := NewName;
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
      for i := 0 to High(FBase) do
      begin
        if FBase[i].FixedMass then
          s.Add('#'+FBase[i].Name+':'+RealToStr(FBase[i].RealMass))
        else
          s.Add('#'+FBase[i].Name);

        if FBase[i].ModifiedTime <> 0 then
          s.Add('%m '+DateTimeToStr(FBase[i].ModifiedTime));

        for j := 0 to High(FBase[i].FContent) do
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
      DishNode.Attributes['name'] := Items[i].Name;
      DishNode.Attributes['time'] := Items[i].ModifiedTime;
      DishNode.Attributes['tag'] := Items[i].Tag;
      if (Items[i].FixedMass) then
        DishNode.Attributes['mass'] := Items[i].ResultMass;

      for j := 0 to Items[i].Count - 1 do
      begin
        ItemNode := DishNode.AddChild('item');

        ItemNode.Attributes['name']  := Items[i].Content[j].Name;
        ItemNode.Attributes['prots'] := Items[i].Content[j].RelProts;
        ItemNode.Attributes['fats']  := Items[i].Content[j].RelFats;
        ItemNode.Attributes['carbs'] := Items[i].Content[j].RelCarbs;
        ItemNode.Attributes['val']   := Items[i].Content[j].RelValue;
        ItemNode.Attributes['mass']  := Items[i].Content[j].Mass;
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
  Items[Index].Tag := Value;
end;

{==============================================================================}
function TDishBase.UsedFood(const FoodName: string): integer;
{==============================================================================}
var
  i, j: integer;
begin
  Result := -1;
  for i := 0 to High(FBase) do
  for j := 0 to Items[i].Count - 1 do
  if (Items[i].Content[j].Name = FoodName) then
  begin
    Result := i;
    Exit;
  end;
end;

initialization
  ActiveX.CoInitialize(nil)
finalization
  ActiveX.CoUninitialize;
end.

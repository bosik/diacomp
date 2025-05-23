unit Bases;

interface

uses
  BusinessObjects, DiaryRoutines, SysUtils, Classes, XMLDoc,
  XMLIntf, Autolog, Variants, ActiveX;

type
  TSortType = (stName, stTag);

{======================================================================================================================}

  TAbstractBase = class
  private
    FBase: array of TVersioned;
    TempIndexList: TIndexList;

    // comparators
    function MoreName(Index1, Index2: integer): boolean;
    function MoreTag(Index1, Index2: integer): boolean;
    function MoreIndName(Index1, Index2: integer): boolean;
    function MoreIndTag(Index1, Index2: integer): boolean;

    procedure Swap(Index1, Index2: integer);
    procedure SwapInd(Index1, Index2: integer);

    function TraceLast(): integer;
  protected
    function GetName(Index: integer): string; virtual; abstract;
    function GetTag(Index: integer): integer; virtual; abstract;
    procedure SetTag(Index, Value: integer); virtual; abstract;
    function GetItem(Index: integer): TVersioned; virtual;
  public
    function Add(Item: TVersioned): integer; virtual;

    // Remove all items
    procedure Clear();

    // Returns total number of items
    function Count(): integer;

    destructor Destroy; override;

    // Search for item with the ID specified. All items are considered
    function FindById(ID: TCompactGUID): integer;

    // Search for item with the name specified (case-insensitive). Non-deleted items are considered only
    function FindByName(const ItemName: string): integer;

    // Sort items by name
    procedure Sort();

    // Sort the specified list using specified sort type
    procedure SortIndexes(IndexList: TIndexList; SortType: TSortType);
  end;

{======================================================================================================================}

  TFoodBase = class(TAbstractBase)
  private
    function GetFood(Index: integer): TFoodItem;
  protected
    function GetName(Index: integer): string; override;
    function GetTag(Index: integer): integer; override;
    procedure SetTag(Index, Value: integer); override;
  public
    function Add(Food: TFoodItem): integer; reintroduce;
    procedure LoadFromFile_Old(const FileName: string);
    procedure LoadFromFile_XML(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property Items[Index: integer]: TFoodItem read GetFood; default;
  end;

{======================================================================================================================}

  TDishBase = class(TAbstractBase)
  private
    function GetDish(Index: integer): TDishItem;
  protected
    function GetName(Index: integer): string; override;
    function GetTag(Index: integer): integer; override;
    procedure SetTag(Index, Value: integer); override;
  public
    function Add(Dish: TDishItem): integer; reintroduce;
    procedure LoadFromFile_Old(const FileName: string);
    procedure LoadFromFile_XML(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure AnalyzeUsing(Base: TAbstractBase);
    {#}function RenameFood(const OldName, NewName: string): boolean;
    function UsedFood(const FoodName: string): integer;

    property Items[Index: integer]: TDishItem read GetDish; default;
  end;

{======================================================================================================================}

implementation

const
  FIELD_FOOD_ID         = 'id';
  FIELD_FOOD_TIMESTAMP  = 'timestamp';
  FIELD_FOOD_HASH       = 'hash';
  FIELD_FOOD_VERSION    = 'version';
  FIELD_FOOD_DELETED    = 'deleted';
  FIELD_FOOD_NAME       = 'name';
  FIELD_FOOD_PROTS      = 'prots';
  FIELD_FOOD_FATS       = 'fats';
  FIELD_FOOD_CARBS      = 'carbs';
  FIELD_FOOD_VALUE      = 'val';
  FIELD_FOOD_TABLE      = 'table';
  FIELD_FOOD_TAG        = 'tag';

  FIELD_DISH_ID         = 'id';
  FIELD_DISH_TIMESTAMP  = 'timestamp';
  FIELD_DISH_HASH       = 'hash';
  FIELD_DISH_VERSION    = 'version';
  FIELD_DISH_DELETED    = 'deleted';
  FIELD_DISH_NAME       = 'name';
  FIELD_DISH_MASS       = 'mass';
  FIELD_DISH_TAG        = 'tag';
  FIELD_DISH_FOOD_NAME  = 'name';
  FIELD_DISH_FOOD_PROTS = 'prots';
  FIELD_DISH_FOOD_FATS  = 'fats';
  FIELD_DISH_FOOD_CARBS = 'carbs';
  FIELD_DISH_FOOD_VALUE = 'val';
  FIELD_DISH_FOOD_MASS  = 'mass';

{ TAbstractBase }

{======================================================================================================================}
function TAbstractBase.Add(Item: TVersioned): integer;
{======================================================================================================================}
begin
  if (Item = nil) then
    raise EInvalidPointer.Create('Can''t add nil object into the list');

  SetLength(FBase, Count + 1);
  FBase[Count - 1] := Item;
  Result := TraceLast();
end;

{======================================================================================================================}
procedure TAbstractBase.Clear();
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to High(FBase) do
    FreeAndNil(FBase[i]);
  SetLength(FBase, 0);
end;

{======================================================================================================================}
function TAbstractBase.Count(): integer;
{======================================================================================================================}
begin
  Result := Length(FBase);
end;

{======================================================================================================================}
destructor TAbstractBase.Destroy;
{======================================================================================================================}
begin
  Clear();
  inherited;
end;

{======================================================================================================================}
function TAbstractBase.FindById(ID: TCompactGUID): integer;
{======================================================================================================================}
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

{======================================================================================================================}
function TAbstractBase.FindByName(const ItemName: string): integer;
{======================================================================================================================}

// linear version

var
  i: integer;
  UpperItemName: string;
begin
  UpperItemName := AnsiUpperCase(ItemName);

  for i := 0 to Count - 1 do
  if (AnsiUpperCase(GetName(i)) = UpperItemName) and
     (not GetItem(i).Deleted) then
  begin
    Result := i;
    Exit;
  end;

  Result := -1;
end;

// binary version

{var
  l,r,k: integer;
  Upper, Cur: string;
begin
  l := 0;
  r := Count - 1;

  Upper := AnsiUpperCase(ItemName);

  while (l <= r) do
  begin
    k := (l + r) div 2;
    Cur := AnsiUpperCase(GetName(k));
    if (Cur < Upper) then l := k + 1 else
    if (Cur > Upper) then r := k - 1 else
    begin
      Result := k;
      Exit;
    end;
  end;

  Result := -1;
end; }

{======================================================================================================================}
function TAbstractBase.GetItem(Index: integer): TVersioned;
{======================================================================================================================}
begin
  if (Index < Low(FBase)) and (Index > High(FBase)) then
    raise ERangeError.Create(Format('TArrayBase.GetItem(): Index out of bounds (%d)', [Index]));

  Result := FBase[Index];
end;

{======================================================================================================================}
function TAbstractBase.MoreIndName(Index1, Index2: integer): boolean;
{======================================================================================================================}
begin
  Index1 := TempIndexList[Index1];
  Index2 := TempIndexList[Index2];
  Result := MoreName(Index1, Index2);
end;

{======================================================================================================================}
function TAbstractBase.MoreIndTag(Index1, Index2: integer): boolean;
{======================================================================================================================}
begin
  Index1 := TempIndexList[Index1];
  Index2 := TempIndexList[Index2];
  Result := MoreTag(Index1, Index2);
end;

{======================================================================================================================}
function TAbstractBase.MoreName(Index1, Index2: integer): boolean;
{======================================================================================================================}
begin
  Result :=
    AnsiUpperCase(GetName(Index1)) >
    AnsiUpperCase(GetName(Index2));
end;

{======================================================================================================================}
function TAbstractBase.MoreTag(Index1, Index2: integer): boolean;
{======================================================================================================================}
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

{======================================================================================================================}
procedure TAbstractBase.Sort();
{======================================================================================================================}
begin
  QuickSort(0, Count() - 1, Swap, MoreName);
end;

{======================================================================================================================}
procedure TAbstractBase.SortIndexes(IndexList: TIndexList; SortType: TSortType);
{======================================================================================================================}
begin
  IndexList.Init(Count());
  TempIndexList := IndexList;

  case SortType of
    stName: QuickSort(0, Count() - 1, SwapInd, MoreIndName);
    stTag:  QuickSort(0, Count() - 1, SwapInd, MoreIndTag);
  end;

  TempIndexList := nil;
end;

{======================================================================================================================}
procedure TAbstractBase.Swap(Index1, Index2: integer);
{======================================================================================================================}
var
  Temp: TVersioned;
begin
  if (Index1 < Low(FBase)) and (Index1 > High(FBase)) then
    raise ERangeError.Create(Format('TArrayBase.GetItem(): Index out of bounds (%d)', [Index1]));
  if (Index2 < Low(FBase)) and (Index2 > High(FBase)) then
    raise ERangeError.Create(Format('TArrayBase.GetItem(): Index out of bounds (%d)', [Index2]));

  Temp := FBase[Index1];
  FBase[Index1] := FBase[Index2];
  FBase[Index2] := Temp;
end;

{======================================================================================================================}
procedure TAbstractBase.SwapInd(Index1, Index2: integer);
{======================================================================================================================}
begin
  TempIndexList.Swap(Index1, Index2);
end;

{======================================================================================================================}
function TAbstractBase.TraceLast: integer;
{======================================================================================================================}
begin
  Result := Count - 1;
  while (Result > 0) and (MoreName(Result - 1, Result)) do
  begin
    Swap(Result, Result - 1);
    dec(Result);
  end;
end;

{ TFoodBase }

{======================================================================================================================}
function TFoodBase.Add(Food: TFoodItem): integer;
{======================================================================================================================}
begin
  Result := inherited Add(Food);
end;

{======================================================================================================================}
function TFoodBase.GetFood(Index: integer): TFoodItem;
{======================================================================================================================}
begin
  Result := TFoodItem(GetItem(Index));
end;

{======================================================================================================================}
function TFoodBase.GetName(Index: integer): string;
{======================================================================================================================}
begin
  Result := GetFood(Index).Name;
end;

{======================================================================================================================}
function TFoodBase.GetTag(Index: integer): integer;
{======================================================================================================================}
begin
  Result := GetFood(Index).Tag;
end;

{======================================================================================================================}
procedure TFoodBase.LoadFromFile_Old(const FileName: string);
{======================================================================================================================}
const
  FOODBASE_FORMAT = 'FBASEFMT';

  procedure Read(Food: TFoodItem; const S: string);
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
  Temp: TFoodItem;
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

    SetLength(FBase, s.Count);

    for i := 0 to s.Count - 1 do
    begin
      Temp := TFoodItem.Create();
      Read(Temp, S[i]);
      FBase[i] := Temp;
    end;
  finally
    s.Free;
  end;

  Sort();
end;

{======================================================================================================================}
procedure TFoodBase.LoadFromFile_XML(const FileName: string);
{======================================================================================================================}
const
  LOGGING = False;

  procedure CheckNode(Node: IXMLNODE; Index: integer);
  const
    A: array[1..6] of string = (
      FIELD_FOOD_NAME,
      FIELD_FOOD_PROTS,
      FIELD_FOOD_FATS,
      FIELD_FOOD_CARBS,
      FIELD_FOOD_VALUE,
      FIELD_FOOD_TABLE
    );
  var
    i: integer;
  begin
    for i := Low(A) to High(A) do
      if (not Node.HasAttribute(A[i])) then
        raise Exception.CreateFmt('Food node #%d has no %s attribute', [Index, A[i]]);
  end;

  function ReadNode(FoodNode: IXMLNODE): TFoodItem;
  begin
    Result := TFoodItem.Create();

    if (FoodNode.HasAttribute(FIELD_FOOD_ID)) then
      Result.ID := FoodNode.Attributes[FIELD_FOOD_ID]
    else
      Result.ID := CreateCompactGUID();

    if FoodNode.HasAttribute(FIELD_FOOD_TIMESTAMP) then
      Result.TimeStamp := StrToDateTime(FoodNode.Attributes[FIELD_FOOD_TIMESTAMP])  // #timefmt
    else
      Result.TimeStamp := GetTimeUTC();

    if (FoodNode.HasAttribute(FIELD_FOOD_HASH)) then
      Result.Hash := FoodNode.Attributes[FIELD_FOOD_HASH]
    else
      Result.Hash := CreateCompactGUID();

    if FoodNode.HasAttribute(FIELD_FOOD_VERSION) then
      Result.Version := FoodNode.Attributes[FIELD_FOOD_VERSION]
    else
      Result.Version := 1;

    if FoodNode.HasAttribute(FIELD_FOOD_DELETED) then
      Result.Deleted := FoodNode.Attributes[FIELD_FOOD_DELETED]
    else
      Result.Deleted := False;

    Result.Name      := FoodNode.Attributes[FIELD_FOOD_NAME];
    Result.RelProts  := StrToFloat(VarAsType(FoodNode.Attributes[FIELD_FOOD_PROTS], varOleStr));
    Result.RelFats   := StrToFloat(VarAsType(FoodNode.Attributes[FIELD_FOOD_FATS], varOleStr));
    Result.RelCarbs  := StrToFloat(VarAsType(FoodNode.Attributes[FIELD_FOOD_CARBS], varOleStr));
    Result.RelValue  := StrToFloat(VarAsType(FoodNode.Attributes[FIELD_FOOD_VALUE], varOleStr));

    // TODO: what if the field is missing?
    Result.FromTable := FoodNode.Attributes[FIELD_FOOD_TABLE];

    if (FoodNode.HasAttribute(FIELD_FOOD_TAG)) then
      Result.Tag := FoodNode.Attributes[FIELD_FOOD_TAG]
    else
      Result.Tag := 0;
  end;

var
  XML: IXMLDocument;
  Root: IXMLNODE;
  i: integer;
  DS: char;
begin
  (*
  { ���� ���� �� ����������, ���� ����� }
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

      Clear();
      SetLength(FBase, Root.ChildNodes.Count);

      Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', LOGGING);
      Log(DEBUG, 'TFoodBase.LoadFromFile_XML(): count = ' + IntToStr(Root.ChildNodes.Count), LOGGING);

      {=========================================================}
      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        //CheckNode(Root.ChildNodes[i], i);
        FBase[i] := ReadNode(Root.ChildNodes[i]);
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

{======================================================================================================================}
procedure TFoodBase.SaveToFile(const FileName: string);
{======================================================================================================================}
  (*try
    s := TStringList.Create;
    try
      { Time Stamp }
      FTimeStamp := GetTimeUTC();
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

    for i := 0 to High(FBase) do
    begin
      FoodNode := Root.AddChild('food');

      FoodNode.Attributes[FIELD_FOOD_ID]        := Items[i].ID;
      FoodNode.Attributes[FIELD_FOOD_TIMESTAMP] := Items[i].TimeStamp;
      FoodNode.Attributes[FIELD_FOOD_HASH]      := Items[i].Hash;
      FoodNode.Attributes[FIELD_FOOD_VERSION]   := Items[i].Version;
      FoodNode.Attributes[FIELD_FOOD_DELETED]   := Items[i].Deleted;

      FoodNode.Attributes[FIELD_FOOD_NAME]      := Items[i].Name;
      FoodNode.Attributes[FIELD_FOOD_PROTS]     := Items[i].RelProts;
      FoodNode.Attributes[FIELD_FOOD_FATS]      := Items[i].RelFats;
      FoodNode.Attributes[FIELD_FOOD_CARBS]     := Items[i].RelCarbs;
      FoodNode.Attributes[FIELD_FOOD_VALUE]     := Items[i].RelValue;
      FoodNode.Attributes[FIELD_FOOD_TABLE]     := Items[i].FromTable;
      FoodNode.Attributes[FIELD_FOOD_TAG]       := Items[i].Tag;
    end;

    XML.SaveToFile(FileName);
  finally
    SysUtils.DecimalSeparator := DS;
  end;
end;

{======================================================================================================================}
procedure TFoodBase.SetTag(Index, Value: integer);
{======================================================================================================================}
begin
  Items[Index].Tag := Value;
end;

{ TDishBase }

{======================================================================================================================}
function TDishBase.Add(Dish: TDishItem): integer;
{======================================================================================================================}
begin
  Result := inherited Add(Dish);
end;

{======================================================================================================================}
procedure TDishBase.AnalyzeUsing(Base: TAbstractBase);
{======================================================================================================================}
var
  i,j,n: integer;
begin
  for i := 0 to Base.Count - 1 do
    Base.SetTag(i, 0);

  for i := 0 to Count - 1 do
  for j := 0 to Items[i].Count - 1 do
  begin
    n := Base.FindByName(Items[i].Content[j].Name);
    if (n <> -1) then
      Base.SetTag(n, Base.GetTag(n) + 1);
  end;
end;

{======================================================================================================================}
function TDishBase.GetDish(Index: integer): TDishItem;
{======================================================================================================================}
begin
  if (Index < 0) or (Index > High(FBase)) then
    raise ERangeError.CreateFmt('TDishBase.GetDish(): index out of bounds (%d)', [Index]);

  Result := TDishItem(FBase[Index]);
end;

{======================================================================================================================}
function TDishBase.GetName(Index: integer): string;
{======================================================================================================================}
begin
  Result := GetDish(Index).Name;
end;

{======================================================================================================================}
function TDishBase.GetTag(Index: integer): integer;
{======================================================================================================================}
begin
  Result := GetDish(Index).Tag;
end;

{======================================================================================================================}
procedure TDishBase.LoadFromFile_Old(const FileName: string);
{======================================================================================================================}
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
    FTimeStamp := GetTimeUTC();
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

    for i := StartLine to s.Count - 1 do
    if s[i] <> '' then
    if s[i][1] = '#' then
    begin
      inc(n);
      SetLength(FBase, n + 1);
      FBase[n] := TDishItem.Create;
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
      Items[n].TimeStamp := StrToDateTime(buf);     // #timefmt
    end else
    if (s[i][1] <> '=') then
    begin
      { � ��� ��� ����� ���������� }
      { ������� ������������� ������:  }

      TempFood := TFoodMassed.Create();

      { ������ ������ - �� ������� �� ���� ���������}
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
            TempFood.Name := buf + ' (�� �������)';
        end else }
          TempFood.Name := buf + ' (�� �������)';
      end else

      { ����� ������ - ����������� }
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

{======================================================================================================================}
procedure TDishBase.LoadFromFile_XML(const FileName: string);
{======================================================================================================================}

  function ReadNode(DishNode: IXMLNODE): TDishItem;
  var
    ItemNode: IXMLNODE;
    Food: TFoodMassed;
    i: integer;
  begin
    Result := TDishItem.Create();
    if DishNode.HasAttribute(FIELD_DISH_ID) then
      Result.ID := DishNode.Attributes[FIELD_DISH_ID]
    else
      Result.ID := CreateCompactGUID();

    if DishNode.HasAttribute(FIELD_DISH_HASH) then
      Result.Hash := DishNode.Attributes[FIELD_DISH_HASH]
    else
      Result.Hash := CreateCompactGUID();

    if DishNode.HasAttribute(FIELD_DISH_TIMESTAMP) then
      Result.TimeStamp := StrToDateTime(DishNode.Attributes[FIELD_DISH_TIMESTAMP])  // #timefmt
    else
      Result.TimeStamp := GetTimeUTC();

    if DishNode.HasAttribute(FIELD_DISH_VERSION) then
      Result.Version := DishNode.Attributes[FIELD_DISH_VERSION]
    else
      Result.Version := 1;
    if DishNode.HasAttribute(FIELD_DISH_DELETED) then
      Result.Deleted := DishNode.Attributes[FIELD_DISH_DELETED]
    else
      Result.Deleted := False;
    Result.Name := DishNode.Attributes[FIELD_DISH_NAME];
    if DishNode.HasAttribute(FIELD_DISH_MASS) then
      Result.SetResultMass(DishNode.Attributes[FIELD_DISH_MASS]);
    if DishNode.HasAttribute(FIELD_DISH_TAG) then
      Result.Tag := DishNode.Attributes[FIELD_DISH_TAG]
    else
      Result.Tag := 0;

    //SetLength(Result.FContent, DishNode.ChildNodes.Count);

    for i := 0 to DishNode.ChildNodes.Count - 1 do
    begin
      ItemNode := DishNode.ChildNodes[i];

      Food := TFoodMassed.Create();
      Food.Name     := ItemNode.Attributes[FIELD_DISH_FOOD_NAME];

      {Food.RelProts := ItemNode.Attributes['prots'];
      Food.RelFats  := ItemNode.Attributes['fats'];
      Food.RelCarbs := ItemNode.Attributes['carbs'];
      Food.RelValue := ItemNode.Attributes['val'];
      Food.Mass     := ItemNode.Attributes['mass'];}
      Food.RelProts := StrToFloat(VarAsType(ItemNode.Attributes[FIELD_DISH_FOOD_PROTS], varOleStr));
      Food.RelFats  := StrToFloat(VarAsType(ItemNode.Attributes[FIELD_DISH_FOOD_FATS], varOleStr));
      Food.RelCarbs := StrToFloat(VarAsType(ItemNode.Attributes[FIELD_DISH_FOOD_CARBS], varOleStr));
      Food.RelValue := StrToFloat(VarAsType(ItemNode.Attributes[FIELD_DISH_FOOD_VALUE], varOleStr));
      Food.Mass     := StrToFloat(VarAsType(ItemNode.Attributes[FIELD_DISH_FOOD_MASS], varOleStr));

      Result.Add(Food);
    end;
  end;

const
  LOGGING = False;

var
  XML: IXMLDocument;
  Root, DishNode: IXMLNODE;
  i: integer;
  DS: char;
begin
  (*
  { ���� ���� �� ����������, ���� ����� }
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

      Clear();
      SetLength(FBase, Root.ChildNodes.Count);

      Log(DEBUG, 'TDishBase.LoadFromFile_XML(): DecimalSeparator is "' + SysUtils.DecimalSeparator + '"', LOGGING);
      Log(DEBUG, 'TDishBase.LoadFromFile_XML(): count = ' + IntToStr(Root.ChildNodes.Count), LOGGING);

      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        DishNode := Root.ChildNodes[i];
        FBase[i] := ReadNode(DishNode);
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

{======================================================================================================================}
function TDishBase.RenameFood(const OldName, NewName: string): boolean;
{======================================================================================================================}
var
  i, j: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  for j := 0 to Items[i].Count - 1 do
  if (Items[i].Content[j].Name = OldName) then
  begin
    Items[i].Content[j].Name := NewName;
    Items[i].Modified();
    Result := True;
  end;
end;

{======================================================================================================================}
procedure TDishBase.SaveToFile(const FileName: string);
{======================================================================================================================}
 (*try
    s := TStringList.Create;
    try
      { Time Stamp }
      FTimeStamp := GetTimeUTC();
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

    for i := 0 to High(FBase) do
    begin
      DishNode := Root.AddChild('dish');
      DishNode.Attributes[FIELD_DISH_ID]        := Items[i].ID;
      DishNode.Attributes[FIELD_DISH_HASH]      := Items[i].Hash;
      DishNode.Attributes[FIELD_DISH_TIMESTAMP] := Items[i].TimeStamp;
      DishNode.Attributes[FIELD_DISH_VERSION]   := Items[i].Version;
      DishNode.Attributes[FIELD_DISH_DELETED]   := Items[i].Deleted;

      DishNode.Attributes[FIELD_DISH_NAME] := Items[i].Name;
      DishNode.Attributes[FIELD_DISH_TAG] := Items[i].Tag;
      if (Items[i].FixedMass) then
        DishNode.Attributes[FIELD_DISH_MASS] := Items[i].ResultMass;

      for j := 0 to Items[i].Count - 1 do
      begin
        ItemNode := DishNode.AddChild('item');

        ItemNode.Attributes[FIELD_DISH_FOOD_NAME]  := Items[i].Content[j].Name;
        ItemNode.Attributes[FIELD_DISH_FOOD_PROTS] := Items[i].Content[j].RelProts;
        ItemNode.Attributes[FIELD_DISH_FOOD_FATS]  := Items[i].Content[j].RelFats;
        ItemNode.Attributes[FIELD_DISH_FOOD_CARBS] := Items[i].Content[j].RelCarbs;
        ItemNode.Attributes[FIELD_DISH_FOOD_VALUE] := Items[i].Content[j].RelValue;
        ItemNode.Attributes[FIELD_DISH_FOOD_MASS]  := Items[i].Content[j].Mass;
      end;
    end;

    XML.SaveToFile(FileName);
  finally
    SysUtils.DecimalSeparator := DS;
  end;
end;

{======================================================================================================================}
procedure TDishBase.SetTag(Index, Value: integer);
{======================================================================================================================}
begin
  Items[Index].Tag := Value;
end;

{======================================================================================================================}
function TDishBase.UsedFood(const FoodName: string): integer;
{======================================================================================================================}
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


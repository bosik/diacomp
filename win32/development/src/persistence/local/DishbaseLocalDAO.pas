unit DishbaseLocalDAO;

interface

uses
  SysUtils,
  Windows,
  BusinessObjects,
  DishbaseDAO,
  Bases,
  ObjectService,
  ExtCtrls,
  HashService,
  DiaryRoutines,
  MerkleTree;

type
  TDishbaseLocalDAO = class (TDishbaseDAO)
  private
    FBaseFileName: string;
    FBase: TDishBase;

    Timer: TTimer;
    FModified: boolean;
    FFirstMod: cardinal;
    FLastMod: cardinal;
    function Add(Dish: TDishItem): TCompactGUID;
    function GetIndex(Dish: TDishItem): integer; overload;
    function GetIndex(ID: TCompactGUID): integer; overload;
    procedure Modified();
    procedure OnTimer(Sender: TObject);
  public
    function Count(Prefix: TCompactGUID): integer; override;
    constructor Create(const BaseFileName: string);
    procedure Delete(ID: TCompactGUID); override;
    destructor Destroy; override;
    function FindAll(ShowRemoved: boolean): TDishItemList; override;
    function FindAny(const Filter: string): TDishItemList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    function FindByIdPrefix(Prefix: TCompactGUID): TVersionedList; override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindOne(const Name: string): TDishItem; override;
    function GetHashTree(): THashTree; override;
    procedure Save(Item: TVersioned); override;
    procedure Save(const Items: TVersionedList); override;
  end;

implementation

{ TDishbaseLocalDAO }

{======================================================================================================================}
function TDishbaseLocalDAO.Add(Dish: TDishItem): TCompactGUID;
{======================================================================================================================}
var
  Index: integer;
  Temp: TDishItem;
begin
  Index := GetIndex(Dish);
  if (Index = -1) then
  begin
    Temp := TDishItem.Create;
    Temp.CopyFrom(Dish);
    FBase.Add(Temp);
    Result := Dish.ID;
    Modified();
  end else
    raise EDuplicateException.Create(Dish);
end;

{======================================================================================================================}
function TDishbaseLocalDAO.Count(Prefix: TCompactGUID): integer;
{======================================================================================================================}
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FBase.Count - 1 do
  if (StartsWith(FBase[i].ID, Prefix)) then
    inc(Result);
end;

{======================================================================================================================}
constructor TDishbaseLocalDAO.Create(const BaseFileName: string);
{======================================================================================================================}
begin
  FBase := TDishBase.Create;
  if FileExists(BaseFileName) then
    FBase.LoadFromFile_XML(BaseFileName);
  FBaseFileName := BaseFileName;

  FModified := False;
  FFirstMod := 0;
  FLastMod := 0;

  Timer := TTimer.Create(nil);
  Timer.Interval := 1000;
  Timer.OnTimer := OnTimer;
  Timer.Enabled := True;
end;

{======================================================================================================================}
procedure TDishbaseLocalDAO.Delete(ID: TCompactGUID);
{======================================================================================================================}
var
  Index: integer;
begin
  Index := GetIndex(ID);
  if (Index > -1) then
  begin
    FBase[Index].Deleted := True;
    FBase[Index].Modified();
    Modified();
  end else
    raise EItemNotFoundException.Create(ID);
end;

{======================================================================================================================}
destructor TDishbaseLocalDAO.Destroy;
{======================================================================================================================}
begin
  if (FModified) then
  begin
    FBase.SaveToFile(FBaseFileName);
    FModified := False;
  end;

  FBase.Free;
  Timer.Free;
  inherited;
end;

{======================================================================================================================}
function TDishbaseLocalDAO.FindAll(ShowRemoved: boolean): TDishItemList;
{======================================================================================================================}
var
  i, k: integer;
begin
  SetLength(Result, FBase.Count);
  k := 0;
  for i := 0 to FBase.Count - 1 do
  if (ShowRemoved or not FBase[i].Deleted) then
  begin
    Result[k] := TDishItem.Create;
    Result[k].CopyFrom(FBase[i]);
    inc(k);
  end;
  SetLength(Result, k);
end;

{======================================================================================================================}
function TDishbaseLocalDAO.FindAny(const Filter: string): TDishItemList;
{======================================================================================================================}
var
  i, k: integer;
begin
  SetLength(Result, FBase.Count);
  k := 0;
  for i := 0 to FBase.Count - 1 do
  // TODO: optimize
  if (not FBase[i].Deleted) and
     (pos(AnsiUpperCase(Filter), AnsiUpperCase(FBase[i].Name)) > 0) then
  begin
    inc(k);
    SetLength(Result, k);
    Result[k - 1] := TDishItem.Create;
    Result[k - 1].CopyFrom(FBase[i]);
  end;
  SetLength(Result, k);
end;

{======================================================================================================================}
function TDishbaseLocalDAO.FindById(ID: TCompactGUID): TVersioned;
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to FBase.Count - 1 do
  if (FBase[i].ID = ID) then
  begin
    Result := TDishItem.Create;
    Result.CopyFrom(FBase[i]);
    Exit;
  end;

  Result := nil;
end;

{======================================================================================================================}
function TDishbaseLocalDAO.FindByIdPrefix(Prefix: TCompactGUID): TVersionedList;
{======================================================================================================================}
var
  i: integer;
begin
  SetLength(Result, 0);

  for i := 0 to FBase.Count - 1 do
  if (StartsWith(FBase[i].ID, Prefix)) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := TDishItem.Create;
    Result[High(Result)].CopyFrom(FBase[i]);
  end;
end;

{======================================================================================================================}
function TDishbaseLocalDAO.FindChanged(Since: TDateTime): TVersionedList;
{======================================================================================================================}
var
  i, k: integer;
begin
  SetLength(Result, FBase.Count);
  k := 0;
  for i := 0 to FBase.Count - 1 do
  // TODO: optimize
  if (FBase[i].TimeStamp > Since) then
  begin
    inc(k);
    SetLength(Result, k);
    Result[k - 1] := TDishItem.Create;
    Result[k - 1].CopyFrom(FBase[i]);
  end;
  SetLength(Result, k);
end;

{======================================================================================================================}
function TDishbaseLocalDAO.FindOne(const Name: string): TDishItem;
{======================================================================================================================}
var
  Index: integer;
begin
  Index := FBase.FindByName(Name);
  if (Index <> -1) then
  begin
    Result := TDishItem.Create;
    Result.CopyFrom(FBase[Index]);
  end else
    Result := nil;
end;

{======================================================================================================================}
function TDishbaseLocalDAO.GetHashTree(): THashTree;
{======================================================================================================================}
var
  Tree: TMerkleTree;
  i: integer;
begin
  Tree := TMerkleTree.Create();

  for i := 0 to FBase.Count - 1 do
  begin
    Tree.Put(FBase[i].ID, FBase[i].Hash, MAX_PREFIX_SIZE, False);
  end;

  Tree.UpdateHash();
  Result := Tree;
end;

{======================================================================================================================}
function TDishbaseLocalDAO.GetIndex(Dish: TDishItem): integer;
{======================================================================================================================}
begin
  Result := GetIndex(Dish.ID);
end;

{======================================================================================================================}
function TDishbaseLocalDAO.GetIndex(ID: TCompactGUID): integer;
{======================================================================================================================}
begin
  Result := FBase.FindById(ID);
end;

{======================================================================================================================}
procedure TDishbaseLocalDAO.Modified();
{======================================================================================================================}
begin
  FModified := True;
  if (FFirstMod = 0) then
    FFirstMod := GetTickCount;
  FLastMod := GetTickCount();
end;

{======================================================================================================================}
procedure TDishbaseLocalDAO.OnTimer(Sender: TObject);
{======================================================================================================================}
const
  MAX_UNSAVED = 60000;
  MAX_IDLE    = 20000;
begin
  if (FModified) then
  begin
    if (GetTickCount() - FFirstMod > MAX_UNSAVED) or
       (GetTickCount() - FLastMod > MAX_IDLE) then
    begin
      Timer.Enabled := False;
      try
        FBase.SaveToFile(FBaseFileName);
        FModified := False;
        FFirstMod := GetTickCount();
      finally
        Timer.Enabled := True;
      end;
    end;
  end;
end;

{======================================================================================================================}
procedure TDishbaseLocalDAO.Save(Item: TVersioned);
{======================================================================================================================}
var
  Dish: TDishItem;
  Index: integer;
  NameChanged: boolean;
begin
  Dish := Item as TDishItem;

  Index := GetIndex(Dish.ID);
  if (Index <> -1) then
  begin
    NameChanged := (Dish.Name <> FBase[Index].Name);
    FBase[Index].CopyFrom(Dish);
    if (NameChanged) then
    begin
      FBase.Sort;
    end;
    Modified();
  end else
    Add(Dish);
end;

{======================================================================================================================}
procedure TDishbaseLocalDAO.Save(const Items: TVersionedList);
{======================================================================================================================}
var
  i: integer;
begin
  // TODO: optimize, sort once
  for i := Low(Items) to High(Items) do
    Save(Items[i]);
end;

end.

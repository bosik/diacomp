unit FoodbaseLocalDAO;

interface

uses
  SysUtils,
  Windows,
  BusinessObjects,
  FoodbaseDAO,
  Bases,
  ObjectService,
  ExtCtrls,
  HashService,
  DiaryRoutines,
  MerkleTree;

type
  TFoodbaseLocalDAO = class (TFoodbaseDAO)
  private
    FBase: TFoodBase;
    FBaseFileName: string;

    Timer: TTimer;
    FModified: boolean;
    FFirstMod: cardinal;
    FLastMod: cardinal;
    function Add(Food: TFoodItem): TCompactGUID;
    function GetIndex(Food: TFoodItem): integer; overload;
    function GetIndex(ID: TCompactGUID): integer; overload;
    procedure OnTimer(Sender: TObject);
    procedure Modified();
  public
    constructor Create(const BaseFileName: string);
    destructor Destroy; override;
    procedure Delete(ID: TCompactGUID); override;
    function FindAll(ShowRemoved: boolean): TFoodItemList; override;
    function FindAny(const Filter: string): TFoodItemList; override;
    function FindOne(const Name: string): TFoodItem; override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    function FindByIdPrefix(Prefix: TCompactGUID): TVersionedList; override;
    function GetHashTree(): THashTree; override;
    procedure Save(Item: TVersioned); override;
    procedure Save(const Items: TVersionedList); override;
  end;

implementation

{ TFoodbaseLocalDAO }

{======================================================================================================================}
function TFoodbaseLocalDAO.Add(Food: TFoodItem): TCompactGUID;
{======================================================================================================================}
var
  Index: integer;
  Temp: TFoodItem;
begin
  Index := GetIndex(Food);
  if (Index = -1) then
  begin
    Temp := TFoodItem.Create;
    Temp.CopyFrom(Food);
    FBase.Add(Temp);
    Result := Food.ID;
    Modified();
  end else
    raise EDuplicateException.Create(Food);
end;

{======================================================================================================================}
constructor TFoodbaseLocalDAO.Create(const BaseFileName: string);
{======================================================================================================================}
begin
  FBase := TFoodBase.Create;
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
procedure TFoodbaseLocalDAO.Delete(ID: TCompactGUID);
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
destructor TFoodbaseLocalDAO.Destroy;
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
function TFoodbaseLocalDAO.FindAll(ShowRemoved: boolean): TFoodItemList;
{======================================================================================================================}
var
  i, k: integer;
begin
  SetLength(Result, FBase.Count);
  k := 0;
  for i := 0 to FBase.Count - 1 do
  if (ShowRemoved or not FBase[i].Deleted) then
  begin
    Result[k] := TFoodItem.Create;
    Result[k].CopyFrom(FBase[i]);
    inc(k);
  end;
  SetLength(Result, k);
end;

{======================================================================================================================}
function TFoodbaseLocalDAO.FindAny(const Filter: string): TFoodItemList;
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
    Result[k - 1] := TFoodItem.Create;
    Result[k - 1].CopyFrom(FBase[i]);
  end;
  SetLength(Result, k);
end;

{======================================================================================================================}
function TFoodbaseLocalDAO.FindById(ID: TCompactGUID): TVersioned;
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to FBase.Count - 1 do
  if (FBase[i].ID = ID) then
  begin
    Result := TFoodItem.Create;
    Result.CopyFrom(FBase[i]);
    Exit;
  end;

  Result := nil;
end;

{======================================================================================================================}
function TFoodbaseLocalDAO.FindByIdPrefix(Prefix: TCompactGUID): TVersionedList;
{======================================================================================================================}
var
  i: integer;
begin
  SetLength(Result, 0);

  for i := 0 to FBase.Count - 1 do
  if (StartsWith(FBase[i].ID, Prefix)) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := TFoodItem.Create;
    Result[High(Result)].CopyFrom(FBase[i]);
  end;
end;

{======================================================================================================================}
function TFoodbaseLocalDAO.FindChanged(Since: TDateTime): TVersionedList;
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
    Result[k - 1] := TFoodItem.Create;
    Result[k - 1].CopyFrom(FBase[i]);
  end;
  SetLength(Result, k);
end;

{======================================================================================================================}
function TFoodbaseLocalDAO.FindOne(const Name: string): TFoodItem;
{======================================================================================================================}
var
  Index: integer;
begin
  Index := FBase.FindByName(Name);
  if (Index <> -1) then
  begin
    Result := TFoodItem.Create;
    Result.CopyFrom(FBase[Index]);
  end else
    Result := nil;
end;

{======================================================================================================================}
function TFoodbaseLocalDAO.GetIndex(Food: TFoodItem): integer;
{======================================================================================================================}
begin
  Result := GetIndex(Food.ID);
end;

{======================================================================================================================}
function TFoodbaseLocalDAO.GetHashTree(): THashTree;
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
function TFoodbaseLocalDAO.GetIndex(ID: TCompactGUID): integer;
{======================================================================================================================}
begin
  Result := FBase.FindById(ID);
end;

{======================================================================================================================}
procedure TFoodbaseLocalDAO.Modified;
{======================================================================================================================}
begin
  FModified := True;
  if (FFirstMod = 0) then
    FFirstMod := GetTickCount;
  FLastMod := GetTickCount();

  {* tree outdated *}
end;

{======================================================================================================================}
procedure TFoodbaseLocalDAO.OnTimer(Sender: TObject);
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
procedure TFoodbaseLocalDAO.Save(Item: TVersioned);
{======================================================================================================================}
var
  Food: TFoodItem;
  Index: integer;
  NameChanged: boolean;
begin
  Food := Item as TFoodItem;

  Index := GetIndex(Food.ID);
  if (Index <> -1) then
  begin
    NameChanged := (Food.Name <> FBase[Index].Name);
    FBase[Index].CopyFrom(Food);
    if (NameChanged) then
    begin
      FBase.Sort;
    end;
    Modified();
  end else
    Add(Food);
end;

{======================================================================================================================}
procedure TFoodbaseLocalDAO.Save(const Items: TVersionedList);
{======================================================================================================================}
var
  i: integer;
begin
  // TODO: optimize, sort once
  for i := Low(Items) to High(Items) do
    Save(Items[i]);
end;

end.

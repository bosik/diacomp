unit DishbaseLocalDAO;

interface

uses
  SysUtils,
  Windows,
  BusinessObjects,
  DishbaseDAO,
  DiaryRoutines,
  Bases,
  DAO,
  ExtCtrls;

type
  TDishbaseLocalDAO = class (TDishbaseDAO)
  private
    FFileName: string;
    FBase: TDishBase;

    Timer: TTimer;
    FModified: boolean;
    FFirstMod: cardinal;
    FLastMod: cardinal;
    function Add(Dish: TDish): TCompactGUID;
    function GetIndex(Dish: TDish): integer; overload;
    function GetIndex(ID: TCompactGUID): integer; overload;
    procedure OnTimer(Sender: TObject);
    procedure Modified();
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure Delete(ID: TCompactGUID); override;
    function FindAll(ShowRemoved: boolean): TDishItemList; override;
    function FindAny(const Filter: string): TDishItemList; override;
    function FindOne(const Name: string): TDish; override;
    function FindChanged(Since: TDateTime): TDishItemList; override;
    function FindById(ID: TCompactGUID): TDish; override;
    procedure Save(Item: TDish); override;
    procedure Save(const Items: TDishItemList); override;
  end;

implementation

{ TDishbaseLocalDAO }

{==============================================================================}
function TDishbaseLocalDAO.Add(Dish: TDish): TCompactGUID;
{==============================================================================}
var
  Index: integer;
  Temp: TDish;
begin
  Index := GetIndex(Dish);
  if (Index = -1) then
  begin
    Temp := TDish.Create;
    Temp.CopyFrom(Dish);
    FBase.Add(Temp);
    Result := Dish.ID;
    Modified();
  end else
    raise EDuplicateException.Create(Dish);
end;

{==============================================================================}
constructor TDishbaseLocalDAO.Create(const FileName: string);
{==============================================================================}
begin
  FBase := TDishBase.Create;
  if FileExists(FileName) then
    FBase.LoadFromFile_XML(FileName);
  FFileName := FileName;

  FModified := False;
  FFirstMod := 0;
  FLastMod := 0;

  Timer := TTimer.Create(nil);
  Timer.Interval := 1000;
  Timer.OnTimer := OnTimer;
  Timer.Enabled := True;
end;

{==============================================================================}
procedure TDishbaseLocalDAO.Delete(ID: TCompactGUID);
{==============================================================================}
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

{==============================================================================}
destructor TDishbaseLocalDAO.Destroy;
{==============================================================================}
begin
  if (FModified) then
  begin
    FBase.SaveToFile(FFileName);
    FModified := False;
  end;

  FBase.Free;
  Timer.Free;
  inherited;
end;

{==============================================================================}
function TDishbaseLocalDAO.FindAll(ShowRemoved: boolean): TDishItemList;
{==============================================================================}
var
  i, k: integer;
begin
  SetLength(Result, FBase.Count);
  k := 0;
  for i := 0 to FBase.Count - 1 do
  if (ShowRemoved or not FBase[i].Deleted) then
  begin
    Result[k] := TDish.Create;
    Result[k].CopyFrom(FBase[i]);
    inc(k);
  end;
  SetLength(Result, k);
end;

{==============================================================================}
function TDishbaseLocalDAO.FindAny(const Filter: string): TDishItemList;
{==============================================================================}
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
    Result[k - 1] := TDish.Create;
    Result[k - 1].CopyFrom(FBase[i]);
  end;
  SetLength(Result, k);
end;

{==============================================================================}
function TDishbaseLocalDAO.FindById(ID: TCompactGUID): TDish;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to FBase.Count - 1 do
  if (FBase[i].ID = ID) then
  begin
    Result := TDish.Create;
    Result.CopyFrom(FBase[i]);
    Exit;
  end;

  Result := nil;
end;

{==============================================================================}
function TDishbaseLocalDAO.FindChanged(Since: TDateTime): TDishItemList;
{==============================================================================}
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
    Result[k - 1] := TDish.Create;
    Result[k - 1].CopyFrom(FBase[i]);
  end;
  SetLength(Result, k);
end;

{==============================================================================}
function TDishbaseLocalDAO.FindOne(const Name: string): TDish;
{==============================================================================}
var
  Index: integer;
begin
  Index := FBase.Find(Name);
  if (Index <> -1) then
  begin
    Result := TDish.Create;
    Result.CopyFrom(FBase[Index]);
  end else
    Result := nil;
end;

{==============================================================================}
function TDishbaseLocalDAO.GetIndex(Dish: TDish): integer;
{==============================================================================}
begin
  Result := FBase.GetIndex(Dish.ID);
end;

{==============================================================================}
function TDishbaseLocalDAO.GetIndex(ID: TCompactGUID): integer;
{==============================================================================}
begin
  Result := FBase.GetIndex(ID);
end;

{==============================================================================}
procedure TDishbaseLocalDAO.Modified;
{==============================================================================}
begin
  FModified := True;
  if (FFirstMod = 0) then
    FFirstMod := GetTickCount;
  FLastMod := GetTickCount();
end;

{==============================================================================}
procedure TDishbaseLocalDAO.OnTimer(Sender: TObject);
{==============================================================================}
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
        FBase.SaveToFile(FFileName);
        FModified := False;
        FFirstMod := 0; // TODO: or GetTickCount() ?
      finally
        Timer.Enabled := True;
      end;
    end;
  end;
end;

{==============================================================================}
procedure TDishbaseLocalDAO.Save(Item: TDish);
{==============================================================================}
var
  Index: integer;
  NameChanged: boolean;
begin
  Index := GetIndex(Item.ID);
  if (Index <> -1) then
  begin
    NameChanged := (Item.Name <> FBase[Index].Name);
    FBase[Index].CopyFrom(Item);
    if (NameChanged) then
    begin
      FBase.Sort;
    end;
    Modified();
  end else
    Add(Item);
end;

{==============================================================================}
procedure TDishbaseLocalDAO.Save(const Items: TDishItemList);
{==============================================================================}
var
  i: integer;
begin
  for i := Low(Items) to High(Items) do
    Save(Items[i]);
end;

end.

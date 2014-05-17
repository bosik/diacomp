unit FoodbaseLocalDAO;

interface

uses
  SysUtils,
  Windows,
  BusinessObjects,
  FoodbaseDAO,
  DiaryRoutines,
  Bases,
  DAO,
  ExtCtrls;

type
  TFoodbaseLocalDAO = class (TFoodbaseDAO)
  private
    FFileName: string;
    FBase: TFoodBase;

    Timer: TTimer;
    FModified: boolean;
    FFirstMod: cardinal;
    FLastMod: cardinal;
    function Add(Food: TFood): TCompactGUID;
    procedure Update(Food: TFood);
    function GetIndex(Food: TFood): integer; overload;
    function GetIndex(ID: TCompactGUID): integer; overload;
    procedure OnTimer(Sender: TObject);
    procedure Modified();
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure Delete(ID: TCompactGUID); override;
    function FindAll(ShowRemoved: boolean): TFoodItemList; override;
    function FindAny(const Filter: string): TFoodItemList; override;
    function FindOne(const Name: string): TFood; override;
    function FindChanged(Since: TDateTime): TFoodItemList; override;
    function FindById(ID: TCompactGUID): TFood; override;
    procedure Save(const Items: TFoodItemList); override;
    procedure Save(const Item: TFood); override;
  end;

implementation

{ TFoodbaseLocalDAO }

{==============================================================================}
function TFoodbaseLocalDAO.Add(Food: TFood): TCompactGUID;
{==============================================================================}
var
  Index: integer;
  Temp: TFood;
begin
  Index := GetIndex(Food);
  if (Index = -1) then
  begin
    Temp := TFood.Create;
    Temp.CopyFrom(Food);
    FBase.Add(Temp);
    Result := Food.ID;
    Modified();
  end else
    raise EDuplicateException.Create(Food);
end;

{==============================================================================}
constructor TFoodbaseLocalDAO.Create(const FileName: string);
{==============================================================================}
begin
  FBase := TFoodBase.Create;
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
procedure TFoodbaseLocalDAO.Delete(ID: TCompactGUID);
{==============================================================================}
var
  Index: integer;
begin
  Index := GetIndex(ID);
  if (Index > -1) then
  begin
    FBase[Index].Deleted := True;
    Modified();
  end else
    raise EItemNotFoundException.Create(ID);
end;

{==============================================================================}
destructor TFoodbaseLocalDAO.Destroy;
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
function TFoodbaseLocalDAO.FindAll(ShowRemoved: boolean): TFoodItemList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, FBase.Count);
  for i := 0 to FBase.Count - 1 do
  if (ShowRemoved or not FBase[i].Deleted) then
  begin
    Result[i] := TFood.Create;
    Result[i].CopyFrom(FBase[i]);
  end;
end;

{==============================================================================}
function TFoodbaseLocalDAO.FindAny(const Filter: string): TFoodItemList;
{==============================================================================}
var
  i, k: integer;
begin
  SetLength(Result, FBase.Count);
  k := 0;
  for i := 0 to FBase.Count - 1 do
  // TODO: optimize
  if (pos(AnsiUpperCase(Filter), AnsiUpperCase(FBase[i].Name)) > 0) then
  begin
    inc(k);
    SetLength(Result, k);
    Result[k - 1] := TFood.Create;
    Result[k - 1].CopyFrom(FBase[i]);
  end;
  SetLength(Result, k);
end;

{==============================================================================}
function TFoodbaseLocalDAO.FindById(ID: TCompactGUID): TFood;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to FBase.Count - 1 do
  if (FBase[i].ID = ID) then
  begin
    Result := TFood.Create;
    Result.CopyFrom(FBase[i]);
    Exit;
  end;

  Result := nil;
end;

{==============================================================================}
function TFoodbaseLocalDAO.FindChanged(Since: TDateTime): TFoodItemList;
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
    Result[k - 1] := TFood.Create;
    Result[k - 1].CopyFrom(FBase[i]);
  end;
  SetLength(Result, k);
end;

{==============================================================================}
function TFoodbaseLocalDAO.FindOne(const Name: string): TFood;
{==============================================================================}
var
  Index: integer;
begin
  Index := FBase.Find(Name);
  if (Index <> -1) then
  begin
    Result := TFood.Create;
    Result.CopyFrom(FBase[Index]);
  end else
    Result := nil;
end;

{==============================================================================}
function TFoodbaseLocalDAO.GetIndex(Food: TFood): integer;
{==============================================================================}
begin
  Result := FBase.GetIndex(Food.ID);
end;

{==============================================================================}
function TFoodbaseLocalDAO.GetIndex(ID: TCompactGUID): integer;
{==============================================================================}
begin
  Result := FBase.GetIndex(ID);
end;

{==============================================================================}
procedure TFoodbaseLocalDAO.Modified;
{==============================================================================}
begin
  FModified := True;
  if (FFirstMod = 0) then
    FFirstMod := GetTickCount;
  FLastMod := GetTickCount();
end;

{==============================================================================}
procedure TFoodbaseLocalDAO.OnTimer(Sender: TObject);
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
procedure TFoodbaseLocalDAO.Save(const Items: TFoodItemList);
{==============================================================================}
var
  i: integer;
begin
  for i := Low(Items) to High(Items) do
  try
    Update(Items[i]);
  except
    on e: EItemNotFoundException do
      Add(Items[i]);
  end;
end;

{==============================================================================}
procedure TFoodbaseLocalDAO.Save(const Item: TFood);
{==============================================================================}
var
  i: integer;
begin
  try
    Update(Item);
  except
    on e: EItemNotFoundException do
      Add(Item);
  end;
end;

{==============================================================================}
procedure TFoodbaseLocalDAO.Update(Food: TFood);
{==============================================================================}
var
  Index: integer;
  NameChanged: boolean;
begin
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
  begin
    raise EItemNotFoundException.Create(Food.ID);
  end;
end;

end.

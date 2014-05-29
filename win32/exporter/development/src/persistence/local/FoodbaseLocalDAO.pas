unit FoodbaseLocalDAO;

interface

uses
  SysUtils,
  Windows,
  BusinessObjects,
  FoodbaseDAO,
  DiaryRoutines,
  Bases,

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
  private
    function GetIndex(Food: TFood): integer; overload;
    function GetIndex(ID: TCompactGUID): integer; overload;
    procedure OnTimer(Sender: TObject);
    procedure Modified();
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    function Add(Food: TFood): TCompactGUID; override;
    procedure Delete(ID: TCompactGUID); override;
    function FindAll(): TFoodList; override;
    function FindAny(const Filter: string): TFoodList; override;
    function FindOne(const Name: string): TFood; override;
    procedure ReplaceAll(const NewList: TFoodList; NewVersion: integer); override;
    procedure Update(Food: TFood); override;
    function Version(): integer; override;
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
    Temp.CopyFrom(Food, True);
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
    FBase.Delete(Index);
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
function TFoodbaseLocalDAO.FindAll(): TFoodList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, FBase.Count);
  for i := 0 to FBase.Count - 1 do
  begin
    Result[i] := TFood.Create;
    Result[i].CopyFrom(FBase[i], True);
  end;
end;

{==============================================================================}
function TFoodbaseLocalDAO.FindAny(const Filter: string): TFoodList;
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
    Result[k - 1].CopyFrom(FBase[i], True);
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
    Result.CopyFrom(FBase[Index], True);
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
        FFirstMod := 0;
      finally
        Timer.Enabled := True;
      end;
    end;
  end;
end;

{==============================================================================}
procedure TFoodbaseLocalDAO.ReplaceAll(const NewList: TFoodList;
  NewVersion: integer);
{==============================================================================}
var
  i: integer;
  Food: TFood;
begin
  FBase.Clear;
  for i := 0 to High(NewList) do
  begin
    Food := TFood.Create;
    Food.CopyFrom(NewList[i], True);
    FBase.Add(Food);
  end;
  FBase.Version := NewVersion;
  Modified();
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
    FBase[Index].CopyFrom(Food, True);
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

{==============================================================================}
function TFoodbaseLocalDAO.Version: integer;
{==============================================================================}
begin
  Result := FBase.Version;
end;

end.

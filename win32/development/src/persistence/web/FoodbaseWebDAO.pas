unit FoodbaseWebDAO;

interface

uses
  SysUtils,
  BusinessObjects,
  FoodbaseDAO,
  DiaryWeb,
  Bases,
  DiaryRoutines;

type
  TFoodbaseWebDAO = class (TFoodbaseDAO)
  private
    FBase: TFoodBase;
    FClient: TDiacompClient;
  private
    procedure Download;
    function GetIndex(Food: TFood): integer; overload;
    function GetIndex(ID: TCompactGUID): integer; overload;
    procedure Upload;
  public
    constructor Create(Client: TDiacompClient);
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

{ TFoodbaseWebDAO }

const
  FILENAME_TEMP = 'TFoodbaseWebDAO_temp.txt';

{==============================================================================}
function TFoodbaseWebDAO.Add(Food: TFood): TCompactGUID;
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
    Upload();
    Result := Food.ID;
  end else
    raise EDuplicateException.Create(Food);
end;

{==============================================================================}
constructor TFoodbaseWebDAO.Create(Client: TDiacompClient);
{==============================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');
    
  FClient := Client;
  FBase := TFoodBase.Create;
end;

{==============================================================================}
procedure TFoodbaseWebDAO.Delete(ID: TCompactGUID);
{==============================================================================}
var
  Index: integer;
begin
  Index := GetIndex(ID);
  if (Index > -1) then
  begin
    FBase.Delete(Index);
    Upload();
  end else
    raise EItemNotFoundException.Create(ID);
end;

{==============================================================================}
destructor TFoodbaseWebDAO.Destroy;
{==============================================================================}
begin
  FBase.Free;
  inherited;
end;

{==============================================================================}
procedure TFoodbaseWebDAO.Download;
{==============================================================================}
var
  Data: string;
begin
  try
    Data := FClient.DownloadFoodBase();
    DiaryRoutines.WriteFile(FILENAME_TEMP, Data);
    FBase.LoadFromFile_XML(FILENAME_TEMP);
  finally
    DeleteFile(FILENAME_TEMP);
  end;
end;

{==============================================================================}
function TFoodbaseWebDAO.FindAll: TFoodList;
{==============================================================================}
var
  i: integer;
begin
  Download();
  
  SetLength(Result, FBase.Count);
  for i := 0 to FBase.Count - 1 do
  begin
    Result[i] := TFood.Create;
    Result[i].CopyFrom(FBase[i], True);
  end;
end;

{==============================================================================}
function TFoodbaseWebDAO.FindAny(const Filter: string): TFoodList;
{==============================================================================}
var
  i, k: integer;
begin
  //Download; // LOL.

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
function TFoodbaseWebDAO.FindOne(const Name: string): TFood;
{==============================================================================}
var
  Index: integer;
begin
  Download;

  Index := FBase.Find(Name);
  if (Index <> -1) then
  begin
    Result := TFood.Create;
    Result.CopyFrom(FBase[Index], True);
  end else
    Result := nil;
end;

{==============================================================================}
function TFoodbaseWebDAO.GetIndex(Food: TFood): integer;
{==============================================================================}
begin
  Result := FBase.GetIndex(Food.ID);
end;

{==============================================================================}
function TFoodbaseWebDAO.GetIndex(ID: TCompactGUID): integer;
{==============================================================================}
begin
  Result := FBase.GetIndex(ID);
end;

{==============================================================================}
procedure TFoodbaseWebDAO.ReplaceAll(const NewList: TFoodList;
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
  Upload;
end;

{==============================================================================}
procedure TFoodbaseWebDAO.Update(Food: TFood);
{==============================================================================}
var
  Index: integer;
begin
  Index := GetIndex(Food);
  if (Index <> -1) then
  begin
    FBase[Index].CopyFrom(Food, True);
    FBase.Sort;
    Upload();
  end else
    raise EItemNotFoundException.Create(Food.ID);
end;

{==============================================================================}
procedure TFoodbaseWebDAO.Upload;
{==============================================================================}
var
  Data: string;
begin
  try
    FBase.SaveToFile(FILENAME_TEMP);
    Data := ReadFile(FILENAME_TEMP);
    FClient.UploadFoodBase(Data, FBase.Version);
  finally
    DeleteFile(FILENAME_TEMP);
  end;
end;

{==============================================================================}
function TFoodbaseWebDAO.Version: integer;
{==============================================================================}
begin
  Result := FClient.GetFoodBaseVersion();
end;

end.

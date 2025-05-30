unit DishbaseWebDAO;

interface

uses
  SysUtils,
  uLkJSON,
  BusinessObjects,
  DishbaseDAO,
  DiaryWeb,
  DiaryRoutines,
  JsonSerializer,
  DiaryPageSerializer,
  AutoLog,
  MerkleTree;

type
  TDishbaseWebDAO = class (TDishbaseDAO)
  private
    FClient: TDiacompClient;
  public
    function Count(Prefix: TCompactGUID): integer; override;
    constructor Create(Client: TDiacompClient);
    function FindAll(ShowRemoved: boolean): TDishItemList; override;
    function FindAny(const Filter: string): TDishItemList; override;
    function FindOne(const Name: string): TDishItem; override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    function FindByIdPrefix(Prefix: TCompactGUID): TVersionedList; override;
    function GetHashTree(): THashTree; override;
    procedure Save(const Items: TVersionedList); override;
  end;

  TDishbaseWebHash = class(THashTree)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);
    function GetHash(const Prefix: string): string; override;
    function GetHashChildren(const Prefix: string): TStringMap; override;
  end;

implementation

{======================================================================================================================}
function ParseDishItemsResponse(const S: string): TDishItemList;
{======================================================================================================================}
var
  Json: TlkJSONlist;
begin
  Json := TlkJSON.ParseText(MakeSureJsonList(S)) as TlkJSONlist;
  try
    Result := ParseVersionedDishItems(json);
  finally
    FreeAndNil(Json);
  end;
end;

{ TDishbaseWebDAO }

{======================================================================================================================}
function TDishbaseWebDAO.Count(Prefix: TCompactGUID): integer;
{======================================================================================================================}
var
  StdResp: TStdResponse;
  Query: string;
begin
  Query := FClient.GetApiURL() + 'dish/count/' + Prefix;
  StdResp := FClient.DoGetSmart(query) ;
  try
    Result := StrToInt(StdResp.Response);
  finally
    FreeAndNil(StdResp);
  end;
end;

{======================================================================================================================}
constructor TDishbaseWebDAO.Create(Client: TDiacompClient);
{======================================================================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');
    
  FClient := Client;
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindAll(ShowRemoved: boolean): TDishItemList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/all/?show_rem=' + IntToStr(byte(ShowRemoved)));
  try
    Result := ParseDishItemsResponse(Response.Response);
  finally
    FreeAndNil(Response);
  end;
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindAny(const Filter: string): TDishItemList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/search/?q=' + Filter);
  try
    Result := ParseDishItemsResponse(Response.Response);
  finally
    FreeAndNil(Response);
  end;
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindById(ID: TCompactGUID): TVersioned;
{======================================================================================================================}
var
  Response: TStdResponse;
  List: TDishItemList;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/guid/' + ID);
  try
    List := nil; // for compiler
    // TODO: constants
    case Response.Code of
      0:   begin
             List := ParseDishItemsResponse(Response.Response);
             Result := List[0];
           end;
      404: Result := nil;
      else
      begin
        Result := nil;
        FClient.CheckResponse(Response);
      end;
    end;
  finally
    FreeAndNil(Response);
  end;
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindByIdPrefix(Prefix: TCompactGUID): TVersionedList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/guid/' + Prefix);
  try
    Result := DishItemListToVersionedList(ParseDishItemsResponse(Response.Response));
  finally
    FreeAndNil(Response);
  end;
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindChanged(Since: TDateTime): TVersionedList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/changes/?since=' + FormatDateTime(Since));
  try
    Result := DishItemListToVersionedList(ParseDishItemsResponse(Response.Response));
  finally
    FreeAndNil(Response);
  end;
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindOne(const Name: string): TDishItem;
{======================================================================================================================}
var
  Items: TDishItemList;
  i: integer;
begin
  Items := FindAny(Name);

  for i := 0 to High(Items) do
  if (Items[i].Name = Name) then
  begin
    Result := Items[i];
    Exit;
  end;

  Result := nil;
end;

{======================================================================================================================}
function TDishbaseWebDAO.GetHashTree(): THashTree;
{======================================================================================================================}
begin
  Result := TDishbaseWebHash.Create(FClient);
end;

{======================================================================================================================}
procedure TDishbaseWebDAO.Save(const Items: TVersionedList);
{======================================================================================================================}
var
  Par: TStringArray;
  Response: TStdResponse;
begin
  // заглушка
  if (Length(Items) = 0) then
  begin
    Exit;
  end;

  SetLength(Par, 1);
  par[0] := 'items=' + JsonWrite(SerializeVersionedDishItems(VersionedListToDishItemList(Items)));

  Response := FClient.DoPutSmart(FClient.GetApiURL() + 'dish/', Par);

  // TODO: check response, throw exception if non-zero
  // Response.Code = 0     it's ok
  // Response.Code = 500   Internal server error
  // Response.Code = xxx   Connection error
  // etc.

  Response.Free;
end;

{ TDishbaseWebHash }

{======================================================================================================================}
constructor TDishbaseWebHash.Create(Client: TDiacompClient);
{======================================================================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');

  FClient := Client;
end;

{======================================================================================================================}
function TDishbaseWebHash.GetHash(const Prefix: string): string;
{======================================================================================================================}
var
  StdResp: TStdResponse;
begin
  StdResp := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/hash/' + Prefix);
  try
    Result := StdResp.Response;
  finally
    FreeAndNil(StdResp);
  end;
end;

{======================================================================================================================}
function TDishbaseWebHash.GetHashChildren(const Prefix: string): TStringMap;
{======================================================================================================================}
var
  StdResp: TStdResponse;
begin
  StdResp := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/hashes/' + Prefix);
  try
    Result := ParseStringMap(StdResp.Response);
  finally
    StdResp.Free;
  end;
end;

end.

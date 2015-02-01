unit FoodbaseWebDAO;

interface

uses
  SysUtils,
  uLkJSON,
  BusinessObjects,
  FoodbaseDAO,
  DiaryWeb,
  DiaryRoutines,
  JsonSerializer,
  DiaryPageSerializer,
  AutoLog,
  MerkleTree;

type
  TFoodbaseWebDAO = class (TFoodbaseDAO)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    procedure Delete(ID: TCompactGUID); override;
    function FindAll(ShowRemoved: boolean): TFoodItemList; override;
    function FindAny(const Filter: string): TFoodItemList; override;
    function FindOne(const Name: string): TFoodItem; override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    function FindByIdPrefix(Prefix: TCompactGUID): TVersionedList; override;
    function GetHashTree(): THashTree; override;
    procedure Save(const Items: TVersionedList); override;
  end;

  TFoodbaseWebHash = class(THashTree)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);
    function GetHash(const Prefix: string): string; override;
  end;

implementation

{======================================================================================================================}
function ParseFoodItemsResponse(const S: string): TFoodItemList;
{======================================================================================================================}
var
  Json: TlkJSONlist;
begin
  Json := TlkJSON.ParseText(MakeSureJsonList(S)) as TlkJSONlist;
  try
    Result := ParseVersionedFoodItems(json);
  finally
    Json.Free;
  end;
end;

{ TFoodbaseWebDAO }

{======================================================================================================================}
constructor TFoodbaseWebDAO.Create(Client: TDiacompClient);
{======================================================================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');
    
  FClient := Client;
end;

{======================================================================================================================}
procedure TFoodbaseWebDAO.Delete(ID: TCompactGUID);
{======================================================================================================================}
var
  Item: TVersioned;
begin
  Item := FindById(ID);
  if (Item <> nil) then  
  begin
    Item.Deleted := True;
    Save(Item);
  end;
end;

{======================================================================================================================}
function TFoodbaseWebDAO.FindAll(ShowRemoved: boolean): TFoodItemList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'food/all/?show_rem=' + IntToStr(byte(ShowRemoved)));
  Result := ParseFoodItemsResponse(Response.Response);
end;

{======================================================================================================================}
function TFoodbaseWebDAO.FindAny(const Filter: string): TFoodItemList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'food/search/?q=' + Filter);
  Result := ParseFoodItemsResponse(Response.Response);
end;

{======================================================================================================================}
function TFoodbaseWebDAO.FindById(ID: TCompactGUID): TVersioned;
{======================================================================================================================}
var
  Response: TStdResponse;
  List: TFoodItemList;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'food/guid/' + ID);
  List := nil; // for compiler
  // TODO: constants
  case Response.Code of
    0:   begin
           List := ParseFoodItemsResponse(Response.Response);
           Result := List[0];
         end;
    404: Result := nil;
    else
    begin
      Result := nil;
      FClient.CheckResponse(Response);
    end;
  end;
end;

{======================================================================================================================}
function TFoodbaseWebDAO.FindByIdPrefix(Prefix: TCompactGUID): TVersionedList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'food/guid/' + Prefix);
  Result := FoodItemListToVersionedList(ParseFoodItemsResponse(Response.Response));
end;

{======================================================================================================================}
function TFoodbaseWebDAO.FindChanged(Since: TDateTime): TVersionedList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'food/changes/?since=' + DateTimeToStr(Since, STD_DATETIME_FMT));
  Result := FoodItemListToVersionedList(ParseFoodItemsResponse(Response.Response));
end;

{======================================================================================================================}
function TFoodbaseWebDAO.FindOne(const Name: string): TFoodItem;
{======================================================================================================================}
var
  Items: TFoodItemList;
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
function TFoodbaseWebDAO.GetHashTree: THashTree;
{======================================================================================================================}
begin
  Result := TFoodbaseWebHash.Create(FClient);
end;

{======================================================================================================================}
procedure TFoodbaseWebDAO.Save(const Items: TVersionedList);
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
  par[0] := 'items=' + JsonWrite(SerializeVersionedFoodItems(VersionedListToFoodItemList(Items)));

  Response := FClient.DoPutSmart(FClient.GetApiURL() + 'food/', Par);

  // TODO: check response, throw exception if non-zero
  // Response.Code = 0     it's ok
  // Response.Code = 500   Internal server error
  // Response.Code = xxx   Connection error
  // etc.

  Response.Free;
end;

{ TFoodbaseWebHash }

{======================================================================================================================}
constructor TFoodbaseWebHash.Create(Client: TDiacompClient);
{======================================================================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');

  FClient := Client;
end;

{======================================================================================================================}
function TFoodbaseWebHash.GetHash(const Prefix: string): string;
{======================================================================================================================}
var
  StdResp: TStdResponse;
  Query: string;
begin
  Query := FClient.GetApiURL() + 'food/hash/' + Prefix;
  StdResp := FClient.DoGetSmart(query);
  Result := StdResp.Response;
  StdResp.Free;
end;

end.

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
  AutoLog;

type
  TDishbaseWebDAO = class (TDishbaseDAO)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    procedure Delete(ID: TCompactGUID); override;
    function FindAll(ShowRemoved: boolean): TDishItemList; override;
    function FindAny(const Filter: string): TDishItemList; override;
    function FindOne(const Name: string): TDishItem; override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    function FindByIdPrefix(Prefix: TCompactGUID): TVersionedList; override;
    function GetHash(Prefix: TCompactGUID): TCompactGUID; override;
    procedure Save(const Items: TVersionedList); override;
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
    Json.Free;
  end;
end;

{ TDishbaseWebDAO }

{======================================================================================================================}
constructor TDishbaseWebDAO.Create(Client: TDiacompClient);
{======================================================================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');
    
  FClient := Client;
end;

{======================================================================================================================}
procedure TDishbaseWebDAO.Delete(ID: TCompactGUID);
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
function TDishbaseWebDAO.FindAll(ShowRemoved: boolean): TDishItemList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/all/?show_rem=' + IntToStr(byte(ShowRemoved)));
  Result := ParseDishItemsResponse(Response.Response);
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindAny(const Filter: string): TDishItemList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/search/?q=' + Filter);
  Result := ParseDishItemsResponse(Response.Response);
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindById(ID: TCompactGUID): TVersioned;
{======================================================================================================================}
var
  Response: TStdResponse;
  List: TDishItemList;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/guid/' + ID);
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
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindByIdPrefix(Prefix: TCompactGUID): TVersionedList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/guid/' + Prefix);
  Result := DishItemListToVersionedList(ParseDishItemsResponse(Response.Response));
end;

{======================================================================================================================}
function TDishbaseWebDAO.FindChanged(Since: TDateTime): TVersionedList;
{======================================================================================================================}
var
  Response: TStdResponse;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'dish/changes/?since=' + DateTimeToStr(Since, STD_DATETIME_FMT));
  Result := DishItemListToVersionedList(ParseDishItemsResponse(Response.Response));
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
function TDishbaseWebDAO.GetHash(Prefix: TCompactGUID): TCompactGUID;
{======================================================================================================================}
var
  Query, Resp: string;
begin
  Query := FClient.GetApiURL() + 'dish/hash/' + Prefix;
  Resp := FClient.DoGetSmart(query).Response;
  {#}Log(VERBOUS, 'TDishbaseWebDAO.GetHash(): quered OK, Resp = "' + Resp + '"');

  Result := Resp;
end;

{======================================================================================================================}
procedure TDishbaseWebDAO.Save(const Items: TVersionedList);
{======================================================================================================================}
var
  Par: TParamList;
  //Response: TStdResponse;
begin
  // заглушка
  if (Length(Items) = 0) then
  begin
    Exit;
  end;

  SetLength(Par, 1);
  par[0] := 'items=' + JsonWrite(SerializeVersionedDishItems(VersionedListToDishItemList(Items)));

  {Response :=} FClient.DoPutSmart(FClient.GetApiURL() + 'dish/', Par);

  // TODO: check response, throw exception if non-zero
  // Response.Code = 0     it's ok
  // Response.Code = 500   Internal server error
  // Response.Code = xxx   Connection error
  // etc.
end;

end.

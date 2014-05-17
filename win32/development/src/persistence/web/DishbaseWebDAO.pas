unit DishbaseWebDAO;

interface

uses
  SysUtils,
  uLkJSON,
  BusinessObjects,
  DishbaseDAO,
  DiaryWeb,
  Bases,
  DiaryRoutines,
  //JsonDishItemSerializer,
  JsonSerializer;

type
  TDishbaseWebDAO = class (TDishbaseDAO)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    function FindAll(ShowRemoved: boolean): TDishItemList; override;
    function FindAny(const Filter: string): TDishItemList; override;
    function FindOne(const Name: string): TDish; override;
    function FindChanged(Since: TDateTime): TDishItemList; override;
    function FindById(ID: string): TDish; override;
    procedure Save(const Items: TDishItemList); override;
  end;

implementation

{==============================================================================}
function ParseDishItemsResponse(const S: string): TDishItemList;
{==============================================================================}
var
  Response: TStdResponse;
  Json: TlkJSONlist;
begin
  Response := TStdResponse.Create(S);
  try
    Json := Response.ConvertResponseToJson() as TlkJSONlist;
    //Result := ParseDishItems(json);
    //TODO 1: FIXME
  finally
    Response.Free;
    Json.Free;
  end;
end;

{ TDishbaseWebDAO }

{==============================================================================}
constructor TDishbaseWebDAO.Create(Client: TDiacompClient);
{==============================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');
    
  FClient := Client;
end;

{==============================================================================}
function TDishbaseWebDAO.FindAll(ShowRemoved: boolean): TDishItemList;
{==============================================================================}
var
  Resp: string;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'food/all/?show_rem=' + IntToStr(byte(ShowRemoved)));
  Result := ParseDishItemsResponse(Resp);
end;

{==============================================================================}
function TDishbaseWebDAO.FindAny(const Filter: string): TDishItemList;
{==============================================================================}
var
  Resp: string;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'food/search/?q=' + Filter);
  Result := ParseDishItemsResponse(Resp);
end;

{==============================================================================}
function TDishbaseWebDAO.FindById(ID: string): TDish;
{==============================================================================}
var
  Resp: string;
  List: TDishItemList;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'food/guid/' + ID);
  List := ParseDishItemsResponse(Resp);
  Result := List[0];
end;

{==============================================================================}
function TDishbaseWebDAO.FindChanged(Since: TDateTime): TDishItemList;
{==============================================================================}
var
  Resp: string;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'food/changes/?since=' + DateTimeToStr(Since, STD_DATETIME_FMT));
  Result := ParseDishItemsResponse(Resp);
end;

{==============================================================================}
function TDishbaseWebDAO.FindOne(const Name: string): TDish;
{==============================================================================}
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

{==============================================================================}
procedure TDishbaseWebDAO.Save(const Items: TDishItemList);
{==============================================================================}
var
  Par: TParamList;
  Msg: string;
  Response: TStdResponse;
begin
  // заглушка
  if (Length(Items) = 0) then
  begin
    Exit;
  end;

  SetLength(Par, 1);
  par[0] := 'items=' + '[]';
  //JsonWrite(SerializeDishItems(Items));
  // TODO 1: FIXME

  Msg := FClient.DoPutSmart(FClient.GetApiURL() + 'food/', Par);

  // TODO: check response, throw exception if non-zero
  // Response.Code = 0     it's ok
  // Response.Code = 500   Internal server error
  // Response.Code = xxx   Connection error
  // etc.
end;

end.

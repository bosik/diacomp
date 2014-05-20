unit FoodbaseWebDAO;

interface

uses
  SysUtils,
  uLkJSON,
  BusinessObjects,
  FoodbaseDAO,
  DiaryWeb,
  Bases,
  DiaryRoutines,
  JsonFoodItemSerializer,
  JsonSerializer;

type
  TFoodbaseWebDAO = class (TFoodbaseDAO)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    function FindAll(ShowRemoved: boolean): TFoodItemList; override;
    function FindAny(const Filter: string): TFoodItemList; override;
    function FindOne(const Name: string): TFood; override;
    function FindChanged(Since: TDateTime): TFoodItemList; override;
    function FindById(ID: TCompactGUID): TFood; override;
    procedure Save(const Items: TFoodItemList); override;
  end;

implementation

{==============================================================================}
function ParseFoodItemsResponse(const S: string): TFoodItemList;
{==============================================================================}
var
  Response: TStdResponse;
  Json: TlkJSONlist;
begin
  Response := TStdResponse.Create(S);
  try
    Json := Response.ConvertResponseToJson() as TlkJSONlist;
    Result := ParseFoodItems(json);
  finally
    Response.Free;
    Json.Free;
  end;
end;

{ TFoodbaseWebDAO }

{==============================================================================}
constructor TFoodbaseWebDAO.Create(Client: TDiacompClient);
{==============================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');
    
  FClient := Client;
end;

{==============================================================================}
function TFoodbaseWebDAO.FindAll(ShowRemoved: boolean): TFoodItemList;
{==============================================================================}
var
  Resp: string;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'food/all/?show_rem=' + IntToStr(byte(ShowRemoved)));
  Result := ParseFoodItemsResponse(Resp);
end;

{==============================================================================}
function TFoodbaseWebDAO.FindAny(const Filter: string): TFoodItemList;
{==============================================================================}
var
  Resp: string;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'food/search/?q=' + Filter);
  Result := ParseFoodItemsResponse(Resp);
end;

{==============================================================================}
function TFoodbaseWebDAO.FindById(ID: TCompactGUID): TFood;
{==============================================================================}
var
  Resp: string;
  List: TFoodItemList;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'food/guid/' + ID);
  List := ParseFoodItemsResponse(Resp);
  Result := List[0];
end;

{==============================================================================}
function TFoodbaseWebDAO.FindChanged(Since: TDateTime): TFoodItemList;
{==============================================================================}
var
  Resp: string;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'food/changes/?since=' + DateTimeToStr(Since, STD_DATETIME_FMT));
  Result := ParseFoodItemsResponse(Resp);
end;

{==============================================================================}
function TFoodbaseWebDAO.FindOne(const Name: string): TFood;
{==============================================================================}
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

{==============================================================================}
procedure TFoodbaseWebDAO.Save(const Items: TFoodItemList);
{==============================================================================}
var
  Par: TParamList;
  Msg: string;
  // Response: TStdResponse;
begin
  // заглушка
  if (Length(Items) = 0) then
  begin
    Exit;
  end;

  SetLength(Par, 1);
  par[0] := 'items=' + JsonWrite(SerializeFoodItems(Items));

  Msg := FClient.DoPutSmart(FClient.GetApiURL() + 'food/', Par);

  // TODO: check response, throw exception if non-zero
  // Response.Code = 0     it's ok
  // Response.Code = 500   Internal server error
  // Response.Code = xxx   Connection error
  // etc.
end;

end.

unit DiaryWebSource;

interface

uses
  SysUtils,
  Classes,
  DiaryDAO,
  DiaryWeb,
  DiaryPage,
  DiaryPageSerializer,
  DiaryRecords,
  DiaryRoutines,
  BusinessObjects,
  AutoLog,
  uLkJSON,
  JsonSerializer,
  ObjectService,
  MerkleTree;

type
  TDiaryWebSource = class (TDiaryDAO)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    procedure Delete(ID: TCompactGUID); override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    function FindByIdPrefix(Prefix: TCompactGUID): TVersionedList; override;
    function GetHashTree(): THashTree; override;
    procedure Save(const Recs: TVersionedList); override;
  end;

  TDiaryWebHash = class(THashTree)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);
    function GetHash(const Prefix: string): string; override;
    function GetHashChildren(const Prefix: string): TStringMap; override;
  end;

implementation

{======================================================================================================================}
function ParseRecordList(const S: string): TRecordList;
{======================================================================================================================}
var
  Json: TlkJSONlist;
begin
  Json := TlkJSON.ParseText(MakeSureJsonList(S)) as TlkJSONlist;
  try
    Result := ParseVersionedDiaryRecords(json);
  finally
    Json.Free;
  end;
end;

{======================================================================================================================}
function ParseStringMap(const S: string): TStringMap;
{======================================================================================================================}
var
  Json: TlkJSONlist;
  Item: TlkJSONobject;
  Key, Value: string;
  i: integer;
begin
  Json := TlkJSON.ParseText(MakeSureJsonList(S)) as TlkJSONlist;
  try
    Result := TStringMap.Create;
    for i := 0 to json.Count - 1 do
    begin
      Item := json.Child[i] as TlkJSONobject;
      Key := Item.getString('key');
      Value := Item.GetString('value');
      Result.Add(Key, Value, True);
    end;
  finally
    Json.Free;
  end;
end;

{ TDiaryWebSource }

{======================================================================================================================}
constructor TDiaryWebSource.Create(Client: TDiacompClient);
{======================================================================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');

  FClient := Client;
end;

{======================================================================================================================}
procedure TDiaryWebSource.Delete(ID: TCompactGUID);
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
function TDiaryWebSource.FindById(ID: TCompactGUID): TVersioned;
{======================================================================================================================}
var
  Response: TStdResponse;
  List: TRecordList;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'diary/guid/' + ID);
  List := nil; // for compiler
  case Response.Code of
    STATUS_OK:
      begin
        List := ParseRecordList(Response.Response);
        Result := List[0];
      end;
    STATUS_NOT_FOUND:
      begin
        Result := nil;
      end;
    else
      begin
        Result := nil;
        FClient.CheckResponse(Response);
      end;
  end;
  Response.Free;
end;

{======================================================================================================================}
function TDiaryWebSource.FindByIdPrefix(Prefix: TCompactGUID): TVersionedList;
{======================================================================================================================}
var
  StdResp: TStdResponse;
  Query: string;
begin
  Query := FClient.GetApiURL() + 'diary/guid/' + Prefix;
  StdResp := FClient.DoGetSmart(query) ;
  Result := RecordToVersioned(ParseRecordList(StdResp.Response));
  StdResp.Free;
end;

{======================================================================================================================}
function TDiaryWebSource.FindChanged(Since: TDateTime): TVersionedList;
{======================================================================================================================}
var
  Query, Resp: string;
begin
  Query := FClient.GetApiURL() + 'diary/changes/?since=' + DateTimeToStr(Since, STD_DATETIME_FMT);
  Resp := FClient.DoGetSmart(query).Response;
  {#}Log(VERBOUS, 'TDiaryWebSource.FindChanged(): quered OK, Resp = "' + Resp + '"');

  Result := RecordToVersioned(ParseRecordList(Resp));
end;

{======================================================================================================================}
function TDiaryWebSource.FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList;
{======================================================================================================================}
var
  Query: string;
  Resp: string;
begin
  Query :=
    FClient.GetApiURL() + 'diary/period/?show_rem=0' +
    '&start_time=' + DateTimeToStr(TimeFrom, STD_DATETIME_FMT) +
    '&end_time=' + DateTimeToStr(TimeTo, STD_DATETIME_FMT);

  Resp := FClient.DoGetSmart(query).Response;
  {#}Log(VERBOUS, 'TDiaryWebSource.FindPeriod(): quered OK, Resp = "' + Resp + '"');

  Result := ParseRecordList(Resp);
end;

{======================================================================================================================}
function TDiaryWebSource.GetHashTree(): THashTree;
{======================================================================================================================}
begin
  Result := TDiaryWebHash.Create(FClient);
end;

{======================================================================================================================}
procedure TDiaryWebSource.Save(const Recs: TVersionedList);
{======================================================================================================================}
var
  Par: TStringArray;
  json: TlkJSONlist;
  Response: TStdResponse;
begin
  // заглушка
  if (Length(Recs) = 0) then
  begin
    Exit;
  end;

  SetLength(Par, 1);
  json := SerializeVersionedDiaryRecords(VersionedToRecord(Recs));
  par[0] := 'items=' + JsonWrite(json);
  json.Free;

  Response := FClient.DoPutSmart(FClient.GetApiURL() + 'diary/', Par);

  // TODO: check response, throw exception if non-zero
  // Response.Code = 0     it's ok
  // Response.Code = 500   Internal server error
  // Response.Code = xxx   Connection error
  // etc.

  Response.Free;
end;

{ TDiaryWebHash }

{======================================================================================================================}
constructor TDiaryWebHash.Create(Client: TDiacompClient);
{======================================================================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');

  FClient := Client;
end;

{======================================================================================================================}
function TDiaryWebHash.GetHash(const Prefix: string): string;
{======================================================================================================================}
var
  StdResp: TStdResponse;
  Query: string;
begin
  Query := FClient.GetApiURL() + 'diary/hash/' + Prefix;
  StdResp := FClient.DoGetSmart(query);
  Result := StdResp.Response;
  StdResp.Free;
end;

{======================================================================================================================}
function TDiaryWebHash.GetHashChildren(const Prefix: string): TStringMap;
{======================================================================================================================}
var
  Query: string;
  StdResp: TStdResponse;
begin
  Query := FClient.GetApiURL() + 'diary/hashes/' + Prefix;
  try
    StdResp := FClient.DoGetSmart(query);
    Result := ParseStringMap(StdResp.Response);
  finally
    StdResp.Free;
  end;
end;

end.

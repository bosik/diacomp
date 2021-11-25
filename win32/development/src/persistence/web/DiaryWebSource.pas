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
    function Count(Prefix: TCompactGUID): integer; override;
    constructor Create(Client: TDiacompClient);   
    procedure Delete(ID: TCompactGUID); override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TVersionedList; override;
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

{ TDiaryWebSource }

{======================================================================================================================}
function TDiaryWebSource.Count(Prefix: TCompactGUID): integer;
{======================================================================================================================}
var
  StdResp: TStdResponse;
  Query: string;
begin
  Query := FClient.GetApiURL() + 'diary/count/' + Prefix;
  StdResp := FClient.DoGetSmart(query) ;
  Result := StrToInt(StdResp.Response);
  StdResp.Free;
end;

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
  List: TVersionedList;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'diary/guid/' + ID);
  List := nil; // for compiler
  case Response.Code of
    STATUS_OK:
      begin
        List := ReadVersionedDiaryRecords(Response.Response);
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
  Query: string;
  StdResp: TStdResponse;
begin
  Query := FClient.GetApiURL() + 'diary/guid/' + Prefix;
  StdResp := FClient.DoGetSmart(query);
  try
    Result := ReadVersionedDiaryRecords(StdResp.Response);
  finally
    StdResp.Free;
  end;
end;

{======================================================================================================================}
function TDiaryWebSource.FindChanged(Since: TDateTime): TVersionedList;
{======================================================================================================================}
var
  Query: string;
  StdResp: TStdResponse;
begin
  Query := FClient.GetApiURL() + 'diary/changes/?since=' + FormatDateTime(Since);
  StdResp := FClient.DoGetSmart(query);
  try
    Result := ReadVersionedDiaryRecords(StdResp.Response);
  finally
    StdResp.Free;
  end;
end;

{======================================================================================================================}
function TDiaryWebSource.FindPeriod(TimeFrom, TimeTo: TDateTime): TVersionedList;
{======================================================================================================================}
var
  Query: string;
  StdResp: TStdResponse;
begin
  Query :=
    FClient.GetApiURL() + 'diary/period/?show_rem=0' +
    '&start_time=' + FormatDateTime(TimeFrom) +
    '&end_time=' + FormatDateTime(TimeTo);

  StdResp := FClient.DoGetSmart(query);
  try
    {#}Log(VERBOUS, 'TDiaryWebSource.FindPeriod(): quered OK, Resp = "' + StdResp.Response + '"');
    Result := ReadVersionedDiaryRecords(StdResp.Response);
  finally
    StdResp.Free;
  end;
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
  json := SerializeVersionedDiaryRecords(Recs);
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
  StdResp := nil;

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

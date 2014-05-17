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
  AutoLog,
  uLkJSON,
  JsonSerializer;

type
  TDiaryWebSource = class (TDiaryDAO)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    function FindChanged(Since: TDateTime): TRecordList; override;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; override;
    function FindById(ID: TCompactGUID): TCustomRecord; override;
    procedure Post(const Recs: TRecordList); override;
  end;

implementation

{==============================================================================}
function ParseRecordList(const S: string): TRecordList;
{==============================================================================}
var
  Response: TStdResponse;
  Json: TlkJSONlist;
begin
  Response := TStdResponse.Create(S);
  try
    Json := Response.ConvertResponseToJson() as TlkJSONlist;
    Result := ParseRecords(json);
  finally
    Response.Free;
    Json.Free;
  end;
end;

{ TDiaryWebSource }

{==============================================================================}
constructor TDiaryWebSource.Create(Client: TDiacompClient);
{==============================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');
    
  FClient := Client;
end;

{==============================================================================}
function TDiaryWebSource.FindById(ID: TCompactGUID): TCustomRecord;
{==============================================================================}
var
  Resp: string;
  List: TRecordList;
begin
  Resp := FClient.DoGetSmart(FClient.GetApiURL() + 'diary/guid/' + ID);
  List := ParseRecordList(Resp);
  Result := List[0];
end;

{==============================================================================}
function TDiaryWebSource.FindChanged(Since: TDateTime): TRecordList;
{==============================================================================}
var
  Query, Resp: string;
begin
  {#}Log(VERBOUS, 'TDiaryWebSource.FindChanged(): started, since = "' + DateTimeToStr(Since, STD_DATETIME_FMT) + '"');

  Query := FClient.GetApiURL() + 'diary/changes/?since=' + ChkSpace(DateTimeToStr(Since, STD_DATETIME_FMT));
  {#}Log(VERBOUS, 'TDiaryWebSource.FindChanged(): quering ' + Query);

  Resp := FClient.DoGetSmart(query);
  {#}Log(VERBOUS, 'TDiaryWebSource.FindChanged(): quered OK, Resp = "' + Resp + '"');

  Result := ParseRecordList(Resp);
  {#}Log(VERBOUS, 'TDiaryWebSource.FindChanged(): done OK');
end;

{==============================================================================}
function TDiaryWebSource.FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList;
{==============================================================================}
var
  Query: string;
  Resp: string;
begin
  {#}Log(VERBOUS, 'TDiaryWebSource.FindPeriod: started');

  Query :=
    FClient.GetApiURL() + 'diary/period/?show_rem=0' +
    '&start_time=' + ChkSpace(DateTimeToStr(TimeFrom, STD_DATETIME_FMT)) +
    '&end_time=' + ChkSpace(DateTimeToStr(TimeTo, STD_DATETIME_FMT));

  {#}Log(VERBOUS, 'TDiaryWebSource.FindPeriod: quering "' + Query + '"');


  Resp := FClient.DoGetSmart(query);
  {#}Log(VERBOUS, 'TDiaryWebSource.FindPeriod(): quered OK, Resp = "' + Resp + '"');

  Result := ParseRecordList(Resp);
  {#}Log(VERBOUS, 'TDiaryWebSource.FindPeriod(): done OK');
end;

{==============================================================================}
procedure TDiaryWebSource.Post(const Recs: TRecordList);
{==============================================================================}
var
  Par: TParamList;
  Msg: string;
  Response: TStdResponse;
begin
  // заглушка
  if (Length(Recs) = 0) then
  begin
    Exit;
  end;

  SetLength(Par, 1);
  par[0] := 'items=' + JsonWrite(SerializeRecords(Recs));

  Msg := FClient.DoPutSmart(FClient.GetApiURL() + 'diary/', Par);

  // TODO: check response, throw exception if non-zero
  // Response.Code = 0     it's ok
  // Response.Code = 500   Internal server error
  // Response.Code = xxx   Connection error
  // etc.
end;

end.

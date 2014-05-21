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
  JsonSerializer;

type
  TDiaryWebSource = class (TDiaryDAO)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    procedure Save(const Recs: TRecordList); override;
  end;

implementation

{==============================================================================}
function ParseRecordList(S: string): TRecordList;
{==============================================================================}
var
  Json: TlkJSONlist;
begin
  try
    if (s <> '') and (s[1] = '{') and (s[Length(S)] = '}') then
      S := '[' + s + ']';

    Json := TlkJSON.ParseText(S) as TlkJSONlist;
    Result := ParseVersionedDiaryRecords(json);
  finally
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
function TDiaryWebSource.FindById(ID: TCompactGUID): TVersioned;
{==============================================================================}
var
  Response: TStdResponse;
  List: TRecordList;
begin
  Response := FClient.DoGetSmart(FClient.GetApiURL() + 'diary/guid/' + ID);
  case Response.Code of
    // TODO: constants
    0: begin
         List := ParseRecordList(Response.Response);
         Result := List[0];
       end;
    404: Result := nil;
  end;
end;

{==============================================================================}
function TDiaryWebSource.FindChanged(Since: TDateTime): TVersionedList;
{==============================================================================}
var
  Query, Resp: string;
begin
  {#}Log(VERBOUS, 'TDiaryWebSource.FindChanged(): started, since = "' + DateTimeToStr(Since, STD_DATETIME_FMT) + '"');

  Query := FClient.GetApiURL() + 'diary/changes/?since=' + ChkSpace(DateTimeToStr(Since, STD_DATETIME_FMT));
  {#}Log(VERBOUS, 'TDiaryWebSource.FindChanged(): quering ' + Query);

  Resp := FClient.DoGetSmart(query).Response;
  {#}Log(VERBOUS, 'TDiaryWebSource.FindChanged(): quered OK, Resp = "' + Resp + '"');

  Result := RecordToVersioned(ParseRecordList(Resp));
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


  Resp := FClient.DoGetSmart(query).Response;
  {#}Log(VERBOUS, 'TDiaryWebSource.FindPeriod(): quered OK, Resp = "' + Resp + '"');

  Result := ParseRecordList(Resp);
  {#}Log(VERBOUS, 'TDiaryWebSource.FindPeriod(): done OK');
end;

{==============================================================================}
procedure TDiaryWebSource.Save(const Recs: TRecordList);
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
  par[0] := 'items=' + JsonWrite(SerializeVersionedDiaryRecords(Recs));

  Response := FClient.DoPutSmart(FClient.GetApiURL() + 'diary/', Par);

  // TODO: check response, throw exception if non-zero
  // Response.Code = 0     it's ok
  // Response.Code = 500   Internal server error
  // Response.Code = xxx   Connection error
  // etc.
end;

end.

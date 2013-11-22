unit DiaryWeb;

{ Работа с сервером }

interface

uses
  SysUtils, // IntToStr
  Classes, // TStrings
  Windows, // debug: GetTickCount
  Dialogs, // debug
  IdHTTP,
  IdException,
  DiaryRoutines, // Separate()
  DiarySources, // TModList
  uLkJSON,
  AutoLog;

type
  ECommonException     = class (Exception);
  EConnectionException = class(ECommonException);   // Ошибка подключения
  EFormatException     = class(ECommonException);   // Ошибка формата данных
  EAPIException        = class(EFormatException);   // Ошибка версии API
  EAuthException       = class(ECommonException);   // Ошибка авторизации

  TParamList = array of string;

  TDiacompClient = class
  private
    // поля
    FHTTP: TIdHTTP;
    FServer: string;
    FUsername: string;
    FPassword: string;
    FOnline: boolean;
    FTimeShift: double;

    function DoGet(const URL: string): string;
    function DoPost(const URL: string; const Par: TParamList): string;

    function DoGetSmart(const URL: string): string;
    function DoPostSmart(const URL: string; const Par: TParamList): string;
    function Kicked(const Answer: string): boolean;
  public
    // TODO: sort
    constructor Create;
    destructor Destroy; override;

    procedure GetModList(Time: TDateTime; out ModList: TModList);
    procedure GetVersions(const Dates: TDateList; out ModList: TModList);
    function GetPages(const Dates: TDateList): string;
    function PostPages(const Pages: string): boolean;

    procedure Login();
    procedure Logout;
    procedure SetTimeout(Timeout: integer);
    procedure UpdateStatus;

    function GetFoodBaseVersion(): integer;
    function DownloadFoodBase(): string;
    function UploadFoodBase(const Data: string; Version: integer): boolean;

    function GetDishBaseVersion(): integer;
    function DownloadDishBase(): string;
    function UploadDishBase(const Data: string; Version: integer): boolean;

    function DownloadKoofs(): string;
    function UploadKoofs(const Data: string): boolean;

    function ConvertUTCToLocal(Time: TDateTime): TDateTime;
    function ConvertLocalToUTC(Time: TDateTime): TDateTime;

    // демо
    function Report(const Msg: string): boolean;
    function Search(const Key: string): string;

    { свойства }
//    property LoginResult: TLoginResult read FLoginResult;
    property Online: boolean read FOnline;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Server: string read FServer write FServer;
  end;

var
  WebFmt: TFormatSettings;

implementation

const
  STATUS_OK                 = 0;

type
  TResponse = record
    Status: integer;
    Message: string;
  end;

  function DecodeResponse(const S: string): TResponse;
  var
    json: TlkJSONobject;
  begin
    json := TlkJSON.ParseText(S) as TlkJSONobject;

    if Assigned(json) then
    begin
      Result.Status := (json.Field['status'] as TlkJSONnumber).Value;
      Result.Message := (json.Field['message'] as TlkJSONstring).Value
    end else
    begin
      raise EFormatException.Create('Invalid JSON: ' + S);
    end;    
  end;

{ TDiacompClient }
  
const
  CURRENT_API_VERSION  = '1.2';

  PAGE_LOGIN   = 'login.php';
  PAGE_CONSOLE = 'console.php';

  MESSAGE_DONE            = 'DONE';
  MESSAGE_FAIL            = 'FAIL';
  MESSAGE_FAIL_AUTH       = MESSAGE_FAIL + '|BADNAME';
  MESSAGE_FAIL_APIVERSION = MESSAGE_FAIL + '|DEPAPI';
  MESSAGE_ONLINE          = 'online';
  MESSAGE_OFFLINE         = 'offline';
  MESSAGE_UNAUTH          = 'Error: log in first';

{==============================================================================}
function ChkSpace(const S: string): string;
{==============================================================================}
const
  SPACE_CODE = '%20';
var
  k: integer;
begin
  Result := S;
  k := Pos(' ', Result);
  while (k > 0) do
  begin
    Result := Copy(Result, 1, k - 1) + SPACE_CODE + Copy(Result, k + 1, Length(Result) - k);
    k := Pos(' ', Result);
  end;
end;

{==============================================================================}
function TDiacompClient.ConvertUTCToLocal(Time: TDateTime): TDateTime;
{==============================================================================}
begin
  if (not FOnline) then Login();
  Result := Time + FTimeShift;
end;

{==============================================================================}
function TDiacompClient.ConvertLocalToUTC(Time: TDateTime): TDateTime;
{==============================================================================}
begin
  if (not FOnline) then Login();
  Result := Time - FTimeShift;
end;

{==============================================================================}
constructor TDiacompClient.Create;
{==============================================================================}
begin
  FOnline := False;
  FTimeShift := 0;
  FHTTP := TIdHTTP.Create(nil);
end;

{==============================================================================}
destructor TDiacompClient.Destroy;
{==============================================================================}
begin
  FHTTP.Free;
end;

{==============================================================================}
function TDiacompClient.DoGet(const URL: string): string;
{==============================================================================}
var
  Tick: cardinal;
begin
  {#}Log(VERBOUS, 'TDiacompClient.DoGet("' + URL + '")');
  {#} Tick := GetTickCount();
  Result := FHTTP.Get(URL);
  {#}Log(VERBOUS, Format('TDiacompClient.DoGet(): responsed in %d msec: "%s"', [GetTickCount - Tick, Result]));

  if (Kicked(Result)) then
    raise EAuthException.Create('Необходима авторизация');
end;

{==============================================================================}
function TDiacompClient.DoPost(const URL: string; const Par: TParamList): string;
{==============================================================================}

  function PrintParams: string;
  var
    i: integer;
  begin
    Result := '';
    for i := Low(Par) to High(Par) do
    begin
      Result := Result + Par[i];
      if (i < High(Par)) then
        Result := Result + '&';
    end;
  end;

var
  Data: TStrings;
  i: integer;
  Tick: cardinal;
begin
  {#}Log(VERBOUS, 'TDiacompClient.DoPost("' + URL + '"), ' + PrintParams());

  Data := TStringList.Create;
  try
    // формируем тело запроса
    for i := Low(Par) to High(Par) do
      Data.Add(Par[i]);

    {#} Tick := GetTickCount();
    Result := FHTTP.Post(URL, Data);
    {#}Log(VERBOUS, Format('TDiacompClient.DoPost(): responsed in %d msec: "%s"', [GetTickCount - Tick, Result]));
  finally
    //FHTTP.Disconnect;
    Data.Free;
  end;

  if (Kicked(Result)) then
    raise EAuthException.Create('Необходима авторизация');
end;

{==============================================================================}
function TDiacompClient.DoGetSmart(const URL: string): string;
{==============================================================================}
begin
  try
    Result := DoGet(URL);
  except
    on E: EAuthException do
    begin
      Login();
      Result := DoGet(URL); // здесь не ловим возможное исключение отсутствия авторизации
    end;

    on SocketError: EIdSocketError do
    begin
      FHTTP.Disconnect;
      Result := DoGet(URL);
    end;
  end;
end;

{==============================================================================}
function TDiacompClient.DoPostSmart(const URL: string; const Par: TParamList): string;
{==============================================================================}
begin
  try
    Result := DoPost(URL, Par);
  except
    on E: EAuthException do
    begin
      Login();
      Result := DoPost(URL, Par); // здесь не ловим возможное исключение отсутствия авторизации
    end;

    on SocketError: EIdSocketError do
    begin
      FHTTP.Disconnect;
      Result := DoPost(URL, Par);
    end;
  end;
end;

{==============================================================================}
procedure TDiacompClient.GetModList(Time: TDateTime; out ModList: TModList);
{==============================================================================}

  function ParseModList(const Text: string): TModList;
  var
    S: TStrings;
    i, count: integer;
    Date, Version: string;  
  begin
    S := TStringList.Create;
    try
      S.Text := Trim(Text);
      SetLength(Result, S.Count);
      count := 0;
      {#}Log(VERBOUS, 'TDiacompClient.GetModList(): lines count = ' + IntToStr(S.Count));

      for i := 0 to S.Count - 1 do
      if (S[i] <> '') then
      begin
        {#}Log(VERBOUS, 'TDiacompClient.GetModList(): parsing line "' + S[i] + '"');
        Separate(S[i], Date, '|', Version);
        {#}Log(VERBOUS, 'TDiacompClient.GetModList(): date = "' + Date + '"');
        {#}Log(VERBOUS, 'TDiacompClient.GetModList(): version = "' + Version + '"');

        Result[Count].Date := Trunc(StrToDate(Date, WebFmt));
        Result[Count].Version := StrToInt(Version);
        inc(Count);
      end;

      SetLength(ModList, Count);
    finally
      S.Free;
    end;
  end;

var
  Query, Resp: string;

begin
  {#}Log(VERBOUS, 'TDiacompClient.GetModList(): started, Time = "' + DateTimeToStr(Time) + '"');

  if (Time > 0) then
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=' + ChkSpace(DateTimeToStr(Time, WebFmt))
  else
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=0';

  {#}Log(VERBOUS, 'TDiacompClient.GetModList(): quering ' + Query);
  Resp := DoGetSmart(Query);
  {#}Log(VERBOUS, 'TDiacompClient.GetModList(): quered OK, Resp = "' + Resp + '"');

  ModList := ParseModList(Resp);

  {#}Log(VERBOUS, 'TDiacompClient.GetModList(): done OK');
end;

{==============================================================================}
procedure TDiacompClient.GetVersions(const Dates: TDateList; out ModList: TModList);
{==============================================================================}
var
  Query, Resp: string;
  Date, Version: string;
  S: TStrings;
  i,count: integer;
begin
  {#}Log(VERBOUS, 'TDiacompClient.GetVersions(): started');

  // заглушка
  if (Length(Dates) = 0) then
  begin
    SetLength(ModList, 0);
    Exit;
  end;

  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?diary:getModList&dates=';
  for i := 0 to High(Dates) do
    Query := Query + DateToStr(Dates[i], WebFmt) + ',';

  {#}Log(VERBOUS, 'TDiacompClient.GetVersions(): quering ' + Query);
  Resp := DoGetSmart(Query);
  {#}Log(VERBOUS, 'TDiacompClient.GetVersions(): quered OK, Resp = "' + Resp + '"');

  S := TStringList.Create;
  try
    S.Text := Trim(Resp);
    SetLength(ModList, S.Count);
    count := 0;
    {#}Log(VERBOUS, 'TDiacompClient.GetVersions(): lines count = ' + IntToStr(S.Count));

    for i := 0 to S.Count - 1 do
    if (S[i] <> '') then
    begin
      {#}Log(VERBOUS, 'TDiacompClient.GetVersions(): parsing line "' + S[i] + '"');
      Separate(S[i], Date, '|', Version);
      {#}Log(VERBOUS, 'TDiacompClient.GetVersions(): date = "' + Date + '"');
      {#}Log(VERBOUS, 'TDiacompClient.GetVersions(): version = "' + Version + '"');

      ModList[Count].Date := Trunc(StrToDate(Date, WebFmt));
      ModList[Count].Version := StrToInt(Version);
      inc(Count);
    end;

    SetLength(ModList, Count);
    {#}Log(VERBOUS, 'TDiacompClient.GetVersions(): done OK');
  finally
    S.Free;
  end;
end;

{==============================================================================}
function TDiacompClient.GetPages(const Dates: TDateList): string;
{==============================================================================}
var
  Query: string;
  i: integer;
begin
  // заглушка
  if (Length(Dates) = 0) then
  begin
    Result := '';
    Exit;
  end;

  {#}Log(VERBOUS, 'TDiacompClient.GetPages: started');

  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?diary:download&dates=';
  for i := 0 to High(Dates) do
    Query := Query + DateToStr(Dates[i], WebFmt) + ',';

  {#}Log(VERBOUS, 'TDiacompClient.GetPages: quering "' + Query + '"');

  // отправляем
  Result := DoGetSmart(Query);
  {#}Log(VERBOUS, 'TDiacompClient.GetPages: quered OK, resp = "' + Result + '"');
end;

{==============================================================================}
function TDiacompClient.Kicked(const Answer: string): boolean;
{==============================================================================}
begin
  Result := (Answer = MESSAGE_UNAUTH);
end;

{==============================================================================}
procedure TDiacompClient.Login();
{==============================================================================}
var
  Par: TParamList;
  ServerTime: TDateTime;
  Msg: string;
  Res, Desc: string;
  SendedTime: TDateTime;
begin
  FOnline := False;

  SetLength(Par, 4);
  par[0] := 'login=' + FUsername;
  par[1] := 'password=' + FPassword;
  par[2] := 'api=' + CURRENT_API_VERSION;
  par[3] := 'noredir=';
  {#}Log(VERBOUS, 'TDiacompClient.Login(): quering ' + FServer + PAGE_LOGIN);

  SendedTime := GetTimeUTC();
  Msg := DoPost(FServer + PAGE_LOGIN, par);
  {#}Log(VERBOUS, 'TDiacompClient.Login(): quered OK, resp = "' + Msg + '"');

  Separate(Msg, Res, '|', Desc);
  {#}Log(VERBOUS, 'TDiacompClient.Login(): Res = "' + Res + '"');
  {#}Log(VERBOUS, 'TDiacompClient.Login(): Desc = "' + Desc + '"');

  if (Res = MESSAGE_DONE) then
  begin
    {#}Log(VERBOUS, 'TDiacompClient.Login(): it means "DONE", parsing desc...');
    ServerTime := StrToDateTime(Desc, WebFmt);
    FTimeShift := (SendedTime + GetTimeUTC())/2 - ServerTime;  // pretty cool :)
  end else
  // TODO: update format (w/o concatenation)
  if (Msg = MESSAGE_FAIL_AUTH) then
    raise EAuthException.Create('Ошибка авторизации') else
  if (Msg = MESSAGE_FAIL_APIVERSION) then
    raise EAPIException.Create('Версия API не поддерживается')
  else
    raise EFormatException.CreateFmt('Неверный формат данных: %s', [Msg]);

  FOnline := True;
  {#}Log(VERBOUS, 'TDiacompClient.Login(): done');
end;

{==============================================================================}
procedure TDiacompClient.Logout;
{==============================================================================}
begin
  DoGet(FServer + PAGE_LOGIN + '?logout&noredir');  // Put Smart here :D
  FOnline := False;
end;

{==============================================================================}
function TDiacompClient.PostPages(const Pages: string): boolean;
{==============================================================================}
var
  Par: TParamList;
  Msg: string;
  Response: TResponse;
begin
  // заглушка
  if (Length(Pages) = 0) then
  begin
    Result := True;
    Exit;
  end;

  SetLength(Par, 2);
  par[0] := 'diary:upload=';
  par[1] := 'pages=' + Pages;

  // TODO
  // Response := DoPostSmart(FServer + PAGE_CONSOLE, Par)
  // Response.Status = 0     it's ok
  // Response.Status = 500   Internal server error
  // Response.Status = xxx   Connection error
  // etc.

  Msg := DoPostSmart(FServer + PAGE_CONSOLE, Par);
  Response := DecodeResponse(Msg);
  Result := (Response.Status = 0);  // TODO: create constants
end;

{==============================================================================}
procedure TDiacompClient.SetTimeout(Timeout: integer);
{==============================================================================}
begin
  FHTTP.ReadTimeout := TimeOut;
end;

{==============================================================================}
procedure TDiacompClient.UpdateStatus;
{==============================================================================}
var
  Resp: string;
begin
  Resp := DoGet(FServer + PAGE_LOGIN + '?status');
  FOnline := (Resp = MESSAGE_ONLINE);
end;

{==============================================================================}
function TDiacompClient.DownloadFoodBase(): string;
{==============================================================================}
begin
  Result := DoGetSmart(FServer + PAGE_CONSOLE + '?foodbase:download');
end;

{==============================================================================}
function TDiacompClient.DownloadDishBase(): string;
{==============================================================================}
begin
  Result := DoGetSmart(FServer + PAGE_CONSOLE + '?dishbase:download');
end;

{==============================================================================}
function TDiacompClient.GetFoodBaseVersion(): integer;
{==============================================================================}
var
  Resp: string;
begin
  Resp := DoGetSmart(FServer + PAGE_CONSOLE + '?foodbase:getVersion');
  if (not TryStrToInt(Resp, Result)) then
    raise EFormatException.CreateFmt('Неверный формат версии базы: %s', [Resp]);
end;

{==============================================================================}
function TDiacompClient.GetDishBaseVersion(): integer;
{==============================================================================}
var
  Resp: string;
begin
  Resp := DoGetSmart(FServer + PAGE_CONSOLE + '?dishbase:getVersion');
  if (not TryStrToInt(Resp, Result)) then
    raise EFormatException.CreateFmt('Неверный формат версии базы: %s', [Resp]);
end;

{==============================================================================}
function TDiacompClient.UploadFoodBase(const Data: string; Version: integer): boolean;
{==============================================================================}
var
  Par: TParamList;
  Resp: string;
begin
  SetLength(Par, 3);
  par[0] := 'foodbase:upload=';
  par[1] := 'version=' + IntToStr(Version);
  par[2] := 'data=' + Data;

  Resp := DoPostSmart(FServer + PAGE_CONSOLE, Par);
  Result := (Resp = MESSAGE_DONE);
end;

{==============================================================================}
function TDiacompClient.UploadDishBase(const Data: string; Version: integer): boolean;
{==============================================================================}
var
  Par: TParamList;
  Resp: string;
begin
  SetLength(Par, 3);
  par[0] := 'dishbase:upload=';
  par[1] := 'version=' + IntToStr(Version);
  par[2] := 'data=' + Data;

  Resp := DoPostSmart(FServer + PAGE_CONSOLE, Par);
  Result := (Resp = MESSAGE_DONE);
end;

{==============================================================================}
function TDiacompClient.DownloadKoofs(): string;
{==============================================================================}
begin
  Result := DoGetSmart(FServer + PAGE_CONSOLE + '?koofs:download');
end;

{==============================================================================}
function TDiacompClient.UploadKoofs(const Data: string): boolean;
{==============================================================================}
var
  Par: TParamList;
  Resp: string;
begin
  SetLength(Par, 2);
  par[0] := 'koofs:upload=';
  par[1] := 'data=' + Data;

  Resp := DoPostSmart(FServer + PAGE_CONSOLE, Par);
  Result := (Resp = MESSAGE_DONE)
end;

{==============================================================================}
function TDiacompClient.Report(const Msg: string): boolean;
{==============================================================================}
var
  Par: TParamList;
  Resp: string;
begin
  SetLength(Par, 2);
  par[0] := 'report=';
  par[1] := 'msg=' + Msg;

  Resp := DoPostSmart(FServer + PAGE_CONSOLE, Par);
  Result := (Resp = MESSAGE_DONE);
end;

{==============================================================================}
function TDiacompClient.Search(const Key: string): string;
{==============================================================================}
begin
  Result := DoGetSmart(FServer + PAGE_CONSOLE + '?foodbase:search&q=' + Key);
end;

initialization
  // 1992-04-02 09:45:00
  GetLocaleFormatSettings(GetThreadLocale, WebFmt);
  WebFmt.DateSeparator := '-';
  WebFmt.TimeSeparator := ':';
  WebFmt.ShortDateFormat := 'yyyy-mm-dd';
  WebFmt.LongTimeFormat := 'hh:nn:ss';
end.

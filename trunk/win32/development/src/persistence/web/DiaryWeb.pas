// TODO: rename to WebClient
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
  DiaryRecords,
  DiaryDAO, // TModList // TODO: remove
  uLkJSON,
  AutoLog;

type
  ECommonException     = class (Exception);
  EConnectionException = class (ECommonException);   // Ошибка подключения
  EFormatException     = class (ECommonException);   // Ошибка формата данных
  EAPIException        = class (EFormatException);   // Ошибка версии API
  EAuthException       = class (ECommonException);   // Ошибка авторизации

  TParamList = array of string;

  TStdResponse = class
  private
    FCode: integer;
    FResponse: string;
  public
    constructor Create(const S: string);
    function ConvertResponseToJson(): TlkJSONbase;
    property Code: integer read FCode write FCode;
    property Response: string read FResponse write FResponse;
  end;

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
    function DoPut(const URL: string; const Par: TParamList): string;
  public
    function DoGetSmart(const URL: string): string;
    function DoPostSmart(const URL: string; const Par: TParamList): string;
    function DoPutSmart(const URL: string; const Par: TParamList): string;

    // TODO: sort
    constructor Create;
    destructor Destroy; override;

    procedure Login();
    procedure Logout;
    procedure SetTimeout(Timeout: integer);

    function ConvertUTCToLocal(Time: TDateTime): TDateTime;
    function ConvertLocalToUTC(Time: TDateTime): TDateTime;

    function GetApiURL(): string;

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

{type
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
  end;   }

const
  CURRENT_API_VERSION  = '1.2';

  //PAGE_LOGIN   = 'login.php';
  //PAGE_CONSOLE = 'console.php';

  //MESSAGE_DONE            = 'DONE';
  //MESSAGE_FAIL            = 'FAIL';
  //MESSAGE_FAIL_AUTH       = MESSAGE_FAIL + '|BADNAME';
  //MESSAGE_FAIL_APIVERSION = MESSAGE_FAIL + '|DEPAPI';
  //MESSAGE_ONLINE          = 'online';
  //MESSAGE_OFFLINE         = 'offline';
  //MESSAGE_UNAUTH          = 'Error: log in first';

{ TStdResponse }

{==============================================================================}
function TStdResponse.ConvertResponseToJson(): TlkJSONbase;
{==============================================================================}
var
  json: TlkJSONobject;
  s: string;
begin
  s := ReplaceAll(FResponse, '\"', '"');

  Result := TlkJSON.ParseText(S);

  if Assigned(Result) then
  begin
    // ok, great.
  end else
  begin
    raise Exception.Create('Invalid JSON: ' + S);
  end;
end;

{==============================================================================}
constructor TStdResponse.Create(const S: string);
{==============================================================================}
var
  json: TlkJSONobject;
begin
  json := TlkJSON.ParseText(S) as TlkJSONobject;

  if Assigned(json) then
  begin
    FCode := (json['code'] as TlkJSONnumber).Value;
    FResponse := (json['resp'] as TlkJSONstring).Value;
  end else
  begin
    raise Exception.Create('Invalid JSON: ' + S);
  end;
end;

{ TDiacompClient }

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

  // TODO: constants
  if (TStdResponse.Create(Result).Code = 401) then
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

  // TODO: constants
  if (TStdResponse.Create(Result).Code = 401) then
    raise EAuthException.Create('Необходима авторизация');
end;

{==============================================================================}
function TDiacompClient.DoPut(const URL: string; const Par: TParamList): string;
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
  {#}Log(VERBOUS, 'TDiacompClient.DoPut("' + URL + '"), ' + PrintParams());

  Data := TStringList.Create;
  try
    // формируем тело запроса
    for i := Low(Par) to High(Par) do
      Data.Add(Par[i]);

    {#} Tick := GetTickCount();
    Result := FHTTP.Put(URL, TStringStream.Create(Data.Text));
    {#}Log(VERBOUS, Format('TDiacompClient.DoPut(): responsed in %d msec: "%s"', [GetTickCount - Tick, Result]));
  finally
    //FHTTP.Disconnect;
    Data.Free;
  end;

  // TODO: constants
  if (TStdResponse.Create(Result).Code = 401) then
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
      Wait(1200);
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
      Wait(1200);
      Result := DoPost(URL, Par);
    end;
  end;
end;

{==============================================================================}
function TDiacompClient.DoPutSmart(const URL: string; const Par: TParamList): string;
{==============================================================================}
begin
  try
    Result := DoPut(URL, Par);
  except
    on E: EAuthException do
    begin
      Login();
      Result := DoPut(URL, Par); // здесь не ловим возможное исключение отсутствия авторизации
    end;

    on SocketError: EIdSocketError do
    begin
      FHTTP.Disconnect;
      Wait(1200);
      Result := DoPut(URL, Par);
    end;
  end;
end;

{==============================================================================}
procedure TDiacompClient.Login();
{==============================================================================}
var
  Query: string;
  Par: TParamList;
  ServerTime: TDateTime;
  S: string;
  Response: TStdResponse;
  Res, Desc: string;
  SendedTime: TDateTime;
begin
  Query := GetApiURL + 'auth/login/';

  SetLength(Par, 4);
  par[0] := 'login=' + FUsername;
  par[1] := 'pass=' + FPassword;
  par[2] := 'api=' + CURRENT_API_VERSION;
  {#}Log(VERBOUS, 'TDiacompClient.Login(): quering ' + Query);

  SendedTime := GetTimeUTC();
  S := DoPost(Query, par);
  {#}Log(VERBOUS, 'TDiacompClient.Login(): quered OK, resp = "' + S + '"');

  Response := TStdResponse.Create(S);

  // TODO: constants

  case Response.Code of
    0:    {#}Log(VERBOUS, 'TDiacompClient.Login(): logged OK');
    401:  raise EAuthException.Create('Ошибка авторизации');
    4051: raise EAPIException.Create('Версия API не поддерживается');
    else  raise EFormatException.CreateFmt('Неизвестная ошибка (%d): %s', [Response.Code, Response.Response]);
  end;

  {#}Log(VERBOUS, 'TDiacompClient.Login(): done');
end;

{==============================================================================}
procedure TDiacompClient.Logout;
{==============================================================================}
begin
  DoGet(GetApiURL() + 'auth/logout/'); // Put Smart here :D
end;

{==============================================================================}
procedure TDiacompClient.SetTimeout(Timeout: integer);
{==============================================================================}
begin
  FHTTP.ReadTimeout := TimeOut;
end;

{==============================================================================}
function TDiacompClient.GetApiURL: string;
{==============================================================================}
begin
  Result := FServer + 'api/';
end;

initialization
  // 1992-04-02 09:45:00
  GetLocaleFormatSettings(GetThreadLocale, WebFmt);
  WebFmt.DateSeparator := '-';
  WebFmt.TimeSeparator := ':';
  WebFmt.ShortDateFormat := 'yyyy-mm-dd';
  WebFmt.LongTimeFormat := 'hh:nn:ss';
end.

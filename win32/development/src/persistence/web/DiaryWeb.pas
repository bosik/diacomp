// TODO: rename to WebClient
unit DiaryWeb;

{ Работа с сервером }

interface

uses
  SysUtils, // Exception
  Classes, // TStrings
  Windows, // debug: GetTickCount
  Dialogs, // debug
  IdHTTP,
  IdException,
  DiaryRoutines, // ReplaceAll()
  uLkJSON,
  AutoLog;

type
  ECommonException         = class (Exception);
  EConnectionException     = class (ECommonException);   // Ошибка подключения
  EFormatException         = class (ECommonException);   // Ошибка формата данных
  EAPIException            = class (EFormatException);   // Ошибка версии API
  EBadCredentialsException = class (ECommonException);   // Wrong username/password
  ENotAuthorizedException  = class (ECommonException);   // Must be authorized to perform
  EInternalServerException = class (ECommonException);   // Internal server error

  TParamList = array of string;

  TStdResponse = class
  private
    FCode: integer;
    FResponse: string;
    function ConvertResponseToJson(): TlkJSONbase;
  public
    constructor Create(const S: string);
    function Encode(): string;
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

    function DoGet(URL: string; Autocheck: boolean = True): TStdResponse;
    function DoPost(URL: string; const Par: TParamList; Autocheck: boolean = True): TStdResponse;
    function DoPut(URL: string; const Par: TParamList; Autocheck: boolean = True): TStdResponse;

    //procedure PrintProtocolVersion(const Msg: string = 'TDiacompClient');
  public
    procedure CheckResponse(const Response: TStdResponse);

    function DoGetSmart(const URL: string): TStdResponse;
    function DoPostSmart(const URL: string; const Par: TParamList): TStdResponse;
    function DoPutSmart(const URL: string; const Par: TParamList): TStdResponse;

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
    property Online: boolean read FOnline;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Server: string read FServer write FServer;
  end;

var
  WebFmt: TFormatSettings;

implementation

const
  CURRENT_API_VERSION               = '20';

  STATUS_OK                         = 0;
  STATUS_NOT_FOUND                  = 404;
  STATUS_DEPRECATED_STILL_SUPPORTED = 4050;
  STATUS_DEPERECATED                = 4051;
  STATUS_BAD_CREDENTIALS            = 4010;
  STATUS_NOT_AUTHORIZED             = 4011;
  STATUS_SERVER_ERROR               = 500;

  TIME_RECONNECT                    = 1200;

  function PrintParams(const Par: TParamList): string;
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

{ TStdResponse }

{==============================================================================}
function TStdResponse.ConvertResponseToJson(): TlkJSONbase;
{==============================================================================}
{var
  s: string; }
begin
 { s := FResponse;
  //s := ReplaceAll(s, '\"', '"');
  //s := ReplaceAll(s, '\\', '\');

  Result := TlkJSON.ParseText(S);

  if Assigned(Result) then
  begin
    // ok, great.
  end else
  begin
    raise Exception.Create('Invalid JSON: ' + S);
  end;    }
end;

{==============================================================================}
function TStdResponse.Encode: string;
{==============================================================================}
var
  json: TlkJSONobject;
begin
  json := TlkJSONobject.Create();
  try
    json.Add('code', FCode);
    json.Add('resp', FResponse); // wrong
    Result := TlkJSON.GenerateText(json);
  finally
    json.Free;
  end;
end;

{==============================================================================}
constructor TStdResponse.Create(const S: string);
{==============================================================================}
var
  base: TlkJSONbase;
  json: TlkJSONobject;
begin
  base := TlkJSON.ParseText(S);

  if Assigned(base) then
  try
    json := base as TlkJSONobject;
    FCode := (json['code'] as TlkJSONnumber).Value;
    FResponse := TlkJSON.GenerateText(json['resp']);
  finally
    base.Free;
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
  FHTTP.HandleRedirects := True;
  FHTTP.ProtocolVersion := pv1_1;
  FHTTP.Request.ContentEncoding := 'UTF-8';
  // {#}PrintProtocolVersion('TDiacompClient.Create: ');
end;

{==============================================================================}
destructor TDiacompClient.Destroy;
{==============================================================================}
begin
  FHTTP.Free;
end;

{==============================================================================}
procedure TDiacompClient.CheckResponse(const Response: TStdResponse);
{==============================================================================}
begin
  case Response.Code of
    STATUS_OK:                         ; // life is good
    STATUS_NOT_FOUND:                  ; // something is not found
    STATUS_DEPRECATED_STILL_SUPPORTED: ; // API is about to be deprecated; ignore
    STATUS_DEPERECATED:     raise EAPIException.Create('Deprecated API version');
    STATUS_BAD_CREDENTIALS: raise EBadCredentialsException.Create('Bad credentials');
    STATUS_NOT_AUTHORIZED:  raise ENotAuthorizedException.Create('Not authorized');
    STATUS_SERVER_ERROR:    raise EInternalServerException.Create('Internal server error');
    else                    raise EFormatException.CreateFmt('Unknown response; code: %d, message: %s', [Response.Code, Response.Response])
  end;
end;

{==============================================================================}
function TDiacompClient.DoGet(URL: string; Autocheck: boolean): TStdResponse;
{==============================================================================}
var
  Tick: cardinal;
  S: string;
begin
  URL := ChkSpace(URL);
  FHTTP.ProtocolVersion := pv1_1;
  {#}Log(VERBOUS, 'TDiacompClient.DoGet("' + URL + '")');
  // {#}PrintProtocolVersion('TDiacompClient.DoGet.1');
  {#} Tick := GetTickCount();
  S := FHTTP.Get(URL);
  {#}Log(VERBOUS, Format('TDiacompClient.DoGet(): responsed in %d msec: "%s"', [GetTickCount - Tick, S]));

  Result := TStdResponse.Create(S);
  if (Autocheck) then
    CheckResponse(Result);
  // {#}PrintProtocolVersion('TDiacompClient.DoGet.2');
end;

{==============================================================================}
function TDiacompClient.DoPost(URL: string; const Par: TParamList; Autocheck: boolean): TStdResponse;
{==============================================================================}
var
  Data: TStrings;
  i: integer;
  Tick: cardinal;
  S: string;
begin
  URL := ChkSpace(URL);
  FHTTP.ProtocolVersion := pv1_1;
  {#}Log(VERBOUS, 'TDiacompClient.DoPost("' + URL + '"), ' + PrintParams(Par));
  // {#}PrintProtocolVersion('TDiacompClient.DoPost.1');

  Data := TStringList.Create;
  try
    // формируем тело запроса
    for i := Low(Par) to High(Par) do
      Data.Add(Par[i]);

    {#} Tick := GetTickCount();
    S := FHTTP.Post(URL, Data);
    {#}Log(VERBOUS, Format('TDiacompClient.DoPost(): responsed in %d msec: "%s"', [GetTickCount - Tick, S]));

    Result := TStdResponse.Create(S);
    if (Autocheck) then
      CheckResponse(Result);
  finally
    //FHTTP.Disconnect;
    // {#}PrintProtocolVersion('TDiacompClient.DoPost.2');
    Data.Free;
  end;
end;

{==============================================================================}
function TDiacompClient.DoPut(URL: string; const Par: TParamList; Autocheck: boolean): TStdResponse;
{==============================================================================}
var
  Data: TStrings;
  i: integer;
  Tick: cardinal;
  Req: string;
  S: string;
begin
  URL := ChkSpace(URL);
  FHTTP.ProtocolVersion := pv1_1;
  {#}Log(VERBOUS, 'TDiacompClient.DoPut("' + URL + '"), ' + PrintParams(Par));
  // {#}PrintProtocolVersion('TDiacompClient.DoPut.1');

  Data := TStringList.Create;
  try
    // формируем тело запроса
    for i := Low(Par) to High(Par) do
      Data.Add(Par[i]);

    {#} Tick := GetTickCount();
    Req := Trim(Data.Text);
    Req := ReplaceAll(Req, '%', '%25');

    S := FHTTP.Put(URL, TStringStream.Create(Req));

    {#}Log(VERBOUS, Format('TDiacompClient.DoPut(): responsed in %d msec: "%s"', [GetTickCount - Tick, S]));

    Result := TStdResponse.Create(S);
    if (Autocheck) then
      CheckResponse(Result);
  finally
    //FHTTP.Disconnect;
    Data.Free;
    // {#}PrintProtocolVersion('TDiacompClient.DoPut.2');
  end;
end;

{==============================================================================}
function TDiacompClient.DoGetSmart(const URL: string): TStdResponse;
{==============================================================================}
begin
  try
    Result := DoGet(URL, False);
    // TODO: implement POST / PUT in this manner
    if (Result.Code = STATUS_NOT_AUTHORIZED) then
    begin
      Log(INFO, 'TDiacompClient.DoGetSmart(): Session expired, re-login');
      Login();
      Result := DoGet(URL); // здесь не ловим возможное исключение отсутствия авторизации
    end;
  except
    //on E: ENotAuthorizedException do
    on SocketError: EIdSocketError do
    begin
      Log(ERROR, 'TDiacompClient.DoGetSmart(): Waiting for timeout due to SocketException occured: ' + SocketError.Message);
      // {#}PrintProtocolVersion('TDiacompClient.DoGetSmart().1');
      FHTTP.Disconnect;
      // {#}PrintProtocolVersion('TDiacompClient.DoGetSmart().2');
      Wait(TIME_RECONNECT);
      Result := DoGet(URL);
    end;
  end;
end;

{==============================================================================}
function TDiacompClient.DoPostSmart(const URL: string; const Par: TParamList): TStdResponse;
{==============================================================================}
begin
  try
    Result := DoPost(URL, Par);
  except
    on E: ENotAuthorizedException do
    begin
      Log(INFO, 'TDiacompClient.DoPostSmart(): Session expired, re-login');
      Login();
      Result := DoPost(URL, Par); // здесь не ловим возможное исключение отсутствия авторизации
    end;

    on SocketError: EIdSocketError do
    begin
      Log(ERROR, 'TDiacompClient.DoPostSmart(): Waiting for timeout due to SocketException occured: ' + SocketError.Message);
      // {#}PrintProtocolVersion('TDiacompClient.DoPostSmart().1');
      FHTTP.Disconnect;
      // {#}PrintProtocolVersion('TDiacompClient.DoPostSmart().2');
      Wait(TIME_RECONNECT);
      Result := DoPost(URL, Par);
    end;
  end;
end;

{==============================================================================}
function TDiacompClient.DoPutSmart(const URL: string; const Par: TParamList): TStdResponse;
{==============================================================================}
begin
  try
    Result := DoPut(URL, Par);
  except
    on E: ENotAuthorizedException do
    begin
      Log(INFO, 'TDiacompClient.DoPutSmart(): Session expired, re-login');
      Login();
      Result := DoPut(URL, Par); // здесь не ловим возможное исключение отсутствия авторизации
    end;

    on SocketError: EIdSocketError do
    begin
      Log(ERROR, 'TDiacompClient.DoPutSmart(): Waiting for timeout due to SocketException occured: ' + SocketError.Message);
      // {#}PrintProtocolVersion('TDiacompClient.DoPutSmart().1');
      FHTTP.Disconnect;
      // {#}PrintProtocolVersion('TDiacompClient.DoPutSmart().2');
      Wait(TIME_RECONNECT);
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
  Response: TStdResponse;
begin
  Query := GetApiURL + 'auth/login/';

  SetLength(Par, 3);
  par[0] := 'login=' + FUsername;
  par[1] := 'pass=' + FPassword;
  par[2] := 'api=' + CURRENT_API_VERSION;
  {#}Log(VERBOUS, 'TDiacompClient.Login(): quering ' + Query);

  Response := DoPost(Query, par);
  {#}Log(VERBOUS, 'TDiacompClient.Login(): quered OK, resp = "' + Response.Encode() + '"');

  CheckResponse(Response);

  if (Response.Code = STATUS_OK) then
    {#}Log(VERBOUS, 'TDiacompClient.Login(): logged OK');

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

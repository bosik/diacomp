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

  TStdResponse = class
  private
    FCode: integer;
    FResponse: string;
    function ConvertResponseToJson(): TlkJSONbase;
  public
    constructor Create(const S: string); overload; deprecated;
    constructor Create(const Status: integer; const Response: string); overload;
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

    procedure InitHttp();

    function DoGet(URL: string; Autocheck: boolean = True): TStdResponse;
    function DoPost(URL: string; const Par: TStringArray; Autocheck: boolean = True): TStdResponse;
    function DoPut(URL: string; const Par: TStringArray; Autocheck: boolean = True): TStdResponse;

    //procedure PrintProtocolVersion(const Msg: string = 'TDiacompClient');
  public
    procedure CheckResponse(const Response: TStdResponse);

    function DoGetSmart(const URL: string): TStdResponse;
    function DoPostSmart(const URL: string; const Par: TStringArray): TStdResponse;
    function DoPutSmart(const URL: string; const Par: TStringArray): TStdResponse;

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

const
  CURRENT_API_VERSION               = '20';

  STATUS_OK                         = 200;
  STATUS_NOT_FOUND                  = 404;
  //STATUS_DEPRECATED_STILL_SUPPORTED = 4050;
  //STATUS_DEPERECATED                = 4051;
  STATUS_BAD_CREDENTIALS            = 401;
  STATUS_NOT_AUTHORIZED             = 401;
  STATUS_SERVER_ERROR               = 500;
  TIME_RECONNECT                    = 1200;

implementation

  function PrintParams(const Par: TStringArray): string;
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

{======================================================================================================================}
function TStdResponse.ConvertResponseToJson(): TlkJSONbase;
{======================================================================================================================}
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

{======================================================================================================================}
function TStdResponse.Encode: string;
{======================================================================================================================}
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

{======================================================================================================================}
constructor TStdResponse.Create(const S: string);
{======================================================================================================================}
var
  base: TlkJSONbase;
  json: TlkJSONobject;
begin
  base := TlkJSON.ParseText(S);

  if Assigned(base) then
  try
    json := base as TlkJSONobject;
    FCode := (json['code'] as TlkJSONnumber).Value;

    case json['resp'].SelfType of
      jsString:  FResponse := (json['resp'] as TlkJSONstring).Value;
      jsNumber:  FResponse := IntToStr((json['resp'] as TlkJSONnumber).Value);
      else FResponse := TlkJSON.GenerateText(json['resp']);
    end;

  finally
    base.Free;
  end else
  begin
    raise Exception.Create('Invalid JSON: ' + S);
  end;
end;

{======================================================================================================================}
constructor TStdResponse.Create(const Status: integer; const Response: string);
{======================================================================================================================}
begin
  FCode := Status;
  FResponse := Response;
end;

{ TDiacompClient }

{======================================================================================================================}
function TDiacompClient.ConvertUTCToLocal(Time: TDateTime): TDateTime;
{======================================================================================================================}
begin
  if (not FOnline) then Login();
  Result := Time + FTimeShift;
end;

{======================================================================================================================}
function TDiacompClient.ConvertLocalToUTC(Time: TDateTime): TDateTime;
{======================================================================================================================}
begin
  if (not FOnline) then Login();
  Result := Time - FTimeShift;
end;

{======================================================================================================================}
constructor TDiacompClient.Create;
{======================================================================================================================}
begin
  FOnline := False;
  FTimeShift := 0;
  InitHttp();
  // {#}PrintProtocolVersion('TDiacompClient.Create: ');
end;

{======================================================================================================================}
destructor TDiacompClient.Destroy();
{======================================================================================================================}
begin
  FHTTP.Free;
end;

{======================================================================================================================}
procedure TDiacompClient.InitHttp();
{======================================================================================================================}
begin
  FHTTP := TIdHTTP.Create(nil);
  FHTTP.HandleRedirects := True;
  FHTTP.ProtocolVersion := pv1_1;
  FHTTP.Request.ContentEncoding := 'UTF-8';
  FHTTP.Request.BasicAuthentication := True; // this prevents AV somehow
end;

{======================================================================================================================}
procedure TDiacompClient.CheckResponse(const Response: TStdResponse);
{======================================================================================================================}
begin
  case Response.Code of
    STATUS_OK:                         ; // life is good
    STATUS_NOT_FOUND:                  ; // something is not found
    //STATUS_DEPRECATED_STILL_SUPPORTED: ; // API is about to be deprecated; ignore
    //STATUS_DEPERECATED:     raise EAPIException.Create('Deprecated API version');
    //STATUS_BAD_CREDENTIALS: raise EBadCredentialsException.Create('Bad credentials');
    STATUS_NOT_AUTHORIZED:  raise ENotAuthorizedException.Create('Not authorized');
    STATUS_SERVER_ERROR:    raise EInternalServerException.Create('Internal server error');
    else                    raise EFormatException.CreateFmt('Unknown response; code: %d, message: %s', [Response.Code, Response.Response])
  end;
end;

{======================================================================================================================}
function TDiacompClient.DoGet(URL: string; Autocheck: boolean): TStdResponse;
{======================================================================================================================}
var
  Tick: cardinal;
  S: string;
begin
  {#} Tick := GetTickCount();

  URL := ChkSpace(URL);
  FHTTP.ProtocolVersion := pv1_1;

  {#}Log(VERBOUS, 'TDiacompClient.DoGet("' + URL + '")');
  try
    S := FHTTP.Get(URL);
  except
    on e: EIdSocketError do
    begin
      try
        (**)Log(VERBOUS, 'TDiacompClient.DoGet("' + URL + '") failed due to socket error, trying again...');
        FHTTP.Free;
        InitHttp();
        S := FHTTP.Get(URL);
      except
        on e: Exception do
          Log(VERBOUS, 'TDiacompClient.DoGet("' + URL + '") still failing: ' + e.Message);
      end;
    end;

    on e: EIdReadTimeout do
    begin
      try
        (**)Log(VERBOUS, 'TDiacompClient.DoGet("' + URL + '") failed due to timeout, trying again...');
        FHTTP.Free;
        InitHttp();
        S := FHTTP.Get(URL);
      except
        on e: Exception do
          Log(VERBOUS, 'TDiacompClient.DoGet("' + URL + '") still failing: ' + e.Message);
      end;
    end;

    on e: Exception do;
    // Error handling is done via response code
  end;

  Result := TStdResponse.Create(FHTTP.ResponseCode, S);
  {#}Log(VERBOUS, Format('TDiacompClient.DoGet(): responsed in %d msec: "[%d] %s"', [GetTickCount - Tick, Result.Code, Result.Response]));
  if (Autocheck) then
    CheckResponse(Result);
end;

{======================================================================================================================}
function TDiacompClient.DoPost(URL: string; const Par: TStringArray; Autocheck: boolean): TStdResponse;
{======================================================================================================================}
var
  Data: TStrings;
  i: integer;
  Tick: cardinal;
  S: string;
begin
  {#} Tick := GetTickCount();

  URL := ChkSpace(URL);
  FHTTP.ProtocolVersion := pv1_1;
  {#}Log(VERBOUS, 'TDiacompClient.DoPost("' + URL + '"), ' + PrintParams(Par));

  Data := TStringList.Create;
  try
    for i := Low(Par) to High(Par) do
      Data.Add(ReplaceAll(Par[i], '%', '%25'));

    try
      S := FHTTP.Post(URL, Data);
    except
      // Error handling is done via response code
    end;

    Result := TStdResponse.Create(FHTTP.ResponseCode, S);

    {#}Log(VERBOUS, Format('TDiacompClient.DoPost(): responsed in %d msec: "[%d] %s"', [GetTickCount - Tick, Result.Code, Result.Response]));
    if (Autocheck) then
      CheckResponse(Result);
  finally
    //FHTTP.Disconnect;
    Data.Free;
  end;
end;

{======================================================================================================================}
function TDiacompClient.DoPut(URL: string; const Par: TStringArray; Autocheck: boolean): TStdResponse;
{======================================================================================================================}
var
  Data: TStrings;
  Stream: TStream;
  i: integer;
  Tick: cardinal;
  S: string;
begin
  {#} Tick := GetTickCount();

  URL := ChkSpace(URL);
  FHTTP.ProtocolVersion := pv1_1;
  {#}Log(VERBOUS, 'TDiacompClient.DoPut("' + URL + '"), ' + PrintParams(Par));

  // формируем тело запроса
  Data := TStringList.Create;
  for i := Low(Par) to High(Par) do
    Data.Add(ReplaceAll(Par[i], '%', '%25'));

  Stream := TStringStream.Create(Trim(Data.Text));
  try
    try
      S := FHTTP.Put(URL, Stream);
    except
      // Error handling is done via response code
    end;

    Result := TStdResponse.Create(FHTTP.ResponseCode, S);
    {#}Log(VERBOUS, Format('TDiacompClient.DoPut(): responsed in %d msec: "[%d] %s"', [GetTickCount - Tick, Result.Code, Result.Response]));
    if (Autocheck) then
      CheckResponse(Result);
  finally
    //FHTTP.Disconnect;
    Stream.Free;
    Data.Free;
  end;
end;

{======================================================================================================================}
function TDiacompClient.DoGetSmart(const URL: string): TStdResponse;
{======================================================================================================================}
begin
  try
    Result := DoGet(URL, False);
    // TODO: implement POST / PUT in this manner
    if (Result.Code = STATUS_NOT_AUTHORIZED) then
    begin
      Log(INFO, 'TDiacompClient.DoGetSmart(): Session expired, re-login');
      Login();
      Result := DoGet(URL); // здесь не ловим возможное исключение отсутствия авторизации
    end else
    if (Result.Code = STATUS_SERVER_ERROR) then
    begin
      raise Exception.Create('500 Server Error');
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

{======================================================================================================================}
function TDiacompClient.DoPostSmart(const URL: string; const Par: TStringArray): TStdResponse;
{======================================================================================================================}
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

{======================================================================================================================}
function TDiacompClient.DoPutSmart(const URL: string; const Par: TStringArray): TStdResponse;
{======================================================================================================================}
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

{======================================================================================================================}
procedure TDiacompClient.Login();
{======================================================================================================================}
var
  Query: string;
  Par: TStringArray;
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

  try
    CheckResponse(Response);

    if (Response.Code = STATUS_OK) then
    begin
      {#}Log(VERBOUS, 'TDiacompClient.Login(): logged OK');
      FOnline := True;
    end;

    {#}Log(VERBOUS, 'TDiacompClient.Login(): done');
  finally
    Response.Free;
  end;
end;

{======================================================================================================================}
procedure TDiacompClient.Logout;
{======================================================================================================================}
begin
  DoGet(GetApiURL() + 'auth/logout/'); // Put Smart here :D
end;

{======================================================================================================================}
procedure TDiacompClient.SetTimeout(Timeout: integer);
{======================================================================================================================}
begin
  FHTTP.ReadTimeout := TimeOut;
end;

{======================================================================================================================}
function TDiacompClient.GetApiURL: string;
{======================================================================================================================}
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

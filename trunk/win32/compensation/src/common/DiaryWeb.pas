unit DiaryWeb;

{ Работа с сервером }

interface

uses
  SysUtils, // IntToStr
  Classes, // TStrings
  Windows, // debug: GetTickCount
  Dialogs, // debug
  IdHTTP,
  DiaryRoutines, // Separate()
  DiarySources, // TModList
  uLkJSON,
  AutoLog;

type
  TLoginResult = (
    lrFailConnection,  // сервер не отвечает
    lrFailFormat,      // сервер отвечает некорректно
    lrFailAuth,        // сервер сообщает о неправильной паре "логин-пароль"
    lrFailAPIVersion,  // сервер сообщает об изменившейся версии API
    lrDone             // сервер сообщает об успешной авторизации
  );

  TParamList = array of string;

  TLoginEvent = procedure(Sender: TObject; State: TLoginResult) of object;

  TDiacompClient = class
  private
    // поля
    FHTTP: TIdHTTP;
    FServer: string;
    FUsername: string;
    FPassword: string;
    FOnline: boolean;
    FTimeShift: double;
    FLoginResult: TLoginResult;

    // события
    FOnLogin: TLoginEvent;

    function DoGet(const URL: string; out Resp: string): boolean;
    function DoPost(const URL: string; const Par: TParamList; out Resp: string): boolean;

    function DoGetSmart(const URL: string; out Resp: string): boolean;
    function DoPostSmart(const URL: string; const Par: TParamList; out Resp: string): boolean;
    function Kicked(const Answer: string): boolean;
  public
    // TODO: sort

    constructor Create;
    destructor Destroy; override;

    procedure GetModList(Time: TDateTime; out ModList: TModList);
    procedure GetVersions(const Dates: TDateList; out ModList: TModList);
    function GetPages(const Dates: TDateList; out Resp: string): boolean;
    function PostPages(const Pages: string): boolean;

    function Login(): TLoginResult;
    procedure Logout;
    procedure SetTimeout(Timeout: integer);
    procedure UpdateStatus;

    function GetFoodBaseVersion(out Version: integer): boolean;
    function DownloadFoodBase(out Data: string): boolean;
    function UploadFoodBase(const Data: string; Version: integer): boolean;

    function GetDishBaseVersion(out Version: integer): boolean;
    function DownloadDishBase(out Data: string): boolean;
    function UploadDishBase(const Data: string; Version: integer): boolean;

    function DownloadKoofs(out Data: string): boolean;
    function UploadKoofs(const Data: string): boolean;

    function LocalToServer(Time: TDateTime): TDateTime;
    function ServerToLocal(Time: TDateTime): TDateTime;

    // демо
    function Report(const Msg: string): boolean;
    function Search(const Key: string): string;

    { свойства }
    property LoginResult: TLoginResult read FLoginResult;
    property Online: boolean read FOnline;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Server: string read FServer write FServer;

    { события }
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  end;

var
  WebFmt: TFormatSettings;

implementation

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

    if not assigned(json) then
    begin
      Log(ERROR, 'JSON is not assigned');
      Exit;
    end;

    Result.Status := (json.Field['status'] as TlkJSONnumber).Value;
    Result.Message := (json.Field['message'] as TlkJSONstring).Value
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

  EXCEPTION_OFFLINE = 'Для выполнения операции необходимо авторизоваться';

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
function TDiacompClient.DoGet(const URL: string; out Resp: string): boolean;
{==============================================================================}
{#}var
{#}  Tick: cardinal;
begin
  {#} Log(VERBOUS, 'TDiacompClient.DoGet("' + URL + '")');

  Resp := '';
  try
    try
      {#} Tick := GetTickCount();
      Resp := FHTTP.Get(URL);
      {#} Log(VERBOUS, 'TDiacompClient.DoGet(): time is ' + IntToStr(GetTickCount - Tick) + ' msec');
      {#} Log(VERBOUS, 'TDiacompClient.DoGet(): Resp = "' + Resp + '"');
      Result := True;
    finally
      FHTTP.Disconnect;
    end;
  except
    on ESE: Exception do
    begin
      {#} Log(ERROR, 'TDiacompClient.DoGet(): EXCEPTION! ' +  ESE.Message);
      Result := False;
    end;
  end;
end;

{==============================================================================}
function TDiacompClient.DoPost(const URL: string; const Par: TParamList; out Resp: string): boolean;
{==============================================================================}

  function PrintParams: string;
  var
    i: integer;
  begin
    Result := '';
    for i := Low(Par) to High(Par) do
      Result := Result + Par[i] + '&';
  end;

var
  Data: TStrings;
  i: integer;
{#}  Tick: cardinal;
begin
  {#} Log(VERBOUS, 'TDiacompClient.DoPost("' + URL + '"), ' + PrintParams());

  Resp := ''; 
  try
    Data := TStringList.Create;
    try
      // формируем тело запроса
      for i := Low(Par) to High(Par) do
        Data.Add(Par[i]);

      {#} Tick := GetTickCount();
      Resp := FHTTP.Post(URL, Data);
      {#} Log(VERBOUS, 'TDiacompClient.DoPost(): time is ' + IntToStr(GetTickCount - Tick) + ' msec');
      {#} Log(VERBOUS, 'TDiacompClient.DoPost(): Resp = "' + Resp + '"');
      Result := True;
    finally
      FHTTP.Disconnect;
      Data.Free;
    end;
  except
    on ESE: Exception do
    begin
      {#} Log(ERROR, 'TDiacompClient.DoPost(): EXCEPTION! ' +  ESE.Message);
      Result := False;
    end;
  end;
end;

{==============================================================================}
function TDiacompClient.DoGetSmart(const URL: string; out Resp: string): boolean;
{==============================================================================}
begin
  if DoGet(URL, Resp) then
  begin
    if Kicked(Resp) then
    begin
      // вторая попытка
      if {(Autologin) and} (Login() = lrDone) then
        Result := DoGet(URL, Resp)
      else
        Result := False;
    end else
      Result := True;
  end else
    Result := False;
end;

{==============================================================================}
function TDiacompClient.DoPostSmart(const URL: string; const Par: TParamList; out Resp: string): boolean;
{==============================================================================}
begin
  if DoPost(URL, Par, Resp) then
  begin
    if Kicked(Resp) then
    begin
      // вторая попытка
      if {(Autologin) and} (Login() = lrDone) then
        Result := DoPost(URL, Par, Resp)
      else
        Result := False;
    end else
      Result := True;
  end else
    Result := False;
end;

{==============================================================================}
procedure TDiacompClient.GetModList(Time: TDateTime; out ModList: TModList);
{==============================================================================}
var
  Query, Resp, Line: string;
  Date, Version: string;
  S: TStringList;
  i,count: integer;
begin
  Log(VERBOUS, 'TDiacompClient.GetModList(): started');
  Log(VERBOUS, 'TDiacompClient.GetModList(): Time = "' + DateTimeToStr(Time) + '"');

  if Time > 0 then
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=' + ChkSpace(DateTimeToStr(LocalToServer(Time), WebFmt))
  else
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=0';

  Log(VERBOUS, 'TDiacompClient.GetModList(): quering ' + Query);

  if DoGetSmart(Query, Resp) then
  begin
    Log(VERBOUS, 'TDiacompClient.GetModList(): quered OK, Resp = "' + Resp + '"');

    S := TStringList.Create;
    S.Text := Trim(Resp);

    SetLength(ModList, S.Count);
    count := 0;

    Log(VERBOUS, 'TDiacompClient.GetModList(): lines count = ' + IntToStr(S.Count));

    for i := 0 to S.Count - 1 do
    begin
      Line := S[i];
      if (Line <> '') then
      begin
        Log(VERBOUS, 'TDiacompClient.GetModList(): parsing line "' + Line + '"');
        Separate(Line, Date, '|', Version);

        Log(VERBOUS, 'TDiacompClient.GetModList(): date = "' + Date + '"');
        Log(VERBOUS, 'TDiacompClient.GetModList(): version = "' + Version + '"');

        ModList[Count].Date := Trunc(StrToDate(Date, WebFmt));
        ModList[Count].Version := StrToInt(Version);
        inc(Count);
      end;
    end;

    SetLength(ModList, Count);

    Log(VERBOUS, 'TDiacompClient.GetModList(): done OK');
  end else
  begin
    Log(ERROR, 'TDiacompClient.GetModList(): quering failed');
  end;
end;

{==============================================================================}
procedure TDiacompClient.GetVersions(const Dates: TDateList; out ModList: TModList);
{==============================================================================}
var
  Query, Resp, Line: string;
  Date, Version: string;
  S: TStringList;
  i,count: integer;
begin
  Log(VERBOUS, 'TDiacompClient.GetVersions(): started');

  // заглушка
  if (Length(Dates) = 0) then
  begin
    Resp := '';
    SetLength(ModList, 0);
    Exit;
  end;

  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?diary:getModList&dates=';
  for i := 0 to High(Dates) do
    Query := Query + DateToStr(Dates[i], WebFmt) + ',';


  Log(VERBOUS, 'TDiacompClient.GetVersions(): quering ' + Query);

  if DoGetSmart(Query, Resp) then
  begin
    Log(VERBOUS, 'TDiacompClient.GetVersions(): quered OK, Resp = "' + Resp + '"');

    S := TStringList.Create;
    S.Text := Trim(Resp);

    SetLength(ModList, S.Count);
    count := 0;

    Log(VERBOUS, 'TDiacompClient.GetVersions(): lines count = ' + IntToStr(S.Count));

    for i := 0 to S.Count - 1 do
    begin
      Line := S[i];
      if (Line <> '') then
      begin
        Log(VERBOUS, 'TDiacompClient.GetVersions(): parsing line "' + Line + '"');
        Separate(Line, Date, '|', Version);

        Log(VERBOUS, 'TDiacompClient.GetVersions(): date = "' + Date + '"');
        Log(VERBOUS, 'TDiacompClient.GetVersions(): version = "' + Version + '"');

        ModList[Count].Date := Trunc(StrToDate(Date, WebFmt));
        ModList[Count].Version := StrToInt(Version);
        inc(Count);
      end;
    end;

    SetLength(ModList, Count);

    Log(VERBOUS, 'TDiacompClient.GetVersions(): done OK');
  end else
  begin
    Log(ERROR, 'TDiacompClient.GetVersions(): quering failed');
  end;
end;

{==============================================================================}
function TDiacompClient.GetPages(const Dates: TDateList; out Resp: string): boolean;
{==============================================================================}
var
  Query: string;
  i: integer;
begin
  // заглушка
  if (Length(Dates) = 0) then
  begin
    Resp := '';
    Result := True;
    Exit;
  end;

  Log(VERBOUS, 'TDiacompClient.GetPages: started');

  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?diary:download&dates=';
  for i := 0 to High(Dates) do
    Query := Query + DateToStr(Dates[i], WebFmt) + ',';

  Log(VERBOUS, 'TDiacompClient.GetPages: quering "' + Query + '"');

  // отправляем
  if DoGetSmart(Query, Resp) then
  begin
    Log(VERBOUS, 'TDiacompClient.GetPages: quered OK, resp = "' + Resp + '"');
    Result := True;
  end else
  begin
    Log(ERROR, 'TDiacompClient.GetPages: quering failed');
    Result := False;
  end;
end;

{==============================================================================}
function TDiacompClient.Kicked(const Answer: string): boolean;
{==============================================================================}
begin
  Result := (Answer = MESSAGE_UNAUTH);
end;

{==============================================================================}
function TDiacompClient.LocalToServer(Time: TDateTime): TDateTime;
{==============================================================================}
begin
  if (not FOnline) then Login();
  Result := Time - FTimeShift;
end;

{==============================================================================}
function TDiacompClient.Login(): TLoginResult;
{==============================================================================}
var
  Par: TParamList;
  ServerTime: TDateTime;
  Msg: string;
  Res, Desc: string;

  SendedTime: TDateTime;

  //{#} Tick: cardinal;
begin
  //{#} Tick := GetTickCount();

  Result := lrFailConnection;  // TODO: change to common fail

  try
    SetLength(Par, 4);
    par[0] := 'login=' + FUsername;
    par[1] := 'password=' + FPassword;
    par[2] := 'api=' + CURRENT_API_VERSION;
    par[3] := 'noredir=';

    //Log('TDiacompClient.Login(): started');
    Log(VERBOUS, 'TDiacompClient.Login(): quering ' + FServer + PAGE_LOGIN);

    SendedTime := Now;
    if DoPost(FServer + PAGE_LOGIN, par, Msg) then // Not smart, it's o.k. - Hi, C.O.! :D
    begin
      Log(VERBOUS, 'TDiacompClient.Login(): quered OK, resp = "' + Msg + '"');
      //{#}Log('Login()\posting: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;

      Separate(Msg, Res, '|', Desc);

      Log(VERBOUS, 'TDiacompClient.Login(): Res = "' + Res + '"');
      Log(VERBOUS, 'TDiacompClient.Login(): Desc = "' + Desc + '"');

      //{#}Log('Login()\separating: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;

      if (Res = MESSAGE_DONE) then
      begin
        Log(VERBOUS, 'TDiacompClient.Login(): it means "DONE", parsing desc...');
        ServerTime := StrToDateTime(Desc, WebFmt);
        FTimeShift := (SendedTime + Now)/2 - ServerTime;  // pretty cool :)
        Result := lrDone;

        //{#}Log('Login()\calculating FShiftTime: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;
      end else
      // TODO: update format (w/o concatenation)
      if (Msg = MESSAGE_FAIL_AUTH) then
        Result := lrFailAuth else
      if (Msg = MESSAGE_FAIL_APIVERSION) then
        Result := lrFailAPIVersion
      else
        Result := lrFailFormat;
    end else
      Result := lrFailConnection;

    FOnline := (Result = lrDone);
    FLoginResult := Result;
    if Assigned(FOnLogin) then FOnLogin(Self, Result);
    Log(VERBOUS, 'TDiacompClient.Login(): done, FOnline = ' + BoolToStr(FOnline, True));

  except
    on ESE: Exception do
    begin
      {#} Log(ERROR, 'TDiacompClient.Login(): EXCEPTION! ' +  ESE.Message);
      ShowMessage('Exception in Login() procedure, see log file for details');
    end;
  end;

  //{#}Log('Login()\rest: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;
end;

{==============================================================================}
procedure TDiacompClient.Logout;
{==============================================================================}
var
  Resp: string;
begin
  DoGet(FServer + PAGE_LOGIN + '?logout&noredir', Resp);  // Put Smart here :D
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

  if DoPostSmart(FServer + PAGE_CONSOLE, Par, Msg) then
  begin
    Response := DecodeResponse(Msg);
    Result := (Response.Status = 0);
  end else
    Result := False;
end;

{==============================================================================}
function TDiacompClient.ServerToLocal(Time: TDateTime): TDateTime;
{==============================================================================}
begin
  if (not FOnline) then Login();
  Result := Time + FTimeShift;
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
  if DoGet(FServer + PAGE_LOGIN + '?status', Resp) then
    FOnline := (Resp = MESSAGE_ONLINE)
  else
    FOnline := False;
end;

{==============================================================================}
function TDiacompClient.DownloadFoodBase(out Data: string): boolean;
{==============================================================================}
var
  Query: string;
begin
  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?foodbase:download';
  Log(VERBOUS, 'TDiacompClient.DownloadFoodBase: quering ' + Query);

  // отправляем
  Result := DoGetSmart(Query, Data);

  if (not Result) then
    Log(ERROR, 'TDiacompClient.DownloadFoodBase: quering failed');
end;

{==============================================================================}
function TDiacompClient.GetFoodBaseVersion(out Version: integer): boolean;
{==============================================================================}
var
  Query, Resp: string;
begin
  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?foodbase:getVersion';
  Log(VERBOUS, 'TDiacompClient.GetFoodBaseVersion: quering ' + Query);

  // отправляем
  Result := DoGetSmart(Query, Resp);
  Result := Result and TryStrToInt(Resp, Version);

  if (not Result) then
    Log(ERROR, 'TDiacompClient.GetFoodBaseVersion: quering failed');
end;

function TDiacompClient.UploadFoodBase(const Data: string;
  Version: integer): boolean;
var
  Par: TParamList;
  Resp: string;
begin
  SetLength(Par, 3);
  par[0] := 'foodbase:upload=';
  par[1] := 'version=' + IntToStr(Version);
  par[2] := 'data=' + Data;

  if DoPostSmart(FServer + PAGE_CONSOLE, Par, Resp) then
    Result := (Resp = MESSAGE_DONE)
  else
    Result := False;
end;

// ================================================

function TDiacompClient.DownloadDishBase(out Data: string): boolean;
var
  Query: string;
begin
  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?dishbase:download';
  Log(VERBOUS, 'TDiacompClient.DownloadDishBase: quering ' + Query);

  // отправляем
  Result := DoGetSmart(Query, Data);

  if (not Result) then
    Log(ERROR, 'TDiacompClient.DownloadDishBase: quering failed');
end;

{==============================================================================}
function TDiacompClient.GetDishBaseVersion(out Version: integer): boolean;
{==============================================================================}
var
  Query, Resp: string;
begin
  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?dishbase:getVersion';
  Log(VERBOUS, 'TDiacompClient.GetDishBaseVersion: quering ' + Query);

  // отправляем
  Result := DoGetSmart(Query, Resp);
  Result := Result and TryStrToInt(Resp, Version);

  if (not Result) then
    Log(ERROR, 'TDiacompClient.GetDishBaseVersion: quering failed');
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

  if DoPostSmart(FServer + PAGE_CONSOLE, Par, Resp) then
    Result := (Resp = MESSAGE_DONE)
  else
    Result := False;
end;

// =================================================

{==============================================================================}
function TDiacompClient.DownloadKoofs(out Data: string): boolean;
{==============================================================================}
var
  Query: string;
begin
  Query := FServer + PAGE_CONSOLE + '?koofs:download';
  Result := DoGetSmart(Query, Data);
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

  if DoPostSmart(FServer + PAGE_CONSOLE, Par, Resp) then
    Result := (Resp = MESSAGE_DONE)
  else
    Result := False;
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

  if DoPostSmart(FServer + PAGE_CONSOLE, Par, Resp) then
    Result := (Resp = MESSAGE_DONE)
  else
    Result := False;
end;

{==============================================================================}
function TDiacompClient.Search(const Key: string): string;
{==============================================================================}
var
  Resp: string;
begin
  if DoGetSmart(FServer + PAGE_CONSOLE + '?foodbase:search&q=' + Key, Resp) then
    Result := Resp
  else
    Result := 'FAILED';
end;

initialization
  // 1992-04-02 09:45:00
  GetLocaleFormatSettings(GetThreadLocale, WebFmt);
  WebFmt.DateSeparator := '-';
  WebFmt.TimeSeparator := ':';
  WebFmt.ShortDateFormat := 'yyyy-mm-dd';
  WebFmt.LongTimeFormat := 'hh:nn:ss';
end.

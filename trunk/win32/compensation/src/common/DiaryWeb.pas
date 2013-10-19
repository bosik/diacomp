unit DiaryWeb;

{ –абота с сервером }

interface

uses
  SysUtils,
  Classes,
  Windows, // debug: GetTickCount
  Dialogs, // debug
  IdHTTP,
  DiaryRoutines,
  DiarySources,
  DiaryPageSerializer {TODO: remove},
  AutoLog;

type
  TLoginResult = (
    lrFailConnection,  // сервер не отвечает
    lrFailFormat,      // сервер отвечает некорректно
    lrFailAuth,        // сервер сообщает о неправильной паре "логин-пароль"
    lrFailAPIVersion,  // сервер сообщает об изменившейс€ версии API
    lrDone             // сервер сообщает об успешной авторизации
  );

  TParamList = array of string;

  TLoginEvent = procedure(Sender: TObject; State: TLoginResult) of object;

  TDiacompClient = class
  private
    // пол€
    FHTTP: TIdHTTP;
    FServer: string;
    FUsername: string;
    FPassword: string;
    FOnline: boolean;
    FTimeShift: double;
    FLoginResult: TLoginResult;

    // событи€
    FOnLogin: TLoginEvent;

    function DoGet(const URL: string; out Resp: string): boolean;
    function DoPost(const URL: string; const Par: TParamList; out Resp: string): boolean;

    function DoGetSmart(const URL: string; out Resp: string): boolean;
    function DoPostSmart(const URL: string; const Par: TParamList; out Resp: string): boolean;
    function Kicked(const Answer: string): boolean;

    function LocalToServer(Time: TDateTime): TDateTime;
    function ServerToLocal(Time: TDateTime): TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModList(Time: TDateTime; out ModList: TModList): boolean;
    function GetPages(const Dates: TDateList; out Pages: TPageDataList): boolean;
    function PostPages(const Pages: TPageDataList): boolean;

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

    // демо
    function Report(const Msg: string): boolean;
    function Search(const Key: string): string;

    { свойства }
    property LoginResult: TLoginResult read FLoginResult;
    property Online: boolean read FOnline;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Server: string read FServer write FServer;

    { событи€ }
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  end;
  
implementation

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

  EXCEPTION_OFFLINE = 'ƒл€ выполнени€ операции необходимо авторизоватьс€';

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
  Data: TStringList;
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
      // втора€ попытка
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
      // втора€ попытка
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
function TDiacompClient.GetModList(Time: TDateTime; out ModList: TModList): boolean;
{==============================================================================}

  function ChkSpace(const S: string): string;
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

var
  Query, Resp, Line: string;
  Date, Version: string;
  S: TStringList;
  i,count: integer;
begin
  Log(VERBOUS, 'TDiacompClient.GetModList(): started');
  Log(VERBOUS, 'TDiacompClient.GetModList(): Time = "' + DateTimeToStr(Time) + '"');

  try

  if Time > 0 then
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=' + ChkSpace(DateTimeToStr(LocalToServer(Time), WebFmt))
  else
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=0';

  Log(VERBOUS, 'TDiacompClient.GetModList(): quering ' + Query);

  if DoGetSmart(Query, Resp) then
  begin
    Log(VERBOUS, 'TDiacompClient.GetModList(): quered OK');
    Log(VERBOUS, 'TDiacompClient.GetModList(): Resp = "' + Resp + '"');

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

    {n := pos(#10, Resp);
    while n > 0 do
    begin
      if Length(ModList) = Count then
        SetLength(ModList, Length(ModList) * 2);

      buf := Copy(Resp, 1, n-1);
      k := pos('|', buf);
      ModList[Count].Date := Trunc(StrToDate(Copy(buf, 1, k-1), WebFmt));
      ModList[Count].Version := StrToInt(Copy(buf, k+1, Length(buf)-k));
      inc(Count);

      Delete(Resp, 1, n);
      n := pos(#10, Resp);
    end;   }

    SetLength(ModList, Count);
    Result := True;

    Log(VERBOUS, 'TDiacompClient.GetModList(): done OK');
  end else
  begin
    Log(ERROR, 'TDiacompClient.GetModList(): quering failed');
    Result := False;
  end;

  except
    on ESE: Exception do
    begin
      {#} Log(ERROR, 'TDiacompClient.GetModList(): EXCEPTION! ' +  ESE.Message, True);
      Result := False;
      ShowMessage('Error in GetModList(), see log file for details');
    end;    
  end;
end;

{==============================================================================}
function TDiacompClient.GetPages(const Dates: TDateList; out Pages: TPageDataList): boolean;
{==============================================================================}
var
  Query, Resp: string;
  S: TStringList;
  i: integer;
begin
  // заглушка
  if (Length(Dates) = 0) then
  begin
    SetLength(Pages, 0);
    Result := True;
    Exit;
  end;

  Log(VERBOUS, 'TDiacompClient.GetPages: started');

  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?diary:download&dates=';
  for i := 0 to High(Dates) do
    Query := Query + DateToStr(Dates[i], WebFmt) + ',';

  Log(VERBOUS, 'TDiacompClient.GetPages: quering ' + Query);

  // отправл€ем
  if DoGetSmart(Query, Resp) then
  begin
    Log(VERBOUS, 'TDiacompClient.GetPages: quered OK');
    Log(VERBOUS, 'TDiacompClient.GetPages: resp = "' + Resp + '"');

    // обрабатываем
    S := TStringList.Create;
    S.Text := Resp;
    try
      TPageSerializer.MultiRead(S, True, Pages);
      for i := 0 to High(Pages) do
        Pages[i].TimeStamp := ServerToLocal(Pages[i].TimeStamp);
    finally
      S.Free;
    end;
    Log(VERBOUS, 'TDiacompClient.GetPages: done OK');
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
function TDiacompClient.PostPages(const Pages: TPageDataList): boolean;
{==============================================================================}
var
  Par: TParamList;
  Msg: string;
  OldStamp: TDateTime;
  i: integer;
  Source: string;
begin
  // заглушка
  if (Length(Pages) = 0) then
  begin
    Result := True;
    Exit;
  end;

  // пустые страницы отсекать нельз€, ведь они могли быть непустыми до этого
  Source := '';
  for i := 0 to High(Pages) do
  begin
    {#}OldStamp := Pages[i].TimeStamp;
    Pages[i].TimeStamp := LocalToServer(Pages[i].TimeStamp);
    Source := Source + Pages[i].Write(True) + #13;
    {#}Pages[i].TimeStamp := OldStamp;
  end;

  SetLength(Par, 2);
  par[0] := 'diary:upload=';
  par[1] := 'pages=' + Source;

  if DoPostSmart(FServer + PAGE_CONSOLE, Par, Msg) then
    Result := (Msg = MESSAGE_DONE)
  else
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

  // отправл€ем
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

  // отправл€ем
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

  // отправл€ем
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

  // отправл€ем
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

end.

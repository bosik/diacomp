unit DiaryWeb;

{ –абота с сервером }

interface

uses
  SysUtils,
  Classes,
  Windows, // debug: GetTickCount
  Dialogs, // debug
  IdHTTP, DiaryRoutines, DiarySources,
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

  // TODO: separate Web client / Web source

  TDiaryWebSource = class (IDiarySource)
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

    { интерфейс }
    {#}function GetModList(Time: TDateTime; out ModList: TModList): boolean; override;
    {#}function GetPages(const Dates: TDateList; out Pages: TPageDataList): boolean; override;
    {#}function PostPages(const Pages: TPageDataList): boolean; override;

    { специализаци€ }

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
    //property Autologin: boolean read FAutologin write FAutologin;
    property LoginResult: TLoginResult read FLoginResult;
    property Online: boolean read FOnline;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Server: string read FServer write FServer;

    { событи€ }
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  end;

 (* TWebDiary = class
  private
    FCache: array of TWebDiaryPage;
    FCapacity: integer;
    FRealSize: integer;
    FCursor: integer;
    {web}FLogged: boolean;
  protected
    procedure Add(Page: TWebDiaryPage);
    function FindInCache(Date: TDateTime): TWebDiaryPage; // или TDiaryPage ?
  public
    constructor Create(Capacity: integer = 16);
    destructor Destroy; override;
    function GetPage(Date: TDateTime; ForceReload: boolean = False): TWebDiaryPage;
    {web}function Login(const Name, Password: string): boolean;
    {web}procedure Logout;
    function PostModified(Limit: integer = 0): integer;
    procedure ReloadCash;

    {web}property Logged: boolean read FLogged;
   end;   *)
  
implementation

{ TDiaryWebSource }

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
constructor TDiaryWebSource.Create;
{==============================================================================}
begin
  FOnline := False;
  FTimeShift := 0;
  FHTTP := TIdHTTP.Create(nil);
end;

{==============================================================================}
destructor TDiaryWebSource.Destroy;
{==============================================================================}
begin
  FHTTP.Free;
end;

{==============================================================================}
function TDiaryWebSource.DoGet(const URL: string; out Resp: string): boolean;
{==============================================================================}
{#}var
{#}  Tick: cardinal;
begin
  {#} Log(VERBOUS, 'TDiaryWebSource.DoGet("' + URL + '")');

  Resp := '';
  try
    try
      {#} Tick := GetTickCount();
      Resp := FHTTP.Get(URL);
      {#} Log(VERBOUS, 'TDiaryWebSource.DoGet(): time is ' + IntToStr(GetTickCount - Tick) + ' msec');
      {#} Log(VERBOUS, 'TDiaryWebSource.DoGet(): Resp = "' + Resp + '"');
      Result := True;
    finally
      FHTTP.Disconnect;
    end;
  except
    on ESE: Exception do
    begin
      {#} Log(ERROR, 'TDiaryWebSource.DoGet(): EXCEPTION! ' +  ESE.Message);
      Result := False;
    end;
  end;
end;

{==============================================================================}
function TDiaryWebSource.DoPost(const URL: string; const Par: TParamList; out Resp: string): boolean;
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
  {#} Log(VERBOUS, 'TDiaryWebSource.DoPost("' + URL + '"), ' + PrintParams());

  Resp := ''; 
  try
    Data := TStringList.Create;
    try
      // формируем тело запроса
      for i := Low(Par) to High(Par) do
        Data.Add(Par[i]);

      {#} Tick := GetTickCount();
      Resp := FHTTP.Post(URL, Data);
      {#} Log(VERBOUS, 'TDiaryWebSource.DoPost(): time is ' + IntToStr(GetTickCount - Tick) + ' msec');
      {#} Log(VERBOUS, 'TDiaryWebSource.DoPost(): Resp = "' + Resp + '"');
      Result := True;
    finally
      FHTTP.Disconnect;
      Data.Free;
    end;
  except
    on ESE: Exception do
    begin
      {#} Log(ERROR, 'TDiaryWebSource.DoPost(): EXCEPTION! ' +  ESE.Message);
      Result := False;
    end;
  end;
end;

{==============================================================================}
function TDiaryWebSource.DoGetSmart(const URL: string; out Resp: string): boolean;
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
function TDiaryWebSource.DoPostSmart(const URL: string; const Par: TParamList; out Resp: string): boolean;
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
function TDiaryWebSource.GetModList(Time: TDateTime; out ModList: TModList): boolean;
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
  Log(VERBOUS, 'TDiaryWebSource.GetModList(): started');
  Log(VERBOUS, 'TDiaryWebSource.GetModList(): Time = "' + DateTimeToStr(Time) + '"');

  try

  if Time > 0 then
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=' + ChkSpace(DateTimeToStr(LocalToServer(Time), WebFmt))
  else
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=0';

  Log(VERBOUS, 'TDiaryWebSource.GetModList(): quering ' + Query);

  if DoGetSmart(Query, Resp) then
  begin
    Log(VERBOUS, 'TDiaryWebSource.GetModList(): quered OK');
    Log(VERBOUS, 'TDiaryWebSource.GetModList(): Resp = "' + Resp + '"');

    S := TStringList.Create;
    S.Text := Trim(Resp);

    SetLength(ModList, S.Count);
    count := 0;

    Log(VERBOUS, 'TDiaryWebSource.GetModList(): lines count = ' + IntToStr(S.Count));

    for i := 0 to S.Count - 1 do
    begin
      Line := S[i];
      if (Line <> '') then
      begin
        Log(VERBOUS, 'TDiaryWebSource.GetModList(): parsing line "' + Line + '"');
        Separate(Line, Date, '|', Version);

        Log(VERBOUS, 'TDiaryWebSource.GetModList(): date = "' + Date + '"');
        Log(VERBOUS, 'TDiaryWebSource.GetModList(): version = "' + Version + '"');

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

    Log(VERBOUS, 'TDiaryWebSource.GetModList(): done OK');
  end else
  begin
    Log(ERROR, 'TDiaryWebSource.GetModList(): quering failed');
    Result := False;
  end;

  except
    on ESE: Exception do
    begin
      {#} Log(ERROR, 'TDiaryWebSource.GetModList(): EXCEPTION! ' +  ESE.Message, True);
      Result := False;
      ShowMessage('Error in GetModList(), see log file for details');
    end;    
  end;
end;

{==============================================================================}
function TDiaryWebSource.GetPages(const Dates: TDateList; out Pages: TPageDataList): boolean;
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

  Log(VERBOUS, 'TDiaryWebSource.GetPages: started');

  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?diary:download&dates=';
  for i := 0 to High(Dates) do
    Query := Query + DateToStr(Dates[i], WebFmt) + ',';

  Log(VERBOUS, 'TDiaryWebSource.GetPages: quering ' + Query);

  // отправл€ем
  if DoGetSmart(Query, Resp) then
  begin
    Log(VERBOUS, 'TDiaryWebSource.GetPages: quered OK');
    Log(VERBOUS, 'TDiaryWebSource.GetPages: resp = "' + Resp + '"');

    // обрабатываем
    S := TStringList.Create;
    S.Text := Resp;
    try
      TPageData.MultiRead(S, True, Pages);
      for i := 0 to High(Pages) do
        Pages[i].TimeStamp := ServerToLocal(Pages[i].TimeStamp);
    finally
      S.Free;
    end;
    Log(VERBOUS, 'TDiaryWebSource.GetPages: done OK');
    Result := True;
  end else
  begin
    Log(ERROR, 'TDiaryWebSource.GetPages: quering failed');
    Result := False;
  end;
end;

{==============================================================================}
function TDiaryWebSource.Kicked(const Answer: string): boolean;
{==============================================================================}
begin
  Result := (Answer = MESSAGE_UNAUTH);
end;

{==============================================================================}
function TDiaryWebSource.LocalToServer(Time: TDateTime): TDateTime;
{==============================================================================}
begin
  if (not FOnline) then Login();
  Result := Time - FTimeShift;
end;

{==============================================================================}
function TDiaryWebSource.Login(): TLoginResult;
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

    //Log('TDiaryWebSource.Login(): started');
    Log(VERBOUS, 'TDiaryWebSource.Login(): quering ' + FServer + PAGE_LOGIN);

    SendedTime := Now;
    if DoPost(FServer + PAGE_LOGIN, par, Msg) then // Not smart, it's o.k. - Hi, C.O.! :D
    begin
      Log(VERBOUS, 'TDiaryWebSource.Login(): quered OK, resp = "' + Msg + '"');
      //{#}Log('Login()\posting: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;

      Separate(Msg, Res, '|', Desc);

      Log(VERBOUS, 'TDiaryWebSource.Login(): Res = "' + Res + '"');
      Log(VERBOUS, 'TDiaryWebSource.Login(): Desc = "' + Desc + '"');

      //{#}Log('Login()\separating: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;

      if (Res = MESSAGE_DONE) then
      begin
        Log(VERBOUS, 'TDiaryWebSource.Login(): it means "DONE", parsing desc...');
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
    Log(VERBOUS, 'TDiaryWebSource.Login(): done, FOnline = ' + BoolToStr(FOnline, True));

  except
    on ESE: Exception do
    begin
      {#} Log(ERROR, 'TDiaryWebSource.Login(): EXCEPTION! ' +  ESE.Message);
      ShowMessage('Exception in Login() procedure, see log file for details');
    end;
  end;

  //{#}Log('Login()\rest: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;
end;

{==============================================================================}
procedure TDiaryWebSource.Logout;
{==============================================================================}
var
  Resp: string;
begin
  DoGet(FServer + PAGE_LOGIN + '?logout&noredir', Resp);  // Put Smart here :D
  FOnline := False;
end;

{==============================================================================}
function TDiaryWebSource.PostPages(const Pages: TPageDataList): boolean;
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
function TDiaryWebSource.ServerToLocal(Time: TDateTime): TDateTime;
{==============================================================================}
begin
  if (not FOnline) then Login();
  Result := Time + FTimeShift;
end;

{==============================================================================}
procedure TDiaryWebSource.SetTimeout(Timeout: integer);
{==============================================================================}
begin
  FHTTP.ReadTimeout := TimeOut;
end;

{==============================================================================}
procedure TDiaryWebSource.UpdateStatus;
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
function TDiaryWebSource.DownloadFoodBase(out Data: string): boolean;
{==============================================================================}
var
  Query: string;
begin
  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?foodbase:download';
  Log(VERBOUS, 'TDiaryWebSource.DownloadFoodBase: quering ' + Query);

  // отправл€ем
  Result := DoGetSmart(Query, Data);

  if (not Result) then
    Log(ERROR, 'TDiaryWebSource.DownloadFoodBase: quering failed');
end;

{==============================================================================}
function TDiaryWebSource.GetFoodBaseVersion(out Version: integer): boolean;
{==============================================================================}
var
  Query, Resp: string;
begin
  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?foodbase:getVersion';
  Log(VERBOUS, 'TDiaryWebSource.GetFoodBaseVersion: quering ' + Query);

  // отправл€ем
  Result := DoGetSmart(Query, Resp);
  Result := Result and TryStrToInt(Resp, Version);

  if (not Result) then
    Log(ERROR, 'TDiaryWebSource.GetFoodBaseVersion: quering failed');
end;

function TDiaryWebSource.UploadFoodBase(const Data: string;
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

function TDiaryWebSource.DownloadDishBase(out Data: string): boolean;
var
  Query: string;
begin
  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?dishbase:download';
  Log(VERBOUS, 'TDiaryWebSource.DownloadDishBase: quering ' + Query);

  // отправл€ем
  Result := DoGetSmart(Query, Data);

  if (not Result) then
    Log(ERROR, 'TDiaryWebSource.DownloadDishBase: quering failed');
end;

function TDiaryWebSource.GetDishBaseVersion(out Version: integer): boolean;
var
  Query, Resp: string;
begin
  // конструируем запрос
  Query := FServer + PAGE_CONSOLE + '?dishbase:getVersion';
  Log(VERBOUS, 'TDiaryWebSource.GetDishBaseVersion: quering ' + Query);

  // отправл€ем
  Result := DoGetSmart(Query, Resp);
  Result := Result and TryStrToInt(Resp, Version);

  if (not Result) then
    Log(ERROR, 'TDiaryWebSource.GetDishBaseVersion: quering failed');
end;

function TDiaryWebSource.UploadDishBase(const Data: string;
  Version: integer): boolean;
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

function TDiaryWebSource.DownloadKoofs(out Data: string): boolean;
var
  Query: string;
begin
  Query := FServer + PAGE_CONSOLE + '?koofs:download';
  Result := DoGetSmart(Query, Data);
end;

function TDiaryWebSource.UploadKoofs(const Data: string): boolean;
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


function TDiaryWebSource.Report(const Msg: string): boolean;
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

function TDiaryWebSource.Search(const Key: string): string;
var
  Resp: string;
begin
  if DoGetSmart(FServer + PAGE_CONSOLE + '?foodbase:search&q=' + Key, Resp) then
    Result := Resp
  else
    Result := 'FAILED';
end;

end.

{ TWebDiaryPage }
 (*
{==============================================================================}
procedure TWebDiaryPage.Download;
{==============================================================================}
var
  Source, Stamp: string;
begin
  {RequestPage(Date, Source);
  RequestStamp(Date, Stamp);
  Read(Source);
  TimeLoaded := Now;
  TimeStamp := StrToDateTime(Stamp);  }
  //Modified := False; - присутствует в Read'е
end;

{==============================================================================}
procedure TWebDiaryPage.Read(const S: string);
{==============================================================================}
var
  temp: TStringList;
begin
  temp := TStringList.Create;
  try
    temp.Text := S;
    Read(Temp);
  finally
    temp.Free;
  end;
end;

{==============================================================================}
procedure TWebDiaryPage.Upload;
{==============================================================================}
var
  Source: string;
begin
  {Write(Source);
  PostPage(Date, Source);
  Modified := False; }
end;

{==============================================================================}
procedure TWebDiaryPage.Write(out S: string);
{==============================================================================}
var
  temp: TStringList;
begin
  temp := TStringList.Create;
  try
    Write(Temp);
    S := Temp.Text;
  finally
    temp.Free;
  end;
end;

{ TWebDiary }

{==============================================================================}
procedure TWebDiary.Add(Page: TWebDiaryPage);
{==============================================================================}
begin
  FCursor := (FCursor + 1) mod FCapacity;
  if FRealSize < FCapacity then
    inc(FRealSize) else
  // перед перезаписью надо проверить, не была ли изменена страница
  if FCache[FCursor].Modified then
    FCache[FCursor].Upload;

  FCache[FCursor] := Page;     
end;

{==============================================================================}
constructor TWebDiary.Create(Capacity: integer);
{==============================================================================}
begin
  if Capacity <= 0 then
    raise Exception.Create('Capacity must be positive')
  else    
  begin
    FCapacity := Capacity;
    FRealSize := 0;
    FCursor := -1;
    SetLength(FCache, Capacity);
  end;
end;

{==============================================================================}
destructor TWebDiary.Destroy;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to FRealSize-1 do
    FCache[i].Free; // Page удал€ютс€ внутри
  FRealSize := 0;
end;

{==============================================================================}
function TWebDiary.FindInCache(Date: TDateTime): TWebDiaryPage;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to FRealSize-1 do
  if FCache[i].Date = Date then
  begin
    Result := FCache[i];
    Exit;
  end;
  Result := nil;
end;

{==============================================================================}
function TWebDiary.GetPage(Date: TDateTime; ForceReload: boolean): TWebDiaryPage;
{==============================================================================}
begin
  if not FLogged then raise Exception.Create('Not logged in');

  Result := FindInCache(Date);

  if Result = nil then
  begin
    Result := TWebDiaryPage.Create(Date);
    Result.Download;
    Add(Result);
  end else
  if ForceReload then
    Result.Download;
end;

{==============================================================================}
function TWebDiary.Login(const Name, Password: string): boolean;
{==============================================================================}
var
  Data: TStrings;
  Res: string;
  HasErrors: boolean;
begin
 { Data := TStringList.Create;
  HasErrors := False;
  try
    try
      Data.Add('login=' + Name);
      Data.Add('password=' + Password);
      Data.Add('noredir=');
      Res := Form1.IdHttp1.Post(URL_LOGIN, Data);
    except
      HasErrors := True;
    end;
  finally
    Data.Free;
  end;

  if HasErrors then
    raise Exception.Create('Ќет св€зи с сервером')
  else
  begin
    Result := (Res = 'DONE');
    FLogged := (Res = 'DONE');
    if not Logged then
    if Res = 'BADNAME' then
    begin
      raise Exception.Create('Ќеверное им€ пользовател€ или пароль');
    end else
      raise Exception.Create('Ќеизвестна€ ошибка подключени€');
  end; }
end;

{==============================================================================}
procedure TWebDiary.Logout;
{==============================================================================}
begin
 { Form1.IdHttp1.Get(URL_LOGIN + '?logout');
  FLogged := False;    }
end;

{==============================================================================}
function TWebDiary.PostModified(Limit: integer): integer;
{==============================================================================}
var
  i: integer;
begin
  Result := 0;
  if not FLogged then raise Exception.Create('Not logged in');

  for i := 0 to FRealSize-1 do
  if FCache[i].Modified then
  begin
    FCache[i].Upload;
    inc(Result);
    if (Limit > 0)and(Result = Limit) then Exit;
  end;
end;

{==============================================================================}
procedure TWebDiary.ReloadCash;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to FRealSize-1 do
    FCache[i].Download;
end;   *)

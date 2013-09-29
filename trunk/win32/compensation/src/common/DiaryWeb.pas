unit DiaryWeb;

{ ������ � �������� }

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
    lrFailConnection,  // ������ �� ��������
    lrFailFormat,      // ������ �������� �����������
    lrFailAuth,        // ������ �������� � ������������ ���� "�����-������"
    lrFailAPIVersion,  // ������ �������� �� ������������ ������ API
    lrDone             // ������ �������� �� �������� �����������
  );

  TParamList = array of string;

  TLoginEvent = procedure(Sender: TObject; State: TLoginResult) of object;

  TDiaryWebSource = class (IDiarySource)
  private
    // ����
    FHTTP: TIdHTTP;
    FServer: string;
    FUsername: string;
    FPassword: string;
    FOnline: boolean;
    FTimeShift: double;
    FLoginResult: TLoginResult;

    // �������
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

    { ��������� }
    {#}function GetModList(Time: TDateTime; out ModList: TModList): boolean; override;
    {#}function GetPages(const Dates: TDateList; out Pages: TPageList): boolean; override;
    {#}function PostPages(const Pages: TPageList): boolean; override;

    { ������������� }

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

    // ����
    function Report(const Msg: string): boolean;
    function Search(const Key: string): string;

    { �������� }
    //property Autologin: boolean read FAutologin write FAutologin;
    property LoginResult: TLoginResult read FLoginResult;
    property Online: boolean read FOnline;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Server: string read FServer write FServer;

    { ������� }
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
    function FindInCache(Date: TDateTime): TWebDiaryPage; // ��� TDiaryPage ?
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

  EXCEPTION_OFFLINE = '��� ���������� �������� ���������� ��������������';

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
  {#} Log('TDiaryWebSource.DoGet("' + URL + '")');

  Resp := '';
  try
    try
      {#} Tick := GetTickCount();
      Resp := FHTTP.Get(URL);
      {#} Log('TDiaryWebSource.DoGet(): time is ' + IntToStr(GetTickCount - Tick) + ' msec');
      {#} Log('TDiaryWebSource.DoGet(): Resp = "' + Resp + '"');
      Result := True;
    finally
      FHTTP.Disconnect;
    end;
  except
    on ESE: Exception do
    begin
      {#} Log('TDiaryWebSource.DoGet(): EXCEPTION! ' +  ESE.Message);
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
  {#} Log('TDiaryWebSource.DoPost("' + URL + '"), ' + PrintParams());

  Resp := ''; 
  try
    Data := TStringList.Create;
    try
      // ��������� ���� �������
      for i := Low(Par) to High(Par) do
        Data.Add(Par[i]);

      {#} Tick := GetTickCount();
      Resp := FHTTP.Post(URL, Data);
      {#} Log('TDiaryWebSource.DoPost(): time is ' + IntToStr(GetTickCount - Tick) + ' msec');
      {#} Log('TDiaryWebSource.DoPost(): Resp = "' + Resp + '"');
      Result := True;
    finally
      FHTTP.Disconnect;
      Data.Free;
    end;
  except
    on ESE: Exception do
    begin
      {#} Log('TDiaryWebSource.DoPost(): EXCEPTION! ' +  ESE.Message);
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
      // ������ �������
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
      // ������ �������
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
  Log('TDiaryWebSource.GetModList(): started');
  Log('TDiaryWebSource.GetModList(): Time = "' + DateTimeToStr(Time) + '"');

  try

  if Time > 0 then
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=' + ChkSpace(DateTimeToStr(LocalToServer(Time), WebFmt))
  else
    Query := FServer + PAGE_CONSOLE + '?diary:getModList&time=0';

  Log('TDiaryWebSource.GetModList(): quering ' + Query);

  if DoGetSmart(Query, Resp) then
  begin
    Log('TDiaryWebSource.GetModList(): quered OK');
    Log('TDiaryWebSource.GetModList(): Resp = "' + Resp + '"');

    S := TStringList.Create;
    S.Text := Trim(Resp);

    SetLength(ModList, S.Count);
    count := 0;

    Log('TDiaryWebSource.GetModList(): lines count = ' + IntToStr(S.Count));

    for i := 0 to S.Count - 1 do
    begin
      Line := S[i];
      if (Line <> '') then
      begin
        Log('TDiaryWebSource.GetModList(): parsing line "' + Line + '"');

        // TODO: �� ���������� :)
        //if Length(ModList) = Count then
        //  SetLength(ModList, Length(ModList) * 2);

        Separate(Line, Date, '|', Version);

        Log('TDiaryWebSource.GetModList(): date = "' + Date + '"');
        Log('TDiaryWebSource.GetModList(): version = "' + Version + '"');

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

    Log('TDiaryWebSource.GetModList(): done OK');
  end else
  begin
    Log('TDiaryWebSource.GetModList(): quering failed');
    Result := False;
  end;

  except
    on ESE: Exception do
    begin
      {#} Log('TDiaryWebSource.GetModList(): EXCEPTION! ' +  ESE.Message, True);
      Result := False;
      ShowMessage('Error in GetModList(), see log file for details');
    end;    
  end;
end;

{==============================================================================}
function TDiaryWebSource.GetPages(const Dates: TDateList; out Pages: TPageList): boolean;
{==============================================================================}
var
  Query, Resp: string;
  S: TStringList;
  i: integer;
begin
  // ��������
  if (Length(Dates) = 0) then
  begin
    SetLength(Pages, 0);
    Result := True;
    Exit;
  end;

  Log('TDiaryWebSource.GetPages: started');

  // ������������ ������
  Query := FServer + PAGE_CONSOLE + '?diary:download&dates=';
  for i := 0 to High(Dates) do
    Query := Query + DateToStr(Dates[i], WebFmt) + ',';

  Log('TDiaryWebSource.GetPages: quering ' + Query);

  // ����������
  if DoGetSmart(Query, Resp) then
  begin
    Log('TDiaryWebSource.GetPages: quered OK');
    Log('TDiaryWebSource.GetPages: resp = "' + Resp + '"');

    // ������������
    S := TStringList.Create;
    S.Text := Resp;
    try
      TPageData.MultiRead(S, True, Pages);
      for i := 0 to High(Pages) do
        Pages[i].TimeStamp := ServerToLocal(Pages[i].TimeStamp);
    finally
      S.Free;
    end;
    Log('TDiaryWebSource.GetPages: done OK');
    Result := True;
  end else
  begin
    Log('TDiaryWebSource.GetPages: quering failed');
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
  Log('TDiaryWebSource.Login(): quering ' + FServer + PAGE_LOGIN);

  SendedTime := Now;
  if DoPost(FServer + PAGE_LOGIN, par, Msg) then // Not smart, it's o.k. - Hi, C.O.! :D
  begin
    Log('TDiaryWebSource.Login(): quered OK, resp = "' + Msg + '"');
    //{#}Log('Login()\posting: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;

    Separate(Msg, Res, '|', Desc);

    Log('TDiaryWebSource.Login(): Res = "' + Res + '"');
    Log('TDiaryWebSource.Login(): Desc = "' + Desc + '"');

    //{#}Log('Login()\separating: ' + IntToStr(GetTickCount() - Tick)); Tick := GetTickCount;

    if (Res = MESSAGE_DONE) then
    begin
      Log('TDiaryWebSource.Login(): it means "DONE", parsing desc...');
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
  Log('TDiaryWebSource.Login(): done, FOnline = ' + BoolToStr(FOnline, True));

  except
    on ESE: Exception do
    begin
      {#} Log('TDiaryWebSource.Login(): EXCEPTION! ' +  ESE.Message);
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
function TDiaryWebSource.PostPages(const Pages: TPageList): boolean;
{==============================================================================}
var
  Par: TParamList;
  Msg: string;
  OldStamp: TDateTime;
  i: integer;
  Source: string;
begin
  // ��������
  if (Length(Pages) = 0) then
  begin
    Result := True;
    Exit;
  end;

  // ������ �������� �������� ������, ���� ��� ����� ���� ��������� �� �����
  Source := '';
  for i := 0 to high(Pages) do
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

function TDiaryWebSource.DownloadFoodBase(out Data: string): boolean;
var
  Query: string;
begin
  // ������������ ������
  Query := FServer + PAGE_CONSOLE + '?foodbase:download';
  Log('TDiaryWebSource.DownloadFoodBase: quering ' + Query);

  // ����������
  Result := DoGetSmart(Query, Data);

  if (not Result) then
    Log('TDiaryWebSource.DownloadFoodBase: quering failed');
end;

function TDiaryWebSource.GetFoodBaseVersion(out Version: integer): boolean;
var
  Query, Resp: string;
begin
  // ������������ ������
  Query := FServer + PAGE_CONSOLE + '?foodbase:getVersion';
  Log('TDiaryWebSource.GetFoodBaseVersion: quering ' + Query);

  // ����������
  Result := DoGetSmart(Query, Resp);
  Result := Result and TryStrToInt(Resp, Version);

  if (not Result) then
    Log('TDiaryWebSource.GetFoodBaseVersion: quering failed');
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
  // ������������ ������
  Query := FServer + PAGE_CONSOLE + '?dishbase:download';
  Log('TDiaryWebSource.DownloadDishBase: quering ' + Query);

  // ����������
  Result := DoGetSmart(Query, Data);

  if (not Result) then
    Log('TDiaryWebSource.DownloadDishBase: quering failed');
end;

function TDiaryWebSource.GetDishBaseVersion(out Version: integer): boolean;
var
  Query, Resp: string;
begin
  // ������������ ������
  Query := FServer + PAGE_CONSOLE + '?dishbase:getVersion';
  Log('TDiaryWebSource.GetDishBaseVersion: quering ' + Query);

  // ����������
  Result := DoGetSmart(Query, Resp);
  Result := Result and TryStrToInt(Resp, Version);

  if (not Result) then
    Log('TDiaryWebSource.GetDishBaseVersion: quering failed');
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
  //Modified := False; - ������������ � Read'�
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
  // ����� ����������� ���� ���������, �� ���� �� �������� ��������
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
    FCache[i].Free; // Page ��������� ������
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
    raise Exception.Create('��� ����� � ��������')
  else
  begin
    Result := (Res = 'DONE');
    FLogged := (Res = 'DONE');
    if not Logged then
    if Res = 'BADNAME' then
    begin
      raise Exception.Create('�������� ��� ������������ ��� ������');
    end else
      raise Exception.Create('����������� ������ �����������');
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

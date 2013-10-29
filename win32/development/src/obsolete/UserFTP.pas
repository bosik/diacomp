unit UserFTP;

interface

uses
  SysUtils,
  Windows,
  Classes,
  IdFTP,
  Forms,
  PassCode,
  SyncBase;

type
  TConnectionStatus = (stOffline, stConnecting, stOnline);
  TRights = (rtNone, rtReadOnly, rtFull);
  EPathError = class(Exception);

  TUser = record
    ID: integer;
    TimeStampCRT: TDateTime;
    TimeStampMOD: TDateTime;
    HaveDR: boolean;
    HaveFB: boolean;
    HaveDB: boolean;
  end;
  TUserList = array of TUser;
  TChangeStatusEvent = procedure(Sender: TObject; NewStatus: TConnectionStatus) of object;
  //TBaseType = (btDiary, btFoodBase, btDishBase);

  {======================================================}

  TCompensationCommon = class (TComponent)
  private
    FFTP: TIdFTP;
    FCounter: integer;
    FStatus: TConnectionStatus;
    FOnStatus: TChangeStatusEvent;
    FTempFolder: string;
    FDownloadFolder: string;
  protected
    function ExistsID(ID: integer): boolean;
    procedure OpenFolder(ID: integer);
    procedure SaveCounter;
    procedure StatusChanged(NewStatus: TConnectionStatus);
    function UserFileExists(const FileName: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function BaseExists(ID: integer; BaseType: TBaseType): boolean;
    function LoadCounter: boolean;
    function LoadUserStamp(ID: integer; var Stamp: TDateTime): boolean;
    function Connect(const HostName, Username, Password: string): boolean;
    procedure Disconnect;
    procedure UpdateCounter;

    property Counter: integer read FCounter write FCounter;
    property Status: TConnectionStatus read FStatus write FStatus;
  published
    property DownloadFolder: string read FDownloadFolder write FDownloadFolder;
    property TempFolder: string read FTempFolder write FTempFolder;

    property OnStatusChanged: TChangeStatusEvent read FOnStatus write FOnStatus;
  end;

  {======================================================}

  TCompensationServer = class (TCompensationCommon)
  private
    FUserList: TUserList;
  public
    function Connect(const HostName, Username, Password: string): boolean;
    procedure UpdateCounter;
    procedure UpdateUserlist;
    property Counter;
    property UserList: TUserList read FUserList;
  published
    property OnStatusChanged;
  end;

  {======================================================}

  TCompensationClient = class (TCompensationCommon)
  private
    FRights: TRights;
    FID: integer;
    FPassword: string;
    FUploadFolder: string;
  public
    //function BaseExists(BaseType: TBaseType): boolean;
    constructor Create(AOwner: TComponent); override;
    function Connect(const HostName,Username,Password: string): boolean;
    procedure Disconnect;
    function DownloadBase(BaseType: TBaseType): boolean;
    function Login_Doctor(ID: integer; const Password: string): boolean;
    function Login_User(ID: integer; const Password,CL: string): boolean;
    function Registration(const UserPass: string;
      var NewID: integer): boolean;
    function UploadBase(BaseType: TBaseType): boolean;

    property Rights: TRights read FRights;
    property Status;
  published
    property UploadFolder: string read FUploadFolder write FUploadFolder;
    property OnStatusChanged;
  end;

  function LoadStr(var Str: string; const FileName: string): boolean;
  procedure SaveStr(const Str, FileName: string);
  function LoadStamp(var Stamp: TDateTime; const FileName: string): boolean;
  procedure SaveStamp(Stamp: TDateTime; const FileName: string);

var
  WORK_FOLDER: string;
  Status: TConnectionStatus;
  Counter: integer;
const
  { files }
  CounterFile  = 'Counter';

  UserCLCrypt      = 'CheckLine';
  UserCLDecrypt    = 'CLDec.txt';
  UserCLAnswer     = 'CLAnswer.txt';
  UserTimeStampCRT = 'TimeStampCRT';
  UserTimeStampMOD = 'TimeStampMOD';

  ServerDR = 'Diary.txt';
  ServerFB = 'FoodBase.txt';
  ServerDB = 'DishBase.txt';

  UserDR = 'Diary.txt';
  UserFB = 'FoodBase.txt';
  UserDB = 'DishBase.txt';

  CryptedDR = 'Diary.crp';
  CryptedFB = 'FoodBase.crp';
  CryptedDB = 'DishBase.crp';

  EDirectoryNotFounded = 'Указанная папка не существует';
implementation

{ Базовые }

function LoadStr(var Str: string; const FileName: string): boolean;
var
  c: char;
begin
  if not FileExists(FileName) then
    result:=false else
  try
    with TFileStream.Create(FileName,fmOpenRead) do
    begin
      Position:=0;
      while Position<Size do
      begin
        Read(c,1);
        Str:=Str+c;
      end;
      Free;
    end;
    result:=true;
  except
    result:=false;
  end;
end;

procedure SaveStr(const Str, FileName: string);
var
  i: integer;
begin
  with TFileStream.Create(FileName,fmCreate) do
  begin
    for i:=1 to length(Str) do
      Write(Str[i],1);
    Free;
  end;
end;

function LoadStamp(var Stamp: TDateTime;
  const FileName: string): boolean;
begin
  if not FileExists(FileName) then
    result:=false else
  try
    with TFileStream.Create(FileName,fmOpenRead) do
    begin
      Read(Stamp,SizeOf(Stamp));
      Free;
    end;
    result:=true;
  except
    result:=false;
  end;
end;

procedure SaveStamp(Stamp: TDateTime;
  const FileName: string);
begin
  with TFileStream.Create(FileName,fmCreate) do
  begin
    Write(Stamp,SizeOf(Stamp));
    Free;
  end;
end;

{ TCompensationCommon }

constructor TCompensationCommon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFtp:=TIdFTP.Create(nil);
  FStatus:=stOffline;
end;

function TCompensationCommon.ExistsID(ID: integer): boolean;
begin
  result:=UserFileExists('/users/id'+IntToStr(ID));
end;

destructor TCompensationCommon.Destroy;
begin
  FFTP.Destroy;
  inherited Destroy;
end;

function TCompensationCommon.LoadCounter: boolean;
begin
  result:=false;
  if FStatus <> stOnline then exit;

  try
    FFtp.ChangeDir('/');
    FFtp.Get(CounterFile,FTempFolder+CounterFile,True);
    if FileExists(FTempFolder+CounterFile) then
    begin
      with TStringList.Create do
      begin
        LoadFromFile(FTempFolder+CounterFile);
        if (Count=0)or(not TryStrToInt(Strings[0],FCounter))or
           (FCounter<0) then
        begin
          result:=false;
          UpdateCounter;
        end else
          result:=true;
        Free;
      end;
      DeleteFile(PChar(FTempFolder+CounterFile));
    end;
  except
    result:=false;
    UpdateCounter;
  end;
end;

function TCompensationCommon.BaseExists(ID: integer; BaseType: TBaseType): boolean;
begin
  if (FStatus <> stOnline)or
     (not ExistsID(ID)) then
    result:=false else
  case BaseType of
    btDiary:    result:=UserFileExists('/users/id'+IntToStr(ID)+'/'+ServerDR);
    btFoodBase: result:=UserFileExists('/users/id'+IntToStr(ID)+'/'+ServerFB);
    btDishBase: result:=UserFileExists('/users/id'+IntToStr(ID)+'/'+ServerDB);
    else result:=false;
  end;
end;

function TCompensationCommon.Connect(const HostName, Username,
  Password: string): boolean;
begin
  if (FTempFolder<>'')and
     (not DirectoryExists(FTempFolder))
  then
  begin
    //result:=false;
    raise EStreamError.Create(EDirectoryNotFounded);
  end else
  try
    if not FFtp.Connected then
    begin
      StatusChanged(stConnecting);
      FFtp.Port:=21;
      FFtp.Host:=HostName;
      FFtp.Username:=Username;
      FFtp.Password:=Password;
      FFtp.Passive:=false;
      FFtp.Connect;
      FFtp.Login;
      result:=FFtp.Connected;
      if result then
        StatusChanged(stOnline)
      else
        StatusChanged(stOffline);
    end else
      result:=true;
  except
    StatusChanged(stOffline);
    result:=false;
  end;
end;

procedure TCompensationCommon.Disconnect;
begin
  try
    FFtp.Quit;
    FFtp.Disconnect;
  except
  end;
  StatusChanged(stOffline);
end;

procedure TCompensationCommon.SaveCounter;
begin
  if FStatus <> stOnline then exit;

  SaveStr(IntToStr(FCounter),FTempFolder+CounterFile);
  FFtp.ChangeDir('/');
  FFtp.Put(FTempFolder+CounterFile,CounterFile);
  DeleteFile(PChar(FTempFolder+CounterFile));
end;

procedure TCompensationCommon.StatusChanged(NewStatus: TConnectionStatus);
begin
  if (NewStatus<>FStatus) then
  begin
    FStatus:=NewStatus;
    if (Assigned(OnStatusChanged)) then
      OnStatusChanged(Self,NewStatus);
  end;
end;

procedure TCompensationCommon.UpdateCounter;
begin
  if FStatus <> stOnline then exit;

  FCounter:=0;
  while ExistsID(FCounter+1) do
    inc(FCounter);

  SaveCounter;
end;

procedure TCompensationCommon.OpenFolder(ID: integer);
begin
  FFtp.ChangeDir('/users/id'+IntToStr(ID));
end;

function TCompensationCommon.UserFileExists(const FileName: string): boolean;

  function GetLastSlash(const S: string): integer;
  var
    i: integer;
  begin
    result:=-1;
    for i:=length(S) downto 1 do
    if S[i] = '/' then
    begin
      result:=i;
      exit;
    end;
  end;

  function ExtractFilePath(const S: string): string;
  var
    n: integer;
  begin
    n:=GetLastSlash(S);
    if n=-1 then
      result:=''
    else
     result:=Copy(s,1,n);
  end;

  function ExtractFileName(const S: string): string;
  var
    n: integer;
  begin
    n:=GetLastSlash(S);
    if n=-1 then
      result:=S
    else
     result:=Copy(s,n+1,length(s)-n);
  end;

var
  s: TStrings;
  i: integer;
  FName, DName: string;
begin
  result:=false;
  if Status <> stOnline then exit;
  try
    DName:=ExtractFilePath(FileName);
    FName:=ExtractFileName(FileName);

    s:=TStringList.Create;
    FFtp.ChangeDir(DName);
    FFtp.List(s,'',false);
    for i:=0 to s.Count-1 do
    if s[i] = FName then
    begin
      result:=true;
      exit;
    end;
  except
  end;
end;

function TCompensationCommon.LoadUserStamp(ID: integer; var Stamp: TDateTime): boolean;
var
  FileName: string;
begin
  result:=false;
  if FStatus <> stOnline then exit;

  FileName:='/users/id'+IntToStr(ID)+'/'+UserTimeStampMOD;
  if UserFileExists(FileName) then
  try
    FFTP.Get(FileName, FTempFolder+UserTimeStampMOD);
    result:=LoadStamp(Stamp, FTempFolder+UserTimeStampMOD);
    DeleteFile(PChar(FTempFolder+UserTimeStampMOD));
  except
    result:=false;
  end;
end;

{ TCompensationServer }

function TCompensationServer.Connect(const HostName, Username,
  Password: string): boolean;
begin
  result:=inherited Connect(HostName, Username, Password);
  if result then
  begin
    LoadCounter;
    UpdateUserlist;
  end;
end;

procedure TCompensationServer.UpdateCounter;
begin
  inherited;
  UpdateUserlist;
end;

procedure TCompensationServer.UpdateUserlist;
var
  i: integer;
begin
  if Status <> stOnline then exit;

  SetLength(FUserList,FCounter);

  for i:=0 to FCounter-1 do
  if ExistsID(i+1) then
  with FUserList[i] do
  begin
    OpenFolder(i+1);

    { ID }
    FUserList[i].ID:=i+1;

    { TimeStampCRT }
    if UserFileExists('/users/id'+IntToStr(i+1)+'/'+UserTimeStampCRT) then
    begin
      FFtp.Get(UserTimeStampCRT,FTempFolder+UserTimeStampCRT,true);
      LoadStamp(
         FUserList[i].TimeStampCRT,
         FTempFolder+UserTimeStampCRT);
      DeleteFile(PChar(FTempFolder+UserTimeStampCRT));
    end else
      FUserList[i].TimeStampCRT:=0;  { что-то типа того... }

    { TimeStampMOD }
    if UserFileExists('/users/id'+IntToStr(i+1)+'/'+UserTimeStampMOD) then
    begin
      FFtp.Get(UserTimeStampMOD,FTempFolder+UserTimeStampMOD,true);
      LoadStamp(
         FUserList[i].TimeStampMOD,
         FTempFolder+UserTimeStampMOD);
      DeleteFile(PChar(FTempFolder+UserTimeStampMOD));
    end else
      FUserList[i].TimeStampMOD:=0;  { что-то типа того... }

    { Bases }
    FUserList[i].HaveDR:=false;
    FUserList[i].HaveFB:=false;
    FUserList[i].HaveDB:=false;
  end;
end;

{ TCompensationClient }

constructor TCompensationClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRights:=rtNone;
end;

function TCompensationClient.Connect(const HostName,
  Username, Password: string): boolean;
begin
  result:=inherited Connect(HostName, Username, Password);
end;

procedure TCompensationClient.Disconnect;
begin
  inherited Disconnect;
  FRights:=rtNone;
end;

function TCompensationClient.Login_Doctor(ID: integer;
  const Password: string): boolean;
var
  Stamp: TDateTime;
  BaseType: TBaseType;
begin
  result:=false;
  if (FStatus <> stOnline) then exit;
  if (not ExistsID(ID)) then exit;

  if (FDownloadFolder<>'')and
     (not DirectoryExists(FDownloadFolder))
  then exit;

  OpenFolder(ID);
  try
    result:=true;
    if BaseExists(ID,btDiary) then
    begin
      FFtp.Get(ServerDR,FDownloadFolder+CryptedDR,true);
      DecryptPass(
        FDownloadFolder+CryptedDR,
        FDownloadFolder+UserDR,
        Password);
      DeleteFile(PChar(FDownloadFolder+CryptedDR));

      if (not GetSyncInfo(FDownloadFolder+UserDR,Stamp,BaseType)) or
         (BaseType<>btDiary) then
        result:=false;
    end;

    if result then
    begin
      FRights:=rtReadonly;
      FID:=ID;
      FPassword:=Password;
    end else
    begin
      FRights:=rtNone;
      FID:=-1;
      FPassword:='';
    end;
  except
    FRights:=rtNone;
    result:=false;
  end;
end;

function TCompensationClient.Login_User(ID: integer; const Password,
  CL: string): boolean;
var
  CLDecrypt: string;
begin
  result:=false;
  if (FStatus <> stOnline) then exit;
  if (not ExistsID(ID)) then exit;

  if (FDownloadFolder<>'')and
     (not DirectoryExists(FDownloadFolder))
  then exit;

  if (FUploadFolder<>'')and
     (not DirectoryExists(FUploadFolder))
  then exit;

  OpenFolder(ID);
  try
    FFtp.Get(UserCLCrypt,FTempFolder+UserCLCrypt,true);
  except
    exit;
  end;

  DecryptPass(
    FTempFolder+UserCLCrypt,
    FTempFolder+UserCLDecrypt,
    Password);

  DeleteFile(PChar(FTempFolder+UserCLCrypt));

  if LoadStr(CLDecrypt, FTempFolder+UserCLDecrypt) and
     (CLDecrypt=CL) then
  begin
    result:=true;
    FRights:=rtFull;
    FID:=ID;
    FPassword:=Password;
  end;

  DeleteFile(PChar(FTempFolder+UserCLDecrypt));
end;

function TCompensationClient.Registration(const UserPass: string;
  var NewID: integer): boolean;

  procedure CreateCL;
  const
    CL_SIZE = 20;
  var
    CheckLine: string;
  begin
    { создаем строку }  
    Randomize;
    CheckLine:='';
    while length(CheckLine)<CL_SIZE do
      CheckLine:=CheckLine+char(Random(256));

    { сохраняем в файл }
    SaveStr(CheckLine,FUploadFolder+UserCLAnswer);

    { создаём зашифрованный вариант }
    EncryptPass(
      FUploadFolder+UserCLAnswer,
      FTempFolder+UserCLCrypt,
      UserPass
    );
  end;

  procedure CreateTimeStamp;
  begin
    SaveStamp(now, FTempFolder+UserTimeStampCRT);
  end;

begin
  result:=false;
  if (FStatus <> stOnline) then exit; 

  if (FTempFolder<>'')and
     (FTempFolder[length(FTempFolder)]<>'\')then
    FTempFolder:=FTempFolder+'\';
  if not DirectoryExists(FTempFolder) then exit;

  if (FUploadFolder<>'')and
     (FUploadFolder[length(FUploadFolder)]<>'\')then
    FUploadFolder:=FUploadFolder+'\';
  if not DirectoryExists(FUploadFolder) then exit;

  try
    LoadCounter;

    try
      repeat
        inc(FCounter);
      until not ExistsID(FCounter);
    except
    end;

    SaveCounter;

    FFtp.ChangeDir('/users/');
    FFtp.MakeDir('id'+IntToStr(Counter));
    OpenFolder(Counter);

    CreateCL;
    FFtp.Put(FTempFolder+UserCLCrypt,UserCLCrypt);
    DeleteFile(PChar(FTempFolder+UserCLCrypt));

    CreateTimeStamp;
    FFtp.Put(FTempFolder+UserTimeStampCRT,UserTimeStampCRT);
    DeleteFile(PChar(FTempFolder+UserTimeStampCRT));

    result:=true;
    NewID:=Counter;
  except
    result:=false;
  end;
end;

function TCompensationClient.DownloadBase(BaseType: TBaseType): boolean;
var
  DownloadedBaseType: TBaseType;
  Stamp: TDateTime;
begin
  result:=false;
  if (FStatus <> stOnline) then exit;
  if not DirectoryExists(FTempFolder) then exit;
  if not DirectoryExists(FDownloadFolder) then exit;
  if FRights = rtNone then exit;

   try
    case BaseType of
      btDiary: begin
                 OpenFolder(FID);
                 FFtp.Get(ServerDR,FTempFolder+CryptedDR,true);

                 DecryptPass(
                   FTempFolder+CryptedDR,
                   FDownloadFolder+UserDR,
                   FPassword
                 );

                 DeleteFile(PChar(FTempFolder+CryptedDR));

                 result:=
                   (GetSyncInfo(FDownloadFolder+UserDR,Stamp,DownloadedBaseType)) and
                   (BaseType=DownloadedBaseType);
               end;
    end;
  except
    result:=false;
  end;
end;

function TCompensationClient.UploadBase(BaseType: TBaseType): boolean;
begin
  result:=false;
  if (FStatus <> stOnline) then exit;
  if not DirectoryExists(FUploadFolder) then exit;
  if not DirectoryExists(FTempFolder) then exit;
  if FRights <> rtFull then exit;

  try
    case BaseType of
      btDiary: if FileExists(FUploadFolder+UserDR) then
               begin
                 EncryptPass(
                   FUploadFolder+UserDR,
                   FTempFolder+CryptedDR,
                   FPassword
                 );
                 OpenFolder(FID);
                 FFtp.Put(FTempFolder+CryptedDR,ServerDR);
                 DeleteFile(PChar(FTempFolder+CryptedDR));

                 SaveStamp(now, FTempFolder+UserTimeStampMOD);
                 FFtp.Put(FTempFolder+UserTimeStampMOD,UserTimeStampMOD);
                 DeleteFile(PChar(FTempFolder+UserTimeStampMOD));

                 result:=true;
               end;
    end;
  except
    result:=false;
  end;
end;

end.

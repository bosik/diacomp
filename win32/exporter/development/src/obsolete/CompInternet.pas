unit CompInternet;

interface

uses
  Classes, SysUtils, Windows, PassCode, IdFTP;

  //function HostName: string;
  //function UserName: string;
  //function Password: string;
  procedure SaveStamp(Stamp: TDateTime; const FileName: string);

  procedure ConnectFTP(IdFTP: TIdFTP);
  procedure DisconnectFTP(IdFTP: TIdFTP);
  function UpdateFile(IdFTP: TIdFTP; ID: integer;
    const APassword: string): boolean;

implementation

const
  { Интернет }
  SERVER_DIARY   = 'Diary.txt';
  CRYPTED_DIARY  = 'Diary.crp';
  USER_DIARY     = 'Bases\Diary.txt';

  SERVER_FBASE   = 'FoodBase.txt';
  CRYPTED_FBASE  = 'FoodBase.crp';
  USER_FBASE     = 'Bases\FoodBase.txt';

  SERVER_DBASE   = 'DishBase.txt';
  CRYPTED_DBASE  = 'DishBase.crp';
  USER_DBASE     = 'Bases\DishBase.txt';

  STAMP_FILE     = 'TimeStampMOD';

{=======================================================}
function FTP_HostName: string;
{=======================================================}
const
  W: array[1..12] of single =
  (101.6, 115.62, 112.29, 45.76, 109.82, 97.14, 113.85, 110.73, 99.9, 45.94, 
  113.66, 116.98);
var
  i: integer;
begin
  result:='';
  for i:=1 to 12 do
    result:=result+Char(Round(W[i]));
end;

{=======================================================}
function FTP_UserName: string;
{=======================================================}
const
  W: array[1..18] of single =
  (98.66, 111.27, 108.64, 111.83, 101.33, 109.89, 115.22, 96.86, 116.15, 105.27, 
  111.17, 109.84, 114.73, 100.86, 113.97, 117.79, 101.26, 113.82);
var
  i: integer;
begin
  result:='';
  for i:=1 to 18 do
    result:=result+Char(Round(W[i]));
end;

{=======================================================}
function FTP_Password: string;
{=======================================================}
const
  W: array[1..20] of single =
  (102.98, 108.71, 76.3, 39.82, 36.22, 110.38, 103.99, 107.3, 105.26, 42.62, 
  67.71, 106.71, 83, 121.62, 37.07, 114.6, 100.22, 68.12, 65.22, 78.16);
var
  i: integer;
begin
  result:='';
  for i:=1 to 20 do
    result:=result+Char(Round(W[i]));
end;

{=======================================================}
procedure SaveStamp(Stamp: TDateTime; const FileName: string);
{=======================================================}
begin
  with TFileStream.Create(FileName,fmCreate) do
  begin
    Write(Stamp,SizeOf(Stamp));
    Free;
  end;
end;

{=======================================================}
procedure ConnectFTP(IdFTP: TIdFTP);
{=======================================================}
begin
  try
    if not IdFTP.Connected then
    begin
      IdFTP.Host:=FTP_HostName;
      IdFTP.Username:=FTP_UserName;
      IdFTP.Password:=FTP_Password;
      IdFTP.Passive:=false;
      IdFTP.Port:=21;
      IdFTP.Connect;
      IdFTP.Login;
    end;
  except
  end;
end;

{=======================================================}
procedure DisconnectFTP(IdFTP: TIdFTP);
{=======================================================}
begin
  try
    IdFTP.Quit;
    IdFTP.Disconnect;
  except
  end;
end;

{=======================================================}
function UpdateFile(IdFTP: TIdFTP; ID: integer;
  const APassword: string): boolean;
{=======================================================}
begin
  result:=false;
  try
    if IdFTP.Connected then
    begin
      SaveStamp(now, STAMP_FILE);
      {*}EncryptPass(USER_DIARY, CRYPTED_DIARY, APassword);
      {*}EncryptPass(USER_FBASE, CRYPTED_FBASE, APassword);
      {*}EncryptPass(USER_DBASE, CRYPTED_DBASE, APassword);

      IdFTP.ChangeDir('/users/id'+IntToStr(ID)+'/');
      {*}IdFTP.Put(CRYPTED_DIARY, SERVER_DIARY);
      {*}IdFTP.Put(CRYPTED_FBASE, SERVER_FBASE);
      {*}IdFTP.Put(CRYPTED_DBASE, SERVER_DBASE);
      IdFTP.Put(STAMP_FILE,STAMP_FILE);

      DeleteFile(PChar(STAMP_FILE));
      {*}DeleteFile(PChar(CRYPTED_DIARY));
      {*}DeleteFile(PChar(CRYPTED_FBASE));
      {*}DeleteFile(PChar(CRYPTED_DBASE));
      result:=true;
    end else
    begin
      DisconnectFTP(IdFTP);
      ConnectFTP(IdFTP);
    end;
  except
  end;
end;

end.

unit SelfUpdate;

interface

uses
  SysUtils,
  ShellApi,
  Windows,
  Dialogs,
  
  DiaryRoutines,
  DiaryCore,
  InetDownload,
  SettingsINI,
  AutoLog;

  function GetLatestVersion(const ServerURL: string): integer;
  procedure DownloadAndRunUpdater();

const
  URL_VERINFO         = 'api/windows/version';
  URL_UPDATER         = 'api/windows/file/updater.exe';

implementation

{======================================================================================================================}
function GetLatestVersion(const ServerURL: string): integer;
{======================================================================================================================}
var
  Response: String;
begin
  Result := -1;
  Log(DEBUG, 'Checking for app updates...');

  // TODO: move outside
  Value['LastUpdateCheck'] := GetTimeUTC();

  try
    if DoGet(ServerURL + URL_VERINFO, 1024, nil, Response) then
    begin
      Log(DEBUG, 'Server response: ' + Response);
      Result := StrToInt(Response);
    end else
    begin
      Log(ERROR, 'Failed to request ' + URL_VERINFO);
    end;
  except
    on e: Exception do
    begin
      Log(ERROR, 'Checking failed: ' + e.Message);
    end;
  end;
end;

{======================================================================================================================}
procedure DownloadAndRunUpdater();
{======================================================================================================================}
const
  FILE_UPDATER = 'Diacomp_upd.exe';
begin
  if// GetInetFile(URL_UPDATER, WORK_FOLDER + FILE_UPDATER, 10 * 1024 * 1024) and
     //FileExists(WORK_FOLDER + FILE_UPDATER) and
     (FileSize(WORK_FOLDER + FILE_UPDATER) > 10 * 1024) then
  begin
    ShellExecute(0, 'runas', PChar(WORK_FOLDER + FILE_UPDATER), nil, nil, SW_SHOW);
    //ShellExecute(0, 'runas', PChar(WORK_FOLDER + 'CompensationTest.exe'), nil, PChar(WORK_FOLDER), SW_RESTORE);
  end else
    MessageDlg('Файл установки повреждён.', mtError, [mbOK], 0); // i18n
end;

end.

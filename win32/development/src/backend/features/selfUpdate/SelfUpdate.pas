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
  procedure DownloadAndRunUpdater(ParentHandle: HWND);

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
procedure RunAsAdminAndWaitForCompletion(hWnd: HWND; filename: string; Parameters: string);
{======================================================================================================================}
var
  info: TShellExecuteInfo;
  ReturnCode: Cardinal;
begin
  ZeroMemory(@info, SizeOf(info));
  info.cbSize := SizeOf(TShellExecuteInfo);
  info.Wnd := hwnd;
  info.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  info.lpVerb := PChar('runas');
  info.lpFile := PChar(Filename);
  if parameters <> '' then
    info.lpParameters := PChar(parameters);
  info.nShow := SW_SHOW;

  if ShellExecuteEx(@info) then
  begin
    if (info.hProcess <> 0) then
    begin
      ReturnCode := WaitForSingleObject(info.hProcess, 60 * 1000);
      CloseHandle(info.hProcess);

      case ReturnCode of
        WAIT_OBJECT_0:
          begin
            MessageDlg('Обновление установлено. Перезапустите приложение, чтобы изменения вступили в силу', mtInformation, [mbOK], 0);
          end;
        WAIT_TIMEOUT:
          begin
            MessageDlg('Время ожидания истекло. Похоже, приложение не было обновлено.', mtWarning, [mbOK], 0);
          end;

        else
          begin
            MessageDlg('Ошибка установки', mtError, [mbOK], 0);
          end;
      end;
    end;
  end;
end;

{======================================================================================================================}
procedure DownloadAndRunUpdater(ParentHandle: HWND);
{======================================================================================================================}

  function GetTempDirectory: String;
  var
    tempFolder: array[0..MAX_PATH] of Char;
  begin
    GetTempPath(MAX_PATH, @tempFolder);
    Result := StrPas(tempFolder);
  end;

const
  FILE_UPDATER = 'diacomp-update.exe';
var
  Command: String;
  Params: String;
begin
  Command := GetTempDirectory + FILE_UPDATER;

  if GetInetFile(Value['ServerURL'] + URL_UPDATER, Command, 10 * 1024 * 1024) and
     FileExists(Command) and
     (FileSize(Command) > 10 * 1024) then
  begin
    Params := '"' + ParamStr(0) + '" ' + IntToStr(PROGRAM_VERSION_CODE);
    RunAsAdminAndWaitForCompletion(ParentHandle, Command, Params);
    SysUtils.DeleteFile(Command);
  end else
    MessageDlg('Файл установки повреждён.', mtError, [mbOK], 0); // i18n
end;

end.

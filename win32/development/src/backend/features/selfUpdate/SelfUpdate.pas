unit SelfUpdate;

interface

uses
  SysUtils,
  ShellApi,
  Windows,
  Dialogs,
  DiaryWeb,
  AutoLog;

  function GetLatestVersion(Client: TDiacompClient): integer;
  procedure RunLoader(Client: TDiacompClient; ParentHandle: HWND);

const
  FILE_LOADER = 'Diacomp_upd.exe';
  URL_VERINFO = 'windows/version';
  URL_APP     = 'windows/file/compensation.exe';

implementation

{======================================================================================================================}
function GetLatestVersion(Client: TDiacompClient): integer;
{======================================================================================================================}
var
  Response: String;
begin
  Log(DEBUG, 'Checking for app updates...');
  Response := Client.DoGetSmart(Client.GetApiURL + URL_VERINFO).Response;

  Log(DEBUG, 'Server response: ' + Response);
  Result := StrToInt(Response);
end;

{======================================================================================================================}
procedure RunAsAdminAndWaitForCompletion(hWnd: HWND; FileName: String; Parameters: String);
{======================================================================================================================}
var
  ExecInfo: TShellExecuteInfo;
  ReturnCode: Cardinal;
begin
  ZeroMemory(@ExecInfo, SizeOf(ExecInfo));
  ExecInfo.cbSize := SizeOf(TShellExecuteInfo);
  ExecInfo.Wnd := hwnd;
  ExecInfo.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  ExecInfo.lpVerb := PChar('runas');
  ExecInfo.lpFile := PChar(FileName);
  if (Parameters <> '') then
    ExecInfo.lpParameters := PChar(Parameters);
  ExecInfo.nShow := SW_SHOW;

  if ShellExecuteEx(@ExecInfo) then
  begin
    if (ExecInfo.hProcess <> 0) then
    begin
      Log(INFO, 'Updater ran OK');
      ReturnCode := WaitForSingleObject(ExecInfo.hProcess, 60 * 1000);
      CloseHandle(ExecInfo.hProcess);
      Log(INFO, 'Updater exited with code ' + IntToStr(ReturnCode));

      case ReturnCode of
        WAIT_OBJECT_0:
          begin
            Log(INFO, 'Update done OK');
            MessageDlg('Обновление установлено. Перезапустите приложение, чтобы изменения вступили в силу', mtInformation, [mbOK], 0);
          end;
        WAIT_TIMEOUT:
          begin
            Log(ERROR, 'Update failed due to timeout');
            MessageDlg('Время ожидания истекло. Похоже, приложение не было обновлено', mtWarning, [mbOK], 0);
          end;

        else
          begin
            Log(ERROR, 'Update failed due to unexpected error');
            MessageDlg('Ошибка установки', mtError, [mbOK], 0);
          end;
      end;
    end else
    begin
      Log(ERROR, 'Failed to start updater process');
      MessageDlg('Не удалось запустить процесс ' + FileName, mtError, [mbOK], 0);
    end;
  end else
  begin
    Log(ERROR, 'Failed to run updater');
    MessageDlg('Не удалось запустить ' + FileName, mtError, [mbOK], 0);
  end;
end;

{======================================================================================================================}
procedure RunLoader(Client: TDiacompClient; ParentHandle: HWND);
{======================================================================================================================}
var
  SourceURL: String;
  TargetFile: String;
begin
  if (not FileExists(FILE_LOADER)) then
  begin
    Log(ERROR, 'Can''t find loader file: ' + FILE_LOADER);
    MessageDlg('Загрузочный файл ' + FILE_LOADER + ' не найден', mtError, [mbOK], 0); // i18n
    Exit;
  end;

  SourceURL := Client.GetApiURL + URL_APP;
  TargetFile := ParamStr(0);
  Log(INFO, 'Update: downloading ' + SourceURL + ' to ' + TargetFile);

  RunAsAdminAndWaitForCompletion(
    ParentHandle,
    FILE_LOADER,
    Format('"%s" "%s"', [SourceURL, TargetFile])
  );
end;

end.

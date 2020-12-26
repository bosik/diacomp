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
      ReturnCode := WaitForSingleObject(ExecInfo.hProcess, 60 * 1000);
      CloseHandle(ExecInfo.hProcess);

      case ReturnCode of
        WAIT_OBJECT_0:
          begin
            MessageDlg('Обновление установлено. Перезапустите приложение, чтобы изменения вступили в силу', mtInformation, [mbOK], 0);
          end;
        WAIT_TIMEOUT:
          begin
            MessageDlg('Время ожидания истекло. Похоже, приложение не было обновлено', mtWarning, [mbOK], 0);
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
procedure RunLoader(Client: TDiacompClient; ParentHandle: HWND);
{======================================================================================================================}
var
  SourceURL: String;
  TargetFile: String;
begin
  if (not FileExists(FILE_LOADER)) then
  begin
    MessageDlg('Загрузочный файл ' + FILE_LOADER + ' не найден', mtError, [mbOK], 0); // i18n
    Exit;
  end;

  SourceURL := Client.GetApiURL + URL_APP;
  TargetFile := ParamStr(0);

  RunAsAdminAndWaitForCompletion(
    ParentHandle,
    FILE_LOADER,
    Format('"%s" "%s"', [SourceURL, TargetFile])
  );
end;

end.

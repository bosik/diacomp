program Compensation;

uses
  ShareMem,
  Forms,
  Windows,
  Dialogs,
  Controls,
  SysUtils,
  InetDownload,
  ShellAPI,
  Classes,
  DiaryInterface in 'src\face\DiaryInterface.pas',
  TextInterface in 'src\face\TextInterface.pas',
  AnalyzeGraphic in 'src\face\AnalyzeGraphic.pas',
  MainUnit in 'src\face\forms\MainUnit.pas' {Form1},
  UnitSettings in 'src\face\forms\UnitSettings.pas' {FormSettings},
  UnitEditDish in 'src\face\forms\UnitEditDish.pas' {FormDish},
  UnitEditFood in 'src\face\forms\UnitEditFood.pas' {FormFood},
  UnitAbout in 'src\face\forms\UnitAbout.pas' {FormAbout},
  UnitFirstMan in 'src\face\forms\UnitFirstMan.pas' {FormFirstMan},
  UnitExportText in 'src\face\forms\UnitExportText.pas' {FormExportText},
  UnitReserve in 'src\face\forms\UnitReserve.pas' {FormSync},
  UnitShadow in 'src\face\forms\UnitShadow.pas' {FormShadow},
  UnitStartup in 'src\face\forms\UnitStartup.pas' {FormProcess},
  UnitResources in 'src\face\forms\UnitResources.pas' {FormResources},
  UnitDataInterface in 'src\face\forms\UnitDataInterface.pas' {DataInterface: TDataModule},
  UnitDEditor in 'src\face\forms\UnitDEditor.pas' {FormEditor},
  SettingsINI in 'src\common\SettingsINI.pas',
  DiaryCore in 'src\common\DiaryCore.pas',
  ThreadTask in 'src\common\ThreadTask.pas',
  AutoLog in 'src\common\Autolog.pas',
  AnalyzeInterface in 'src\analyze\AnalyzeInterface.pas',
  CompTest in 'src\common\CompTest.pas',
  DiaryDatabase in 'src\common\DiaryDatabase.pas',
  DiaryWeb in 'src\common\DiaryWeb.pas',
  DiarySync in 'src\common\DiarySync.pas',
  DiarySources in 'src\common\DiarySources.pas',
  DiaryLocalSource in 'src\common\DiaryLocalSource.pas',
  Statistics in '..\Компоненты\Bosik Math\Statistics.pas',
  BusinessObjects in 'src\bo\BusinessObjects.pas',
  Bases in 'src\bo\Bases.pas',
  DiaryRecords in 'src\bo\DiaryRecords.pas',
  DiaryRoutines in 'src\common\DiaryRoutines.pas',
  DiaryAnalyze in 'src\analyze\DiaryAnalyze.pas';

{$R *.res}

  procedure CheckFolders;
  begin
    WORK_FOLDER := ExtractFilePath(Application.ExeName);
    if not DirectoryExists(WORK_FOLDER + FOLDER_BASES) then
      CreateDirectory(PChar(WORK_FOLDER + FOLDER_BASES), nil);
  end;

  // максимум 1 Мб
  function CheckFile(const LocalFileName, URL: string; Crytical, NeedRestart: boolean; var FlagRestart, FlagModificated: boolean): Boolean;
  begin
    Result := True;

    if (not FileExists(LocalFileName)) then
    begin
      if (GetInetFile('Compensation', URL, LocalFileName, nil, 5000, 1024 * 1024)) and
         (FileExists(LocalFileName)) then
      begin
        FlagModificated := True;
        if (NeedRestart) then
          FlagRestart := True;
      end else
      begin
        if (Crytical) then
          ErrorMessage(
            'Файл "' + LocalFileName + '" не найден'#13 +
            'Загрузка с сервера по адресу "' + URL + '" не удалась'#13 +
            'Приложение завершает свою работу, всё тлен :(');
        Result := False;
      end;
    end;
  end;

  procedure Restart;
  const
    RESTARTER = 'restart.exe';
  begin
    ShellExecute(0, 'open', PChar(WORK_FOLDER + RESTARTER), PChar('"' + Application.ExeName + '" 6000'), nil, SW_SHOWNORMAL);
    Halt;
  end;

  function IsWin64: Boolean;
  var
    IsWow64Process : function(hProcess : THandle; var Wow64Process : BOOL): BOOL; stdcall;
    Wow64Process : BOOL;
  begin
    Result := False;
    IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');
    if Assigned(IsWow64Process) then begin
      if IsWow64Process(GetCurrentProcess, Wow64Process) then begin
        Result := Wow64Process;
      end;
    end;
  end;

  function CheckRunningInstance: boolean;
  var
    h: HWND;
    MainClass: string;
  begin
    MainClass := TForm1.ClassName;
    h := FindWindow(PChar(MainClass), 'Компенсация ' + PROGRAM_VERSION);
    if (h <> 0) then
    begin
      SetForegroundWindow(h);
      SendMessage(h, WM_SECOND_START, 0, 0);
      Result := True;
    end else
      Result := False;
  end;

const
  BIT64VER: array[Boolean] of String = ('32 bit', '64 bit');
var
  tick: cardinal;
  FlagRestart, FlagModificated: boolean;
begin
  tick := GetTickCount;

  { ПРОВЕРКА НАЛИЧИЯ УЖЕ ЗАПУЩЕННОГО ЭКЗЕМПЛЯРА }
  if CheckRunningInstance() then Exit;

  {#}Log('Application started');
  {#}Log('No other instances founded');
  {#}Log(Format('OS Version: %d.%d, %s', [Win32MajorVersion, Win32MinorVersion, BIT64VER[IsWin64()]]));

  { TODO: ПРОВЕРКА НАЛИЧИЯ ВСЕХ НЕОБХОДИМЫХ ФАЙЛОВ }

  CheckFolders; // before check file!               
  DiaryCore.Initialize();

  { пре-исполнение (LoadSettings не использует WORK_FOLDER)}
  LoadSettings;

  if not CheckFile(WORK_FOLDER + 'MathAn.dll',   URL_MATHAN,   True, False, FlagRestart, FlagModificated) then Exit;
  if not CheckFile(WORK_FOLDER + 'borlndmm.dll', URL_BORLNDMM, True, True,  FlagRestart, FlagModificated) then Exit;

  if (FlagModificated) then
  begin
    if (FlagRestart) then
    begin
      if CheckFile(WORK_FOLDER + 'restart.exe', URL_RESTART, True, True, FlagRestart, FlagModificated) then
      begin
        InfoMessage('Некоторые важные файлы были загружены. Для продолжения работы приложение будет перезапущено.');
        Restart();
      end else
      begin
        InfoMessage('Некоторые важные файлы были загружены. Для продолжения работы запустите приложение ещё раз');
        Exit;
      end;
    end else
    begin
      //InfoMessage('Необходимые файлы успешно загружены');
    end;
  end;

  if (Value['FirstStart'] = True) and
     (MessageDlg(MESSAGE_CONF_FIRST_WARNING, mtWarning, [mbYes, mbNo], 0) = mrNo)
  then Exit;

  { общая инициализация } 
  Application.Initialize;
  Application.HintHidePause := 20000;

  { основное исполнение }
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormShadow, FormShadow);
  Application.CreateForm(TFormProcess, FormProcess);
  Application.CreateForm(TFormDish, FormDish);
  Application.CreateForm(TFormFood, FormFood);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormExportText, FormExportText);
  {#}Application.CreateForm(TFormSync, FormSync);
  {#}Application.CreateForm(TFormResources, FormResources);
  Application.CreateForm(TDataInterface, DataInterface);

  Log('Время инициализации: ' + IntToStr(GetTickCount - tick));

  if (Value['FirstStart'] = True) then
  begin
    Value['FirstStart'] := False;
    SaveSettings;
    Application.CreateForm(TFormFirstMan, FormFirstMan);
    FormFirstMan.ShowModal;
  end;

  Form1.FullInit;

  Application.Run;

  DiaryCore.Finalize;
end.

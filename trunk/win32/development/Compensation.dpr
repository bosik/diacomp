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
  UnitDEditor in 'src\face\forms\UnitDEditor.pas' {FormEditorOld},
  SettingsINI in 'src\common\SettingsINI.pas',
  DiaryCore in 'src\common\DiaryCore.pas',
  ThreadTask in 'src\common\ThreadTask.pas',
  AutoLog in 'src\common\Autolog.pas',
  AnalyzeInterface in 'src\analyze\AnalyzeInterface.pas',
  DiaryDatabase in 'src\persistence\DiaryDatabase.pas',
  DiaryWeb in 'src\persistence\web\DiaryWeb.pas',
  DiarySync in 'src\persistence\DiarySync.pas',
  DiaryDAO in 'src\persistence\DiaryDAO.pas',
  DiaryLocalSource in 'src\persistence\local\DiaryLocalSource.pas',
  BusinessObjects in 'src\bo\BusinessObjects.pas',
  Bases in 'src\persistence\Bases.pas',
  DiaryRecords in 'src\bo\DiaryRecords.pas',
  DiaryRoutines in 'src\common\DiaryRoutines.pas',
  DiaryAnalyze in 'src\analyze\DiaryAnalyze.pas',
  DiaryWebSource in 'src\persistence\web\DiaryWebSource.pas',
  DiaryPageSerializer in 'src\persistence\DiaryPageSerializer.pas',
  DiaryPage in 'src\bo\DiaryPage.pas',
  DiaryPageSerializerTest in 'src\test\DiaryPageSerializerTest.pas',
  UnitMisc in 'src\face\forms\UnitMisc.pas' {FormMisc},
  FoodbaseDAO in 'src\persistence\FoodbaseDAO.pas',
  FoodbaseLocalDAO in 'src\persistence\local\FoodbaseLocalDAO.pas',
  FoodbaseWebDAO in 'src\persistence\web\FoodbaseWebDAO.pas',
  JsonSerializer in 'src\persistence\JsonSerializer.pas',
  JsonFoodItemSerializer in 'src\persistence\JsonFoodItemSerializer.pas',
  DAO in 'src\persistence\DAO.pas',
  DishbaseDAO in 'src\persistence\DishbaseDAO.pas',
  DishbaseLocalDAO in 'src\persistence\local\DishbaseLocalDAO.pas',
  DishbaseWebDAO in 'src\persistence\web\DishbaseWebDAO.pas',
  UnitEditor in 'src\face\forms\UnitEditor.pas',
  UnitEditorBlood in 'src\face\forms\UnitEditorBlood.pas' {FormEditorBlood},
  UnitEditorIns in 'src\face\forms\UnitEditorIns.pas' {FormEditorIns},
  UnitDMealEditor in 'src\face\forms\UnitDMealEditor.pas' {FormEditorMeal};

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

  procedure SetApplicationTitle(App: TApplication);
  begin
    App.Title := APPLICATION_TITLE;
  end;

const
  BIT64VER: array[Boolean] of String = ('32 bit', '64 bit');
var
  tick: cardinal;
  FlagRestart, FlagModificated: boolean;
begin
  tick := GetTickCount;
  AutoLog.StartLogger;

  { ПРОВЕРКА НАЛИЧИЯ УЖЕ ЗАПУЩЕННОГО ЭКЗЕМПЛЯРА }
  //if CheckRunningInstance() then Exit;

  {#}Log(INFO, 'Application started');
  {#}Log(INFO, 'No other instances founded');
  {#}Log(INFO, Format('OS Version: %d.%d, %s', [Win32MajorVersion, Win32MinorVersion, BIT64VER[IsWin64()]]));

  { TODO: ПРОВЕРКА НАЛИЧИЯ ВСЕХ НЕОБХОДИМЫХ ФАЙЛОВ }

  CheckFolders; // before check file!
  DiaryCore.Initialize();

  { пре-исполнение (LoadSettings не использует WORK_FOLDER)}
  LoadSettings;
  try
    LoadStringResources('strings_en.txt');
  except
    on E: Exception do
    begin
      Log(ERROR, E.Message);
      ErrorMessage('Некоторые строковые ресурсы отсутствуют');
    end;
  end;

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
  SetApplicationTitle(Application);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormShadow, FormShadow);
  Application.CreateForm(TFormProcess, FormProcess);
  Application.CreateForm(TFormDish, FormDish);
  Application.CreateForm(TFormFood, FormFood);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormExportText, FormExportText);
  Application.CreateForm(TFormMisc, FormMisc);
  {#}Application.CreateForm(TFormSync, FormSync);
  {#}Application.CreateForm(TFormResources, FormResources);
  Application.CreateForm(TDataInterface, DataInterface);

  Log(INFO, 'Время инициализации: ' + IntToStr(GetTickCount - tick));

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
  AutoLog.StopLogger;
end.

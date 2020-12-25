program Compensation;

uses
  Forms,
  Windows,
  Dialogs,
  Controls,
  SysUtils,
  InetDownload in 'src\ext\lib\InetDownload.pas',
  ShellAPI,
  Classes,
  DiaryInterface in 'src\frontend\DiaryInterface.pas',
  TextInterface in 'src\frontend\TextInterface.pas',
  AnalyzeGraphic in 'src\frontend\AnalyzeGraphic.pas',
  MainUnit in 'src\frontend\forms\MainUnit.pas' {Form1},
  UnitSettings in 'src\frontend\forms\UnitSettings.pas' {FormSettings},
  UnitEditDish in 'src\frontend\forms\UnitEditDish.pas' {FormDish},
  UnitEditFood in 'src\frontend\forms\UnitEditFood.pas' {FormFood},
  UnitAbout in 'src\frontend\forms\UnitAbout.pas' {FormAbout},
  UnitFirstMan in 'src\frontend\forms\UnitFirstMan.pas' {FormFirstMan},
  UnitExportText in 'src\frontend\forms\UnitExportText.pas' {FormExportText},
  UnitReserve in 'src\frontend\forms\UnitReserve.pas' {FormSync},
  UnitShadow in 'src\frontend\forms\UnitShadow.pas' {FormShadow},
  UnitStartup in 'src\frontend\forms\UnitStartup.pas' {FormProcess},
  UnitResources in 'src\frontend\forms\UnitResources.pas' {FormResources},
  UnitDataInterface in 'src\frontend\forms\UnitDataInterface.pas' {DataInterface: TDataModule},
  SettingsINI in 'src\common\SettingsINI.pas',
  DiaryCore in 'src\common\DiaryCore.pas',
  ThreadTask in 'src\common\ThreadTask.pas',
  AutoLog in 'src\common\AutoLog.pas',
  AnalyzeInterface in 'src\backend\features\analyze\AnalyzeInterface.pas',
  DiaryDatabase in 'src\persistence\DiaryDatabase.pas',
  DiaryWeb in 'src\persistence\web\DiaryWeb.pas',
  DiarySync in 'src\backend\features\sync\DiarySync.pas',
  DiaryDAO in 'src\backend\features\diary\DiaryDAO.pas',
  DiaryLocalSource in 'src\persistence\local\DiaryLocalSource.pas',
  BusinessObjects in 'src\bo\BusinessObjects.pas',
  Bases in 'src\persistence\Bases.pas',
  DiaryRecords in 'src\bo\DiaryRecords.pas',
  DiaryRoutines in 'src\common\DiaryRoutines.pas',
  DiaryAnalyze in 'src\backend\features\analyze\DiaryAnalyze.pas',
  DiaryWebSource in 'src\persistence\web\DiaryWebSource.pas',
  DiaryPageSerializer in 'src\backend\features\diary\DiaryPageSerializer.pas',
  DiaryPage in 'src\bo\DiaryPage.pas',
  DiaryPageSerializerTest in 'src\test\DiaryPageSerializerTest.pas',
  UnitMisc in 'src\frontend\forms\UnitMisc.pas' {FormMisc},
  FoodbaseDAO in 'src\persistence\FoodbaseDAO.pas',
  FoodbaseLocalDAO in 'src\persistence\local\FoodbaseLocalDAO.pas',
  FoodbaseWebDAO in 'src\persistence\web\FoodbaseWebDAO.pas',
  JsonSerializer in 'src\persistence\serializers\JsonSerializer.pas',
  ObjectService in 'src\persistence\ObjectService.pas',
  DishbaseDAO in 'src\persistence\DishbaseDAO.pas',
  DishbaseLocalDAO in 'src\persistence\local\DishbaseLocalDAO.pas',
  DishbaseWebDAO in 'src\persistence\web\DishbaseWebDAO.pas',
  UnitEditor in 'src\frontend\forms\UnitEditor.pas',
  UnitEditorBlood in 'src\frontend\forms\UnitEditorBlood.pas' {FormEditorBlood},
  UnitEditorIns in 'src\frontend\forms\UnitEditorIns.pas' {FormEditorIns},
  UnitEditorMeal in 'src\frontend\forms\UnitEditorMeal.pas' {FormEditorMeal},
  UnitEditorNote in 'src\frontend\forms\UnitEditorNote.pas' {FormEditorNote},
  UnitLogViewer in 'src\frontend\forms\UnitLogViewer.pas' {FormLogViewer},
  JsonVersionedSerializer in 'src\persistence\serializers\JsonVersionedSerializer.pas',
  HashService in 'src\backend\features\sync\HashService.pas',
  MerkleTree in 'src\backend\features\sync\MerkleTree.pas',
  Analyzer in 'src\backend\features\analyze\Analyzer.pas',
  AnalyzeUtils in 'src\backend\features\analyze\AnalyzeUtils.pas',
  SelfUpdate in 'src\backend\features\selfUpdate\SelfUpdate.pas';

{$R *.res}

  procedure CheckFolders();
  begin
    WORK_FOLDER := ExtractFilePath(Application.ExeName);
    if not DirectoryExists(WORK_FOLDER + FOLDER_BASES) then
      CreateDirectory(PChar(WORK_FOLDER + FOLDER_BASES), nil);
  end;

  function IsWin64(): Boolean;
  var
    IsWow64Process: function(hProcess : THandle; var Wow64Process : BOOL): BOOL; stdcall;
    Wow64Process: BOOL;
  begin
    Result := False;
    IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');
    if Assigned(IsWow64Process) then
    begin
      if IsWow64Process(GetCurrentProcess, Wow64Process) then
      begin
        Result := Wow64Process;
      end;
    end;
  end;

  function CheckRunningInstance(): boolean;
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
    // Delphi doesn't allow direct title change, so we have to wrap it into method:
    // http://www.delphikingdom.com/asp/viewitem.asp?catalogid=765
    App.Title := APPLICATION_TITLE;
  end;

const
  BIT64VER: array[Boolean] of String = ('32 bit', '64 bit');
  FOLDER_LOGS = 'Logs';
var
  tick: Cardinal;
begin
  tick := GetTickCount();

  if CheckRunningInstance() then
    Exit;

  CheckFolders(); // before check file!
  AutoLog.StartLogger(WORK_FOLDER + FOLDER_LOGS);

  {#}Log(INFO, 'Application started');
  {#}Log(INFO, Format('OS Version: %d.%d, %s', [Win32MajorVersion, Win32MinorVersion, BIT64VER[IsWin64()]]));

  { TODO: ПРОВЕРКА НАЛИЧИЯ ВСЕХ НЕОБХОДИМЫХ ФАЙЛОВ }

  DiaryCore.Initialize();

  { пре-исполнение (LoadSettings не использует WORK_FOLDER)}
  LoadSettings();
  try
    LoadStringResources('strings_ru.txt');
  except
    on E: Exception do
    begin
      Log(ERROR, E.Message);
      ErrorMessage('Некоторые строковые ресурсы отсутствуют');
    end;
  end;

  { общая инициализация } 
  Application.Initialize();
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
  Application.CreateForm(TFormLogViewer, FormLogViewer);
  {#}Application.CreateForm(TFormSync, FormSync);
  {#}Application.CreateForm(TFormResources, FormResources);
  Application.CreateForm(TDataInterface, DataInterface);

  Log(INFO, 'Время инициализации: ' + IntToStr(GetTickCount - tick));

  if (Value['FirstStart'] = True) then
  begin
    Value['FirstStart'] := False;
    SaveSettings();
    // Application.CreateForm(TFormFirstMan, FormFirstMan);
    // FormFirstMan.ShowModal();
  end;

  Form1.FullInit();

  Application.Run();

  DiaryCore.Finalize();
  AutoLog.StopLogger();
end.

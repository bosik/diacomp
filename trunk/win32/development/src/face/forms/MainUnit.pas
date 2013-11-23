unit MainUnit;

{$R+ O+}

{ Основной модуль }

interface

uses
  // системное, компоненты

  Windows,
  SysUtils,
  ImgList,
  Controls,
  Messages,
  Menus, ExtCtrls,
  ComCtrls, StdCtrls, Grids, ValEdit, Graphics, Buttons,
  Forms, Classes, Dialogs, Calendar, Mask, Spin,
  XPMan, ActnPopupCtrl, CoolTrayIcon, ActnList, XPStyleActnCtrls,
  ActnMan, ToolWin, ActnCtrls, ActnMenus, ShellAPI,

  // формы

  UnitFirstMan,
  UnitStartup,
  UnitDEditor,
  UnitEditDish,
  UnitEditFood,
  UnitSettings,
  UnitExportText,
  UnitAbout,

  // данные и компоненты
  UnitDataInterface,

  // интерфейс
  DiaryInterface,
  TextInterface,
  DiaryView,
  ACCombo,

  // утилиты
  DiaryRoutines,
  Math, // после DiaryRoutines (см.Max)
  Statistics,
  InetDownload,
  AutoLog,
  SettingsINI,

  // ядро
  BusinessObjects,
  DiaryRecords,
  DiaryCore {#},
  DiaryDatabase,
  DiaryPage,
  Bases,
  DiaryLocalSource,
  DiaryWeb, //
  DiarySync, //
  DiaryAnalyze,
  AnalyzeGraphic,
  AnalyzeInterface,

  // прочее
  ThreadExecutor;

const
  WM_SECOND_START = WM_USER + 790;

type
  TBalloonAction = procedure of object;

  TForm1 = class(TAutosetupForm)
    TimerTimeLeft: TTimer;
    MenuHidden: TMainMenu;
    MenuHiddenCuts: TMenuItem;
    MenuItem_ShowDiaryPage: TMenuItem;
    MenuItem_ShowBasePage: TMenuItem;
    MenuItem_ShowGraphicPage: TMenuItem;
    StatusBar: TStatusBar;
    MenuItem_ShowTodolistPage: TMenuItem;
    MenuItem_SaveToDoList: TMenuItem;
    ActionManager: TActionManager;
    ActionSettings: TAction;
    ActionSync: TAction;
    MainMenu: TActionMainMenuBar;
    ActionAddBlood: TAction;
    ActionAddIns: TAction;
    ActionAddMeal: TAction;
    ActionAddNote: TAction;
    ActionExportDiary: TAction;
    ActionPrint: TAction;
    ActionExit: TAction;
    ActionAbout: TAction;
    ActionHelp: TAction;
    PanelMain: TPanel;
    PageControl1: TPageControl;
    TabDiary: TTabSheet;
    Splitter4: TSplitter;
    PanelDiaryTop: TPanel;
    ScrollBoxDiary: TScrollBox;
    DiaryView: TDiaryView;
    PanelDiaryBottom: TPanel;
    BevelRecRight: TBevel;
    PanelAdd: TPanel;
    ButtonAddBlood: TSpeedButton;
    ButtonAddIns: TSpeedButton;
    ButtonAddMeal: TSpeedButton;
    PanelDiaryTopLeft: TPanel;
    ShapeLeft1: TShape;
    ShapeLeft2: TShape;
    ShapeDivImageBS: TShape;
    GroupBoxStatistic: TGroupBox;
    LabelDayMass: TLabel;
    LabelDayValue: TLabel;
    LabelDayCarbs_: TLabel;
    LabelDayFats_: TLabel;
    LabelDayProts_: TLabel;
    ImageMinMaxStat: TImage;
    LabelDayIns: TLabel;
    StatProgProts: TStatProgress;
    StatProgFats: TStatProgress;
    StatProgCarbs: TStatProgress;
    GroupBoxGraph: TGroupBox;
    ImageMinMaxGraph: TImage;
    ImagePreview: TImage;
    CalendarDiary: TMonthCalendar;
    GroupBoxTimeLeft: TGroupBox;
    LabelDiaryTimeLeftInsVal: TLabel;
    LabelDiaryTimeLeftMealVal: TLabel;
    PanelDiaryDish: TPanel;
    ShapeRight3: TShape;
    ShapeRight2: TShape;
    GroupBoxMeal: TGroupBox;
    LabelDiaryMealProts: TLabel;
    LabelDiaryMealCarbs: TLabel;
    LabelDiaryMealFats: TLabel;
    LabelDiaryMealValue: TLabel;
    LabelDiaryMealMass: TLabel;
    CorrectCarbs: TGroupBox;
    PanelCorrect: TPanel;
    ListCorrectCarbs: TListView;
    GroupBoxDose: TGroupBox;
    LabelDiaryMealDose: TLabel;
    LabelDiaryCorrection: TLabel;
    TabBase: TTabSheet;
    SplitterBase: TSplitter;
    PanelBaseFood: TPanel;
    Shape10: TShape;
    LabelFoodBase: TLabel;
    PanelFoodButtons: TPanel;
    ButtonCreateFood: TSpeedButton;
    ButtonDeleteFood: TSpeedButton;
    PanelBaseDish: TPanel;
    Shape11: TShape;
    PanelDishHeader: TPanel;
    LabelDishBase: TLabel;
    PanelDishButtons: TPanel;
    ButtonCreateDish: TSpeedButton;
    ButtonDeleteDish: TSpeedButton;
    TabAnalyze: TTabSheet;
    PanelAnManagment: TPanel;
    ShapeAn1: TShape;
    TheKoofsGroupBox: TGroupBox;
    LabelAnCompValue: TLabel;
    GroupBoxInfo: TGroupBox;
    PanelImageLarge: TPanel;
    ImageLarge: TImage;
    TabStat: TTabSheet;
    GroupBoxBSHistory: TGroupBox;
    ButtonBSHistory: TButton;
    GroupBoxCarbsHistory: TGroupBox;
    ListCB: TListBox;
    ActionExportKoofs: TAction;
    ActionExportFood: TAction;
    BevelTop: TBevel;
    ShapeTop: TShape;
    ButtonBSList: TButton;
    ListBS: TListBox;
    ButtonAvgBSDynamic: TButton;
    ComboValue: TComboBox;
    ListFood: TListView;
    ListDish: TListView;
    TrayIcon: TCoolTrayIcon;
    ActionBalanceOnOff: TAction;
    ActionSettingsStat: TAction;
    ActionEditBlood: TAction;
    ActionEditIns: TAction;
    ActionEditMeal: TAction;
    ActionRemovePanel: TAction;
    ActionEditNote: TAction;
    ActionBalanceMeal: TAction;
    ActionEditFood: TAction;
    ActionCalcMass: TAction;
    ActionDeltaMass: TAction;
    ActionRemoveFood: TAction;
    ActionShowWindow: TAction;
    ActionHideWindow: TAction;
    ActionIsMinToTray: TAction;
    ActionCheckUpdate: TAction;
    ActionSettingsImage: TAction;
    ActionSettingsAnalyze: TAction;
    PopupFoodBase: TPopupActionBarEx;
    Item_EditFood: TMenuItem;
    item_RemoveFood: TMenuItem;
    PopupDishBase: TPopupActionBarEx;
    Item_EditDish: TMenuItem;
    Item_RemoveDish: TMenuItem;
    ItemCopyDish: TMenuItem;
    Item_SepDish: TMenuItem;
    PopupFoodCol: TPopupActionBarEx;
    Item_FoodP: TMenuItem;
    Item_FoodF: TMenuItem;
    Item_FoodC: TMenuItem;
    Item_FoodV: TMenuItem;
    PopupDishCol: TPopupActionBarEx;
    Item_DishP: TMenuItem;
    Item_DishF: TMenuItem;
    Item_DishC: TMenuItem;
    Item_DishV: TMenuItem;
    Item_DishD: TMenuItem;
    Item_DishM: TMenuItem;
    ActionShortMeal: TAction;
    ButtonInsList: TButton;
    ActionMoveMealBack: TAction;
    ActionMoveMealForward: TAction;
    ItemCopyFood: TMenuItem;
    Item_SepFood: TMenuItem;
    TimerAutosave: TTimer;
    ThreadExec_: TThreadExecutor;
    EditBaseFoodSearch: TEdit;
    ButtonAnList: TButton;
    GroupBoxAdd: TGroupBox;
    ButtonDiaryNewAddFood: TSpeedButton;
    ComboDiaryNew: TACComboBox;
    EditDiaryNewMass: TEditNumb;
    LabelDiaryTimeLeftMeal: TLabel;
    LabelDiaryTimeLeftIns: TLabel;
    LabelDiaryFinger: TLabel;
    LabelDiaryFingerVal: TLabel;
    LabelCorrectionEmpty: TLabel;
    ImageMinMaxTimes: TImage;
    LabelDayProtsVal: TLabel;
    LabelDayFatsVal: TLabel;
    LabelDayCarbsVal: TLabel;
    ImageNote: TImage;
    ButtonAvgBS: TBitBtn;
    ButtonInsulinCalc: TButton;
    Shape1: TShape;
    ComboAnalyzers: TComboBox;
    ComboKoof: TComboBox;
    LabelKoofDiscription: TLabel;
    ButtonConfigKoof: TButton;
    ButtonUpdateKoof: TButton;
    Label2: TLabel;
    Label3: TLabel;
    LabelDiaryMealExpectedBS: TLabel;
    LabelCalcTime: TLabel;
    LabelAvgDeviation: TLabel;
    LabelWeight: TLabel;
    PanelDevelopment: TPanel;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCreateFoodClick(Sender: TObject);
    procedure ButtonDeleteFoodClick(Sender: TObject);
    procedure EditKeyProtect(Sender: TObject; var Key: Char);
    procedure EditDiaryNewMassKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonCreateDishClick(Sender: TObject);
    procedure CalendarDiaryChange(Sender: TObject);
    procedure SplitterBaseCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure ButtonDiaryNewAddClick(Sender: TObject);

    procedure TimerTimeLeftTimer(Sender: TObject);
    procedure ImageMinMaxStatMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonDeleteDishClick(Sender: TObject);
    procedure TableFoodBaseKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboDiaryNewCloseUp(Sender: TObject);
    procedure ListDishDblClick(Sender: TObject);
    procedure ListDishKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ImageMinMaxGraphMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBoxDiaryMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Splitter4Moved(Sender: TObject);
    procedure ListFoodDblClick(Sender: TObject);
    procedure MenuItem_ShowDiaryPageClick(Sender: TObject);
    procedure MenuItem_ShowBasePageClick(Sender: TObject);
    procedure MenuItem_ShowGraphicPageClick(Sender: TObject);
    procedure ButtonBSHistoryClick(Sender: TObject);
    procedure ListBSDblClick(Sender: TObject);
    procedure TimerAutosaveTimer(Sender: TObject);
    procedure ResetTabStop(Sender: TObject);

    { дневник }
    procedure DiaryViewPage(Sender: TObject);

    procedure DiaryViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DiaryViewDoubleClickIns(Sender: TObject; Index: Integer;
      Place: TClickPlace);
    procedure DiaryViewDoubleClickMeal(Sender: TObject; Index: Integer;
      Place: TClickPlace);
    procedure DiaryViewDoubleClickBlood(Sender: TObject; Index: Integer;
      Place: TClickPlace);

    procedure ImagePreviewDblClick(Sender: TObject);
    procedure DiaryViewFoodShow(Sender: TObject; Index, Line: Integer;
      var Text: String);
    procedure ComboKoofChange(Sender: TObject);
    procedure ButtonUpdateKoofClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure DiaryViewDoubleClickNote(Sender: TObject; Index: Integer;
      Place: TClickPlace);
    procedure ButtonAvgBSClick(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionAddRecordExecute(Sender: TObject);
    procedure ActionExportDiaryExecute(Sender: TObject);
    procedure ActionSyncExecute(Sender: TObject);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionExportKoofsExecute(Sender: TObject);
    procedure ActionExportFoodExecute(Sender: TObject);
    procedure PanelDiaryTopLeftDblClick(Sender: TObject);
    procedure ListFoodKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonBSListClick(Sender: TObject);
    procedure ButtonAvgBSDynamicClick(Sender: TObject);
    procedure ComboValueChange(Sender: TObject);
    procedure TrayIconBalloonHintClick(Sender: TObject);
    procedure ActionBalanceOnOffExecute(Sender: TObject);
    procedure ActionSettingsStatExecute(Sender: TObject);
    procedure ActionEditBloodExecute(Sender: TObject);
    procedure ActionEditMealExecute(Sender: TObject);
    procedure ActionEditInsExecute(Sender: TObject);
    procedure ActionRemovePanelExecute(Sender: TObject);
    procedure ActionEditNoteExecute(Sender: TObject);
    procedure ActionBalanceMealExecute(Sender: TObject);
    procedure ActionEditFoodExecute(Sender: TObject);
    procedure ActionCalcMassExecute(Sender: TObject);
    procedure ActionDeltaMassExecute(Sender: TObject);
    procedure ActionRemoveFoodExecute(Sender: TObject);
    procedure ActionShowWindowExecute(Sender: TObject);
    procedure ActionHideWindowExecute(Sender: TObject);
    procedure ActionIsMinToTrayExecute(Sender: TObject);
    procedure ActionCheckUpdateExecute(Sender: TObject);
    procedure ActionSettingsImageExecute(Sender: TObject);
    procedure ActionSettingsAnalyzeExecute(Sender: TObject);
    procedure ItemCopyDishClick(Sender: TObject);
    procedure ListBaseColumnRightClick(Sender: TObject;
      Column: TListColumn; Point: TPoint);
    procedure ListBaseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Item_BaseColClick(Sender: TObject);
    procedure ActionShortMealExecute(Sender: TObject);
    procedure ButtonInsListClick(Sender: TObject);
    procedure ActionMoveMealForwardExecute(Sender: TObject);
    procedure ActionMoveMealBackExecute(Sender: TObject);
    procedure ItemCopyFoodClick(Sender: TObject);
    procedure ButtonAnListClick(Sender: TObject);
    procedure ComboDiaryNewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ThreadExec_Done(Sender: TObject; TaskID: Cardinal);
    procedure ThreadExec_TimeOut(Sender: TObject; TaskID: Cardinal);
    procedure ComboDiaryNewDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ListBaseMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DiaryViewChange(Sender: TObject; EventType: TPageEventType;
      Page: TDiaryPage; RecClass: TClassCustomRecord;
      RecInstance: TCustomRecord);
    procedure FormResize(Sender: TObject);
    procedure SplitterBaseMoved(Sender: TObject);
    procedure ImageMinMaxTimesMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ButtonInsulinCalcClick(Sender: TObject);
    procedure PanelDevelopmentClick(Sender: TObject);
  protected
    // определяет расположение компонентов интерфейса
    procedure Designer; override;
  private
    DiaryFoodDishInput: string;

    { события }
    BalloonAction: TBalloonAction;

    procedure MyIdle(Sender: TObject; var Done: Boolean);
    procedure MyActivate(Sender: TObject);
    procedure MyDeactivate(Sender: TObject);
    procedure ProcMessage(var M: TMessage); message WM_SECOND_START;

    procedure BalloonAction_ShowForm;
    procedure BalloonAction_StartUpdate;
    procedure BalloonAction_ShowInternetSettings;
  public                          
    procedure FullInit;
    procedure FullFree;
    procedure ShowMessages;
    procedure LoadPictures;

    { =========== РЕДАКТИРОВАНИЕ =========== }

    { дневник }
    function ClickBlood(New: boolean; Focus: TFocusMode): integer;
    function ClickIns(New: boolean; Focus: TFocusMode): integer;
    function ClickMeal(New: boolean): integer;
    function ClickNote(New: boolean; Focus: TFocusMode): integer;

    procedure MoveMeal(Delta: integer);

    { базы }
    function FoodEditorRect: TRect;
    function DishEditorRect: TRect;

    procedure CreateFood;
    procedure CreateDish;
    procedure EditFood(Index: integer);
    procedure EditDish(Index: integer);
    procedure RemoveFood(Index: integer);
    procedure RemoveDish(Index: integer);

    { отображение }
    procedure ProcessMealSelected(Value: boolean);
    procedure ScrollToSelected;

    { Информационные компоненты }
    procedure UpdateDayInfo;
    procedure UpdateTimeLeft;
    procedure UpdateNextFinger;
    procedure UpdateMealStatistics;
    procedure UpdateMealDose;
    procedure UpdateCombos;
    procedure UpdateFoodTable(FullUpdate: boolean; SaveItemIndex: boolean = False);
    procedure UpdateDishTable(FullUpdate: boolean; SaveItemIndex: boolean = False);

    { Информационные события }
    procedure EventFoodbaseChanged(CountChanged: boolean);
    procedure EventDishbaseChanged(CountChanged, NamesChanged: boolean);
    procedure EventDiaryChanged;
    procedure EventPageChanged;
    procedure EventKoofChanged;
    // TODO: не могу создать то же самое для настроек (потеряю точность, что именно изменилось)

    procedure ApplyInterfaceSettings;

    { анализ }

    procedure UpdateKoofs;
    function GetCompensationMass(const RelCarbs, RelProts: real): real;
    function GetBestMass(const RelCarbs, RelProts, CurMass: real): real;

    procedure ShowBalloon(const Msg: string; MsgType: TBalloonHintIcon; AfterAction: TBalloonAction = nil);
  end;

  { обновление }
  procedure SetupUpdate(UserInformed: boolean; const ExpVersion: string);

var
  { ========================== И Н Т Е Р Ф Е Й С ============================= }
  Form1: TForm1;

  CurrentDB: real;       // текущая коррекция СК
  CurrentNextFinger: integer;
  DiaryMultiMap: TMultimap;

  { ============================ П Р О Ч Е Е ================================= }

  { Idle-задачи }
  IgnoreIdleOnce:       boolean = False;
  ON_IDLE_ShowBases:    boolean = False;
  ON_IDLE_UpdateCombos: boolean = False;
  //ON_IDLE_UpdateKoofs:  boolean = False;
  IN_IDLE_CheckUpdates: boolean = False;

const
  { Интерфейс }
  PANEL_OPEN_TIME    = 250;
  PANEL_CLOSE_TIME   = 250;
  //GOODNIGHT_INTERVAL = 1/24; { час }

  { цвета коррекции }
  CC_NONE   = clSilver;
  CC_LOW    = clSilver;
  CC_MIDDLE = clMaroon;
  CC_HIGH   = clRed;

  { пути к картинкам }
  FOLDER_BUTTONS          = 'Images\Buttons\';
    IMAGE_DIARY_NEW_BLOOD = FOLDER_BUTTONS + 'Diary_New_Blood.bmp';
    IMAGE_DIARY_NEW_INS   = FOLDER_BUTTONS + 'Diary_New_Ins.bmp';
    IMAGE_DIARY_NEW_MEAL  = FOLDER_BUTTONS + 'Diary_New_Meal.bmp';
    IMAGE_DIARY_NEW_NOTE  = FOLDER_BUTTONS + 'Diary_New_Note.bmp';
    IMAGE_PANEL_MIN       = FOLDER_BUTTONS + 'Panel_Min.bmp';
    IMAGE_PANEL_MAX       = FOLDER_BUTTONS + 'Panel_Max.bmp';
    IMAGE_EDITOR_ADD      = FOLDER_BUTTONS + 'Editor_Add.bmp';
    IMAGE_EDITOR_REMOVE   = FOLDER_BUTTONS + 'Editor_Remove.bmp';
    IMAGE_EDITOR_REPLACE  = FOLDER_BUTTONS + 'Editor_Replace.bmp';
    IMAGE_EDITOR_CALC     = FOLDER_BUTTONS + 'Editor_Calc.bmp';
    IMAGE_BASE_NEW_FOOD   = FOLDER_BUTTONS + 'Base_New_Food.bmp';
    IMAGE_BASE_NEW_DISH   = FOLDER_BUTTONS + 'Base_New_Dish.bmp';
    IMAGE_BASE_REMOVE     = FOLDER_BUTTONS + 'Base_Remove.bmp';
    IMAGE_EXPORT_SAVE     = FOLDER_BUTTONS + 'Export_Save.bmp';

    IMAGE_SETTINGS_PRIVATE   = FOLDER_BUTTONS + 'Settings_Private.bmp';
    IMAGE_SETTINGS_ANALYZE   = FOLDER_BUTTONS + 'Settings_Analyze.bmp';
    IMAGE_SETTINGS_INTERFACE = FOLDER_BUTTONS + 'Settings_Interface.bmp';
    IMAGE_SETTINGS_IMAGE     = FOLDER_BUTTONS + 'Settings_Image.bmp';
    IMAGE_SETTINGS_NORMS     = FOLDER_BUTTONS + 'Settings_Norms.bmp';
    IMAGE_SETTINGS_INTERNET  = FOLDER_BUTTONS + 'Settings_Internet.bmp';

    IMAGE_SYNC_TEST      = FOLDER_BUTTONS + 'Sync_Test.bmp';
    IMAGE_SYNC_UPDATE    = FOLDER_BUTTONS + 'Sync_Update.bmp';

  FOLDER_ICONS          = 'Images\Icons\';
    IMAGE_MAIN_TABS     = FOLDER_ICONS + 'Main_Tabs.bmp';
    IMAGE_BASES_TABS    = FOLDER_ICONS + 'Bases_Tabs.bmp';
    IMAGE_SETTINGS_TABS = FOLDER_ICONS + 'Settings_Tabs.bmp';
    IMAGE_MENU          = FOLDER_ICONS + 'Menu.bmp';
    IMAGE_DC_BLOOD      = FOLDER_ICONS + 'Diary_Context_Blood.bmp';
    IMAGE_DC_INS        = FOLDER_ICONS + 'Diary_Context_Ins.bmp';
    IMAGE_DC_MEAL       = FOLDER_ICONS + 'Diary_Context_Meal.bmp';
    IMAGE_DC_DISH       = FOLDER_ICONS + 'Diary_Context_Dish.bmp';
    IMAGE_DC_NOTE       = FOLDER_ICONS + 'Diary_Context_Note.bmp';
    IMAGE_BASE_CONTENT  = FOLDER_ICONS + 'Base_Content.bmp';

  FOLDER_PICTURES      = 'Images\Pictures\';
    IMAGE_DIARY_TIME   = FOLDER_PICTURES + 'Diary_Time.bmp';
    IMAGE_DIARY_FOOD   = FOLDER_PICTURES + 'Diary_Food.bmp';
    IMAGE_DIARY_DISH   = FOLDER_PICTURES + 'Diary_Dish.bmp';
    IMAGE_DIARY_BLOOD  = FOLDER_PICTURES + 'Diary_Blood.bmp';

    IMAGE_SYNC_UNKNOW              = FOLDER_PICTURES + 'Sync_Unknow.png';
    IMAGE_SYNC_TESTING             = FOLDER_PICTURES + 'Sync_Test.png';
    IMAGE_SYNC_EQUAL               = FOLDER_PICTURES + 'Sync_Equal.png';
    IMAGE_SYNC_FLASH_COMP          = FOLDER_PICTURES + 'Sync_FlashToComp.png';
    IMAGE_SYNC_FLASH_COMP_PROCESS  = FOLDER_PICTURES + 'Sync_FlashToComp_Process.png';
    IMAGE_SYNC_COMP_FLASH          = FOLDER_PICTURES + 'Sync_CompToFlash.png';
    IMAGE_SYNC_COMP_FLASH_PROCESS  = FOLDER_PICTURES + 'Sync_CompToFlash_Process.png';
    IMAGE_SYNC_DOUBLE_SYNC         = FOLDER_PICTURES + 'Sync_DoubleSync.png';
    IMAGE_SYNC_DOUBLE_SYNC_PROCESS = FOLDER_PICTURES + 'Sync_DoubleSync_Process.png';
    IMAGE_SYNC_WRONG_PATH          = FOLDER_PICTURES + 'Sync_WrongPath.png';

  { Задачи (в потоках) }
  TASK_LOGIN       = 1;
  TASK_SAVE_N_SYNC = 2;

implementation

uses UnitMisc;

{$R *.dfm}

{ TForm1 }

(*
{==============================================================================}
procedure SyncProgress(P: integer);
{==============================================================================}
begin
  Form1.StatusBar.Panels[1].Text := Format(STATUS_SYNC, [P]);//'Синхронизация ' + IntToStr(p) + '%';
  FormProcess.LabelHint.Caption := Format(STATUS_SYNC, [P]);//'Синхронизация ' + IntToStr(p) + '%';
  Application.ProcessMessages;
end;
*)

{==============================================================================}
function MySyncDiary(Terminated: TBooleanFunction = nil): integer;
{==============================================================================}
var
  T: TDateTime;
begin
  StartProc('MySyncDiary');

  try
    { дневник }
    Form1.StatusBar.Panels[3].Text := STATUS_ACTION_SYNC_DIARY;
    Application.ProcessMessages;

    T := StrToDateTime(Value['LastSync']);
    Result := SyncSources(LocalSource, WebSource, T - 1);

    Value['LastSync'] := DateTimeToStr(Now);

    if (Result > 0) then
    begin
      Form1.DiaryView.OpenPage(Diary[Trunc(Form1.CalendarDiary.Date)], True);
      Form1.UpdateNextFinger;
    end;

    { база продуктов }
    Form1.StatusBar.Panels[3].Text := STATUS_ACTION_SYNC_FOODBASE;
    Application.ProcessMessages;
    if (SyncFoodbase() = srDownloaded) then
    begin
      Form1.EventFoodbaseChanged(True);
    end;

    { база блюд }
    Form1.StatusBar.Panels[3].Text := STATUS_ACTION_SYNC_DISHBASE;
    Application.ProcessMessages;
    if (SyncDishbase() = srDownloaded) then
    begin
      Form1.EventDishbaseChanged(True, True);
    end;
    
    { готово }
    Form1.StatusBar.Panels[3].Text := STATUS_SYNC_DONE;
    Application.ProcessMessages;
  except
    on E: Exception do
    begin
      Log(ERROR, 'Exception in MySyncDiary(): ' + E.Message, True);
      Form1.StatusBar.Panels[3].Text := 'Ошибка синхронизации';
      Application.ProcessMessages;
      Form1.ShowBalloon('Во время синхронизации произошла ошибка', bitError);
    end;
  end;

  FinishProc;
end;

{==============================================================================}
procedure TaskSaveAndSync(Terminated: TBooleanFunction);
{==============================================================================}
begin
  StartProc('TaskSaveAndSync');
  MySyncDiary(Terminated);
  FinishProc;
end;

{==============================================================================}
procedure TForm1.Designer;
{==============================================================================}
const
  ADD_BUTTON_SIZE = 60;
  BASE_BUTTON_HEIGHT = 40;
  BASE_BUTTON_WIDTH  = 160;
var
  t: integer;
  FoodDishBord: integer;
begin
  StartProc('Designer');

  DGroup := LabelDayProts_.Height div 2;

  // разделители
  ShapeLeft1.Height := BORD;
  ShapeLeft2.Height := BORD;
  ShapeDivImageBS.Height := BORD;
  ShapeRight2.Height := BORD;
  ShapeRight3.Height := BORD;
  ShapeAn1.Height := BORD;
  Shape10.Height := BORD;
  Shape11.Height := BORD;

  // Бордеры панелей
  PanelDiaryTopLeft.BorderWidth := BORD;
  PanelDiaryDish.BorderWidth := BORD;
  PanelBaseFood.BorderWidth := BORD;
  PanelBaseDish.BorderWidth := BORD;
  PanelAnManagment.BorderWidth := BORD;

  { ================ ЛЕВАЯ ЧАСТЬ ================ }

  // кнопки свёртывания
  ImageMinMaxStat.Left := GroupBoxStatistic.Width - ImageMinMaxStat.Width-(ImageMinMaxStat.Height div 2);
  ImageMinMaxGraph.Left := GroupBoxStatistic.Width - ImageMinMaxStat.Width-(ImageMinMaxStat.Height div 2);
  ImageMinMaxTimes.Left := GroupBoxStatistic.Width - ImageMinMaxStat.Width-(ImageMinMaxStat.Height div 2);

  // Панель "Статистика"
  LabelDayValue.Left := 2 * BORD;
  LabelDayMass.Left := 2 * BORD;
  LabelDayIns.Left := 2 * BORD;
  StatProgProts.Left := 2 * BORD;
  StatProgFats.Left := 2 * BORD;
  StatProgCarbs.Left := 2 * BORD;
  GroupBoxStatistic.Height := LabelDayIns.Top + 2 * LabelDayIns.Height;

  // Панель "Дополнительно"
  //LabelDiaryTimeLeftInsVal.Caption := '00:00';
  //LabelDiaryTimeLeftMealVal.Caption := '00:00';
  //LabelDiaryFingerVal.Caption := 'XX';

  t := GroupBoxTimeLeft.Width - LabelDiaryTimeLeftInsVal.Width - 2*BORD;

  LabelDiaryTimeLeftMeal.Left := 2*Bord;
  LabelDiaryTimeLeftIns.Left := 2*BORD;
  LabelDiaryFinger.Left := 2*BORD;
  LabelDiaryTimeLeftMealVal.Left := t;
  LabelDiaryTimeLeftInsVal.Left := t;
  LabelDiaryFingerVal.Left := GroupBoxTimeLeft.Width - LabelDiaryFingerVal.Width - 2*BORD;
  GroupBoxTimeLeft.Height := 3*LabelDiaryTimeLeftInsVal.Height + 5*BORD;

  { ================ ПРАВАЯ ЧАСТЬ ================ }
  LabelDiaryMealProts.Left := 2 * BORD;
  LabelDiaryMealFats.Left := 2 * BORD;
  LabelDiaryMealCarbs.Left := 2 * BORD;
  LabelDiaryMealValue.Left := 2 * BORD;
  LabelDiaryMealMass.Left := 2 * BORD;

  { ================ НИЖНЯЯ ЧАСТЬ ================ }

  // Кнопки добавления
  ButtonAddBlood.Width := ADD_BUTTON_SIZE;
  ButtonAddBlood.Height := ADD_BUTTON_SIZE;
  ButtonAddBlood.Left := BORD;
  ButtonAddBlood.Top := BORD;

  ButtonAddIns.Width := ADD_BUTTON_SIZE;
  ButtonAddIns.Height := ADD_BUTTON_SIZE;
  ButtonAddIns.Left := 2*BORD+ADD_BUTTON_SIZE;
  ButtonAddIns.Top := BORD;

  ButtonAddMeal.Width := ADD_BUTTON_SIZE;
  ButtonAddMeal.Height := ADD_BUTTON_SIZE;
  ButtonAddMeal.Left := 3*BORD+2*ADD_BUTTON_SIZE;
  ButtonAddMeal.Top := BORD;

  PanelAdd.Width := 3*ADD_BUTTON_SIZE+4*BORD;

  // Панель ввода продуктов/блюд
  FoodDishBord := 2 * BORD;

  ComboDiaryNew.Top := DGroup + FoodDishBord;
  EditDiaryNewMass.Top := DGroup + FoodDishBord;
  ButtonDiaryNewAddFood.Top := DGroup + FoodDishBord;

  EditDiaryNewMass.Width := MASS_EDIT_WIDTH;
  t := GroupBoxAdd.Width;
  dec(t, BORD + ButtonDiaryNewAddFood.Width);
  ButtonDiaryNewAddFood.Left := t;
  dec(t, BORD + MASS_EDIT_WIDTH);
  EditDiaryNewMass.Left := t;
  dec(t, BORD);

  ComboDiaryNew.Left := 2 * BORD;
  ComboDiaryNew.Width := t - ComboDiaryNew.Left;
  PanelDiaryBottom.Height := Max(
    (ComboDiaryNew.Height + 2 * BORD + DGroup) + 2 * BORD,
    ADD_BUTTON_SIZE + 2 * BORD
  );

  // статус-бар
  StatusBar.Panels[0].Width := PanelDiaryTopLeft.Width+5;

  { БАЗЫ }
  ButtonCreateFood.Height := BASE_BUTTON_HEIGHT;
  ButtonDeleteFood.Height := BASE_BUTTON_HEIGHT;
  ButtonCreateDish.Height := BASE_BUTTON_HEIGHT;
  ButtonDeleteDish.Height := BASE_BUTTON_HEIGHT;

  ButtonCreateFood.Width := BASE_BUTTON_WIDTH;
  ButtonDeleteFood.Width := BASE_BUTTON_WIDTH;
  ButtonCreateDish.Width := BASE_BUTTON_WIDTH;
  ButtonDeleteDish.Width := BASE_BUTTON_WIDTH;

  ButtonCreateFood.Top := BORD;
  ButtonDeleteFood.Top := BORD;
  ButtonCreateDish.Top := BORD;
  ButtonDeleteDish.Top := BORD;

  ButtonCreateFood.Left := 0;
  ButtonDeleteFood.Left := PanelFoodButtons.Width - BASE_BUTTON_WIDTH;
  ButtonCreateDish.Left := 0;
  ButtonDeleteDish.Left := PanelDishButtons.Width - BASE_BUTTON_WIDTH;

  PanelFoodButtons.Height := BASE_BUTTON_HEIGHT + BORD; // no 2*
  PanelDishButtons.Height := BASE_BUTTON_HEIGHT + BORD; // no 2*

  BevelRecRight.Width := 2;

  { АНАЛИЗ }
  // TODO: написать

  FinishProc;
end;

{==============================================================================}
procedure TForm1.FullInit;
{==============================================================================}
var
  Timer: TSmallTimer;
  Prev: string;
  Temp: integer;
  i: integer;

  procedure StartupInfo(const Text: string);
  begin
    if (Prev <> '') then
      // TODO: make log's "Save" parameter False in release version
      Log(DEBUG, Prev + #9 + IntToStr(Timer.Time), False);
    ShowProcess(Text);
    Prev := Text;
  end;

begin
  StartProc('FullInit');

  FormProcess.Show;
  FormProcess.SetMax(17);
  Timer := TSmallTimer.Create;
  Prev := '';

  { ======= СИСТЕМНЫЕ НАСТРОЙКИ, ЗАГРУЗКА ======= }

  try
    { =============== ВЕБ-КОНСОЛЬ =============== }
    {*}StartupInfo(STATUS_ACTION_WEB_SETUP);
    {*}WebClient.Username := Value['Login'];
    {*}WebClient.Password := Value['Password'];
    {*}WebClient.Server := Value['ServerURL'];
    {*}WebClient.SetTimeout(5000);

    { =============== НАСТРОЙКА ДНЕВНИКА =============== }
    {*}StartupInfo(STATUS_ACTION_LOADING_DIARY);
    {*}Diary.PostPrandStd := Value['PostPrandTime'];
    {*}Diary.PostPrandShort := Value['ShortPostPrandTime'];

    { =============== ЗАГРУЗКА БАЗЫ ПРОДУКТОВ =============== }

    // форматирование
    if (not FileExists(WORK_FOLDER + FoodBase_FileName)) and
       (FileExists(WORK_FOLDER + FOLDER_BASES + '\' + FoodBase_Name_Old)) then
    begin
      StartupInfo(STATUS_ACTION_CONVERT_FOODBASE);
      FoodBase.LoadFromFile_Old(WORK_FOLDER + FOLDER_BASES + '\' + FoodBase_Name_Old);
      FoodBase.SaveToFile(WORK_FOLDER + FoodBase_FileName);
      Log(INFO, 'Foodbase converted ok', True);
    end else

    // TODO: загружается дважды :(
    // существует файл в XML-формате
    if FileExists(WORK_FOLDER + FoodBase_FileName) then
    begin
      StartupInfo(STATUS_ACTION_LOADING_FOODBASE);
      FoodBase.LoadFromFile_XML(WORK_FOLDER + FoodBase_FileName);
      Log(INFO, 'Foodbase loaded ok', True);
    end;

    { =============== ЗАГРУЗКА БАЗЫ БЛЮД =============== }

    // форматирование
    if (not FileExists(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name)) and
       (FileExists(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name_Old)) then
    begin
      StartupInfo(STATUS_ACTION_CONVERT_DISHBASE);
      DishBase.LoadFromFile_Old(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name_Old, FoodBase);
      DishBase.SaveToFile(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name);
      Log(INFO, 'DishBase converted ok', True);
    end else

    // TODO: загружается дважды :(
    // существует файл в XML-формате
    if FileExists(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name) then
    begin
      StartupInfo(STATUS_ACTION_LOADING_DISHBASE);
      DishBase.LoadFromFile_XML(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name);
      Log(INFO, 'Dishbase loaded ok', True);
    end;

    LoadExpander;

    { =============== СИНХРОНИЗАЦИЯ =============== }
    if Value['AutoSync'] then
    begin
      //--------------------------------------
      StartupInfo(STATUS_ACTION_AUTH);
      try
        WebClient.Login();

        //--------------------------------------
        StartupInfo(STATUS_ACTION_SYNC_DIARY);
        Temp := MySyncDiary();
        {*}if (Temp <> 0) then
           ShowBalloon('Синхронизация дневника прошла успешно, передано страниц: ' + IntToStr(Temp), bitInfo);
      except
        on ConnectionError: EConnectionException do
        begin
          Log(ERROR, 'Form1.FullInit(): ошибка при синхронизации дневника: ' + ConnectionError.Message);
          ShowBalloon('Авторизация не удалась [сервер не отвечает]', bitError);
        end;

        on AuthError: EAuthException do
        begin
          Log(ERROR, 'Form1.FullInit(): ошибка при синхронизации дневника: ' + AuthError.Message);
          ShowBalloon('Авторизация не удалась [неверный логин/пароль]. Щёлкните это сообщение, чтобы открыть настройки.', bitError, BalloonAction_ShowInternetSettings);
        end;

        on APIError: EAPIException do
        begin
          Log(ERROR, 'Form1.FullInit(): ошибка при синхронизации дневника: ' + APIError.Message);
          ShowBalloon('Авторизация не удалась [API устарел, обновите версию программы]', bitError);
        end;

        on FormatError: EFormatException do
        begin
          Log(ERROR, 'Form1.FullInit(): ошибка при синхронизации дневника: ' + FormatError.Message);
          ShowBalloon('Авторизация не удалась [ошибка формата сервера, рекомендуется обновить версию программы]', bitError);
        end;

        on CommonError: ECommonException do
        begin
          Log(ERROR, 'Form1.FullInit(): ошибка при синхронизации дневника: ' + CommonError.Message);
          ShowBalloon('Авторизация не удалась [общая ошибка клиента]', bitError);
        end;

        on E: Exception do
        begin
          Log(ERROR, 'Form1.FullInit(): ошибка при синхронизации дневника: ' + E.Message);
          ErrorMessage('Не удалось синхронизировать дневник');                            
        end;
      end;
    end;

    { =============== ЗАГРУЗКА ОБРАЗЦОВ =============== }

    // не существует файл в XML-формате
    if (not FileExists(WORK_FOLDER + FoodBase_FileName)) then
    begin
      // TODO: предложить загрузить образец базы
      // TODO: реализовать на сервере API foodbase:sample
      // TODO: сейчас образец базы на сервере - в старом формате

      StartupInfo(STATUS_ACTION_DOWNLOADING_FOODBASE);
      if DownloadFoodBaseSample() and
         FileExists(WORK_FOLDER + FoodBase_FileName) then
      begin
        StartupInfo(STATUS_ACTION_LOADING_FOODBASE);
        FoodBase.LoadFromFile_XML(WORK_FOLDER + FoodBase_FileName);
      end;
    end;

    // не существует файл в XML-формате
    if (not FileExists(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name)) then
    begin
      // TODO: предложить загрузить образец базы
      // TODO: реализовать на сервере API dishbase:sample
      // TODO: сейчас образец базы на сервере - в старом формате

      StartupInfo(STATUS_ACTION_DOWNLOADING_DISHBASE);
      if DownloadDishBaseSample() and
         FileExists(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name) then
      begin
        StartupInfo(STATUS_ACTION_LOADING_DISHBASE);
        DishBase.LoadFromFile_XML(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name);
      end;
    end;

    { =============== ЗАГРУЗКА МОДУЛЯ АНАЛИЗА =============== }
    StartupInfo(STATUS_ACTION_LOADING_MATHAN);
    AddAnalyzer(WORK_FOLDER + ANALYZE_LIB_FileName);
    // TODO: may be extended

    for i := 0 to GetAnalyzersCount - 1 do
      ComboAnalyzers.Items.Add(GetAnalyzer(i).Name);

    ComboAnalyzers.ItemIndex := 0;

    { =============== КОЭФФИЦИЕНТЫ =============== }
    try
      StartupInfo(STATUS_ACTION_PREPARING_KOOFS);
      UpdateKoofs;

      if Value['AutoSync'] then
      begin
        StartupInfo(STATUS_ACTION_UPLOADING_KOOFS);
        UploadKoofs;
      end else
        StartupInfo('');
    except
      on E: Exception do
      begin
        Log(ERROR, 'Exception at Loading\UpdateKoofs: ' + E.Message, True);
        ErrorMessage('Произошла ошибка при расчёте коэффициентов'#13 + E.Message);
      end;
    end;

    { =============== ЗАГРУЗКА ГРАФИКИ =============== }
    StartupInfo(STATUS_ACTION_LOADING_GRAPHICS);
    LoadPictures;

    { =============== ПРИМЕНЕНИЕ НАСТРОЕК ИНТЕРФЕЙСА =============== }
    StartupInfo(STATUS_ACTION_APPLYING_SETTINGS);
    ApplyInterfaceSettings; // до Maximize

    Caption := PROGRAM_TITLE + ' ' + PROGRAM_VERSION;

    PanelDevelopment.Visible := Value['Debug'];
    TabStat.TabVisible := ADVANCED_MODE;
    StatusBar.Panels[3].Text := '';
    CalendarDiary.Date := now;  
    ImagePreview.Align := alNone;
    ImagePreview.Width := GroupBoxGraph.Width - 3;
    PageControl1.ActivePageIndex := 0;
    Width := (Screen.Width * 3) div 4;
    Height := (Screen.Height * 3) div 4;
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
    WindowState := wsMaximized;

    //--------------------------------------
    StartupInfo(STATUS_ACTION_PREPARING_INFOPANELS);
    UpdateMealStatistics;
    UpdateMealDose;
    //ProcessMealSelected(False);
    UpdateNextFinger();
    DiaryView.OpenPage(Diary[Trunc(Now)], True);

    { =============== ВКЛЮЧЕНИЕ IDLE-ЗАДАЧ =============== }

    ON_IDLE_UpdateCombos := True;
    ON_IDLE_ShowBases := True;
    if Value['CheckUpdates'] and
       (Now - Value['LastUpdateCheck']  > UPDATES_CHECKING_PERIOD) then
      IN_IDLE_CheckUpdates := True;

    {=============================================}

    StartupInfo(STATUS_READY);
    StatusBar.Panels[0].Text := Format(STATUS_LOADING_TIME, [Timer.FullTime]);
    Timer.Free;
  except
    on E: Exception do
    begin
      // TODO 1: повысить устойчивость (fail-soft), выделить независимые блоки
      Log(ERROR, Format(MESSAGE_ERROR_INITIALIZATION, [Prev, E.Message]), True);
      ErrorMessage(Format(MESSAGE_ERROR_INITIALIZATION, [Prev, E.Message]));
    end;
  end;

  FormProcess.Hide;
  ShowMessages;

  Application.OnIdle := MyIdle;
  Application.OnActivate := MyActivate;
  Application.OnDeactivate := MyDeactivate;
  TimerAutosave.Enabled := True;
  TimerTimeLeft.Enabled := True;

  FinishProc;
end;

{==============================================================================}
procedure TForm1.FullFree;
{==============================================================================}
begin

end;

{==============================================================================}
procedure TForm1.ShowMessages;
{==============================================================================}
begin
  StartProc('ShowMessages()');

  {
     Замечание. Все действия вызывают всплывающую подсказку, т.е.
     являются взаимоисключающими и потому должны быть записаны
     через ELSE в порядке убывания важности сообщения.
  }

  { проверка корректности установки обновления }
  if Value['UpdatedVersion'] <> '' then
  begin
    if Value['UpdatedVersion'] <= PROGRAM_DATE then
      ShowBalloon('Установка обновления успешно завершена', bitInfo)
    else
      ShowBalloon(
        'При установке обновления возникли ошибки'#13+
        'Ожидалась версия: ' + Value['UpdatedVersion'] + #13 +
        'Установлена версия: ' + PROGRAM_DATE, bitError);
    Value['UpdatedVersion'] := '';
    DeleteFile(WORK_FOLDER + SETUP_FILE);
  end else

  { поиск модуля анализа }
  if (GetAnalyzersCount = 0) then
  begin
    ShowBalloon(BALOON_ERROR_ANALYZER_NOT_FOUNDED, bitError);
  end else

  {  }

  ;

  FinishProc;
end;

{==============================================================================}
procedure TForm1.LoadPictures;
{==============================================================================}

  procedure LoadImageList(const FileName: string; ImageList: TImageList);
  const
    BlendColor = $FF00FF;
  var
    Temp: TBitMap;
  begin
    temp := nil;
    if FileExists(FileName) then
    try
      temp := TBitMap.Create;
      temp.LoadFromFile(FileName);
      ImageList.AddMasked(Temp, BlendColor);
    finally
      temp.Free;
    end

    else if ADVANCED_MODE then
      ShowMessage('Изображение "' + FileName + '" не найдено');
  end;

  procedure LoadImage(const FileName: string; Bitmap: TBitMap); overload;
  begin
    if FileExists(FileName) then
      Bitmap.LoadFromFile(FileName)

    else if ADVANCED_MODE then
      ShowMessage('Изображение "' + FileName + '" не найдено');
  end;

  procedure LoadImage(const FileName: string; Picture: TPicture); overload;
  begin
    LoadImage(FileName, Picture.Bitmap);
  end;

begin
  StartProc('LoadPictures()');

  { Главное меню }
  LoadImageList(WORK_FOLDER + IMAGE_MENU, DataInterface.Images_Menu);

  { Закладки }
  LoadImageList(WORK_FOLDER + IMAGE_MAIN_TABS, DataInterface.Images_Main_Tabs);

  { Контекстные меню }
  LoadImageList(WORK_FOLDER + IMAGE_BASE_CONTENT, DataInterface.Images_BaseContent);

  { Кнопки }
  LoadImage(WORK_FOLDER + IMAGE_DIARY_NEW_BLOOD, ButtonAddBlood.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_DIARY_NEW_INS, ButtonAddIns.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_DIARY_NEW_MEAL, ButtonAddMeal.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_DIARY_NEW_NOTE, ImageNote.Picture);

  LoadImage(WORK_FOLDER + IMAGE_EDITOR_ADD, ButtonDiaryNewAddFood.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_EDITOR_ADD, FormDish.ButtonAddFood.Glyph);

  LoadImage(WORK_FOLDER + IMAGE_EDITOR_CALC, FormDish.ButtonRunCalc.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_EDITOR_REMOVE, FormDish.ButtonSimpleMass.Glyph);

  LoadImage(WORK_FOLDER + IMAGE_BASE_NEW_FOOD, ButtonCreateFood.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_BASE_NEW_DISH, ButtonCreateDish.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_BASE_REMOVE, ButtonDeleteFood.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_BASE_REMOVE, ButtonDeleteDish.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_EXPORT_SAVE, FormExportText.ButtonSave.Glyph);

  // кнопки закладок в настройках
  LoadImage(WORK_FOLDER + IMAGE_SETTINGS_PRIVATE, FormSettings.ButtonTabPrivate.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_SETTINGS_ANALYZE, FormSettings.ButtonTabAnalyze.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_SETTINGS_INTERFACE, FormSettings.ButtonTabInterface.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_SETTINGS_IMAGE, FormSettings.ButtonTabView.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_SETTINGS_NORMS, FormSettings.ButtonTabNorms.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_SETTINGS_INTERNET, FormSettings.ButtonTabInternet.Glyph);

  // minmax
  LoadImageList(WORK_FOLDER + IMAGE_PANEL_MIN, DataInterface.Image_MinMax);
  LoadImageList(WORK_FOLDER + IMAGE_PANEL_MAX, DataInterface.Image_MinMax);
  LoadImage(WORK_FOLDER + IMAGE_PANEL_MIN, ImageMinMaxStat.Picture);
  LoadImage(WORK_FOLDER + IMAGE_PANEL_MIN, ImageMinMaxGraph.Picture);
  LoadImage(WORK_FOLDER + IMAGE_PANEL_MIN, ImageMinMaxTimes.Picture);

  FinishProc;
end;

{==============================================================================}
function TForm1.DishEditorRect: TRect;
{==============================================================================}
begin
  Result := Rect(
    Left + PanelBaseDish.Left,
    Top,
    Left + PanelBaseDish.Left + PanelBaseDish.Width,
    Top + Height
  );
end;

{==============================================================================}
function TForm1.FoodEditorRect: TRect;
{==============================================================================}
begin
  Result := Rect(
    Left + PanelBaseFood.Left,
    Top,
    Left + PanelBaseFood.Left + PanelBaseFood.Width,
    Top + Height
  );
end;

{==============================================================================}
procedure TForm1.CreateFood;
{==============================================================================}
var
  Food: TFood;
  n: integer;
begin
  Food := TFood.Create;
  if FormFood.OpenFoodEditor(Food, True, FoodEditorRect) then
  begin
    n := FoodBase.Add(Food);
    {#}EventFoodbaseChanged(True);

    ShowTableItem(ListFood, n);
    ListFood.SetFocus;
  end else
    Food.Free;
end;

{==============================================================================}
procedure TForm1.CreateDish;
{==============================================================================}
var
  Dish: TDish;
  n: integer;
begin
  // TODO: Create() here, not in the editor
  if FormDish.OpenDishEditor(Dish, True, DishEditorRect) then
  begin
    n := DishBase.Add(Dish);
    Dish.UpdateTimestamp;
    {#}EventDishbaseChanged(True, True);

    ShowTableItem(ListDish, n);
    ListDish.SetFocus;
  end;
end;

{==============================================================================}
procedure TForm1.EditFood(Index: integer);
{==============================================================================}
var
  Temp: TFood;
begin
  if (Index < 0) or (Index >= FoodBase.Count) then
  begin
    Log(WARNING, 'EditFood(): incorrect index ignored (' + IntToStr(Index) + ')', True);
    Exit;
  end;

  Temp := TFood.Create;
  Temp.CopyFrom(FoodBase[Index]);

  if FormFood.OpenFoodEditor(Temp, False, FoodEditorRect) then
  begin
    if DishBase.RenameFood(FoodBase[Index].Name, Temp.Name) then
      SaveDishBase;

    FoodBase.Delete(Index);
    Index := FoodBase.Add(Temp);

    EventFoodbaseChanged(False);

    ShowTableItem(ListFood, Index);
    ListFood.SetFocus;
  end else
    Temp.Free;
end;

{==============================================================================}
procedure TForm1.EditDish(Index: integer);
{==============================================================================}
var
  Temp: TDish;
begin
  // TODO: process everywhere in the same manner
  if (Index < 0) or (Index >= DishBase.Count) then
  begin
    Log(WARNING, 'EditDish(): incorrect index ignored (' + IntToStr(Index) + ')', True);
    Exit;
  end;

  Temp := TDish.Create;
  Temp.CopyFrom(DishBase[Index]);
  if FormDish.OpenDishEditor(Temp, False, DishEditorRect) then
  begin
    DishBase.RenameFood(DishBase[Index].Name, Temp.Name);

    DishBase.Delete(Index);
    Index := DishBase.Add(Temp);
    Temp.UpdateTimestamp;

    EventDishbaseChanged(False, True);

    ShowTableItem(ListDish, Index);
    ListDish.SetFocus;
  end else
    Temp.Free;
end;

{==============================================================================}
procedure TForm1.RemoveFood(Index: integer);
{==============================================================================}

  function AskWarning(FoodIndex, DishIndex: integer): boolean;
  begin
    Result := MessageDlg(
      Format(MESSAGE_CONF_REMOVE_FOOD_USED, [FoodBase[FoodIndex].Name, DishBase[DishIndex].Name]),
      mtWarning, [mbYes, mbNo], 0) = mrYes;
  end;

  function AskConfirm(FoodIndex: integer): boolean;
  begin
    Result := MessageDlg(
      Format(MESSAGE_CONF_REMOVE_FOOD, [FoodBase[FoodIndex].Name]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  end;

var
  DishNumber: integer;
begin
  // TODO: here
  if (Index < 0) or (Index >= FoodBase.Count) then Exit;

  DishNumber := DishBase.UsedFood(FoodBase[Index].Name);
  if ((DishNumber > -1)and(AskWarning(Index, DishNumber)))or
     ((DishNumber = -1)and(AskConfirm(Index))) then
  begin
    { порядок в базе и таблице совпадают }
    FoodBase.Delete(Index);

    UpdateFoodTable(True);
    {*}UpdateCombos;
    {*}SaveFoodBase;
  end;
end;

{==============================================================================}
procedure TForm1.RemoveDish(Index: integer);
{==============================================================================}

  function AskWarning(DishIndex, UsedInDishIndex: integer): boolean;
  begin
    Result := MessageDlg(
      Format(MESSAGE_CONF_REMOVE_DISH_USED, [DishBase[DishIndex].Name, DishBase[UsedInDishIndex].Name]),
      mtWarning, [mbYes, mbNo],0) = mrYes;
  end;

  function AskConfirm(DishIndex: integer): boolean;
  begin
    Result := MessageDlg(
      Format(MESSAGE_CONF_REMOVE_DISH, [DishBase[DishIndex].Name]),
      mtConfirmation,[mbYes,mbNo],0) = mrYes;
  end;

var
  UsedDishNumber: integer;
begin
  if (Index < 0) or (Index >= DishBase.Count) then Exit;

  UsedDishNumber := DishBase.UsedFood(DishBase[Index].Name);
  if ((UsedDishNumber > -1)and(AskWarning(Index, UsedDishNumber)))or
     ((UsedDishNumber = -1)and(AskConfirm(Index))) then
  begin
    DishBase.Delete(Index);
    UpdateDishTable(True);
    UpdateCombos;
    SaveDishBase;
  end;
end;

{==============================================================================}
procedure TForm1.UpdateCombos;
{==============================================================================}
var
  Offset: integer;

  procedure InitMap(var Map: TMultiMap);
  var
    i: integer;
  begin
    Offset := FoodBase.Count;
    SetLength(Map, FoodBase.Count + DishBase.Count);

    for i := 0 to FoodBase.Count - 1 do
    with Map[i] do
    begin
      ItemType := itFood;
      Index := i;
      Tag := 0;
    end;

    for i := 0 to DishBase.Count - 1 do
    with Map[Offset + i] do
    begin
      ItemType := itDish;
      Index := i;
      Tag := 0;
    end;
  end;

  function More(const Item1, Item2: TMultiItem): boolean;
  var
    Name1, Name2: string;
  begin
    if (Item1.Tag <> Item2.Tag) then
      Result := (Item1.Tag < Item2.Tag)
    else 
    begin
      case Item1.ItemType of
        itFood: Name1 := FoodBase[Item1.Index].Name;
        itDish: Name1 := DishBase[Item1.Index].Name;
      end;
      case Item2.ItemType of
        itFood: Name2 := FoodBase[Item2.Index].Name;
        itDish: Name2 := DishBase[Item2.Index].Name;
      end;
      Result := (Name1 > Name2);
    end;
  end;

  procedure qsort(var Map: TMultiMap; l,r: integer);
  var
    i,j: integer;
    x: TMultiItem;
    y: TMultiItem;
  begin
    i := l;
    j := r;
    x := Map[(l+r) div 2];
    repeat
      while More(X, Map[i]) do inc(i);
      while More(Map[j], X) do dec(j);

      if (i <= j) then
      begin
        if More(Map[i], Map[j]) or
           More(Map[j], Map[i]) then
        begin
          y := Map[i];
          Map[i] := Map[j];
          Map[j] := y;
        end;
        inc(i);
        dec(j);
      end;
    until i > j;
    if l < j then qsort(Map, l, j);
    if i < r then qsort(Map, i, r);
  end;

  procedure IncReal(var X: Real; const Value: Real);
  begin
    X := X + Value;
  end;

  procedure Process(const ItemName: string; Tag: real; var Map: TMultimap);
  var
    Index: integer;
  begin
    case IdentifyItem(ItemName, Index) of
      itFood: IncReal(Map[Index].Tag, Tag);
      itDish: IncReal(Map[Offset + Index].Tag, Tag);
    end;
  end;

  procedure AnalyzeUsingDiary;
  var
    StartDate, FinishDate: integer;

    function GetTag(Time: TDateTime): real;
    begin
      if (Time <= StartDate) then
        Result := 0 else
      if (Time >= FinishDate) then
        Result := 1
      else
        Result := IntPower((Time - StartDate) / (FinishDate - StartDate), 7);
    end;

  var
    i,j,k: integer;
    Meal: TMealRecord;
    DeltaTag: real;
    DishModFactor: integer;
  begin
    StartProc('TForm1.AnalyzeUsingDiary()');

    try  
      { инициализируем карту }
      InitMap(DiaryMultiMap);

      { выставляем теги }
      StartDate := Trunc(Now) - Value['AnUsingPeriod'];
      FinishDate := Trunc(Now);

      for i := StartDate to FinishDate do
      //for i := FinishDate downto StartDate do
      for j := 0 to Diary[i].Count - 1 do
      begin
        if (Diary[i][j].RecType = TMealRecord) then
        begin
          Meal := TMealRecord(Diary[i][j]);
          DeltaTag := GetTag(i + Meal.Time / MinPerDay);
          for k := 0 to Meal.Count - 1 do
          begin
            Process(Meal[k].Name, DeltaTag, DiaryMultiMap);
          end;
        end;
      end;

      { дополнительные теги для блюд }

      DishModFactor := Value['DishModFactor'];

      for i := 0 to DishBase.Count - 1 do
      if (DishBase[i].ModifiedTime > 0) then
        IncReal(DiaryMultiMap[Offset + i].Tag, DishModFactor * GetTag(DishBase[i].ModifiedTime));

      { сортируем }
      if Length(DiaryMultiMap) > 0 then
        qsort(DiaryMultiMap, 0, High(DiaryMultiMap));
    except
      on E: Exception do
      begin
        Log(ERROR, 'EXCEPTION in AnalyzeUsingDiary(): ' + E.Message, True);
        ErrorMessage('Ошибка анализа использования дневника');
      end;
    end;

    FinishProc;
  end;

  procedure AnalyzeUsingDish;
  var
    i, j: integer;
  begin
    StartProc('TForm1.AnalyzeUsingDish()');

    try  
      { инициализируем карту }
      InitMap(DishMultiMap);

      { выставляем теги }
      for i := 0 to DishBase.Count - 1 do
      for j := 0 to DishBase[i].Count - 1 do
        Process(DishBase[i].Content[j].Name, 1.0, DishMultiMap);

      { сортируем }
      if Length(DishMultiMap) > 0 then
        qsort(DishMultiMap, 0, High(DishMultiMap));
    except
      on E: Exception do
      begin
        Log(ERROR, 'EXCEPTION in AnalyzeUsingDish(): ' + E.Message, True);
        ErrorMessage('Ошибка анализа использования базы продуктов');
      end;
    end;

    FinishProc;
  end;

  procedure FillACBox(Box: TACComboBox; const Map: TMultiMap);
  var
    i: integer;
  begin
    with Box.ACItems do
    begin
      Clear;
      for i := 0 to High(Map) do
      case Map[i].ItemType of
        itFood: Add(FoodBase[Map[i].Index].Name);
        itDish: Add(DishBase[Map[i].Index].Name);
      end;
    end;
  end;

begin
  StartProc('TForm1.UpdateCombos()');
  try
    AnalyzeUsingDiary;
    AnalyzeUsingDish;
    FillACBox(ComboDiaryNew, DiaryMultiMap);
    FillACBox(FormDish.ComboFood, DishMultiMap);
  finally
    FinishProc;
  end;
end;

// TODO: программа пытается интерпретировать подсказку в поле ввода продуктоблюда как название продуктоблюда

{==============================================================================}
procedure TForm1.ButtonDeleteFoodClick(Sender: TObject);
{==============================================================================}
begin
  RemoveFood(ListFood.ItemIndex);
end;

{==============================================================================}
procedure TForm1.ButtonDeleteDishClick(Sender: TObject);
{==============================================================================}
begin
  RemoveDish(ListDish.ItemIndex);
end;

{==============================================================================}
procedure TForm1.TableFoodBaseKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
var
  j: integer;
  c: char;
begin
  if Key = vk_Return then
    EditFood(ListFood.ItemIndex) else
  if Key = vk_Delete then
    RemoveFood(ListFood.ItemIndex) else
 begin
    c := CodeToRus(Key);
    if c <> #0 then
    for j := 0 to FoodBase.Count-1 do
    if StartsWith(FoodBase[j].Name, C) then
    begin
      ShowTableItem(ListFood, j, True);
      break;
    end;
  end;
end;

{==============================================================================}
procedure TForm1.ListDishKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
var
  j: integer;
  c: char;
begin
  if Key = vk_Return then
    EditDish(ListDish.ItemIndex) else
  if Key = vk_Delete then
    RemoveDish(ListDish.ItemIndex) else
 begin
    c := CodeToRus(Key);
    if c <> #0 then
    for j := 0 to DishBase.Count-1 do
    if StartsWith(DishBase[j].Name, C) then
    begin
      ShowTableItem(ListDish, j, True);
      break;
    end;
  end;
end;

{==============================================================================}
procedure TForm1.EditKeyProtect(Sender: TObject; var Key: Char);
{==============================================================================}
begin
  if (Key in SYSTEM_CHARS) then Key := #0;
end;

{==============================================================================}
procedure TForm1.EditDiaryNewMassKeyPress(Sender: TObject; var Key: Char);
{==============================================================================}
begin
  if (Key = #13) then
  begin
    ButtonDiaryNewAddClick(nil);
    Key := #0;
  end;
end;

{==============================================================================}
procedure TForm1.ButtonCreateFoodClick(Sender: TObject);
{==============================================================================}
begin
  CreateFood;
end;

{==============================================================================}
procedure TForm1.ButtonCreateDishClick(Sender: TObject);
{==============================================================================}
begin
  CreateDish;
end;

{==============================================================================}
procedure TForm1.CalendarDiaryChange(Sender: TObject);
{==============================================================================}
begin
  StartProc('CalendarDiaryChange');

  DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], False);

  {
  UpdateMealStatistic;
  if not DiaryView.IsMealSelected then
    UpdateMealDose;
  }
  UpdateMealStatistics;
  UpdateMealDose;

  FinishProc;
end;

{==============================================================================}
procedure TForm1.SplitterBaseCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
{==============================================================================}
const
  Min = 200;
begin
  Accept := (NewSize>Min)and(Width-NewSize>Min);
end;

{==============================================================================}
procedure TForm1.SplitterBaseMoved(Sender: TObject);
{==============================================================================}
begin
  Value['PanelBaseFood.Width'] := PanelBaseFood.Width / Width;
end;

{==============================================================================}
procedure TForm1.FormResize(Sender: TObject);
{==============================================================================}
begin
  PanelBaseFood.Width := Round(Width * Value['PanelBaseFood.Width']);
end;

{==============================================================================}
procedure TForm1.TimerTimeLeftTimer(Sender: TObject);
{==============================================================================}
{var
  OldTimeMin, NewTimeMin: integer;

  function CrossEdge(Hours: integer): boolean;
  begin
    Result := (OldTimeMin < Hours*60) and (NewTimeMin = Hours*60);
  end;}

begin
  //OldTimeMin := TimeAfterMeal div 60;
  //StartProc('TimerTimeLeft');

  UpdateTimeLeft();
  
  if (GetAnalyzersCount > 0) and (PageControl1.ActivePage = TabAnalyze) then
    ComboKoofChange(nil);

  {StartProc('Updating TrayIcon.Hint');
  TrayIcon.Hint :=
    Application.Title + #13 +
    ' - ' + LabelDiaryTimeLeftMealVal.Caption + #13 +
    ' - ' + LabelDiaryTimeLeftInsVal.Caption;
  FinishProc;}

  { пока сделал фигово, на коленке }
  {if Value['MealNotifies'] then
  begin
    NewTimeMin := TimeAfterMeal div 60;
    if CrossEdge(4) then
      ShowBalloon('После еды прошло 4 часа. Пора кушать.',bitInfo,baJustShow)
    else
    if CrossEdge(5) then
      ShowBalloon('После еды прошло 5 часов. Пора кушать.',bitInfo,baJustShow);
  end;  }

  {if (Frac(now) < 5/24)and(now-LastGoodNight > GOODNIGHT_INTERVAL) then
  begin
    LastGoodNight := now;
    s := TimeToStr(now);
    ShowBalloon('На часах уже '+Copy(s,1,length(s)-3)+', пора спать...', bitInfo, baDoNothing)
  end;  }
  
  //IgnoreIdleOnce := True;

  //FinishProc;
end;

{==============================================================================}
procedure TForm1.ImageMinMaxStatMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{==============================================================================}
begin
  MinMaxBox(
    GroupBoxStatistic,ImageMinMaxStat, DataInterface.Image_MinMax,
    Value['AnimatePanels'],
    LabelDayIns.Top+2*LabelDayIns.Height,
    ImageMinMaxStat.Height+3,PANEL_OPEN_TIME,PANEL_CLOSE_TIME);
end;

{==============================================================================}
procedure TForm1.ImageMinMaxGraphMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{==============================================================================}
begin
  if ImageMinMaxGraph.Tag = 0 then
    ImagePreview.Show;

  MinMaxBox(
    GroupBoxGraph,ImageMinMaxGraph, DataInterface.Image_MinMax,
    Value['AnimatePanels'],
    Round(ImagePreview.Width * 0.8),
    ImageMinMaxGraph.Height + 3,PANEL_OPEN_TIME, PANEL_CLOSE_TIME);

  if ImageMinMaxGraph.Tag = 0 then
    ImagePreview.Hide;
end;

{==============================================================================}
procedure TForm1.ImageMinMaxTimesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{==============================================================================}
begin
  MinMaxBox(
    GroupBoxTimeLeft,ImageMinMaxTimes, DataInterface.Image_MinMax,
    Value['AnimatePanels'],
    LabelDiaryFinger.Top + 4 * BORD,
    ImageMinMaxTimes.Height + 3,
    PANEL_OPEN_TIME,PANEL_CLOSE_TIME);
end;

{==============================================================================}
procedure TForm1.ImagePreviewDblClick(Sender: TObject);
{==============================================================================}
begin
  PageControl1.ActivePage := TabAnalyze;

  {#!!!}
  DrawBS(
    Diary[DiaryView.CurrentDate - 1],
    DiaryView.CurrentPage,
    Diary[DiaryView.CurrentDate + 1],
    ImageLarge,
    False);
end;

{==============================================================================}
procedure TForm1.DiaryViewPage(Sender: TObject);
{==============================================================================}
begin
  StartProc('DiaryViewPage');

  UpdateDayInfo;

  FinishProc;
end;

{==============================================================================}
procedure TForm1.ListFoodKeyPress(Sender: TObject; var Key: Char);
{==============================================================================}
begin
  Key := #0;  // чтобы не было встроенного автоперехода
end;

{==============================================================================}
procedure TForm1.ComboDiaryNewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{==============================================================================}
begin
  { масса - автоматом при закрытии окошка - ОШИБКА: окошко м.б. закрыто }

  if Key = vk_Return then
  begin
    //Caption := '*' + ComboDiaryNew.Text;

    DiaryFoodDishInput := ComboDiaryNew.Text;

    //EditDiaryNewMass.SetFocus - НЕТ
    //TACComboBox(Sender).OnCloseUp(Sender);
    //ComboDiaryNewCloseUp(Sender);

    //Caption := Caption + ' --> ' + ComboDiaryNew.Text;
  end;
end;

{==============================================================================}
procedure TForm1.ComboDiaryNewCloseUp(Sender: TObject);
{==============================================================================}
var
  n: integer;
  Mass: real;
begin
  case IdentifyItem(Trim(ComboDiaryNew.Text), n) of
    itFood:
    begin
      Expander.Add(DiaryFoodDishInput, FoodBase[n].Name);

      ComboDiaryNew.Text := FoodBase[n].Name;
      Mass := GetCompensationMass(
        FoodBase[n].RelCarbs,
        FoodBase[n].RelProts);
      if Mass > 0 then
        EditDiaryNewMass.Text := IntToStr(Round(Mass));
      FocusEdit(EditDiaryNewMass);
    end;

    itDish:
    begin
      Expander.Add(DiaryFoodDishInput, DishBase[n].Name);

      ComboDiaryNew.Text := DishBase[n].Name;
      Mass := GetCompensationMass(
        DishBase[n].RelCarbs,
        DishBase[n].RelProts);
      if Mass > 0 then
        EditDiaryNewMass.Text := IntToStr(Round(Mass));
      FocusEdit(EditDiaryNewMass);
    end;
  end;
end;

{==============================================================================}
procedure TForm1.ButtonDiaryNewAddClick(Sender: TObject);
{==============================================================================}
var
  ItemType: TItemType;
  n: integer;
  Mass: Extended;
  Meal: TMealRecord;
begin
  StartProc('TForm1.ButtonDiaryNewAddClick()');
  try
    try
      if (DiaryView.SelectedRecord is TMealRecord) then
      begin
        Meal := TMealRecord(DiaryView.SelectedRecord);

        ComboDiaryNew.Text := Trim(ComboDiaryNew.Text);
        EditDiaryNewMass.Text := Trim(EditDiaryNewMass.Text);
        ItemType := IdentifyItem(ComboDiaryNew.Text, n);

        if (ItemType = itUnknown) then
        begin
          if (ComboDiaryNew.Text = '') then
            ErrorMessage('Введите название продукта или блюда')
          else
            ErrorMessage('Наименование "' +  ComboDiaryNew.Text + '" не найдено в базах');
          ComboDiaryNew.SetFocus;
          ComboDiaryNew.SelStart := Length(ComboDiaryNew.Text);
          ComboDiaryNew.ShowAC;
        end else
        begin
          EditDiaryNewMass.Text := CheckDot(EditDiaryNewMass.Text);
          if (EditDiaryNewMass.Text = '') then
          begin
            ErrorMessage('Введите массу');
            FocusEdit(EditDiaryNewMass);
          end else
          if (not TryToCalculate(EditDiaryNewMass.Text, Mass)) then
          begin
            ErrorMessage('Неверная масса');
            FocusEdit(EditDiaryNewMass);
          end else
          if (Mass < 0) then
          begin
            ErrorMessage('Масса должна быть неотрицательной');
            FocusEdit(EditDiaryNewMass);
          end else
          begin
            case ItemType of
              itFood: Meal.Add(FoodBase[n].AsFoodMassed(Mass));
              itDish: Meal.Add(DishBase[n].AsFoodMassed(Mass));
            end;

            ComboDiaryNew.Text := '';
            EditDiaryNewMass.Text := '';
            ComboDiaryNew.SetFocus;

            // промотка
            n := DiaryView.SelectedRecordIndex;
            if (n > -1) and (DiaryView.CurrentPage <> nil)and
               (DiaryView.CurrentPage.Count>0) then
              ScrollBoxDiary.VertScrollBar.Position :=
                Round(n / DiaryView.CurrentPage.Count * ScrollBoxDiary.VertScrollBar.Range);
          end;
        end;
      end;
    finally
      FinishProc;
    end;
  except
    on E: Exception do
    begin
      Log(ERROR, 'Exception in ButtonDiaryNewAddClick(): ' + E.Message, True);
      ErrorMessage('Ошибка при добавлении продукта/блюда'#13 + E.Message);
    end;
  end;
end;

{==============================================================================}
procedure TForm1.ScrollBoxDiaryMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{==============================================================================}
begin
  StartProc('TForm1.ScrollBoxDiaryMouseDown()');
  DiaryView.DeselectAll;
  UpdateMealStatistics;
  UpdateMealDose;
  FinishProc;
end;

{==============================================================================}
procedure TForm1.Splitter4Moved(Sender: TObject);
{==============================================================================}
begin
  DiaryView.DrawCurrentPage;
end;

{==============================================================================}
procedure TForm1.ListFoodDblClick(Sender: TObject);
{==============================================================================}
begin
  EditFood(ListFood.ItemIndex);
end;

{==============================================================================}
procedure TForm1.ListDishDblClick(Sender: TObject);
{==============================================================================}
begin
  EditDish(ListDish.ItemIndex);
end;

{==============================================================================}
procedure TForm1.MenuItem_ShowDiaryPageClick(Sender: TObject);
{==============================================================================}
begin
  PageControl1.ActivePage := TabDiary;
  PageControl1Change(nil);
end;

{==============================================================================}
procedure TForm1.MenuItem_ShowBasePageClick(Sender: TObject);
{==============================================================================}
begin
  PageControl1.ActivePage := TabBase;
  PageControl1Change(nil)
end;

{==============================================================================}
procedure TForm1.MenuItem_ShowGraphicPageClick(Sender: TObject);
{==============================================================================}
begin
  PageControl1.ActivePage := TabAnalyze;
  PageControl1Change(nil);
end;

{==============================================================================}
procedure TForm1.ButtonBSHistoryClick(Sender: TObject);
{==============================================================================}
{type
  TBloodItem = record
    Date: TDate;
    Value: real;
  end;

var
  List: array of TBloodItem;
  i,j: integer;

  procedure qsort(l,r: integer);
  var
    i,j: integer;
    x: real;
    y: TBloodItem;
  begin
    i := l;
    j := r;
    x := List[(l+r) div 2].Value;
    repeat
      while List[i].Value<x do inc(i);
      while List[j].Value>x do dec(j);
      if i<=j then
      begin
        if List[i].Value<>List[j].Value then
        begin
          y := List[i];
          List[i] := List[j];
          List[j] := y;
        end;
        inc(i);
        dec(j);
      end;
    until i>j;
    if l<j then qsort(l,j);
    if i<r then qsort(i,r);
  end;  }

begin
 { if not DiaryLocal.IsEmpty then

  for i := DiaryBase.GetFirstDate to DiaryBase.GetLastDate do
  for j := 0 to DiaryBase[i].Count-1 do
  if DiaryBase[i][j].TagType=rtBlood then
  begin
    SetLength(List, length(list) + 1);
    List[High(list)].Date := DiaryBase[i].Date;
    List[High(list)].Value := TBloodRecord(DiaryBase[i][j]).Value;
  end;

  if length(list)>0 then
    qsort(0,High(list));

  with ListBS do
  begin
    Clear;
    for i := 0 to High(List) do
      Items.Add(RealToStrZero(List[i].Value)+':    '+DateToStr(List[i].Date));
  end;    }
end;

procedure TForm1.ListBSDblClick(Sender: TObject);
var
  s: string;
begin
  s := TListBox(Sender).Items[TListBox(Sender).ItemIndex];
  s := Copy(s,1,pos(' ',s)-1);
  CalendarDiary.Date := StrToDate(s);
  CalendarDiary.OnClick(nil);
  PageControl1.TabIndex := 0;
end;

procedure FocusMealInput;
begin
  with Form1 do
  if (not ComboDiaryNew.Focused) and
     (not EditDiaryNewMass.Focused) and
     (ComboDiaryNew.Enabled)and
     (ComboDiaryNew.Visible)
  then ComboDiaryNew.SetFocus;
end;

{==============================================================================}
procedure TForm1.DiaryViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{==============================================================================}
begin
  StartProc('TForm1.DiaryViewMouseDown()');
  //ProcessMealSelected(DiaryView.IsMealSelected); -- on Idle

  FocusMealInput;

  UpdateMealStatistics;
  UpdateMealDose;
  //ScrollToSelected;
  FinishProc;
end;

{==============================================================================}
procedure TForm1.ProcessMealSelected(Value: boolean);
{==============================================================================}
var
  OldValue: boolean;
begin
  OldValue := ButtonDiaryNewAddFood.Enabled;
  if (Value = OldValue) then Exit;

  ComboDiaryNew.Enabled := Value;
  EditDiaryNewMass.Enabled := Value;
  ButtonDiaryNewAddFood.Enabled := Value;
  ButtonDiaryNewAddFood.ShowHint := Value;

  if Value then
  begin
    ComboDiaryNew.Text := '';
    EditDiaryNewMass.Text := '';
    FocusMealInput;
  end else
  begin
    ComboDiaryNew.Text := MESSAGE_SELECT_MEAL;
    EditDiaryNewMass.Text := '';
  end
end;

{==============================================================================}
procedure TForm1.UpdateDayInfo;
{==============================================================================}

  { статистика }
  procedure UpdateDayStatistics;

    function Info(const Val: real; Proc: integer): string;
    begin
      if Proc > -1 then
        Result := IntToStr(Round(Val)) + ' (' + IntToStr(Proc) + '%)'
      else
        Result := IntToStr(Round(Val));
    end;

  var
    summ: real;
    proc1,proc2,proc3: integer;
  begin
    if (DiaryView.CurrentPage = nil) or
       (DiaryView.CurrentPage.Count = 0) then
    begin
      LabelDayProts_.Font.Color := clNoData;
      LabelDayProts_.Left := 2*BORD;
      LabelDayProts_.Caption := 'Нет данных';
      LabelDayProtsVal.Caption := '';
      LabelDayFats_.Caption := '';
      LabelDayFatsVal.Caption := '';
      LabelDayCarbs_.Caption := '';
      LabelDayCarbsVal.Caption := '';
      LabelDayValue.Caption := '';
      LabelDayMass.Caption := '';
      LabelDayIns.Caption := '';

      StatProgProts.Hide;
      StatProgFats.Hide;
      StatProgCarbs.Hide;
    end else
    with DiaryView.CurrentPage do
    begin
      LabelDayProts_.Font.Color := clWindowText;
      LabelDayProts_.Left := StatProgProts.Width+22;

      case Value['ProcShow'] of
        1:  begin
              summ := DayProts + DayFats + DayCarbs;
              if summ > 0 then
              begin
                Proc1 := Round(DayProts / summ * 100);
                Proc2 := Round(DayFats / summ * 100);
                Proc3 := Round(DayCarbs / summ * 100);
              end else
              begin
                Proc1 := -1;
                Proc2 := -1;
                Proc3 := -1;
              end;
            end;
        2:  begin
              summ :=
                ENERGY_PROTS * DayProts +
                ENERGY_FATS  * DayFats +
                ENERGY_CARBS * DayCarbs;
              if summ > 0 then
              begin
                Proc1 := Round(ENERGY_PROTS * DayProts/summ*100);
                Proc2 := Round(ENERGY_FATS * DayFats/summ*100);
                Proc3 := Round(ENERGY_CARBS * DayCarbs/summ*100);
              end else
              begin
                Proc1 := -1;
                Proc2 := -1;
                Proc3 := -1;
              end;
            end;
        else  begin
              if Value['NormProts'] > 0 then
                Proc1 := Round(DayProts/Value['NormProts']*100)
              else Proc1 := -1;

              if Value['NormFats'] > 0 then
                Proc2 := Round(DayFats/Value['NormFats']*100)
              else Proc2 := -1;

              if Value['NormCarbs'] > 0 then
                Proc3 := Round(DayCarbs/Value['NormCarbs']*100)
              else Proc3 := -1;
            end;
      end;

      LabelDayProts_.Caption := 'Белки:';
      LabelDayFats_.Caption  := 'Жиры:';
      LabelDayCarbs_.Caption := 'Угл.:';

      LabelDayProtsVal.Caption := Info(DayProts, Proc1);
      LabelDayFatsVal.Caption := Info(DayFats, Proc2);
      LabelDayCarbsVal.Caption := Info(DayCarbs, Proc3);

      LabelDayProtsVal.Left := GroupBoxStatistic.Width - LabelDayProtsVal.Width - 2 * BORD;
      LabelDayFatsVal.Left := GroupBoxStatistic.Width - LabelDayFatsVal.Width - 2 * BORD;
      LabelDayCarbsVal.Left := GroupBoxStatistic.Width - LabelDayCarbsVal.Width - 2 * BORD;

      StatProgProts.Visible := Value['Balance'];
      StatProgFats.Visible := Value['Balance'];
      StatProgCarbs.Visible := Value['Balance'];

      if Value['Balance'] then
      begin
        StatProgProts.Progress := Round(DayProts);
        StatProgFats.Progress := Round(DayFats);
        StatProgCarbs.Progress := Round(DayCarbs);
        LabelDayProts_.Left := StatProgProts.Width+3*BORD;
        LabelDayFats_.Left := StatProgProts.Width+3*BORD;
        LabelDayCarbs_.Left := StatProgProts.Width+3*BORD;
      end else
      begin
        LabelDayProts_.Left := 2*BORD;
        LabelDayFats_.Left := 2*BORD;
        LabelDayCarbs_.Left := 2*BORD;
      end;

      LabelDayValue.Caption  := 'Ценность: '+ IntToStr(Round(DayValue)) + ' (ккал)';
      LabelDayMass.Caption   := 'Масса: '   + IntToStr(Round(DayMass)) + ' (г)';
      LabelDayIns.Caption    := 'Инсулин: ' + FloatToStr(DayIns) + ' ед';
    end;
  end;

  { график СК }
  procedure UpdateDayGraph;
  begin
    DrawBS(
      Diary[DiaryView.CurrentDate - 1],
      DiaryView.CurrentPage,
      Diary[DiaryView.CurrentDate + 1],
      ImagePreview,
      True);
  end;

begin
  StartProc('TForm1.UpdateDayInfo()');
  UpdateDayStatistics;
  UpdateDayGraph;
  FinishProc;
end;

// статистика приёма пищи
{==============================================================================}
procedure TForm1.UpdateMealStatistics;
{==============================================================================}
var
  SelMeal: TMealRecord;
begin
  StartProc('TForm1.UpdateMealStatistic()');

  if (DiaryView.SelectedRecord is TMealRecord) then
  begin
    SelMeal := TMealRecord(DiaryView.SelectedRecord);

    LabelDiaryMealProts.Font.Color := clWindowText;
    LabelDiaryMealProts.Caption := 'Белки: '+IntToStr(Round(SelMeal.Prots))+' (г)';
    LabelDiaryMealFats.Caption := 'Жиры: '+IntToStr(Round(SelMeal.Fats))+' (г)';
    LabelDiaryMealCarbs.Caption := 'Углеводы: '+IntToStr(Round(SelMeal.Carbs))+' (г)';
    LabelDiaryMealValue.Caption := 'Ценность: '+IntToStr(Round(SelMeal.Value))+' (ккал)';
    LabelDiaryMealMass.Caption := 'Масса: '+IntToStr(Round(SelMeal.Mass))+' (г)';
  end else
  begin
    LabelDiaryMealProts.Font.Color := clNoData;
    LabelDiaryMealProts.Caption := 'Не выбрано';
    LabelDiaryMealFats.Caption := '';
    LabelDiaryMealCarbs.Caption := '';
    LabelDiaryMealValue.Caption := '';
    LabelDiaryMealMass.Caption := '';
  end;

  FinishProc;
end;

// инсулин и подбор
{==============================================================================}
procedure TForm1.UpdateMealDose;
{==============================================================================}

  procedure SetCorrectionHint(Delta: real);
  begin
    LabelDiaryCorrection.ShowHint := False;
    if Delta = 0 then
    begin
      LabelDiaryCorrection.Caption := 'Коррекция:  нет';
      LabelDiaryCorrection.Font.Color := CC_NONE;
    end else
    begin
      LabelDiaryCorrection.Caption := 'Коррекция:  '+FloatToStrP(Delta)+' ммоль/л';
      if abs(Delta)<3 then
        LabelDiaryCorrection.Font.Color := CC_LOW else
      if abs(Delta)<6 then
        LabelDiaryCorrection.Font.Color := CC_MIDDLE
      else
      begin
        LabelDiaryCorrection.Font.Color := CC_HIGH;
        LabelDiaryCorrection.ShowHint := True;
      end;
    end;
  end;

var
  StartBlood: TBloodRecord;
  Ins: TInsRecord;
  Delta: real;
  ExpectedBS: Real;
  i: integer;
  Dose: real;
  DCarbs: real;
  s: string;
  Koof: TKoof;
  SelMeal: TMealRecord;

begin
  StartProc('TForm1.UpdateMealDose()');

  // блюдо не выбрано, очищаем
  if not (DiaryView.SelectedRecord is TMealRecord) then
  begin
    LabelDiaryMealDose.Font.Color := clNoData;
    LabelDiaryMealDose.Font.Style := [];
    LabelDiaryMealDose.Caption := 'Не выбрано';

    LabelDiaryCorrection.Caption := '';

    LabelDiaryMealExpectedBS.Caption := '';

    ListCorrectCarbs.Clear;

    LabelCorrectionEmpty.Caption := '  Не выбрано';
    LabelCorrectionEmpty.Font.Color := clNoData;
    LabelCorrectionEmpty.Show;
    ListCorrectCarbs.Hide;
  end else

  // блюдо выбрано, информируем 
  begin
    { ищем приём пищи, замер и инъекцию }
    SelMeal := TMEalRecord(DiaryView.SelectedRecord);

    StartBlood := TBloodRecord(
      Diary.FindRecord(
        TBloodRecord,
        DiaryView.CurrentDate,
        SelMeal.Time,
        BLOOD_ACTUALITY_TIME,
        sdBack  // TODO: Around?
      )
    );

    Ins := TInsRecord(
      Diary.FindRecord(
        TInsRecord,
        DiaryView.CurrentDate,
        SelMeal.Time,
        INS_ACTUALITY_TIME,
        sdAround
      )
    );

    { Определяем коэффициенты }
    Koof := GetKoof(SelMeal.Time);

    { Коррекция СК }
    if (StartBlood <> nil) then
      Delta := Value['TargetBS'] - StartBlood.Value
    else
      Delta := 0;

    SetCorrectionHint(Delta);

    LabelDiaryMealDose.Font.Color := clBlack;
    LabelDiaryMealDose.Font.Style := [fsBold];

    // есть коэффициенты
    if (Koof.q > 0) then
    begin
      { доза }
      Dose := (Koof.k * SelMeal.Carbs + Koof.p * SelMeal.Prots - Delta) / Koof.q;
      if (Dose >= 0) then
        LabelDiaryMealDose.Caption := 'Расчётная доза: ' + RealToStr(Dose)
      else
        LabelDiaryMealDose.Caption := 'Расчётная доза:  --';



      { <вспомогательный список> }

      if (Koof.K <> 0) then
      begin
        { расчёт CurrentDB, установка ItemIndex }
        if (Ins <> nil) then
        begin
          CurrentDB := Delta +
            Koof.q * Ins.Value -
            Koof.k * SelMeal.Carbs -
            Koof.p * SelMeal.Prots;

          if (Ins.Value > Value['MaxDose']) then
              Value['MaxDose'] := Ins.Value;

          if (StartBlood <> nil) then
            ExpectedBS := StartBlood.Value + Koof.k * SelMeal.Carbs + Koof.p * SelMeal.Prots - Koof.q * Ins.Value
          else
            ExpectedBS := -1;
        end else
        begin
          CurrentDB := 0;
          if (StartBlood <> nil) then
            ExpectedBS := StartBlood.Value + Koof.k * SelMeal.Carbs + Koof.p * SelMeal.Prots
          else
            ExpectedBS := -1;
        end;

        if (ExpectedBS > -1) then
          LabelDiaryMealExpectedBS.Caption := Format('Ожидаемый СК: %.1f', [ExpectedBS])
        else
          LabelDiaryMealExpectedBS.Caption := 'Ожидаемый СК: ?';


        ListCorrectCarbs.Clear;
        for i := 0 to Value['MaxDose'] do
        begin
          DCarbs := Round((Delta+Koof.q*i-Koof.P*SelMeal.Prots)/Koof.K-SelMeal.Carbs);
          if DCarbs>0 then
            s := '+' + RealToStr(DCarbs)+'г'
          else
            s := RealToStr(DCarbs) + 'г';

          with ListCorrectCarbs.Items.Add do
          begin
            Caption := IntToStr(i) + ' ед.';
            if SelMeal.Carbs + DCarbs >= 0 then
            begin
              SubItems.Add(s);
              SubItems.Add(IntToStr(Round(SelMeal.Carbs+DCarbs)) + 'г');
            end else
            begin
              SubItems.Add('--');
              SubItems.Add('--');
            end;
          end;
        end;

        LabelCorrectionEmpty.Hide;

        {
        При выполнении ListCorrectCarbs.Show закладка может переключиться на PageDiary, если до того она была
        другой. Это вызывает баг: когда мы нажимаем на кнопку "Пересчитать" на закладке "Анализ" и при этом в
        дневнике выбран приём пищи, закладка переключается на "Дневник". Для решения этой проблемы вместо метода
        Show() будем использовать свойство Visible.
        }
        //ListCorrectCarbs.Show;
        ListCorrectCarbs.Visible := True;

        if (Ins <> nil) then
          ShowTableItem(ListCorrectCarbs, Round(Ins.Value));
      end else
      begin
        LabelCorrectionEmpty.Caption := 'Ошибка: K=0';
        LabelCorrectionEmpty.Show;
        ListCorrectCarbs.Hide;
      end;
      { </вспомогательный список> }
    end else
    // нет коэффициентов
    begin
      LabelDiaryMealDose.Caption := 'Нет коэффициентов';
      LabelCorrectionEmpty.Caption := 'Нет коэффициентов';
      LabelCorrectionEmpty.Show;
      ListCorrectCarbs.Hide;
    end;
  end;

  FinishProc;
end;

{ ============================== РЕДАКТОРЫ ================================ }

{==============================================================================}
function ShowBloodEditor(var Time: integer; var AValue: real; var AFinger: integer;
  New: boolean; Focus: TFocusMode): boolean;
{==============================================================================}
var
  P: TDialogParams;
  StrValue: string;
begin
  Log(DEBUG, 'ShowBloodEditor()');

  P.Image := Form1.ButtonAddBlood.Glyph;

  if Value['ColoredEditors'] then
    P.Color := Value['Color_SelBlood']
  else
    P.Color := clBtnFace;

  P.Caption := 'Замер СК';
  P.CaptionTime := 'Время';
  P.CaptionValue := 'Значение';
  P.CaptionFinger := 'Палец';
  P.CaptionOK := SAVE_CAPTION[New];
  P.CaptionCancel := 'Отмена';
  P.FocusMode := Focus;
  P.ShowEditors := edTimeValueFinger;
  P.CheckValue := CheckPositiveFloat;

  if New then
  begin
    StrValue := '';
    Time := GetCurrentMinutes;
  end else
    StrValue := FloatToStr(AValue);

  if ShowEditor(Time, StrValue, AFinger, P) then
  begin
    AValue := StrToFloat(CheckDot(StrValue));
    Result := True;
  end else
    Result := False;
end;

{==============================================================================}
function ShowInsEditor(var Time: integer; var AValue: real; New: boolean;
  Focus: TFocusMode): boolean;
{==============================================================================}
var
  P: TDialogParams;
  StrValue: string;
begin
  Log(DEBUG, 'TForm1.ShowInsEditor()');

  P.Image := Form1.ButtonAddIns.Glyph;

  if Value['ColoredEditors'] then
    P.Color := Value['Color_SelIns']
  else
    P.Color := clBtnFace;

  P.Caption := 'Инъекция';
  P.CaptionTime := 'Время';
  P.CaptionValue := 'Доза';
  P.CaptionOK := SAVE_CAPTION[New];
  P.CaptionCancel := 'Отмена';
  P.FocusMode := Focus;
  P.ShowEditors := edTimeValue;
  P.CheckValue := CheckPositiveFloat;

  if New then
  begin
    StrValue := '';
    Time := GetCurrentMinutes;
    // TODO: hack :)
    if (Time mod 2) = 1 then
      dec(Time);
  end else
    StrValue := FloatToStr(AValue);

  if ShowEditor(Time, StrValue, P) then
  begin
    AValue := StrToFloat(CheckDot(StrValue));
    Result := True;
  end else
    Result := False;
end;

{==============================================================================}
function ShowMealEditor(var Time: integer; {var AValue: real;} New: boolean
  {;Focus: TFocusMode}): boolean;
{==============================================================================}
var
  P: TDialogParams;
  StrValue: string;
begin
  Log(DEBUG, 'TForm1.ShowMealEditor()');
  P.Image := Form1.ButtonAddMeal.Glyph;

  if Value['ColoredEditors'] then
    P.Color := Value['Color_SelMeal']
  else
    P.Color := clBtnFace;

  P.Caption := 'Приём пищи';
  P.CaptionTime := 'Время';
  P.CaptionValue := '***';
  P.CaptionOK := SAVE_CAPTION[New];
  P.CaptionCancel := 'Отмена';
  P.FocusMode := fmTime;
  P.ShowEditors := edTime;
  P.CheckValue := nil;//CheckPositiveFloat;

  if New then
  begin
    StrValue := '';
    Time := GetCurrentMinutes;
  end else
    StrValue := '';//FloatToStr(AValue);

  if ShowEditor(Time, StrValue, P) then
  begin
    //AValue := StrToFloat(CheckDot(StrValue));
    Result := True;
  end else
    Result := False;
end;

{==============================================================================}
function ShowNoteEditor(var Time: integer; var AValue: string; New: boolean;
  Focus: TFocusMode): boolean;
{==============================================================================}
var
  P: TDialogParams;
  StrValue: string;
begin
  Log(DEBUG, 'TForm1.ShowNoteEditor()');
  P.Image := Form1.ImageNote.Picture.Bitmap;

  if Value['ColoredEditors'] then
    P.Color := Value['Color_SelNote']
  else
    P.Color := clBtnFace;

  P.Caption := 'Заметка';
  P.CaptionTime := 'Время';
  P.CaptionValue := 'Текст';
  P.CaptionOK := SAVE_CAPTION[New];
  P.CaptionCancel := 'Отмена';
  P.FocusMode := Focus;
  P.ShowEditors := edTimeValue;
  P.CheckValue := nil;

  if New then
  begin
    StrValue := '';
    Time := GetCurrentMinutes;
  end else
    StrValue := AValue;

  if ShowEditor(Time, StrValue, P) then
  begin
    AValue := StrValue;
    Result := True;
  end else
    Result := False;
end;

{==============================================================================}
function TForm1.ClickBlood(New: boolean; Focus: TFocusMode): integer;
{==============================================================================}
var
  ATime: integer;
  AValue: real;
  AFinger: integer;

  BloodRec: TBloodRecord;
begin
  Log(DEBUG, 'TForm1.ClickBlood');

  if New then
  begin
    // определение пальца
    AFinger := Diary.GetNextFinger();

    if ShowBloodEditor(ATime, AValue, AFinger, New, Focus) then
    begin
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      Result := DiaryView.CurrentPage.Add(TBloodRecord.Create(ATime, AValue, AFinger));
      //Result := DiaryView.AddBlood(Time, Value, Finger);
    end else
      Result := -1;
  end else
  begin
    BloodRec := TBloodRecord(DiaryView.SelectedRecord);

    ATime := BloodRec.Time;
    AValue := BloodRec.Value;
    AFinger := BloodRec.Finger;

    // TODO: refactor, pass whole TBloodRecord

    if ShowBloodEditor(ATime, AValue, AFinger, New, Focus) then
    with BloodRec do
    begin
      BeginUpdate;

      Time   := ATime;
      Value  := AValue;
      Finger := AFinger;

      EndUpdate;
    end;
    Result := DiaryView.SelectedRecordIndex;
  end;
end;

{==============================================================================}
function TForm1.ClickIns(New: boolean; Focus: TFocusMode): integer;
{==============================================================================}
var
  ATime: integer;
  AValue: real;
  InsRecord: TInsRecord;
begin
  Log(DEBUG, 'TForm1.ClickIns');
  if New then
  begin
    if ShowInsEditor(ATime, AValue, New, Focus) then
    begin
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      Result := DiaryView.CurrentPage.Add(TInsRecord.Create(ATime, AValue));
      //Result := DiaryView.AddIns(Time,Value);
    end else
      Result := -1;
  end else
  begin
    InsRecord := TInsRecord(DiaryView.SelectedRecord);

    ATime := InsRecord.Time;
    AValue := InsRecord.Value;
    if ShowInsEditor(ATime, AValue, New, Focus) then
    with InsRecord do
    begin
      BeginUpdate;
      Time  := ATime;
      Value := AValue;
      EndUpdate;
    end;
    Result := DiaryView.SelectedRecordIndex;
  end;
end;

{==============================================================================}
function TForm1.ClickMeal(New: boolean): integer;
{==============================================================================}
var
  ATime: integer;
  Meal: TMealRecord;
begin
  Log(DEBUG, 'TForm1.ClickMeal');
  if New then
  begin
    if ShowMealEditor(ATime, New) then
    begin
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      Result := DiaryView.CurrentPage.Add(TMealRecord.Create(ATime, False));
    end else
      Result := -1;
  end else
  begin
    Meal := TMealRecord(DiaryView.SelectedRecord);

    ATime := Meal.Time;
    if ShowMealEditor(ATime, New) then
    with Meal do
    begin
      //BeginUpdate;
      Time := ATime;
      //EndUpdate; - зачем?
      UpdateTimeLeft;
      ComboDiaryNew.SetFocus;
    end;
    Result := DiaryView.SelectedRecordIndex;
  end;
end;

{==============================================================================}
function TForm1.ClickNote(New: boolean; Focus: TFocusMode): integer;
{==============================================================================}
var
  Note: TNoteRecord;
  ATime: integer;
  AValue: string;
begin
  Log(DEBUG, 'TForm1.ClickNote');
  if New then
  begin
    if ShowNoteEditor(ATime, AValue, New, Focus) then
    begin
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      Result := DiaryView.CurrentPage.Add(TNoteRecord.Create(ATime, AValue));
    end else
      Result := -1;
  end else
  begin
    // TODO: use TRec instead of scope of field-variables
    Note := TNoteRecord(DiaryView.SelectedRecord);

    ATime := Note.Time;
    AValue := Note.Text;
    if ShowNoteEditor(ATime, AValue, New, Focus) then
    begin
      with Note do
      begin
        BeginUpdate;
        Time := ATime;
        Text := AValue;
        EndUpdate;
      end;
    end;
    Result := DiaryView.SelectedRecordIndex;
  end;
end;

{==============================================================================}
procedure TForm1.DiaryViewDoubleClickBlood(Sender: TObject; Index: Integer;
  Place: TClickPlace);
{==============================================================================}
begin
  case Place of
    cpTime:  ClickBlood(False, fmTime);
    cpRec,cpPanel: ClickBlood(False, fmValue);
  end;
end;

{==============================================================================}
procedure TForm1.DiaryViewDoubleClickIns(Sender: TObject; Index: Integer;
  Place: TClickPlace);
{==============================================================================}
begin
  case Place of
    cpTime:  ClickIns(False,fmTime);
    cpRec,cpPanel: ClickIns(False,fmValue);
  end;
end;

{==============================================================================}
procedure TForm1.DiaryViewDoubleClickMeal(Sender: TObject; Index: Integer;
  Place: TClickPlace);
{==============================================================================}
var
  NewMass: real;
begin
  case Place of
    cpTime:  ClickMeal(False);
    cpPanel: ClickMeal(False);
    cpRec: begin
             NewMass := DiaryView.SelectedFood.Mass;
             if DialogFoodMass(
               dtChangeMass,DiaryView.SelectedFood.Name,NewMass)
             then
               DiaryView.SelectedFood.Mass := NewMass;

             //DiaryView.EditFoodMass(NewMass);
           end;
  end;
end;

{==============================================================================}
procedure TForm1.DiaryViewDoubleClickNote(Sender: TObject; Index: Integer;
  Place: TClickPlace);
{==============================================================================}
begin
  case Place of
    cpTime:  ClickNote(False,fmTime);
    cpRec,cpPanel: ClickNote(False,fmValue);
  end;
end;

{==============================================================================}
procedure TForm1.ResetTabStop(Sender: TObject);
{==============================================================================}
begin
  TWinControl(Sender).TabStop := False;
end;

{==============================================================================}
procedure AnalyzeCallBack(Progress: integer);
{==============================================================================}
begin
  Form1.StatusBar.Panels[1].Text := Format('Анализ дневника... %d%%', [Progress]);
  Application.ProcessMessages;
end;

{==============================================================================}
procedure TForm1.UpdateKoofs;
{==============================================================================}
var
  Par: TRealArray;
  FromDate, ToDate: TDate;
begin
  if (GetAnalyzersCount = 0) then
  begin
    //LabelDllInfo.Font.Color := clRed;
    //LabelDllInfo.Caption := 'Модуль анализа не найден';
    LabelCalcTime.Caption := '';
    LabelAvgDeviation.Caption := '';
    ButtonUpdateKoof.Enabled := False;
    ComboKoof.Enabled := False;
    Exit;
  end;

  StartProc('TForm1.UpdateKoofs()');
  
  TimerTimeLeft.Enabled := False;
  //LabelCalcTime.Caption := 'Время расчёта: ...';
  //LabelAvgDeviation.Caption := 'Ошибка: ...';
  //LabelWeight.Caption := 'Вес: ...';
  ButtonUpdateKoof.Caption := 'Расчёт...';
  Application.ProcessMessages;

  ToDate := Trunc(Now);
  FromDate := ToDate - Value['DaysProcess'] + 1;

  {===============================================================}
  {#}SetLength(Par, 1);
  {#}Par[PAR_ADAPTATION] := Value['Adaptation'];  { [0.5..1] }
  {#}AnalyzeDiary(Diary, FromDate, ToDate, Par, AnalyzeCallBack);
  {===============================================================}

  ButtonUpdateKoof.Caption := 'Пересчитать';

  if (ComboKoof.ItemIndex <> -1) then
    ComboKoofChange(nil);

  if (DiaryView.SelectedRecord is TMealRecord) then
    UpdateMealDose;

  TimerTimeLeft.Enabled := True;

  FinishProc;
end;

{==============================================================================}
function TForm1.GetCompensationMass(const RelCarbs, RelProts: real): real;
{==============================================================================}
{ используется при выборе продукта/блюда в комбобоксе внизу }
var
  Kf: TKoof;
  RelBS: real;
begin
  Log(DEBUG, 'TForm1.GetCompensationMass()');

  if (DiaryView.SelectedRecord is TMealRecord) then
  begin
    Kf := GetKoof(TMealRecord(DiaryView.SelectedRecord).Time);
    RelBS := (RelCarbs*Kf.k + RelProts*Kf.p)/100;
    if (RelBS > 0) then
      Result := CurrentDB/RelBS
    else
      Result := 0;
  end else
    Result := 0;
end;

{==============================================================================}
function TForm1.GetBestMass(const RelCarbs, RelProts, CurMass: real): real;
{==============================================================================}
{ подбор массы в приёме пищи }
var
  Kf: TKoof;
  RelBS: real;
begin
  if (DiaryView.SelectedRecord is TMealRecord) then
  begin
    Kf := GetKoof(TMealRecord(DiaryView.SelectedRecord).Time);
    RelBS := (RelCarbs*Kf.k + RelProts*Kf.p)/100;
    if (RelBS > 0) then
      Result := (CurrentDB+RelBS*CurMass)/RelBS
    else
      Result := 0;
  end else
    Result := 0;
end;

{==============================================================================}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
{==============================================================================}
begin
  StartProc('TForm1.FormClose');

  FormProcess.Show;
  FormProcess.SetMax(3);

  if Value['AutoSync'] and WebClient.Online then
  begin
    {*}ShowProcess('Синхронизация');
    MySyncDiary;
  end;

  {*}ShowProcess('Сохранение настроек');
  SaveSettings;
  SaveExpander;

  {*}ShowProcess('Освобождение ресурсов');
  FullFree;

  FinishProc;
  SaveLog;
end;

{==============================================================================}
procedure TForm1.TimerAutosaveTimer(Sender: TObject);
{==============================================================================}
begin
  TimerAutosave.Enabled := False;
  try
    try
      if (Value['UpdateKMode'] = 1) then { UpdateKOnChange }
        UpdateKoofs();

      if (Value['AutoSync']) then
        // StatusBar.Panels[3].Text := 'Загрузка на сервер...';
        TaskSaveAndSync(nil);
      //ThreadExec.Execute(TaskSaveAndSync, tpIdle, 80000, TASK_SAVE_N_SYNC);

      SaveLog;
    finally
      TimerAutosave.Enabled := True;
    end;
  except
    on E: Exception do
    begin
      Log(ERROR, 'Caught exception in TimerAutosaveTimer(): ' + E.Message, True);
      ErrorMessage('При синхронизации произошла ошибка'#13 + E.Message);
    end;
  end;
end;

{==============================================================================}
procedure TForm1.DiaryViewFoodShow(Sender: TObject; Index, Line: Integer;
  var Text: String);
{==============================================================================}
var
  R: TKoof;
begin
  R := GetKoof(DiaryView.CurrentPage[Index].Time);

  if (R.q > 0) then
    Text := ' [+'+
      RealToStr(
        TMealRecord(DiaryView.CurrentPage[Index]).Food[Line].Carbs * R.k +
        TMealRecord(DiaryView.CurrentPage[Index]).Food[Line].Prots * R.p)
      +']'
  else
    //Text := ' [?]';
    Text := '';
end;

{==============================================================================}
procedure TForm1.ComboKoofChange(Sender: TObject);
{==============================================================================}
const
  GraphTypes: array[0..3] of TKoofType = (kfK, kfQ, kfP, kfX);
var
  Analyzer: TAnalyzer;
  KoofIndex: integer;
  AnalyzerIndex: integer;
  KoofList: TKoofList;
begin
  KoofIndex := ComboKoof.ItemIndex;
  if (KoofIndex = -1) then Exit;

  AnalyzerIndex := ComboAnalyzers.ItemIndex;
  if (AnalyzerIndex = -1) then Exit;

  StartProc('ComboKoofChange');

  if (KoofIndex >= 0) and (KoofIndex <= 3) then
  begin
    if (AnalyzerIndex > 0) then
    begin
      Analyzer := GetAnalyzer(AnalyzerIndex - 1);
    end else
      Analyzer := AvgAnalyzer;

    KoofList := Analyzer.KoofList;
    LabelCalcTime.Caption := Format('Время расчёта: %d мсек', [Analyzer.Time]);
    LabelAvgDeviation.Caption := Format('Ошибка: ±%.2f ммоль/л', [Analyzer.Error]);
    LabelWeight.Caption := Format('Вес: %.0f', [Analyzer.Weight * 100]) + '%';

    DrawKoof(ImageLarge, KoofList, AnList, GraphTypes[KoofIndex], ADVANCED_MODE or Value['ShowPoints']);
    LabelKoofDiscription.Caption := KoofDisc[KoofIndex];
  end;

  FinishProc;
end;

{==============================================================================}
procedure TForm1.ButtonUpdateKoofClick(Sender: TObject);
{==============================================================================}
begin
  ButtonUpdateKoof.Enabled := False;
  UpdateKoofs;
  ButtonUpdateKoof.Enabled := True;
end;

{==============================================================================}
procedure TForm1.PageControl1Change(Sender: TObject);
{==============================================================================}
begin
  if (PageControl1.ActivePage = TabAnalyze)and
     (ComboKoof.ItemIndex = -1) then
  begin
    ComboKoof.ItemIndex := 3;
    ComboKoofChange(nil);
  end else
  if (PageControl1.ActivePage = TabBase) then
  begin
    with ListFood do
    begin
      Height := Height + 1;
      Height := Height - 1;
      //Columns[0].Width := Columns[0].Width - 10; - тормозит
    end;
    with ListDish do
    begin
      Height := Height + 1;
      Height := Height - 1;
      //Columns[0].Width := Columns[0].Width - 10; - тормозит
    end;
  end;
end;

{==============================================================================}
procedure TForm1.ScrollToSelected;
{==============================================================================}
var
  n: integer;
begin
  if (DiaryView.CurrentPage <> nil) then
  begin
    n := DiaryView.SelectedRecordIndex;
    { вторая проверка, наверно, излишняя }
    if (n > -1)and(DiaryView.CurrentPage.Count > 0) then
    ScrollBoxDiary.VertScrollBar.Position := Round(
      n / DiaryView.CurrentPage.Count * ScrollBoxDiary.VertScrollBar.Range);
  end;
end;

{==============================================================================}
procedure TForm1.ButtonAvgBSClick(Sender: TObject);
{==============================================================================}
{const
  PERIOD = 30;
var
  Summ: real;
  i,j,Count: integer;
  FirstDate, LastDate: integer;
  Page: TDiaryPage;   }
begin
  {Summ := 0;
  Count := 0;

  LastDate := Trunc(Now);
  FirstDate := LastDate - PERIOD;

  for i := FirstDate to LastDate do
  begin
    Page := DiaryBase[i];
    for j := 0 to Page.Count-1 do
    if (Page[j].TagType = rtBlood)and
       (not TBloodRecord(Page[j]).PostPrand) then
    begin
      inc(Count);
      Summ := Summ + TBloodRecord(Page[j]).Value;
    end;
  end;

  if Count>0 then
    Summ := Summ/Count;

  ShowMessage(FloatToStr(Summ)); }
end;

{==============================================================================}
procedure TForm1.ActionExitExecute(Sender: TObject);
{==============================================================================}
begin
  Close;
end;

{==============================================================================}
procedure TForm1.ActionAddRecordExecute(Sender: TObject);
{==============================================================================}
begin
  TrayIcon.ShowMainForm;
  PageControl1.ActivePageIndex := 0;
  case TControl(Sender).Tag of
    1: if ClickBlood(True, fmTime) > -1 then ScrollToSelected;
    2: if ClickIns(True,fmTime)   > -1 then ScrollToSelected;
    3: if ClickMeal(True)         > -1 then ScrollToSelected;
    4: if ClickNote(True,fmTime) > -1 then ScrollToSelected;
  end;
end;

{==============================================================================}
procedure TForm1.ActionExportDiaryExecute(Sender: TObject);
{==============================================================================}
begin
  FormExportText.ShowModal;
end;

{==============================================================================}
procedure TForm1.ActionSyncExecute(Sender: TObject);
{==============================================================================}
begin
  //ThreadExec.Execute(TaskSaveAndSync, tpIdle, 80000, TASK_SAVE_N_SYNC);
  TaskSaveAndSync(nil);
end;

{==============================================================================}
procedure TForm1.ActionSettingsExecute(Sender: TObject);
{==============================================================================}
begin
  FormSettings.OpenSettings;
end;

{==============================================================================}
procedure TForm1.ActionAboutExecute(Sender: TObject);
{==============================================================================}
begin
  FormAbout.ShowModal;
end;

{==============================================================================}
procedure TForm1.ActionExportKoofsExecute(Sender: TObject);
{==============================================================================}
var
  S: string;
begin
  with TStringList.Create do
  begin
    ExportKoofs(False, S);
    Add(S);
    SaveToFile('Koofs.json');
    Free;
  end;
end;

{==============================================================================}
procedure TForm1.ActionExportFoodExecute(Sender: TObject);
{==============================================================================}
{var
  i: integer; }
begin
  {with TStringList.Create do
  try
    for i := 0 to FoodBase.Count-1 do
      Add(
        FoodBase[i].Name+#9+
        Format('%.1f',[FoodBase[i].Carbs])
      );
    SaveToFile('FBase.txt');
  finally
    Free;
  end;   }
end;

{==============================================================================}
procedure TForm1.PanelDiaryTopLeftDblClick(Sender: TObject);
{==============================================================================}
begin
  ImageMinMaxStatMouseDown(nil,mbLeft,[],0,0);
  ImageMinMaxGraphMouseDown(nil,mbLeft,[],0,0);
  ImageMinMaxTimesMouseDown(nil,mbLeft,[],0,0);
end;

{==============================================================================}
procedure TForm1.MyIdle(Sender: TObject; var Done: Boolean);
{==============================================================================}
begin
  Done := True;

  if IgnoreIdleOnce then
  begin
    IgnoreIdleOnce := False;
    Exit;
  end;

  //StartProc('MyIdle');
  try


  { ================ Общие ==================== }

  case PageControl1.ActivePageIndex of
    0: { дневник }
    begin
      ProcessMealSelected(DiaryView.SelectedRecord is TMealRecord);
      if WebClient.Online then
        Form1.StatusBar.Panels[2].Text := STATUS_STATE_ONLINE
      else
        Form1.StatusBar.Panels[2].Text := STATUS_STATE_OFFLINE;
    end;

    1: { базы }
    begin
      ButtonDeleteFood.Enabled := ListFood.ItemIndex > -1;
      ButtonDeleteDish.Enabled := ListDish.ItemIndex > -1;

      {Item_EditFood.Enabled   := ListFood.ItemIndex > -1;
      Item_RemoveFood.Enabled := ListFood.ItemIndex > -1;
       }
     { if ListFood.ItemIndex > -1 then
        ListFood.PopupMenu := PopupFoodBase
      else
        ListFood.PopupMenu := nil;  }
    end;
  end;

  ActionHideWindow.Enabled := Visible;

  { ============== Флаговые ============== }

{  if ON_IDLE_UpdateKoofs then
  begin
    ON_IDLE_UpdateKoofs := False;
    UpdateKoofs;
    DiaryView.Paint;
    Done := False;
    UploadKoofs;
    Exit;
  end;    }

  if ON_IDLE_UpdateCombos then
  begin
    ON_IDLE_UpdateCombos := False;
    UpdateCombos;
    Done := False;
    Exit;
  end;

  if (ON_IDLE_ShowBases)
     {and (PageControl1.ActivePageIndex = 1)} then
  begin
    ON_IDLE_ShowBases := False;

    UpdateFoodTable(True);
    UpdateDishTable(True);

    { после Maximize }
    //ListFood.Columns[0].Width := ListFood.Columns[0].Width - 30;
    //ListDish.Columns[0].Width := ListDish.Columns[0].Width - 30;

    Done := False;
    Exit;
  end;

  if IN_IDLE_CheckUpdates then
  begin
    IN_IDLE_CheckUpdates := False;

    if CheckUpdates(AnExpDate) = urCanUpdate then
      ShowBalloon(BALLOON_INFO_NEW_VERSION_AVAILABLE, bitInfo, BalloonAction_StartUpdate);

    Done := False;
    Exit;
  end;

  finally
    //FinishProc;
  end;
end;

{==============================================================================}
procedure TForm1.MyActivate(Sender: TObject);
{==============================================================================}
begin
  StartProc('TForm1.MyActivate');
  TimerTimeLeftTimer(nil);
  TimerTimeLeft.Interval := 1000;
  FinishProc;
end;

{==============================================================================}
procedure TForm1.MyDeactivate(Sender: TObject);
{==============================================================================}
begin
  Log(DEBUG, 'TForm1.MyDeactivate');
  TimerTimeLeft.Interval := 60000;
end;

procedure TForm1.ButtonBSListClick(Sender: TObject);
{type
  TBloodItem = record
    Value: real;
    Count: integer;
  end;

var
  List: array of TBloodItem;

  function Find(Value: real): integer;
  var
    i: integer;
  begin
    Result := -1;
    for i:=0 to High(List) do
    if List[i].Value = Value then
    begin
      Result := i;
      Exit;
    end;
  end;

  procedure Add(Value: real);
  begin
    SetLength(List, length(List) + 1);
    List[High(List)].Value := Value;
    List[High(List)].Count := 1;
  end;

  function More(n1,n2: integer): boolean;
  begin
    Result := List[n1].Value > List[n2].Value;
  end;

  procedure Exch(n1,n2: integer);
  var
    temp: TBloodItem;
  begin
    temp := List[n1];
    List[n1] := List[n2];
    List[n2] := temp;
  end;

var
  i,j,k: integer;   }
begin
 { for i := Trunc(Now)-29 to Trunc(Now) do
  for j := 0 to DiaryBase[i].Count-1 do
  if DiaryBase[i][j].TagType = rtBlood then
  begin
    k := Find(TBloodRecord(DiaryBase[i][j]).Value);
    if k = -1 then
      Add(TBloodRecord(DiaryBase[i][j]).Value)
    else
      List[k].Count := List[k].Count + 1;
  end;

  //QuickSort(More, Exch, 0, High(List));

  with ListBS do
  begin
    Clear;
    for i := 0 to High(List) do
      Items.Add(
        RealToStrZero(List[i].Value)+#9+IntToStr(List[i].Count));
  end; }
end;

procedure TForm1.ButtonAvgBSDynamicClick(Sender: TObject);
const
  AVG_PERIOD  = 30;
  LOOK_PERIOD = 1370;
var
  //i,j,k: integer;
  //Summ: real;
  //CurWeight: real;
  //SummWeight: real;

  ToDate: TDate;
  Mean, StdDev, Targeted, Less, More: Extended;
begin
  ListBS.Clear;

  for ToDate := Trunc(now) - LOOK_PERIOD to Trunc(Now) do
  begin
    AnalyzeBS(Diary, ToDate - AVG_PERIOD, ToDate, Mean, StdDev, Targeted, Less, More);
    ListBS.Items.Add(Format('%s'#9'%.2f'#9'%.2f'#9'%.1f'#9'%.1f', [DateToStr(ToDate - (AVG_PERIOD div 2)), Mean, StdDev, Less * 100, Targeted * 100]));
  end;

  ListBS.Items.SaveToFile('temp\BS.txt');

  {for i := Trunc(Now)-LOOK_PERIOD to Trunc(Now) do
  begin
    Summ := 0;
    SummWeight := 0;
    for j := i - AVG_PERIOD + 1 to i do
    for k := 0 to DiaryBase[j].Count-1 do
    if (DiaryBase[j][k].TagType = rtBlood)and
       (not TBloodRecord(DiaryBase[j][k]).PostPrand) then
    begin
      CurWeight := (j - (i - AVG_PERIOD + 1)) / (AVG_PERIOD - 1);
      Summ := Summ + CurWeight*TBloodRecord(DiaryBase[j][k]).Value;
      SummWeight := SummWeight + CurWeight;
    end;

    if SummWeight > 0 then
      Summ := Summ / SummWeight;

    ListBS.Items.Add(DateToStr(DiaryBase[i].Date) + '    ' + FloatToStr(Summ));
  end;  }
end;

{==============================================================================}
procedure TForm1.ComboValueChange(Sender: TObject);
{==============================================================================}
type
  TRecItem = record
    Date: TDate;
    Value: real;
  end;

var
  List: array of TRecItem;
  i: integer;
  FirstDate: TDate;
  LastDate: TDate;

  procedure qsort(l, r: integer);
  var
    i, j: integer;
    x: real;
    y: TRecItem;
  begin
    i := l;
    j := r;
    x := List[(l+r) div 2].Value;
    repeat
      while List[i].Value < x do inc(i);
      while List[j].Value > x do dec(j);
      if i <= j then
      begin
        if List[i].Value <> List[j].Value then
        begin
          y := List[i];
          List[i] := List[j];
          List[j] := y;
        end;
        inc(i);
        dec(j);
      end;
    until i > j;
    if l < j then qsort(l, j);
    if i < r then qsort(i, r);
  end;

//var
//  Summ: real;
begin
  StartProc('TForm1.ComboValueChange');
  ListCB.Clear;

  if ComboValue.ItemIndex = -1 then Exit;
  //if not LocalSource.GetFirstDate(FirstDate) then Exit;
  //if not LocalSource.GetLastDate(LastDate) then Exit;

  FirstDate := Trunc(EncodeDate(2010, 01, 01));
  LastDate := Trunc(Now());
  //Summ := 0;

  for i := FirstDate to LastDate do
  begin
    SetLength(List, Length(list) + 1);
    List[High(list)].Date := i;
    case ComboValue.ItemIndex of
      0: List[High(list)].Value := Diary[i].DayProts;
      1: List[High(list)].Value := Diary[i].DayFats;
      2: List[High(list)].Value := Diary[i].DayCarbs;
      3: List[High(list)].Value := Diary[i].DayValue;
      4: List[High(list)].Value := Diary[i].DayMass;
    end;

    //Summ := Summ + Diary[i].DayCarbs;
  end;

  if length(list) > 0 then
  begin
    qsort(0,High(list));
    //Summ := Summ / (LastDate - FirstDate + 1);
  end;

  //ShowMessage(FloatTOStr(summ));

  with ListCB do
  begin
    Clear;
    for i := 0 to High(List) do
      Items.Add(RealToStrZero(List[i].Value)+':    '+DateToStr(List[i].Date));
  end;

  FinishProc;
end;

{==============================================================================}
procedure TForm1.ProcMessage(var M: TMessage);
{==============================================================================}
begin
  TrayIcon.ShowMainForm;
end;

{==============================================================================}
procedure TForm1.BalloonAction_ShowForm;
{==============================================================================}
begin
  TrayIcon.ShowMainForm;
end;

{==============================================================================}
procedure TForm1.BalloonAction_ShowInternetSettings;
{==============================================================================}
begin
  TrayIcon.ShowMainForm;
  FormSettings.OpenSettings(spInternet);
end;

{==============================================================================}
procedure TForm1.BalloonAction_StartUpdate;
{==============================================================================}
begin
  SetupUpdate(True, AnExpDate);
end;

{==============================================================================}
procedure TForm1.TrayIconBalloonHintClick(Sender: TObject);
{==============================================================================}
begin
  {case BalloonAction of
    baJustShow:


    baStartUpdate:


    baShowInternetSettings:
      begin

      end;
  end;}
  if (Assigned(BalloonAction)) then
    BalloonAction();
end;

{==============================================================================}
procedure TForm1.ShowBalloon(const Msg: string; MsgType: TBalloonHintIcon;
  AfterAction: TBalloonAction);
{==============================================================================}
var
  Time: integer;
begin
  case MsgType of
    bitInfo:              Time := 30;
    bitWarning, bitError: Time := 30; // max
    else                  Time := 30;
  end; // cute

  BalloonAction := AfterAction;
  TrayIcon.ShowBalloonHint(PROGRAM_TITLE, Msg, MsgType, Time);
end;

{==============================================================================}
procedure TForm1.ActionBalanceOnOffExecute(Sender: TObject);
{==============================================================================}
begin
  { изменяется после клика }
  Value['Balance'] := not Value['Balance'];
  ActionBalanceOnOff.Checked := Value['Balance'];

  if (not Value['Balance'])and(Value['ProcShow'] = 3) then
    Value['ProcShow'] := 1;

  UpdateDayInfo;
end;

{==============================================================================}
procedure TForm1.ActionEditBloodExecute(Sender: TObject);
{==============================================================================}
begin
  ClickBlood(False, fmTime);
end;

{==============================================================================}
procedure TForm1.ActionEditInsExecute(Sender: TObject);
{==============================================================================}
begin
  ClickIns(False, fmValue);
end;

{==============================================================================}
procedure TForm1.ActionEditMealExecute(Sender: TObject);
{==============================================================================}
begin
  ClickMeal(False);
end;  

{==============================================================================}
procedure TForm1.ActionEditNoteExecute(Sender: TObject);
{==============================================================================}
begin
  ClickNote(False, fmValue);
end;

{==============================================================================}
procedure TForm1.ActionRemovePanelExecute(Sender: TObject);
{==============================================================================}
var
  RecType: integer;
  MsgType: TMsgDlgType;
begin
  // TODO: refactor numbers
  if (DiaryView.SelectedRecord is TBloodRecord) then RecType := 1 else
  if (DiaryView.SelectedRecord is TInsRecord)   then RecType := 2 else
  if (DiaryView.SelectedRecord is TMealRecord)  then RecType := 3 else
  if (DiaryView.SelectedRecord is TNoteRecord)  then RecType := 4 else
                                                     RecType := -1;

  if (RecType > -1) then
  begin
    if (RecType = 3) then
      MsgType := mtWarning
    else
      MsgType := mtConfirmation;

    if (MessageDlg(MESSAGE_CONF_REMOVE_RECORD[RecType], MsgType, [mbYes, mbNo], 0) = mrYes) then
      DiaryView.CurrentPage.Remove(DiaryView.SelectedRecordIndex);
  end;
end;

{==============================================================================}
procedure TForm1.ActionBalanceMealExecute(Sender: TObject);
{==============================================================================}
{var
  i,n: integer;
  SCarbs: string;
  FullCarbs,TCarbs,TProts,TFats: extended;   }
begin
  if (DiaryView.SelectedRecord is TMealRecord) then
  begin
    //SCarbs := InputBox(
    //  'Подбор',
    //  'Целевое количество углеводов:','');
    //CalculateDayStatistic(Diary[CurPage]);

    {n := DiaryView.SelectedLine;
    FullCarbs := DiaryView.CurrentPage.DayCarbs + AddCarbs;
    TCarbs := DiaryView.SelectedMeal.Carbs + AddCarbs;

    TProts := 
      (FullCarbs/NormCarbs) * NormProts -
      DiaryView.CurrentPage.DayProts+
      DiaryView.SelectedMeal.Prots;

    TFats := 
      (FullCarbs/NormCarbs) * NormFats -
      DiaryView.CurrentPage.DayFats+
      DiaryView.SelectedMeal.Fats;
                                }
    //TProts := 
    //Diary[CurPage].

   { for i := 1 to 40 do
    ImproveMasses(
      DiaryView.SelectedMeal.FoodList,
      TProts,
      TFats,
      TCarbs
    );
    DrawCP;    }
  end;
end;

procedure TForm1.ActionCalcMassExecute(Sender: TObject);
var
  NewMass: real;
  Cant: boolean;
begin
  if not DiaryView.IsFoodSelected then Exit;

  NewMass := GetBestMass(
    DiaryView.SelectedFood.RelCarbs,
    DiaryView.SelectedFood.RelProts,
    DiaryView.SelectedFood.Mass
  );

  if (NewMass < 0) then
  begin
    NewMass := 0;
    Cant := True;
  end else
   Cant := False;

  if NewMass <> DiaryView.SelectedFood.Mass then
  begin
    if Cant then InfoMessage(MESSAGE_INFO_CANT_BALANCE);

    DiaryView.SelectedFood.Mass := Round(NewMass);
    //DiaryView.EditFoodMass(Round(NewMass));
  end;
end;

procedure TForm1.ActionEditFoodExecute(Sender: TObject);
var
  NewMass: real;
begin
  NewMass := DiaryView.SelectedFood.Mass;
  if (DialogFoodMass(dtChangeMass, DiaryView.SelectedFood.Name, NewMass))and
     (NewMass >= 0)
  then
    DiaryView.SelectedFood.Mass := NewMass
    //DiaryView.EditFoodMass(NewMass)
  else
    ErrorMessage('Неверная масса');
    //DiaryView.EditFoodMass(0);
end;

procedure TForm1.ActionDeltaMassExecute(Sender: TObject);
var
  DeltaMass: real;
begin
  DeltaMass := 0;
  if DialogFoodMass(dtDeltaMass,DiaryView.SelectedFood.Name,DeltaMass) then
  if DiaryView.SelectedFood.Mass+DeltaMass >= 0 then
    //DiaryView.EditFoodMass(DiaryView.SelectedFood.Mass+DeltaMass)
    DiaryView.SelectedFood.Mass := DiaryView.SelectedFood.Mass + DeltaMass
  else
  begin
    ErrorMessage('Новая масса не может быть отрицательной.');
  end;
end;

{==============================================================================}
procedure TForm1.ActionRemoveFoodExecute(Sender: TObject);
{==============================================================================}
var
  Meal: TMealRecord;
  Food: TFoodMassed;
begin
  if (DiaryView.IsFoodSelected) then
  begin
    Meal := TMealRecord(DiaryView.SelectedRecord);
    Food := DiaryView.SelectedFood;

    if MessageDlg(Format(MESSAGE_CONF_REMOVE_DIARY_FOOD, [Food.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      Meal.Remove(DiaryView.SelectedLine);
  end;
end;

{==============================================================================}
procedure TForm1.ActionShowWindowExecute(Sender: TObject);
{==============================================================================}
begin
  TrayIcon.ShowMainForm;
end;

{==============================================================================}
procedure TForm1.ActionHideWindowExecute(Sender: TObject);
{==============================================================================}
begin
  TrayIcon.HideMainForm;
end;

{==============================================================================}
procedure TForm1.ActionIsMinToTrayExecute(Sender: TObject);
{==============================================================================}
begin
  Value['MinToTray'] := not Value['MinToTray'];
  TrayIcon.MinimizeToTray := Value['MinToTray'];
  ActionIsMinToTray.Checked := Value['MinToTray'];
end;

{==============================================================================}
procedure SetupUpdate(UserInformed: boolean; const ExpVersion: string);
{==============================================================================}

  function FileSize(const FileName: string): Int64;
  var
    f: TFileStream;
  begin
    try
      f := TFileStream.Create(FileName, fmOpenRead);
      Result := f.Size;
      f.Free;
    except
      Result := 0;
    end;
  end;

  function WaitForFile(const FileName: string; Time: cardinal): boolean;
  var
    StartTime: cardinal;
  begin
    Result := FileExists(FileName);
    StartTime := GetTickCount;
    while (not Result) and (GetTickCount-StartTime < Time) do
    begin
      Result := FileExists(FileName);
      Wait(100);
    end;
  end;
  
var
  BackEXE: boolean;
  BackDLL: boolean;
begin
  // TODO: отвязать от интерфейса, перенести в DiaryCore

  if (MessageDlg(MESSAGE_CONF_UPDATE[UserInformed], mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then Exit;

  Application.ProcessMessages; { чтобы исчезло окно диалога }

  try
    try
      FormProcess.Show;
      FormProcess.SetMax(5);

      { ЗАГРУЗКА }
      ShowProcess('Загрузка файла установки...');

      if GetInetFile('Compensation', URL_UPDATE, WORK_FOLDER+SETUP_FILE, nil, CONNECTION_TIME_OUT, 1024*1024) and
         FileExists(WORK_FOLDER+SETUP_FILE) and
         (FileSize(WORK_FOLDER+SETUP_FILE)>50*1024) then
      begin
        { УДАЛЕНИЕ СТАРЫХ КОПИЙ }
        ShowProcess('Удаление старых резервных копий...');
        DeleteFile(Application.ExeName+'.bak');
        DeleteFile(WORK_FOLDER+ANALYZE_LIB_FileName+'.bak');

        { СОЗДАНИЕ НОВЫХ КОПИЙ }
        ShowProcess('Создание новых резервных копий...');
        RenameFile(Application.ExeName, Application.ExeName+'.bak');
        RenameFile(WORK_FOLDER+ANALYZE_LIB_FileName, WORK_FOLDER+ANALYZE_LIB_FileName+'.bak');

        { УСТАНОВКА }
        ShowProcess('Установка...');
        ShellExecute(0,'open',PChar(WORK_FOLDER+SETUP_FILE),'','',SW_HIDE);
        Value['UpdatedVersion'] := ExpVersion;

        Wait(500);

        BackEXE := not WaitForFile(Application.ExeName, 5000);
        BackDLL := not WaitForFile(WORK_FOLDER+ANALYZE_LIB_FileName, 3000);

        if BackEXE or BackDLL then
        begin
          MessageDlg('Ошибка установки обновления. Будет произведён частичный или полный откат.', mtError, [mbOK], 0);
          if BackEXE then
            RenameFile(Application.ExeName+'.bak', Application.ExeName);
          if BackDLL then
            RenameFile(WORK_FOLDER+ANALYZE_LIB_FileName+'.bak', WORK_FOLDER+ANALYZE_LIB_FileName);
        end else
        begin
          FormProcess.Hide;
          MessageDlg('Все необходимые файлы обновлены. Чтобы изменения вступили в силу, перезапустите приложение.', mtInformation, [mbOK], 0);
        end;
      end else
        MessageDlg('Файл установки повреждён.', mtError, [mbOK], 0);
    except
      MessageDlg('Общая ошибка установки обновления.', mtError, [mbOK], 0);
    end;
  finally
    if FormProcess.Visible then
      FormProcess.Hide;
  end;
end;

{==============================================================================}
procedure TForm1.ActionCheckUpdateExecute(Sender: TObject);
{==============================================================================}
var
  Date: string;
begin
  case CheckUpdates(Date) of
    urNoUpdates:
      InfoMessage(MESSAGE_INFO_NO_UPDATES);

   urNoConnection:
      ErrorMessage(MESSAGE_ERROR_NO_INTERNET);

    urCanUpdate:
      SetupUpdate(False, Date);
  end;
end;

{==============================================================================}
procedure TForm1.ActionSettingsImageExecute(Sender: TObject);
{==============================================================================}
begin
  FormSettings.OpenSettings(spView);
end;

{==============================================================================}
procedure TForm1.ActionSettingsAnalyzeExecute(Sender: TObject);
{==============================================================================}
begin
  FormSettings.OpenSettings(spAnalyze);
end;

{==============================================================================}
procedure TForm1.ActionSettingsStatExecute(Sender: TObject);
{==============================================================================}
begin
  FormSettings.OpenSettings(spNorms);
end;

{==============================================================================}
procedure TForm1.ItemCopyFoodClick(Sender: TObject);
{==============================================================================}
var
  Temp: TFood;
  n,i: integer;
  NewName: string;
begin
  n := ListFood.ItemIndex;
  if n <> -1 then
  begin
    i := 1;
    repeat
      inc(i);
      NewName := FoodBase[n].Name+' ('+IntToStr(i)+')';
    until FoodBase.Find(NewName{, True}) = -1;

    Temp := TFood.Create;
    Temp.CopyFrom(FoodBase[n]);
    Temp.Name := NewName;
    n := FoodBase.Add(Temp);

    (*ShowFoodBase(True);
    {*}UpdateCombos;
    {*}SaveFoodBase; *)  
    EventFoodbaseChanged(True);

    ShowTableItem(ListFood, n);
    ListFood.SetFocus;
  end;
end;

{==============================================================================}
procedure TForm1.ItemCopyDishClick(Sender: TObject);
{==============================================================================}
var
  Temp: TDish;
  n,i: integer;
  NewName: string;
begin
  n := ListDish.ItemIndex;
  if (n <> -1) then
  begin
    i := 1;
    repeat
      inc(i);
      NewName := DishBase[n].Name + ' ('+IntToStr(i) + ')';
    until DishBase.Find(NewName) = -1;

    Temp := TDish.Create;
    Temp.CopyFrom(DishBase[n]);
    // Temp.Modified; - иначе копия будет выше оригинала
    Temp.Name := NewName;
    n := DishBase.Add(Temp);

    EventDishbaseChanged(True, True);

    ShowTableItem(ListDish, n);
    ListDish.SetFocus;
  end;
end;

{==============================================================================}
procedure TForm1.ListBaseColumnRightClick(Sender: TObject; Column: TListColumn; Point: TPoint);
{==============================================================================}
begin
  GetCursorPos(Point);
  case TWinControl(Sender).Tag of
    1: PopupFoodCol.Popup(Point.X, Point.Y);
    2: PopupDishCol.Popup(Point.X, Point.Y);
  end;
end;

{==============================================================================}
procedure TForm1.ListBaseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{==============================================================================}
var
  Item : TListItem;
  A: TPoint;
begin
  if Button = mbRight then
  begin
    Item := TListView(Sender).GetItemAt(X, Y);  
    if Item <> nil then
    begin
      Item.Focused := True;
      Item.Selected := True;
      GetCursorPos(A);
      case TWinControl(Sender).Tag of
        1: PopupFoodBase.Popup(A.X, A.Y);
        2: PopupDishBase.Popup(A.X, A.Y);
      end;
    end;
  end;
end;

{==============================================================================}
procedure TForm1.Item_BaseColClick(Sender: TObject);
{==============================================================================}
var
  Val: boolean;
begin
  Val := not TMenuItem(Sender).Checked;

  TMenuItem(Sender).Checked := Val;
  case TMenuItem(Sender).Tag of
    1: Value['FoodP']  := Val;
    2: Value['FoodF']  := Val;
    3: Value['FoodC']  := Val;
    4: Value['FoodV']  := Val;
    5: Value['FoodGI'] := Val;

    -1: Value['DishM'] := Val;
    -2: Value['DishP'] := Val;
    -3: Value['DishF'] := Val;
    -4: Value['DishC'] := Val;
    -5: Value['DishV'] := Val;
    -6: Value['DishD'] := Val;
  end;

  if TMenuItem(Sender).Tag in [1..5] then
    UpdateFoodTable(True, True) else
  if -TMenuItem(Sender).Tag in [1..6] then
    UpdateDishTable(True, True);
end;

{==============================================================================}
procedure TForm1.ActionShortMealExecute(Sender: TObject);
{==============================================================================}
begin
  //{ все проверки - внутри }
  //DiaryView.SetShortMeal(ActionShortMeal.Checked);

  if (DiaryView.SelectedRecord is TMealRecord) then
    TMealRecord(DiaryView.SelectedRecord).ShortMeal := ActionShortMeal.Checked;
end;

{==============================================================================}
procedure TForm1.ButtonInsListClick(Sender: TObject);
{==============================================================================}
{const
  AVG_PERIOD  = 30;
  LOOK_PERIOD = 400;
var
  i,j: integer;
  Val: real;
  Summ: real;
  Count: integer; }
begin
  ListBS.Clear;

  {for i := DiaryBase.Count-LOOK_PERIOD to DiaryBase.Count-1 do
  for i := DiaryBase.Count-LOOK_PERIOD to DiaryBase.Count-1 do
  if DiaryBase[i].DayCarbs <> 0 then
  begin
    Val := DiaryBase[i].DayIns / DiaryBase[i].DayCarbs;
    ListBS.Items.Add(DateToStr(DiaryBase[i].Date) + #9 + FloatToStr(Val));
  end;  }

  {===============================================================}

 (* for i := Trunc(Now) - LOOK_PERIOD + 1 to Trunc(Now) do
  begin
    Summ := 0;
    Count := 0;
    for j := i - AVG_PERIOD + 1 to i do
    if DiaryBase[j].DayCarbs <> 0 then
    begin
      inc(Count);
      Val := DiaryBase[j].DayIns / DiaryBase[j].DayCarbs;
      Summ := Summ + Val;
    end;

    if Count > 0 then
      Summ := Summ / Count;

    ListBS.Items.Add(DateToStr(DiaryBase[i - (AVG_PERIOD div 2)].Date) + #9 + FloatToStr(Summ));
  end;   *)
end;

procedure TForm1.ActionMoveMealForwardExecute(Sender: TObject);
begin
  MoveMeal(+1);
end;

procedure TForm1.ActionMoveMealBackExecute(Sender: TObject);
begin
  MoveMeal(-1);
end;

procedure TForm1.MoveMeal(Delta: integer);
var
  n: integer;
  CurDate: TDate;
  OldPage: TDiaryPage;
  NewPage: TDiaryPage;
  Meal: TMealRecord;
begin
  {!!!}
  if not (DiaryView.SelectedRecord is TMealRecord) then Exit;

  CurDate := DiaryView.CurrentDate;
  OldPage := DiaryView.CurrentPage;
  NewPage := Diary[CurDate + Delta];

  n := DiaryView.SelectedRecordIndex;
  Meal := TMealRecord(DiaryView.SelectedRecord);
  OldPage.Remove(n, False);
  n := NewPage.Add(Meal);

  DiaryView.OpenPage(Diary[Trunc(CurDate + Delta)], True);
  CalendarDiary.Date := CurDate + Delta;
  DiaryView.SelectedRecordIndex := n;

  {DiaryView.OpenPage(CurDate+Delta, True);
  //n2 := DiaryView.AddMealBlank(Rec.Time, Rec.ShortMeal);
  n2 := DiaryView.CurrentPage.Add(TMealRecord.Create(DiaryView.CurrentPage, Rec.ShortMeal));

  for i := 0 to Rec.Count-1 do
  begin
    Item := TFoo dMassed.Create(Rec.Food[i]);
    DiaryView.AddFoodToMeal(Item);
  end;

  DiaryView.OpenPage(CurDate, True);
  DiaryView.CurrentPage.Remove(n1);  }
end;

procedure TForm1.ButtonAnListClick(Sender: TObject);

  function GetType(const Rec: TAnalyzeRec): integer;
  begin
    if (Rec.BSOut = 5) then  Result := 0 else
    if (Rec.BSIn > 5) then
    begin
      if (Rec.BSOut > Rec.BSIn) then Result := 1 else
      //if (Rec.BSOut = Rec.BSIn) then Result := 2 else
      if (Rec.BSOut > 5)        then Result := 2 else
                                     Result := 3;
    end else
    begin
      if (Rec.BSOut < Rec.BSIn) then Result := 4 else
      //if (Rec.BSOut = Rec.BSIn) then Result := 6 else
      if (Rec.BSOut < 5)        then Result := 5 else
                                     Result := 6;
    end;
  end;

//var
//  Cnt: array[0..6] of integer;
// i,j: integer;
begin
  {for i := 0 to High(Cnt) do
    Cnt[i] := 0;

  for i := 0 to MinPerDay - 1 do
  begin
    j := GetType(Analyzer.AnList[i]);
    inc(Cnt[j]);
  end;

  ListBS.Items.Clear;
  for i := 0 to High(Cnt) do
    ListBS.Items.Add(IntToStr(i) + '    ' + IntToStr(Cnt[i]));  }
end;

{==============================================================================}
procedure TForm1.ThreadExec_Done(Sender: TObject; TaskID: Cardinal);
{==============================================================================}
begin
  // DONE

  // Если мы пришли сюда, то это значит только то, что задача не вылетела
  // по тайм-ауту. Не факт, что задача выполнена успешно.

  {case TaskID of
    TASK_SAVE_N_SYNC: StatusBar.Panels[3].Text := 'Дневник загружен на сервер';
    TASK_LOGIN: StatusBar.Panels[2].Text := 'Онлайн';
  end;   }
end;

{==============================================================================}
procedure TForm1.ThreadExec_TimeOut(Sender: TObject; TaskID: Cardinal);
{==============================================================================}
begin
  // TIME OUT
  case TaskID of
    TASK_SAVE_N_SYNC: StatusBar.Panels[3].Text := 'Ошибка загрузки [t/o]';
    //TASK_LOGIN: StatusBar.Panels[2].Text := 'Оффлайн [t/o]';
  end;
end;

{==============================================================================}
procedure TForm1.ComboDiaryNewDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
{==============================================================================}
begin
  DrawACItem(
    Control,
    Rect,
    DiaryMultiMap[ComboDiaryNew.ShowedIndex[Index]],
    odSelected in State,
    Value['CarbsInfo']
  );
end;

procedure TForm1.ListBaseMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (TListView(Sender).Enabled) and (not TListView(Sender).Focused) then
    TListView(Sender).SetFocus;
end;

procedure TForm1.DiaryViewChange(Sender: TObject; EventType: TPageEventType;
  Page: TDiaryPage; RecClass: TClassCustomRecord;
  RecInstance: TCustomRecord);
begin
  { TODO: Проблема: по идее, лучше наблюдать за изменением дневника непосредственно -
  то есть за событием от типа TDiary. Но тогда получается, что при изменении
  дневника нужно оповестить сразу два объекта, подписанные на изменения:
  TDiaryView и интерфейс приложения. Для решения этой проблемы временно
  используется последовательное оповещение:

    TDiary -> TDiaryView -> Интерфейс

  Задача: подумать, как сделать это лучше.

  Должно быть:

    TDiaryPage > TDiaryView > интерфейс
               > TDiary
  }

  StatusBar.Panels[1].Text := 'Дневник изменён';
  StatusBar.Panels[3].Text := '';

  if (RecClass = TBloodRecord) then
  begin
    UpdateDayInfo(); // для пустой страницы это даёт смену "Нет данных" на "Данные: 0"
    //UpdateMealInfo();
    UpdateMealStatistics(); // для смены на "Не выбрано" ?
    UpdateMealDose();  // иначе зависнет  - ???
    UpdateNextFinger();
  end else

  if (RecClass = TInsRecord) then
  begin
    UpdateDayInfo();
    //UpdateMealInfo();
    UpdateMealStatistics; // нужно, иначе зависнет - ???
    UpdateMealDose; // нужно, иначе зависнет
    UpdateTimeLeft();
  end else

  if (RecClass = TMealRecord) then
  begin
    UpdateDayInfo;
    //UpdateMealInfo;
    UpdateMealStatistics;
    UpdateMealDose;
    UpdateTimeLeft;
  end;

  Log(VERBOUS, '#MOD');
end;

{==============================================================================}
procedure TForm1.UpdateNextFinger;
{==============================================================================}

  function FingerHint(): string;
  var
    i: integer;
  begin
    Result := '';
    for i := 0 to 9 do
      Result := Result + #13 + ShortFingerNames[i] + ' — ' + LongFingerNames[i];
  end;

begin
  StartProc('UpdateNextFinger()');

  LabelDiaryFinger.Caption := LABEL_FINGER;

  CurrentNextFinger := Diary.GetNextFinger();
  if (CurrentNextFinger > -1) then
  begin
    LabelDiaryFingerVal.Caption := MiddleFingerNames[CurrentNextFinger];
    LabelDiaryFingerVal.Hint := LongFingerNames[CurrentNextFinger] + #13 +
      '-----------------------------------' + FingerHint();
    // TODO: вычислять один раз
  end else
  begin
    LabelDiaryFingerVal.Caption := '?';
    LabelDiaryFingerVal.Hint := HINT_FINGER_NOT_FOUND;
  end;

  FinishProc;
end;

{==============================================================================}
procedure TForm1.UpdateTimeLeft;
{==============================================================================}
var
  hour,min,sec,msec: word;
  i,j: integer;
  Founded: boolean;
  Meal: TMealRecord;
  Ins: TInsRecord;

  TempTime: integer;

  Page: TDiaryPage;
  Today: TDate;
begin
  //StartProc('UpdateTimeLeft(): search for meal');

  LabelDiaryTimeLeftIns.Caption := LABEL_AFTER_INS;
  LabelDiaryTimeLeftMeal.Caption := LABEL_AFTER_MEAL;

  DecodeTime(SysUtils.Time, Hour, Min, Sec, msec);
  Today := Trunc(Now);

  {=====================================================}

  Founded := False;
  for i := Today downto Today - SEARCH_INTERVAL + 1 do
  begin
    Page := Diary[i];

    for j := Page.Count - 1 downto 0 do
    if (Page[j].RecType = TMealRecord) then
    begin
      Meal := TMealRecord(Page[j]);
      if (Meal.Count > 0) and (not Meal.ShortMeal) then
      begin
        TempTime := {Trunc}(Today - i)*SecPerDay+
          Hour*3600+Min*60+Sec-Meal.Time*60;

        if (TempTime >= 0) then
        begin
          Founded := True;
          Form1.LabelDiaryTimeLeftMealVal.Caption := TimeTrncSecToStr(TempTime);
          break;
        end;
      end;
    end;
    if Founded then break;
  end;
  Form1.LabelDiaryTimeLeftMealVal.Font.Color := COLOR_HASDATA[Founded];
  if not Founded then
    Form1.LabelDiaryTimeLeftMealVal.Caption := '?';
  {=====================================================}

  Founded := false;
  for i := Today downto Today - SEARCH_INTERVAL + 1 do
  begin
    Page := Diary[i];

    for j := Page.Count - 1 downto 0 do
    if (Page[j].RecType = TInsRecord) then
    begin
      Ins := TInsRecord(Page[j]);

      TempTime := {Trunc}(Today - i)*SecPerDay+
        Hour*3600+Min*60+Sec-Ins.Time*60;
      if (TempTime > 0) then
      begin
        Founded := True;
        Form1.LabelDiaryTimeLeftInsVal.Caption := TimeTrncSecToStr(TempTime);
        break;
      end;
    end;
    if Founded then Break;
  end;
  Form1.LabelDiaryTimeLeftInsVal.Font.Color := COLOR_HASDATA[Founded];
  if not Founded then
    Form1.LabelDiaryTimeLeftInsVal.Caption := '?';

  //FinishProc;
end;

{==============================================================================}
procedure TForm1.ApplyInterfaceSettings;
{==============================================================================}

  procedure SwitchDB(C: array of TWinControl);
  var
    Val: boolean;
    i: integer;
  begin
    Val := Value['DoubleBuffered'];
    for i := Low(C) to High(C) do
      C[i].DoubleBuffered := Val;
  end;

begin
  StartProc('ApplyInterfaceSettings()');

  { форма }

  StatProgProts.Visible := Value['Balance'];
  StatProgFats.Visible := Value['Balance'];
  StatProgCarbs.Visible := Value['Balance'];
  ActionBalanceOnOff.Checked := Value['Balance'];

  StatProgProts.Max := Value['NormProts'];
  StatProgFats.Max := Value['NormFats'];
  StatProgCarbs.Max := Value['NormCarbs'];

  with DiaryView.Colors do
  begin
    Panel_StdBlood   := Value['Color_StdBlood'];
    Panel_StdIns     := Value['Color_StdIns'];
    Panel_StdMeal    := Value['Color_StdMeal'];
    Panel_StdNote    := Value['Color_StdNote'];
    Panel_StdBloodPP := Value['Color_StdBloodPP'];
    Panel_SelBlood   := Value['Color_SelBlood'];
    Panel_SelIns     := Value['Color_SelIns'];
    Panel_SelMeal    := Value['Color_SelMeal'];
    Panel_SelNote    := Value['Color_SelNote'];
    Panel_SelBloodPP := Value['Color_SelBloodPP'];
  end;

  DiaryView.Font.Size := Value['FontSize'];
  DiaryView.Font.Name := Value['FontName'];
  DiaryView.Font.Color := clBlack;

  TimerAutosave.Interval := Value['AutosaveInterval'] * 60 * 1000;

  { ======= ВКЛЮЧЕНИЕ ДВОЙНОЙ БУФЕРИЗАЦИИ ======= }

  SwitchDB([
    ListFood,
    ListDish,
    GroupBoxStatistic,
    GroupBoxGraph,
    PanelDiaryBottom,
    PanelAdd,
    ScrollBoxDiary,
    PanelBaseFood,
    PanelBaseDish
  ]);
    
  TrayIcon.MinimizeToTray := Value['MinToTray'];
  ActionIsMinToTray.Checked := Value['MinToTray'];

  { столбцы }
  Item_FoodP.Checked  := Value['FoodP'];
  Item_FoodF.Checked  := Value['FoodF'];
  Item_FoodC.Checked  := Value['FoodC'];
  Item_FoodV.Checked  := Value['FoodV'];

  Item_DishM.Checked := Value['DishM'];
  Item_DishP.Checked := Value['DishP'];
  Item_DishF.Checked := Value['DishF'];
  Item_DishC.Checked := Value['DishC'];
  Item_DishV.Checked := Value['DishV'];
  Item_DishD.Checked := Value['DishD'];

  FinishProc;
end;

{==============================================================================}
procedure TForm1.UpdateFoodTable(FullUpdate: boolean; SaveItemIndex: boolean = False);
{==============================================================================}
const
  COL_CAPTIONS: array[0..4] of string = (
    'Наименование',
    'Б',
    'Ж',
    'У',
    'ккал'
  );

var
  i: integer;
  SavedIndex: integer;

  FoodP: boolean;
  FoodF: boolean;
  FoodC: boolean;
  FoodV: boolean;
begin
  StartProc('UpdateFoodTable()');

  LabelFoodBase.Caption := Format('База продуктов (%d), v%d', [FoodBase.Count, FoodBase.Version]);

  FoodP := Value['FoodP'];
  FoodF := Value['FoodF'];
  FoodC := Value['FoodC'];
  FoodV := Value['FoodV'];

  with ListFood do
  begin
    if SaveItemIndex or (not FullUpdate) then
      SavedIndex := ItemIndex
    else
      SavedIndex := -1; // для компилятора

    if FullUpdate then
    begin
      { ЗАГОЛОВКИ }
      Columns.Clear;

      with Columns.Add do
      begin
        Caption := COL_CAPTIONS[0];
        AutoSize := True;
        MinWidth := 150;
      end;

      if FoodP  then Columns.Add.Caption := COL_CAPTIONS[1];
      if FoodF  then Columns.Add.Caption := COL_CAPTIONS[2];
      if FoodC  then Columns.Add.Caption := COL_CAPTIONS[3];
      if FoodV  then Columns.Add.Caption := COL_CAPTIONS[4];

      { СТРОКИ }
      Items.BeginUpdate;
      Clear;
      ListFood.AllocBy := FoodBase.Count;
      for i := 0 to FoodBase.Count - 1 do
      begin
        with Items.Add do
        begin
          Caption := FoodBase[i].Name;//+' ['+IntToStr(FoodBase[i].Tag)+']';;
          ImageIndex := Byte(FoodBase[i].FromTable);

          if FoodP  then SubItems.Add(RealToStr(FoodBase[i].RelProts));
          if FoodF  then SubItems.Add(RealToStr(FoodBase[i].RelFats));
          if FoodC  then SubItems.Add(RealToStr(FoodBase[i].RelCarbs));
          if FoodV  then SubItems.Add(IntToStr(Round(FoodBase[i].RelValue)));
        end;
      end;
      Items.EndUpdate;

      Height := Height + 1;
      Height := Height - 1;
      Columns[0].Width := Columns[0].Width - 10;
    end else
    { not FullUpdate }
    begin
      if (FoodBase.Count <> Items.Count) then
        UpdateFoodTable(True)
      else

      for i := 0 to FoodBase.Count - 1 do
      with Items[i] do
      begin
        Caption := FoodBase[i].Name;
        ImageIndex := Byte(FoodBase[i].FromTable);

        SubItems.Clear; 
        if FoodP  then SubItems.Add(RealToStr(FoodBase[i].RelProts));
        if FoodF  then SubItems.Add(RealToStr(FoodBase[i].RelFats));
        if FoodC  then SubItems.Add(RealToStr(FoodBase[i].RelCarbs));
        if FoodV  then SubItems.Add(IntToStr(Round(FoodBase[i].RelValue)));
      end;
    end;

    if SaveItemIndex or (not FullUpdate) then
      ShowTableItem(ListFood, SavedIndex);
  end;

  FinishProc;
end;

{==============================================================================}
procedure TForm1.UpdateDishTable(FullUpdate: boolean; SaveItemIndex: boolean = False);
{==============================================================================}

  function MyTimeToStr(Date: TDateTime): string;
  begin
    if (Date <> 0) then
      Result := DateToStr(Date)
    else
      Result := '---';
  end;

const
  COL_CAPTIONS: array[0..6] of string = (
    'Наименование',
    'Масса',
    'Б',
    'Ж',
    'У',
    'ккал',
    'Дата'
  );

var
  i: integer;
  SavedIndex: integer;

  DishM: boolean;
  DishP: boolean;
  DishF: boolean;
  DishC: boolean;
  DishV: boolean;
  DishD: boolean;
begin
  StartProc('UpdateDishTable()');

  LabelDishBase.Caption := Format('База блюд (%d), v%d', [DishBase.Count, DishBase.Version]);

  DishM := Value['DishM'];
  DishP := Value['DishP'];
  DishF := Value['DishF'];
  DishC := Value['DishC'];
  DishV := Value['DishV'];
  DishD := Value['DishD'];

  with ListDish do
  begin
    if SaveItemIndex or (not FullUpdate) then
      SavedIndex := ItemIndex
    else
      SavedIndex := -1; // для компилятора

    if FullUpdate then
    begin
      { ЗАГОЛОВКИ }
      Columns.Clear;

      with Columns.Add do
      begin
        Caption := COL_CAPTIONS[0];
        AutoSize := True;
        MinWidth := 150;
      end;

      if DishM then
      with Columns.Add do
      begin
        Caption := COL_CAPTIONS[1];
        MinWidth := 50;
        Width := 60;
      end;

      if DishP then Columns.Add.Caption := COL_CAPTIONS[2];
      if DishF then Columns.Add.Caption := COL_CAPTIONS[3];
      if DishC then Columns.Add.Caption := COL_CAPTIONS[4];
      if DishV then Columns.Add.Caption := COL_CAPTIONS[5];
      if DishD then with Columns.Add do
      begin
        Caption := COL_CAPTIONS[6];
        MinWidth := 80;
        Width := 80;
      end;

      { СТРОКИ }
      Items.BeginUpdate;
      Clear;
      AllocBy := DishBase.Count;
      for i := 0 to DishBase.Count-1 do
      begin
        with Items.Add do
        begin
          Caption := DishBase[i].Name;//+' ['+IntToStr(DishBase[i].Tag)+']';
          ImageIndex := 2;//+Byte(DishBase[i].HasErrors);

          if DishM then SubItems.Add(RealToStr(DishBase[i].RealMass));
          if DishP then SubItems.Add(RealToStr(DishBase[i].RelProts));
          if DishF then SubItems.Add(RealToStr(DishBase[i].RelFats));
          if DishC then SubItems.Add(RealToStr(DishBase[i].RelCarbs));
          if DishV then SubItems.Add(IntToStr(Round(DishBase[i].RelValue)));
          if DishD then SubItems.Add(MyTimeToStr(DishBase[i].ModifiedTime));
        end;
      end;
      Items.EndUpdate;

      Height := Height + 1;
      Height := Height - 1;
      
      if Columns[0].Width > 10 then
        Columns[0].Width := Columns[0].Width - 10;
    end else
    { not FullUpdate }
    begin
      if (DishBase.Count <> Items.Count) then
        UpdateDishTable(True)
      else
      
      for i := 0 to DishBase.Count - 1 do
      with Items[i] do
      begin
        Caption := DishBase[i].Name;
        ImageIndex := 2;

        SubItems.Clear;

        if DishM then SubItems.Add(RealToStr(DishBase[i].RealMass));
        if DishP then SubItems.Add(RealToStr(DishBase[i].RelProts));
        if DishF then SubItems.Add(RealToStr(DishBase[i].RelFats));
        if DishC then SubItems.Add(RealToStr(DishBase[i].RelCarbs));
        if DishV then SubItems.Add(IntToStr(Round(DishBase[i].RelValue)));
        if DishD then SubItems.Add(MyTimeToStr(DishBase[i].ModifiedTime));
      end;
    end;

    if SaveItemIndex or (not FullUpdate) then
      ShowTableItem(ListDish, SavedIndex);
  end;

  FinishProc;
end;

procedure TForm1.EventDiaryChanged;
begin
  // TODO: implement
end;

procedure TForm1.EventDishbaseChanged(CountChanged, NamesChanged: boolean);
begin
  StartProc('EventDishbaseChanged');

  if (CountChanged) then NamesChanged := True;

  if (NamesChanged) then
  begin
    UpdateDishTable(CountChanged);
    UpdateCombos;
  end;
  SaveDishBase; // TODO: remove it

  FinishProc;
end;

procedure TForm1.EventFoodbaseChanged(CountChanged: boolean);
begin
  StartProc('EventFoodbaseChenged');

  UpdateFoodTable(True);
  UpdateCombos;
  SaveFoodBase; // TODO: remove it

  FinishProc;
end;

procedure TForm1.EventKoofChanged;
begin

end;

procedure TForm1.EventPageChanged;
begin

end;

procedure TForm1.ButtonInsulinCalcClick(Sender: TObject);
const
  PERIOD    = 30;
  CARTRIDGE = 300;
var
  Summ: real;
  i,j,Count: integer;
  FirstDate, LastDate: integer;
  Page: TDiaryPage;
begin
  Summ := 0;
  Count := 0;

  LastDate := Trunc(Now);
  FirstDate := LastDate - PERIOD;

  for i := FirstDate to LastDate do
  begin
    Page := Diary[i];
    for j := 0 to Page.Count - 1 do
    if (Page[j].RecType = TInsRecord) then
    begin
      inc(Count);
      Summ := Summ + TInsRecord(Page[j]).Value;
    end;
  end;

  ShowMessage(Format(
    'Проанализирован срок, дней: %d'#13+
    'Всего инъекций: %d'#13 +
    'Всего доза (ед): %.1f'#13 +
    'Всего израсходовано (ед): %.1f'#13 +
    'Средний расход в месяц (ед): %.2f'#13 +
    'Средний расход в месяц (карт.): %.2f'#13 +
    'Средний расход в день (ед): %.2f',
    [PERIOD,
    Count,
    Summ,
    Summ + Count,
    (Summ + Count) / PERIOD * 30,
    (Summ + Count) / CARTRIDGE / PERIOD * 30,
    (Summ + Count) / PERIOD]
  ));
end;

procedure TForm1.PanelDevelopmentClick(Sender: TObject);
begin
  FormMisc.ShowModal;
end;

end.

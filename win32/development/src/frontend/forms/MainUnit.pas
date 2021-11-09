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
  UnitEditorBlood,
  UnitEditorIns,
  UnitEditorMeal,
  UnitEditorNote,
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
  FoodBaseDAO,
  DiaryLocalSource,
  DiaryWeb, //
  DiarySync, //
  DiaryAnalyze,
  AnalyzeGraphic,
  AnalyzeInterface,
  SelfUpdate,

  // прочее
  ThreadExecutor;

const
  WM_SECOND_START = WM_USER + 790;

type
  TBalloonAction = procedure of object;

  TSortMode = (smName, smDate);

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
    LabelFoodBase: TLabel;
    PanelFoodButtons: TPanel;
    ButtonCreateFood: TSpeedButton;
    PanelBaseDish: TPanel;
    Shape11: TShape;
    PanelDishHeader: TPanel;
    LabelDishBase: TLabel;
    PanelDishButtons: TPanel;
    ButtonCreateDish: TSpeedButton;
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
    ItemCopyFood: TMenuItem;
    Item_SepFood: TMenuItem;
    TimerAutosave: TTimer;
    ThreadExec_: TThreadExecutor;
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
    Shape2: TShape;
    GroupBasesSearch: TGroupBox;
    EditBaseFoodSearch: TEdit;
    ActionViewLogs: TAction;
    ButtonAddNote: TSpeedButton;
    Item_FoodD: TMenuItem;

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
    procedure ItemCopyFoodClick(Sender: TObject);
    procedure ButtonAnListClick(Sender: TObject);
    procedure ComboDiaryNewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ThreadExec_Done(Sender: TObject; TaskID: Cardinal);
    procedure ThreadExec_TimeOut(Sender: TObject; TaskID: Cardinal);
    procedure ComboDiaryNewDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure DiaryViewChange(Sender: TObject; EventType: TPageEventType;
      Page: TRecordList; RecClass: TClassCustomRecord;
      RecInstance: TCustomRecord);
    procedure FormResize(Sender: TObject);
    procedure SplitterBaseMoved(Sender: TObject);
    procedure ImageMinMaxTimesMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ButtonInsulinCalcClick(Sender: TObject);
    procedure PanelDevelopmentClick(Sender: TObject);
    procedure EditBaseFoodSearchChange(Sender: TObject);
    procedure EditBaseFoodSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonBasesFilterResetClick(Sender: TObject);
    procedure EditBaseFoodSearchKeyPress(Sender: TObject; var Key: Char);
    procedure ListFoodData(Sender: TObject; Item: TListItem);
    procedure ListDishData(Sender: TObject; Item: TListItem);
    procedure ActionViewLogsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListFoodColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListDishColumnClick(Sender: TObject; Column: TListColumn);
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
    procedure ClickBlood(New: boolean);
    procedure ClickIns(New: boolean);
    procedure ClickMeal(New: boolean);
    procedure ClickNote(New: boolean);

    procedure ClickMealFoodMassChange();
    procedure ClickMealFoodMassDelta();

    function SelectedRecord(): TCustomRecord;

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

    procedure WarnBadCredentials();

    { Информационные компоненты }
    procedure UpdateDayInfo;
    procedure UpdateTimeLeft;
    procedure UpdateNextFinger;
    procedure UpdateMealStatistics;
    procedure UpdateMealDose;
    procedure UpdateCombos;
    procedure UpdateFoodTableHeaders();
    procedure UpdateDishTableHeaders();
    procedure UpdateFoodTable();
    procedure UpdateDishTable();

    { Информационные события }
    procedure EventFoodbaseChanged(CountChanged: boolean);
    procedure EventDishbaseChanged(CountChanged, NamesChanged: boolean);
    procedure EventDiaryChanged;
    procedure EventPageChanged;
    procedure EventKoofChanged;
    // TODO: не могу создать то же самое для настроек (потеряю точность, что именно изменилось)

    procedure ApplyInterfaceSettings;

    { анализ }

    procedure UpdateKoofs();
    function GetCompensationMass(const RelCarbs, RelProts: real): real;
    function GetBestMass(const RelCarbs, RelProts, CurMass: real): real;

    procedure ShowBalloon(const Msg: string; MsgType: TBalloonHintIcon; AfterAction: TBalloonAction = nil);
  end;

  { обновление }
  procedure SetupUpdate(const ExpectedVersion: integer);

var
  { ========================== И Н Т Е Р Ф Е Й С ============================= }
  Form1: TForm1;

  CurrentDB: real;       // текущая коррекция СК
  CurrentNextFinger: integer;
  DiaryMultiMap: TMultimap;
  MaxDiaryTag: real;
  MaxDishTag: real;
  //FoodBaseMap: TIndexList;

  // required to store TListView's projection
  FoodList: TFoodItemList;
  DishList: TDishItemList;

  FoodBaseSort: TSortMode = smName;
  DishBaseSort: TSortMode = smName;

  { ============================ П Р О Ч Е Е ================================= }

  { Idle-задачи }
  IgnoreIdleOnce:       boolean = False;
  ON_IDLE_ShowBases:    boolean = False;
  ON_IDLE_UpdateCombos: boolean = False;
  //ON_IDLE_UpdateKoofs:  boolean = False;
  IN_IDLE_CheckUpdates: boolean = False;

  LatestVersion:      integer;

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
  FOLDER_BUTTONS             = 'Images\Buttons\';
    IMAGE_DIARY_NEW_BLOOD    = FOLDER_BUTTONS + 'Diary_New_Blood.bmp';
    IMAGE_DIARY_NEW_INS      = FOLDER_BUTTONS + 'Diary_New_Ins.bmp';
    IMAGE_DIARY_NEW_MEAL     = FOLDER_BUTTONS + 'Diary_New_Meal.bmp';
    IMAGE_DIARY_NEW_NOTE     = FOLDER_BUTTONS + 'Diary_New_Note.bmp';
    IMAGE_PANEL_MIN          = FOLDER_BUTTONS + 'Panel_Min.bmp';
    IMAGE_PANEL_MAX          = FOLDER_BUTTONS + 'Panel_Max.bmp';
    IMAGE_EDITOR_ADD         = FOLDER_BUTTONS + 'Editor_Add.bmp';
    IMAGE_EDITOR_REMOVE      = FOLDER_BUTTONS + 'Editor_Remove.bmp';
    IMAGE_EDITOR_REPLACE     = FOLDER_BUTTONS + 'Editor_Replace.bmp';
    IMAGE_EDITOR_CALC        = FOLDER_BUTTONS + 'Editor_Calc.bmp';
    IMAGE_BASE_SEARCH_RESET  = FOLDER_BUTTONS + 'Base_Search_Clear.bmp';
    IMAGE_BASE_NEW_FOOD      = FOLDER_BUTTONS + 'Base_New_Food.bmp';
    IMAGE_BASE_NEW_DISH      = FOLDER_BUTTONS + 'Base_New_Dish.bmp';
    IMAGE_BASE_REMOVE        = FOLDER_BUTTONS + 'Base_Remove.bmp';
    IMAGE_EXPORT_SAVE        = FOLDER_BUTTONS + 'Export_Save.bmp';

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

uses UnitMisc, UnitLogViewer;

{$R *.dfm}

{ TForm1 }

{======================================================================================================================}
procedure SyncProgress(P: integer);
{======================================================================================================================}
begin
  Form1.StatusBar.Panels[1].Text := Format('Синхронизация %d%%...', [P]);//'Синхронизация ' + IntToStr(p) + '%';
  FormProcess.LabelHint.Caption := Format('Синхронизация %d%%...', [P]);//'Синхронизация ' + IntToStr(p) + '%';
  Application.ProcessMessages;
end;

{======================================================================================================================}
function MySyncDiary(Terminated: TBooleanFunction = nil): integer;
{======================================================================================================================}
var
  LastSyncTime: TDateTime;
begin
  StartProc('MySyncDiary');
  Result := 0;

  try
    { дневник }
    Form1.StatusBar.Panels[3].Text := STATUS_ACTION_SYNC_DIARY;
    Application.ProcessMessages;

    Result := SyncSources(LocalSource, WebSource, SyncProgress);

    if (Result > 0) then
    begin
      Form1.DiaryView.OpenPage(Diary[Trunc(Form1.CalendarDiary.Date)], True);
      Form1.UpdateNextFinger;
    end;

    { база продуктов }
    Form1.StatusBar.Panels[3].Text := STATUS_ACTION_SYNC_FOODBASE;
    Application.ProcessMessages;
    if (SyncSources(FoodBaseLocal, FoodBaseWeb, SyncProgress) > 0) then
    begin
      Form1.EventFoodbaseChanged(True);
    end;

    { база блюд }
    Form1.StatusBar.Panels[3].Text := STATUS_ACTION_SYNC_DISHBASE;
    Application.ProcessMessages;
    if (SyncSources(DishBaseLocal, DishBaseWeb, SyncProgress) > 0) then
    begin
      Form1.EventDishbaseChanged(True, True);
      // TODO: workaround
    end;

    LastSyncTime := GetTimeUTC();
    Value['LastSync'] := DateTimeToStr(LastSyncTime);
    
    { готово }
    Form1.StatusBar.Panels[3].Text := STATUS_RESULT_SYNC_DONE + ' ' + DateTimeToStr(UTCToLocal(LastSyncTime));
    Application.ProcessMessages;
  except
    on E: ENotAuthorizedException do
    begin
      Log(ERROR, 'ENotAuthorizedException in MySyncDiary(): ' + E.Message, True);
      Form1.WarnBadCredentials();
    end;

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

{======================================================================================================================}
procedure TaskSaveAndSync(Terminated: TBooleanFunction);
{======================================================================================================================}
begin
  StartProc('TaskSaveAndSync');
  MySyncDiary(Terminated);
  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.Designer;
{======================================================================================================================}
const
  ADD_BUTTON_SIZE = 60;
  BASE_BUTTON_HEIGHT = 40;
  BASE_BUTTON_WIDTH  = 160;
var
  t: integer;
  FoodDishBord: integer;
begin
  DGroup := LabelDayProts_.Height div 2;

  // разделители
  ShapeLeft1.Height := BORD;
  ShapeLeft2.Height := BORD;
  ShapeDivImageBS.Height := BORD;
  ShapeRight2.Height := BORD;
  ShapeRight3.Height := BORD;
  ShapeAn1.Height := BORD;
  Shape11.Height := BORD;

  // Бордеры панелей
  PanelDiaryTopLeft.BorderWidth := BORD;
  PanelDiaryDish.BorderWidth := BORD;
  PanelBaseFood.BorderWidth := BORD;
  PanelBaseDish.BorderWidth := BORD;
  PanelAnManagment.BorderWidth := BORD;

  { ================ ЛЕВАЯ ЧАСТЬ ================ }

  CalendarDiary.Left := 0;
  CalendarDiary.Top := 0;
  CalendarDiary.Show;
  CalendarDiary.Align := alNone;
  PanelDiaryTopLeft.Width := CalendarDiary.Width + 4 * BORD;
  CalendarDiary.Align := alTop;

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
  ButtonAddIns.Left := 2 * BORD + ADD_BUTTON_SIZE;
  ButtonAddIns.Top := BORD;

  ButtonAddMeal.Width := ADD_BUTTON_SIZE;
  ButtonAddMeal.Height := ADD_BUTTON_SIZE;
  ButtonAddMeal.Left := 3 * BORD + 2 * ADD_BUTTON_SIZE;
  ButtonAddMeal.Top := BORD;

  ButtonAddNote.Width := ADD_BUTTON_SIZE;
  ButtonAddNote.Height := ADD_BUTTON_SIZE;
  ButtonAddNote.Left := 4 * BORD + 3 * ADD_BUTTON_SIZE;
  ButtonAddNote.Top := BORD;

  PanelAdd.Width := 4 * ADD_BUTTON_SIZE + 5 * BORD;

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
  ButtonCreateDish.Height := BASE_BUTTON_HEIGHT;

  ButtonCreateFood.Width := BASE_BUTTON_WIDTH;
  ButtonCreateDish.Width := BASE_BUTTON_WIDTH;

  ButtonCreateFood.Top := BORD;
  ButtonCreateDish.Top := BORD;

  ButtonCreateFood.Left := 0;
  ButtonCreateDish.Left := 0;

  PanelFoodButtons.Height := BASE_BUTTON_HEIGHT + BORD; // no 2*
  PanelDishButtons.Height := BASE_BUTTON_HEIGHT + BORD; // no 2*

  { АНАЛИЗ }
  // TODO: написать
end;

{======================================================================================================================}
procedure TForm1.FullInit;
{======================================================================================================================}
var
  Timer: TSmallTimer;
  Prev: string;
  Temp: integer;
  i: integer;

  procedure StartupInfo(const Text: string);
  begin
    if (Prev <> '') then
      // TODO: RELEASE: make log's "Save" parameter False in release version
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
    AutoLog.LogLevel := IndexToLevel(Value['LogLevel']);

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
      {StartupInfo(STATUS_ACTION_CONVERT_FOODBASE);
      FoodBase.LoadFromFile_Old(WORK_FOLDER + FOLDER_BASES + '\' + FoodBase_Name_Old);
      FoodBase.SaveToFile(WORK_FOLDER + FoodBase_FileName);
      Log(INFO, 'Foodbase converted ok', True); }

      // TODO 3: RF: convert foodbase from old format to new one
      ErrorMessage('Конвертирование базы отключено');
    end else   ;

    // TODO: загружается дважды :(
    // существует файл в XML-формате
    {if FileExists(WORK_FOLDER + FoodBase_FileName) then
    begin
      StartupInfo(STATUS_ACTION_LOADING_FOODBASE);
      FoodBase.LoadFromFile_XML(WORK_FOLDER + FoodBase_FileName);
      Log(INFO, 'Foodbase loaded ok', True);
    end;}

    //FoodBaseMap := TIndexList.Create;

    { =============== ЗАГРУЗКА БАЗЫ БЛЮД =============== }

    // форматирование
   { if (not FileExists(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name)) and
       (FileExists(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name_Old)) then
    begin
      StartupInfo(STATUS_ACTION_CONVERT_DISHBASE);
      DishBase.LoadFromFile_Old(WORK_FOLDER + FOLDER_BASES + '\' + DishBase_Name_Old);
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
    end;  }

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
           ShowBalloon('Синхронизация дневника прошла успешно, передано записей: ' + IntToStr(Temp), bitInfo);
      except
        on ConnectionError: EConnectionException do
        begin
          Log(ERROR, 'Form1.FullInit(): ошибка при синхронизации дневника: ' + ConnectionError.Message);
          ShowBalloon('Авторизация не удалась [сервер не отвечает]', bitError);
        end;

        on AuthError: ENotAuthorizedException do
        begin
          Log(ERROR, 'Form1.FullInit(): ошибка при синхронизации дневника: ' + AuthError.Message);
          WarnBadCredentials();
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

    // moved to Compensation.dpr

    { =============== ЗАГРУЗКА МОДУЛЯ АНАЛИЗА =============== }
    StartupInfo(STATUS_ACTION_LOADING_MATHAN);
    AddAnalyzers(LoadAnalyzers(), Analyzers);

    // TODO: may be extended

    for i := 0 to High(Analyzers) do
      ComboAnalyzers.Items.Add(Analyzers[i].GetName());

    ComboAnalyzers.ItemIndex := 0;

    Log(DEBUG, 'Analyzers loaded: ' + IntToStr(Length(Analyzers)), True);

    { =============== КОЭФФИЦИЕНТЫ =============== }
    try
      StartupInfo(STATUS_ACTION_PREPARING_KOOFS);
      UpdateKoofs;

      if Value['AutoSync'] then
      begin
        StartupInfo(STATUS_ACTION_UPLOADING_KOOFS);
        //UploadKoofs;
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

    Caption := APPLICATION_TITLE + ' ' + PROGRAM_VERSION;

    PanelDevelopment.Visible := Value['Debug'];
    ActionViewLogs.Visible := Value['Debug'];
    TabStat.TabVisible := Value['Debug'];
    CalendarDiary.Date := Now();  
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

    { =============== ВКЛЮЧЕНИЕ IDLE-ЗАДАЧ =============== }

    ON_IDLE_UpdateCombos := True;
    ON_IDLE_ShowBases := True;
    if Value['CheckUpdates'] and
       (GetTimeUTC() - Value['LastUpdateCheck']  > UPDATES_CHECKING_PERIOD) then
      IN_IDLE_CheckUpdates := True;

    {=============================================}

    StartupInfo(STATUS_RESULT_READY);
    StatusBar.Panels[0].Text := Format(STATUS_RESULT_LOADING_TIME, [Timer.FullTime]);
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

{======================================================================================================================}
procedure TForm1.FullFree;
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to High(DiaryMultiMap) do
  begin
    FreeAndNil(DiaryMultiMap[i]);
  end;

  for i := 0 to High(DishMultiMap) do
  begin
    FreeAndNil(DishMultiMap[i]);
  end;

  BusinessObjects.Free(FoodList);
  BusinessObjects.Free(DishList);
end;

{======================================================================================================================}
procedure TForm1.ShowMessages;
{======================================================================================================================}
begin
  StartProc('ShowMessages()');

  {
     Замечание. Все действия вызывают всплывающую подсказку, т.е.
     являются взаимоисключающими и потому должны быть записаны
     через ELSE в порядке убывания важности сообщения.
  }

  { проверка корректности установки обновления }
  if (Value['UpdatedVersion'] <> '') then
  begin
    if (StrToInt(Value['UpdatedVersion']) <= PROGRAM_VERSION_CODE) then
      ShowBalloon('Установка обновления успешно завершена', bitInfo)
    else
      ShowBalloon(
        'При установке обновления возникли ошибки'#13+
        'Ожидалась версия: ' + Value['UpdatedVersion'] + #13 +
        'Установлена версия: ' + IntToStr(PROGRAM_VERSION_CODE), bitError);
    Value['UpdatedVersion'] := '';
  end else

  { поиск модуля анализа }
  if (Length(Analyzers) = 0) then
  begin
    ShowBalloon(BALLOON_ERROR_ANALYZER_NOT_FOUND, bitError);
  end else

  {  }

  ;

  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.LoadPictures;
{======================================================================================================================}

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
  LoadImage(WORK_FOLDER + IMAGE_DIARY_NEW_NOTE, ButtonAddNote.Glyph);

  LoadImage(WORK_FOLDER + IMAGE_EDITOR_ADD, ButtonDiaryNewAddFood.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_EDITOR_ADD, FormDish.ButtonAddFood.Glyph);

  LoadImage(WORK_FOLDER + IMAGE_EDITOR_CALC, FormDish.ButtonRunCalc.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_EDITOR_REMOVE, FormDish.ButtonSimpleMass.Glyph);

  LoadImage(WORK_FOLDER + IMAGE_BASE_NEW_FOOD, ButtonCreateFood.Glyph);
  LoadImage(WORK_FOLDER + IMAGE_BASE_NEW_DISH, ButtonCreateDish.Glyph);
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

{======================================================================================================================}
function TForm1.DishEditorRect: TRect;
{======================================================================================================================}
begin
  Result := Rect(
    Left + PanelBaseDish.Left,
    Top,
    Left + PanelBaseDish.Left + PanelBaseDish.Width,
    Top + Height
  );
end;

{======================================================================================================================}
function TForm1.FoodEditorRect: TRect;
{======================================================================================================================}
begin
  Result := Rect(
    Left + PanelBaseFood.Left,
    Top,
    Left + PanelBaseFood.Left + PanelBaseFood.Width,
    Top + Height
  );
end;

{======================================================================================================================}
procedure TForm1.CreateFood();
{======================================================================================================================}
var
  Item: TFoodItem;
begin
  Item := TFoodItem.Create();
  if FormFood.OpenFoodEditor(Item, True, FoodEditorRect) then
  begin
    Item.Modified();
    FoodBaseLocal.Save(Item);
    {#}EventFoodbaseChanged(True);

    //ShowTableItem(ListFood, n);
    ListFood.SetFocus;
  end else
    Item.Free;
end;

{======================================================================================================================}
procedure TForm1.CreateDish;
{======================================================================================================================}
var
  Item: TDishItem;
begin
  Item := TDishItem.Create();
  if FormDish.OpenDishEditor(Item, True, DishEditorRect) then
  begin
    Item.Modified();
    DishBaseLocal.Save(Item);
    {#}EventDishbaseChanged(True, True);

    //ShowTableItem(ListDish, n);
    ListDish.SetFocus;
  end else
    Item.Free;
end;

{======================================================================================================================}
procedure TForm1.EditFood(Index: integer);
{======================================================================================================================}
var
  Item: TFoodItem;
  OldName: string;
begin
  if (Index < 0) or (Index > High(FoodList)) then
  begin
    Log(WARNING, 'EditFood(): incorrect index ignored (' + IntToStr(Index) + ')', True);
    Exit;
  end;

  Item := FoodBaseLocal.FindById(FoodList[Index].ID) as TFoodItem;
  OldName := Item.Name;

  if FormFood.OpenFoodEditor(Item, False, FoodEditorRect) then
  begin
    // TODO 1: RF: renaming food in dishes
   { if DishBaseLocal.RenameFood(OldName, Temp.Name) then
      SaveDishBase;  }

    Item.Modified();
    FoodBaseLocal.Save(Item);
    EventFoodbaseChanged(False);

    // TODO 5: RF: selecting edited food
    //ShowTableItem(ListFood, Index);
    ListFood.SetFocus;
  end;
end;

{======================================================================================================================}
procedure TForm1.EditDish(Index: integer);
{======================================================================================================================}
var
  Item: TDishItem;
  OldName: string;
begin
  // TODO: process everywhere in the same manner
  if (Index < 0) or (Index > High(DishList)) then
  begin
    Log(WARNING, 'EditDish(): incorrect index ignored (' + IntToStr(Index) + ')', True);
    Exit;
  end;

  Item := DishBaseLocal.FindById(DishList[Index].ID) as TDishItem;
  OldName := Item.Name;

  if FormDish.OpenDishEditor(Item, False, DishEditorRect) then
  begin
    // TODO 1: RF: renaming dish in dishes
   { if DishBaseLocal.RenameDish(OldName, Temp.Name) then
      SaveDishBase;  }

    Item.Modified();
    DishBaseLocal.Save(Item);
    EventDishbaseChanged(False, True);
    // TODO: rough

    // TODO 5: RF: selecting edited dish
    //ShowTableItem(ListDish, Index);
    ListDish.SetFocus;
  end;
end;

{======================================================================================================================}
procedure TForm1.RemoveFood(Index: integer);
{======================================================================================================================}

  function AskWarning(Food: TFoodItem; Dish: TDishItem): boolean;
  begin
    Result := MessageDlg(
      Format(MESSAGE_CONF_REMOVE_FOOD_USED, [Food.Name, Dish.Name]),
      mtWarning, [mbYes, mbNo], 0) = mrYes;
  end;

  function AskConfirm(Food: TFoodItem): boolean;
  begin
    Result := MessageDlg(
      Format(MESSAGE_CONF_REMOVE_FOOD, [Food.Name]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  end;

var
  Item: TFoodItem;
  DishNumber: integer;
begin
  if (Index < 0) or (Index > High(FoodList)) then
  begin
    Log(WARNING, 'RemoveFood(): incorrect index ignored (' + IntToStr(Index) + ')', True);
    Exit;
  end;

  // TODO 1: RF: checking dishes on removing food
  DishNumber := -1;//DishBase.UsedFood(FoodList[Index].Name);

  Item := FoodList[Index];

  if //((DishNumber > -1)and(AskWarning(FoodList[Index], DishBase[DishNumber])))or
     ((DishNumber = -1) and (AskConfirm(Item))) then
  begin
    FoodBaseLocal.Delete(Item.ID);
    UpdateFoodTable();
    {*}UpdateCombos;
  end;
end;

{======================================================================================================================}
procedure TForm1.RemoveDish(Index: integer);
{======================================================================================================================}

  function AskWarning(Dish: TDishItem; DishParent: TDishItem): boolean;
  begin
    Result := MessageDlg(
      Format(MESSAGE_CONF_REMOVE_DISH_USED, [Dish.Name, DishParent.Name]),
      mtWarning, [mbYes, mbNo], 0) = mrYes;
  end;

  function AskConfirm(Dish: TDishItem): boolean;
  begin
    Result := MessageDlg(
      Format(MESSAGE_CONF_REMOVE_DISH, [Dish.Name]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  end;

var
  Item: TDishItem;
  DishNumber: integer;
begin
  // TODO: update
  if (Index < 0) or (Index > High(DishList)) then
  begin
    Log(WARNING, 'RemoveDish(): incorrect index ignored (' + IntToStr(Index) + ')', True);
    Exit;
  end;

  // TODO 1: RF: checking dishes on removing dish
  DishNumber := -1;//DishBase.UsedDish(DishList[Index].Name);

  Item := DishList[Index];

  if //((DishNumber > -1)and(AskWarning(DishList[Index], DishBase[DishNumber])))or
     ((DishNumber = -1)and(AskConfirm(Item))) then
  begin
    DishBaseLocal.Delete(Item.ID);
    UpdateDishTable();
    {*}UpdateCombos;
  end;
end;

{======================================================================================================================}
procedure TForm1.UpdateCombos;
{======================================================================================================================}
var
  Offset: integer;

  function FormatDate(Stamp: TDateTime): string;
  var
    Today, S: TDate;
  begin
    S := Trunc(Stamp);
    Today := Trunc(now);

    if (s = Today) then Result := 'сегодня' else
    if (s = Today - 1) then Result := 'вчера' else
    Result := DateToStr(Stamp);
  end;

  procedure InitMap(var Map: TMultiMap);
  var
    FoodList: TFoodItemList;
    DishList: TDishItemList;
    i: integer;
  begin
    FoodList := FoodBaseLocal.FindAll(false);
    DishList := DishBaseLocal.FindAll(false);
    Offset := Length(FoodList);
    SetLength(Map, Length(FoodList) + Length(DishList));

    for i := 0 to High(FoodList) do
    begin
      Map[i] := TMealItem.Create;
      Map[i].Data := TFoodRelative.Create();
      Map[i].Data.CopyFrom(FoodList[i]);
      Map[i].Help1 := '';
      Map[i].Help2 := Format('  %.1f', [FoodList[i].RelCarbs]);
      Map[i].Icon := Byte(FoodList[i].FromTable);
      Map[i].Tag := 0;
    end;

    for i := 0 to High(DishList) do
    begin
      Map[Offset + i] := TMealItem.Create;
      Map[Offset + i].Data := DishList[i].AsFoodRelative();
      Map[Offset + i].Help1 := FormatDate(DishList[i].TimeStamp);
      Map[Offset + i].Help2 := Format('  %.1f', [DishList[i].RelCarbs]);
      Map[Offset + i].Icon := 2;
      Map[Offset + i].Tag := 0;
      Tag := 0;
    end;

    BusinessObjects.Free(FoodList);
    BusinessObjects.Free(DishList);
  end;

  function More(const Item1, Item2: TMealItem): boolean;
  begin
    if (abs(Item1.Tag - Item2.Tag) > EPS) then
      Result := (Item1.Tag < Item2.Tag)
    else
      Result := (Item1.Data.Name > Item2.Data.Name);
  end;

  procedure qsort(var Map: TMultiMap; l,r: integer);
  var
    i,j: integer;
    x: TMealItem;
    y: TMealItem;
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

  procedure Process(const ItemName: string; Tag: real; var Map: TMultimap);
  var
    i: integer;
  begin
    for i := 0 to High(Map) do
    if (Map[i].Data.Name = ItemName) then
    begin
      Map[i].Tag := Map[i].Tag + Tag;
      Exit;
    end;
  end;

  procedure AnalyzeUsingDiary;
  var
    StartDate, FinishDate: TDateTime;

    function GetTag(Time: TDateTime): real;
    const
      FACTOR = 10000;
    begin
      if (Time <= StartDate) then
        Result := 0 else
      if (Time >= FinishDate) then
        Result := FACTOR
      else
      begin
        //Result := IntPower((Time - StartDate) / (FinishDate - StartDate), 7);
        Result := (Time - StartDate) / (FinishDate - StartDate);
        Result := Round(FACTOR * Result * Result);
      end;
    end;

  var
    i, k: integer;
    Meal: TMealRecord;
    DeltaTag: real;
    DishModFactor: integer;

    //Food: TFoodItem;
    //Dish: TDishItem;
    Recs: TRecordList;
  begin
    StartProc('TForm1.AnalyzeUsingDiary()');

    try  
      { инициализируем карту }
      InitMap(DiaryMultiMap);

      { выставляем теги }
      StartDate := GetTimeUTC() - Value['AnUsingPeriod'];
      FinishDate := GetTimeUTC();

      Recs := LocalSource.FindPeriod(StartDate, FinishDate);

      for i := 0 to High(Recs) do
      begin
        if (Recs[i].RecType = TMealRecord) then
        begin
          Meal := TMealRecord(Recs[i]);
          DeltaTag := GetTag(Meal.Time);
          for k := 0 to Meal.Count - 1 do
          begin
            Process(Meal[k].Name, DeltaTag, DiaryMultiMap);
          end;
        end;
      end;

      FreeRecords(Recs);

      { дополнительные теги для блюд }

      DishModFactor := Value['DishModFactor'];

      // TODO 1: RF: relevant search: taking dish mod time into account
     { for i := 0 to DishBase.Count - 1 do
      if (DishBase[i].ModifiedTime > 0) then
        DiaryMultiMap[Offset + i].Tag := DiaryMultiMap[Offset + i].Tag +
        DishModFactor * GetTag(DishBase[i].ModifiedTime);    }

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
    List: TDishItemList;
  begin
    StartProc('TForm1.AnalyzeUsingDish()');
    List := nil; // for compiler

    try  
      { инициализируем карту }
      InitMap(DishMultiMap);

      { выставляем теги }
      List := DishBaseLocal.FindAll(false);
      for i := 0 to High(List) do
      for j := 0 to List[i].Count - 1 do
        Process(List[i].Content[j].Name, 1.0, DishMultiMap);

      { сортируем }
      if Length(DishMultiMap) > 0 then
        qsort(DishMultiMap, 0, High(DishMultiMap));

      BusinessObjects.Free(List);
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
        Add(Map[i].Data.Name);
    end;
  end;

  function MaxTag(const Map: TMultiMap): Real;
  var
    i: integer;
  begin
    Result := 0;
    for i := 0 to High(Map) do
    if (Map[i].Tag > Result) then
      Result := Map[i].Tag;
  end;

begin                                             
  StartProc('TForm1.UpdateCombos()');
  try
    AnalyzeUsingDiary;
    AnalyzeUsingDish;
    FillACBox(ComboDiaryNew, DiaryMultiMap);
    FillACBox(FormDish.ComboFood, DishMultiMap);
    MaxDiaryTag := MaxTag(DiaryMultiMap);
    MaxDishTag := MaxTag(DishMultiMap);
  finally
    FinishProc;
  end;
end;

// TODO: программа пытается интерпретировать подсказку в поле ввода продуктоблюда как название продуктоблюда

{======================================================================================================================}
procedure TForm1.ButtonDeleteFoodClick(Sender: TObject);
{======================================================================================================================}
begin
  RemoveFood(ListFood.ItemIndex);
end;

{======================================================================================================================}
procedure TForm1.ButtonDeleteDishClick(Sender: TObject);
{======================================================================================================================}
begin
  RemoveDish(ListDish.ItemIndex);
end;

{======================================================================================================================}
procedure TForm1.TableFoodBaseKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{======================================================================================================================}
{var
  j: integer;
  c: char;}
begin
  if Key = vk_Return then
    EditFood(ListFood.ItemIndex) else
  if Key = vk_Delete then
    RemoveFood(ListFood.ItemIndex) else
 begin
    {c := CodeToRus(Key);
    if c <> #0 then
    for j := 0 to FoodBaseMap.Count - 1 do
    if StartsWith(FoodBase[FoodBaseMap[j]].Name, C) then
    begin
      ShowTableItem(ListFood, j, True);
      break;
    end; }
  end;
end;

{======================================================================================================================}
procedure TForm1.ListDishKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{======================================================================================================================}
{var
  j: integer;
  c: char; }
begin
  if Key = vk_Return then
    EditDish(ListDish.ItemIndex) else
  if Key = vk_Delete then
    RemoveDish(ListDish.ItemIndex) else
 begin
    {c := CodeToRus(Key);
    if c <> #0 then
    for j := 0 to DishBase.Count - 1 do
    if StartsWith(DishBase[j].Name, C) then
    begin
      ShowTableItem(ListDish, j, True);
      break;
    end; }
  end;
end;

{======================================================================================================================}
procedure TForm1.EditKeyProtect(Sender: TObject; var Key: Char);
{======================================================================================================================}
begin
  if (Key in SYSTEM_CHARS) then Key := #0;
end;

{======================================================================================================================}
procedure TForm1.EditDiaryNewMassKeyPress(Sender: TObject; var Key: Char);
{======================================================================================================================}
begin
  if (Key = #13) then
  begin
    ButtonDiaryNewAddClick(nil);
    Key := #0;
  end;
end;

{======================================================================================================================}
procedure TForm1.ButtonCreateFoodClick(Sender: TObject);
{======================================================================================================================}
begin
  CreateFood;
end;

{======================================================================================================================}
procedure TForm1.ButtonCreateDishClick(Sender: TObject);
{======================================================================================================================}
begin
  CreateDish;
end;

{======================================================================================================================}
procedure TForm1.CalendarDiaryChange(Sender: TObject);
{======================================================================================================================}
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

{======================================================================================================================}
procedure TForm1.SplitterBaseCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
{======================================================================================================================}
const
  Min = 200;
begin
  Accept := (NewSize>Min)and(Width-NewSize>Min);
end;

{======================================================================================================================}
procedure TForm1.SplitterBaseMoved(Sender: TObject);
{======================================================================================================================}
begin
  Value['PanelBaseFood.Width'] := PanelBaseFood.Width / Width;
end;

{======================================================================================================================}
procedure TForm1.FormResize(Sender: TObject);
{======================================================================================================================}
begin
  PanelBaseFood.Width := Round(Width * Value['PanelBaseFood.Width']);
end;

{======================================================================================================================}
procedure TForm1.TimerTimeLeftTimer(Sender: TObject);
{======================================================================================================================}
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
  
  if (Length(Analyzers) > 0) and (PageControl1.ActivePage = TabAnalyze) then
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

{======================================================================================================================}
procedure TForm1.ImageMinMaxStatMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{======================================================================================================================}
begin
  MinMaxBox(
    GroupBoxStatistic,ImageMinMaxStat, DataInterface.Image_MinMax,
    Value['AnimatePanels'],
    LabelDayIns.Top+2*LabelDayIns.Height,
    ImageMinMaxStat.Height+3,PANEL_OPEN_TIME,PANEL_CLOSE_TIME);
end;

{======================================================================================================================}
procedure TForm1.ImageMinMaxGraphMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{======================================================================================================================}
begin
  if ImageMinMaxGraph.Tag = 0 then
    ImagePreview.Show;

  MinMaxBox(
    GroupBoxGraph,ImageMinMaxGraph, DataInterface.Image_MinMax,
    Value['AnimatePanels'],
    Round(ImagePreview.Width * 0.8),
    ImageMinMaxGraph.Height + 3, PANEL_OPEN_TIME, PANEL_CLOSE_TIME);

  if ImageMinMaxGraph.Tag = 0 then
    ImagePreview.Hide;
end;

{======================================================================================================================}
procedure TForm1.ImageMinMaxTimesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{======================================================================================================================}
begin
  MinMaxBox(
    GroupBoxTimeLeft,ImageMinMaxTimes, DataInterface.Image_MinMax,
    Value['AnimatePanels'],
    LabelDiaryFinger.Top + 4 * BORD,
    ImageMinMaxTimes.Height + 3,
    PANEL_OPEN_TIME,PANEL_CLOSE_TIME);
end;

{======================================================================================================================}
procedure TForm1.ImagePreviewDblClick(Sender: TObject);
{======================================================================================================================}
var
  Today: TDateTime;
  Recs: TRecordList;
begin
  ComboKoof.ItemIndex := -1;
  PageControl1.ActivePage := TabAnalyze;

  Today := LocalToUTC(Trunc(CalendarDiary.Date));
  Recs := LocalSource.FindPeriod(Today - 1, Today + 2);
  try
    DrawBS(Recs, Today, ImageLarge, False);
  finally
    FreeRecords(Recs);
  end;
end;

{======================================================================================================================}
procedure TForm1.DiaryViewPage(Sender: TObject);
{======================================================================================================================}
begin
  StartProc('DiaryViewPage');

  UpdateDayInfo;

  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.ListFoodKeyPress(Sender: TObject; var Key: Char);
{======================================================================================================================}
begin
  Key := #0;  // чтобы не было встроенного автоперехода
end;

{======================================================================================================================}
procedure TForm1.ComboDiaryNewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{======================================================================================================================}
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

{======================================================================================================================}
procedure TForm1.ComboDiaryNewCloseUp(Sender: TObject);
{======================================================================================================================}
var
  Mass: real;
  Item: TVersioned;
  Temp: TFoodRelative;
begin
  case IdentifyItem(Trim(ComboDiaryNew.Text), Item) of
    itFood: Temp := TFoodItem(Item);
    itDish: Temp := TDishItem(Item).AsFoodRelative();
    itUnknown: Exit;
  end;

  Expander.Add(DiaryFoodDishInput, Temp.Name);
  ComboDiaryNew.Text := Temp.Name;
  Mass := GetCompensationMass(Temp.RelCarbs, Temp.RelProts);
  if (Mass > 0) then
    EditDiaryNewMass.Text := IntToStr(Round(Mass));
  FocusEdit(EditDiaryNewMass);

  Temp.Free;
end;

{======================================================================================================================}
procedure TForm1.ButtonDiaryNewAddClick(Sender: TObject);
{======================================================================================================================}
var
  ItemType: TItemType;
  n: integer;
  Mass: Extended;
  Rec: TCustomRecord;
  Meal: TMealRecord;
  Item: TVersioned;
begin
  StartProc('TForm1.ButtonDiaryNewAddClick()');
  try
    try
      Rec := SelectedRecord();
      if ((Rec <> nil) and (Rec is TMealRecord)) then
      begin
        Meal := TMealRecord(Rec);

        ComboDiaryNew.Text := Trim(ComboDiaryNew.Text);
        EditDiaryNewMass.Text := Trim(EditDiaryNewMass.Text);

        ItemType := IdentifyItem(ComboDiaryNew.Text, Item);

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
              itFood: Meal.Add(TFoodItem(Item).AsFoodMassed(Mass));
              itDish: Meal.Add(TDishItem(Item).AsFoodMassed(Mass));
            end;

            Meal.Modified();
            LocalSource.Save(Meal);
            DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
            DiaryView.SelectedRecordID := Meal.ID;
            DiaryView.SelectedLine := Meal.Count - 1;
            UpdateMealStatistics;
            UpdateMealDose;
            EventDiaryChanged();

            ComboDiaryNew.Text := '';
            EditDiaryNewMass.Text := '';
            ComboDiaryNew.SetFocus;

            // промотка
            n := DiaryView.GetSelectedRecordIndex;
            if (n > -1) and (Length(DiaryView.CurrentPage) > 0) then
              ScrollBoxDiary.VertScrollBar.Position :=
                Round(n / Length(DiaryView.CurrentPage) * ScrollBoxDiary.VertScrollBar.Range);
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

{======================================================================================================================}
procedure TForm1.ScrollBoxDiaryMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{======================================================================================================================}
begin
  StartProc('TForm1.ScrollBoxDiaryMouseDown()');
  DiaryView.DeselectAll;
  UpdateMealStatistics;
  UpdateMealDose;
  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.Splitter4Moved(Sender: TObject);
{======================================================================================================================}
begin
  DiaryView.DrawCurrentPage;
end;

{======================================================================================================================}
procedure TForm1.ListFoodDblClick(Sender: TObject);
{======================================================================================================================}
begin
  EditFood(ListFood.ItemIndex);
end;

{======================================================================================================================}
procedure TForm1.ListDishDblClick(Sender: TObject);
{======================================================================================================================}
begin
  EditDish(ListDish.ItemIndex);
end;

{======================================================================================================================}
procedure TForm1.MenuItem_ShowDiaryPageClick(Sender: TObject);
{======================================================================================================================}
begin
  PageControl1.ActivePage := TabDiary;
  PageControl1Change(nil);
end;

{======================================================================================================================}
procedure TForm1.MenuItem_ShowBasePageClick(Sender: TObject);
{======================================================================================================================}
begin
  PageControl1.ActivePage := TabBase;
  PageControl1Change(nil)
end;

{======================================================================================================================}
procedure TForm1.MenuItem_ShowGraphicPageClick(Sender: TObject);
{======================================================================================================================}
begin
  PageControl1.ActivePage := TabAnalyze;
  PageControl1Change(nil);
end;

{======================================================================================================================}
procedure TForm1.ButtonBSHistoryClick(Sender: TObject);
{======================================================================================================================}
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

{======================================================================================================================}
procedure TForm1.DiaryViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{======================================================================================================================}
begin
  StartProc('TForm1.DiaryViewMouseDown()');
  //ProcessMealSelected(DiaryView.IsMealSelected); -- on Idle

  FocusMealInput;

  UpdateMealStatistics;
  UpdateMealDose;
  //ScrollToSelected;
  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.ProcessMealSelected(Value: boolean);
{======================================================================================================================}
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
    ComboDiaryNew.Text := MAIN_DIARY_PANEL_ADD_SELECT_MEAL;
    EditDiaryNewMass.Text := '';
  end
end;

{======================================================================================================================}
procedure TForm1.UpdateDayInfo;
{======================================================================================================================}

  procedure Demo(pa, pt, fa, ft, ca, ct: Real);
  {var
    np, nf, nc: Real;
    M, D: Real;  }
  begin
    {np := pa / pt;
    nf := fa / ft;
    nc := ca / ct;
    M := (np + nf + nc) / 3;
    D := (Sqr(np - M) + Sqr(nf - M) + Sqr(nc - M)) / 3;
    Caption := Format('%.3f', [D]);   }
  end;

  { статистика }
  procedure UpdateDayStatistics;

    function Info(const Val: real; Proc: integer): string;
    begin
      if Proc > -1 then
        Result := Format('%.0f г (%d%%)', [Val, Proc])
      else
        Result := Format('%.0f', [Val]);
    end;

  var
    summ: real;
    proc1,proc2,proc3: integer;

    DayProts, DayFats, DayCarbs, DayValue, DayMass, DayIns: real;
  begin
    if (Length(DiaryView.CurrentPage) = 0) then
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
    begin
      DiaryRecords.Statistics(DiaryView.CurrentPage, DayProts, DayFats, DayCarbs, DayValue, DayMass, DayIns);

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
                Proc1 := Round(DayProts / Value['NormProts']*100)
              else Proc1 := -1;

              if Value['NormFats'] > 0 then
                Proc2 := Round(DayFats / Value['NormFats']*100)
              else Proc2 := -1;

              if Value['NormCarbs'] > 0 then
                Proc3 := Round(DayCarbs / Value['NormCarbs']*100)
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

      LabelDayValue.Caption  := Format('Ценность: %.0f ккал', [DayValue]);
      LabelDayMass.Caption   := Format('Масса: %.0f г', [DayMass]);
      LabelDayIns.Caption    := Format('Инсулин: %.1f ед', [DayIns]);

      Demo(DayProts, Value['NormProts'], DayFats, Value['NormFats'], DayCarbs, Value['NormCarbs']);
    end;
  end;

  { график СК }
  procedure UpdateDayGraph;
  var
    Today: TDateTime;
    Recs: TRecordList;
  begin
    Today := LocalToUTC(Trunc(CalendarDiary.Date));
    Recs := LocalSource.FindPeriod(Today - 1, Today + 2);
    try
      DrawBS(Recs, Today, ImagePreview, True);
    finally
      FreeRecords(Recs);
    end;
  end;

begin
  StartProc('TForm1.UpdateDayInfo()');
  UpdateDayStatistics;
  UpdateDayGraph;
  FinishProc;
end;

// статистика приёма пищи
{======================================================================================================================}
procedure TForm1.UpdateMealStatistics;
{======================================================================================================================}
const
  CARBS_PER_BU = 12.0;
var
  Rec: TCustomRecord;
  SelMeal: TMealRecord;
begin
  StartProc('TForm1.UpdateMealStatistic()');

  Rec := SelectedRecord();
  if ((Rec <> nil) and (Rec is TMealRecord)) then
  begin
    SelMeal := TMealRecord(Rec);

    LabelDiaryMealProts.Font.Color := clWindowText;
    LabelDiaryMealProts.Caption := Format('Белки: %.0f г', [SelMeal.Prots]);
    LabelDiaryMealFats.Caption := Format('Жиры: %.0f г', [SelMeal.Fats]);
    LabelDiaryMealCarbs.Caption := Format('Углеводы: %.0f г / %.1f ХЕ', [SelMeal.Carbs, SelMeal.Carbs / CARBS_PER_BU]);
    LabelDiaryMealValue.Caption := Format('Ценность: %.0f ккал', [SelMeal.Value]);
    LabelDiaryMealMass.Caption := Format('Масса: %.0f г', [SelMeal.Mass]);
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
{======================================================================================================================}
procedure TForm1.UpdateMealDose;
{======================================================================================================================}

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
      if abs(Delta) < 3 then
        LabelDiaryCorrection.Font.Color := CC_LOW else
      if abs(Delta) < 6 then
        LabelDiaryCorrection.Font.Color := CC_MIDDLE
      else
      begin
        LabelDiaryCorrection.Font.Color := CC_HIGH;
        LabelDiaryCorrection.ShowHint := True;
      end;
    end;
  end;

  function FindBlood(Time: TDateTime): TBloodRecord;
  var
    Recs: TRecordList;
    TimeFrom: TDateTime;
    TimeTo: TDateTIme;
    i: integer;
  begin
    TimeFrom := Time - 1;//BLOOD_ACTUALITY_TIME / MinPerDay;
    TimeTo := Time;
    Recs := LocalSource.FindPeriod(TimeFrom, TimeTo);

    // TODO: ins time hardcoded
    UpdatePostprand(Recs, 3.5 / HourPerDay, Value['PostPrandTime'] / MinPerDay, Value['ShortPostPrandTime'] / MinPerDay);

    for i := High(Recs) downto 0 do
    if (abs(Recs[i].Time - Time) <= BLOOD_ACTUALITY_TIME / MinPerDay) then
    begin
      if (Recs[i].RecType = TBloodRecord) and
         (not TBloodRecord(Recs[i]).PostPrand) then
      begin
        Result := Recs[i] as TBloodRecord;
        Exit;
      end;
    end else
      break;

    Result := nil;
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
  Rec: TCustomRecord;
begin
  StartProc('TForm1.UpdateMealDose()');

  Rec := SelectedRecord();
  
  if ((Rec <> nil) and (Rec is TMealRecord)) then
  // блюдо выбрано, информируем
  begin
    { ищем приём пищи, замер и инъекцию }
    SelMeal := TMealRecord(Rec);

    StartBlood := FindBlood(SelMeal.Time);

    Ins := TInsRecord(
      Diary.FindRecord(
        TInsRecord,
        SelMeal.Time,
        INS_ACTUALITY_TIME,
        sdAround
      )
    );

    { Определяем коэффициенты }
    Koof := GetKoof(SelMeal.TimeInMinutes);

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
        LabelDiaryMealDose.Caption := 'Расчётная доза: ' + RealToStr(Dose) + ' ед'
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
          LabelDiaryMealExpectedBS.Caption := Format('Ожидаемый СК: %.1f ммоль/л', [ExpectedBS])
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
            Caption := IntToStr(i) + ' ед';
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
  end else

  // блюдо не выбрано, очищаем
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
  end;

  FinishProc;
end;

{ ============================== РЕДАКТОРЫ ================================ }

{======================================================================================================================}
procedure TForm1.ClickBlood(New: boolean);
{======================================================================================================================}

  function ShowBloodEditor(var Rec: TBloodRecord; New: boolean): boolean;
  begin
    Log(DEBUG, 'ShowBloodEditor()');
    Result := TFormEditorBlood.ShowEditor(TVersioned(Rec), New);
  end;

var
  ID: TCompactGUID;
  Rec: TBloodRecord;
begin
  Log(DEBUG, 'TForm1.ClickBlood');

  if New then
  begin
    // определение пальца
    Log(VERBOUS, 'TForm1.ClickBlood: Creating new temp record...');
    Rec := TBloodRecord.Create();

    Log(VERBOUS, 'TForm1.ClickBlood: Searching for the next finger...');
    Rec.Finger := Diary.GetNextFinger();

    Log(VERBOUS, 'TForm1.ClickBlood: Showing the editor in modal mode...');
    if ShowBloodEditor(Rec, New) then
    begin
      Log(VERBOUS, 'TForm1.ClickBlood: Editor OK, saving record...');
      LocalSource.Add(Rec);

      Log(VERBOUS, 'TForm1.ClickBlood: Reloading diary page...');
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Rec.ID;
      ScrollToSelected;
      EventDiaryChanged();
    end else
    begin
      Log(VERBOUS, 'TForm1.ClickBlood: Editor cancelled');
    end;
  end else
  begin
    ID := DiaryView.SelectedRecordID;
    Rec := TBloodRecord(LocalSource.FindById(ID));

    if ShowBloodEditor(Rec, New) then
    begin
      LocalSource.Save(Rec);
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Rec.ID;
      ScrollToSelected;
      EventDiaryChanged();
    end;
  end;

  Log(DEBUG, 'TForm1.ClickBlood: finished');
end;

{======================================================================================================================}
procedure TForm1.ClickIns(New: boolean);
{======================================================================================================================}

  function ShowInsEditor(var Rec: TInsRecord; New: boolean): boolean;
  begin
    Log(DEBUG, 'TForm1.ShowInsEditor()');
    Result := TFormEditorIns.ShowEditor(TVersioned(Rec), New);
  end;

var
  ID: TCompactGUID;
  Rec: TInsRecord;
begin
  Log(DEBUG, 'TForm1.ClickIns');
  if New then
  begin
    Rec := TInsRecord.Create();
    if ShowInsEditor(Rec, New) then
    begin
      LocalSource.Add(Rec);
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Rec.ID;
      ScrollToSelected;
      EventDiaryChanged();
    end;
  end else
  begin
    ID := DiaryView.SelectedRecordID;
    Rec := TInsRecord(LocalSource.FindById(ID));

    if ShowInsEditor(Rec, New) then
    begin
      LocalSource.Save(Rec);
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Rec.ID;
      ScrollToSelected;
      EventDiaryChanged();
    end;
  end;
end;

{======================================================================================================================}
procedure TForm1.ClickMeal(New: boolean);
{======================================================================================================================}

  function ShowMealEditor(var Rec: TMealRecord; New: boolean): boolean;
  begin
    Log(DEBUG, 'TForm1.ShowMealEditor()');
    Result := TFormEditorMeal.ShowEditor(TVersioned(Rec), New);
end;

var
  ID: TCompactGUID;
  Rec: TMealRecord;
begin
  Log(DEBUG, 'TForm1.ClickMeal');
  if New then
  begin
    Rec := TMealRecord.Create();
    if ShowMealEditor(Rec, New) then
    begin
      LocalSource.Add(Rec);
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Rec.ID;
      ScrollToSelected;

      UpdateMealDose;
      UpdateMealStatistics;
      EventDiaryChanged();
    end;
  end else
  begin
    ID := DiaryView.SelectedRecordID;
    Rec := TMealRecord(LocalSource.FindById(ID));

    if ShowMealEditor(Rec, New) then
    begin
      LocalSource.Save(Rec);
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Rec.ID;
      ScrollToSelected;

      UpdateMealDose;

      // UX features
      UpdateTimeLeft;
      ComboDiaryNew.SetFocus;
      EventDiaryChanged();
    end;
  end;
end;

{======================================================================================================================}
procedure TForm1.ClickNote(New: boolean);
{======================================================================================================================}

  function ShowNoteEditor(var Rec: TNoteRecord; New: boolean): boolean;
  begin
    Log(DEBUG, 'TForm1.ShowNoteEditor()');
    Result := TFormEditorNote.ShowEditor(TVersioned(Rec), New);
  end;
  
var
  ID: TCompactGUID;
  Rec: TNoteRecord;
begin
  Log(DEBUG, 'TForm1.ClickNote');
  if New then
  begin
    Rec := TNoteRecord.Create();
    if ShowNoteEditor(Rec, New) then
    begin
      LocalSource.Add(Rec);
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Rec.ID;
      ScrollToSelected;
      EventDiaryChanged();
    end;
  end else
  begin
    ID := DiaryView.SelectedRecordID;
    Rec := TNoteRecord(LocalSource.FindById(ID));

    if ShowNoteEditor(Rec, New) then
    begin
      LocalSource.Save(Rec);
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Rec.ID;
      ScrollToSelected;
      EventDiaryChanged();
    end;
  end;
end;

{======================================================================================================================}
procedure TForm1.ClickMealFoodMassChange();
{======================================================================================================================}
var
  NewMass: real;
  Rec: TCustomRecord;
  Meal: TMealRecord;
  Line: integer;
begin
  Rec := SelectedRecord();
  if ((Rec <> nil) and (Rec is TMealRecord)) then
  begin
    Meal := TMealRecord(Rec);
    NewMass := Meal[DiaryView.SelectedLine].Mass;
    if (DialogFoodMass(dtChangeMass, DiaryView.SelectedFood.Name, NewMass)) then
    begin
      if (NewMass >= 0) then
      begin
        Meal[DiaryView.SelectedLine].Mass := NewMass;
        Meal.Modified();
        LocalSource.Save(Meal);
        UpdateMealStatistics();
        UpdateMealDose();

        Line := DiaryView.SelectedLine;
        DiaryView.OpenPage(Diary[Trunc(Form1.CalendarDiary.Date)], True);
        DiaryView.SelectedRecordID := Rec.ID;
        DiaryView.SelectedLine := Line;
        EventDiaryChanged();
      end else
        ErrorMessage('Масса не может быть отрицательной');
    end;
  end;
end;

{======================================================================================================================}
procedure TForm1.ClickMealFoodMassDelta;
{======================================================================================================================}
var
  DeltaMass: real;
  Rec: TCustomRecord;
  Meal: TMealRecord;
  Line: integer;
begin
  Rec := SelectedRecord();
  if ((Rec <> nil) and (Rec is TMealRecord)) then
  begin
    Meal := TMealRecord(Rec);
    DeltaMass := 0;
    if DialogFoodMass(dtDeltaMass, Meal[DiaryView.SelectedLine].Name, DeltaMass) then
    begin
      if (DiaryView.SelectedFood.Mass+DeltaMass >= 0) then
      begin
        Meal[DiaryView.SelectedLine].Mass := Meal[DiaryView.SelectedLine].Mass + DeltaMass;
        Meal.Modified();
        LocalSource.Save(Meal);
        UpdateMealStatistics();
        UpdateMealDose();

        Line := DiaryView.SelectedLine;
        DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
        DiaryView.SelectedRecordID := Rec.ID;
        DiaryView.SelectedLine := Line;
        EventDiaryChanged();
      end else
        ErrorMessage('Новая масса не может быть отрицательной.');
    end;
  end;
end;

{======================================================================================================================}
procedure TForm1.DiaryViewDoubleClickBlood(Sender: TObject; Index: Integer;
  Place: TClickPlace);
{======================================================================================================================}
begin
  // TODO 5: sense less
  case Place of
    cpTime:  ClickBlood(False);
    cpRec,cpPanel: ClickBlood(False);
  end;
end;

{======================================================================================================================}
procedure TForm1.DiaryViewDoubleClickIns(Sender: TObject; Index: Integer;
  Place: TClickPlace);
{======================================================================================================================}
begin
  // TODO 5: sense less
  case Place of
    cpTime:  ClickIns(False);
    cpRec,cpPanel: ClickIns(False);
  end;
end;

{======================================================================================================================}
procedure TForm1.DiaryViewDoubleClickMeal(Sender: TObject; Index: Integer;
  Place: TClickPlace);
{======================================================================================================================}
begin
  case Place of
    cpTime:  ClickMeal(False);
    cpPanel: ClickMeal(False);
    cpRec:   ClickMealFoodMassChange();
  end;
end;

{======================================================================================================================}
procedure TForm1.DiaryViewDoubleClickNote(Sender: TObject; Index: Integer;
  Place: TClickPlace);
{======================================================================================================================}
begin
  // TODO 5: sense less
  case Place of
    cpTime:  ClickNote(False);
    cpRec,cpPanel: ClickNote(False);
  end;
end;

{======================================================================================================================}
procedure TForm1.ResetTabStop(Sender: TObject);
{======================================================================================================================}
begin
  TWinControl(Sender).TabStop := False;
end;

{======================================================================================================================}
procedure AnalyzeCallBack(Progress: integer);
{======================================================================================================================}
begin
  Form1.StatusBar.Panels[1].Text := Format('Анализ дневника... %d%%', [Progress]);
  Application.ProcessMessages;
end;

{======================================================================================================================}
procedure TForm1.UpdateKoofs();
{======================================================================================================================}
var
  Par: TRealArray;
  TimeFrom, TimeTo: TDateTime;
  Items: TRecordList;
begin
  if (Length(Analyzers) = 0) then
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

  TimeTo := GetTimeUTC();
  TimeFrom := GetTimeUTC() - Value['DaysProcess'];

  {===============================================================}
  SetLength(Par, 1);
  Par[PAR_ADAPTATION] := Value['Adaptation'];  { [0.5..1.0] }
  Items := LocalSource.FindPeriod(TimeFrom, TimeTo);
  AnalyzeResults := Analyze(Analyzers, Items, Par, AnalyzeCallBack);
  AvgAnalyzeResult := BuildAvgAnalyzer(AnalyzeResults);
  {===============================================================}

  ButtonUpdateKoof.Caption := 'Пересчитать';

  if (ComboKoof.ItemIndex <> -1) then
    ComboKoofChange(nil);

  if (SelectedRecord() is TMealRecord) then
    UpdateMealDose();

  TimerTimeLeft.Enabled := True;

  FinishProc;
end;

{======================================================================================================================}
function TForm1.GetCompensationMass(const RelCarbs, RelProts: real): real;
{======================================================================================================================}
{ используется при выборе продукта/блюда в комбобоксе внизу }
var
  Kf: TKoof;
  RelBS: real;
begin
  Log(DEBUG, 'TForm1.GetCompensationMass()');

  if (SelectedRecord() is TMealRecord) then
  begin
    Kf := GetKoof(TMealRecord(SelectedRecord()).TimeInMinutes);
    RelBS := (RelCarbs*Kf.k + RelProts*Kf.p)/100;
    if (RelBS > 0) then
      Result := CurrentDB/RelBS
    else
      Result := 0;
  end else
    Result := 0;
end;

{======================================================================================================================}
function TForm1.GetBestMass(const RelCarbs, RelProts, CurMass: real): real;
{======================================================================================================================}
{ подбор массы в приёме пищи }
var
  Rec: TCustomRecord;
  Kf: TKoof;
  RelBS: real;
begin
  Rec := SelectedRecord();
  if (Rec is TMealRecord) then
  begin
    Kf := GetKoof(TMealRecord(Rec).TimeInMinutes);
    RelBS := (RelCarbs * Kf.k + RelProts * Kf.p) / 100;
    if (RelBS > 0) then
      Result := (CurrentDB + RelBS * CurMass) / RelBS
    else
      Result := 0.0;
  end else
    Result := 0.0;
end;

{======================================================================================================================}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
{======================================================================================================================}
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

{======================================================================================================================}
procedure TForm1.TimerAutosaveTimer(Sender: TObject);
{======================================================================================================================}
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

{======================================================================================================================}
procedure TForm1.DiaryViewFoodShow(Sender: TObject; Index, Line: Integer;
  var Text: String);
{======================================================================================================================}
var
  R: TKoof;
begin
  R := GetKoof(DiaryView.CurrentPage[Index].TimeInMinutes);

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

{======================================================================================================================}
procedure TForm1.ComboKoofChange(Sender: TObject);
{======================================================================================================================}
const
  GraphTypes: array[0..3] of TKoofType = (kfK, kfQ, kfP, kfX);
var
  AnalyzeResult: TAnalyzeResult;
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
      AnalyzeResult := AnalyzeResults[AnalyzerIndex - 1];
    end else
      AnalyzeResult := AvgAnalyzeResult;

    KoofList := AnalyzeResult.KoofList;
    LabelCalcTime.Caption := Format('Время расчёта: %d мсек', [AnalyzeResult.Time]);
    LabelAvgDeviation.Caption := Format('Ошибка: ±%.2f ммоль/л', [AnalyzeResult.Error]);
    LabelWeight.Caption := Format('Вес: %.0f', [AnalyzeResult.Weight * 100]) + '%';

    if (Length(AnalyzeResults) > 0) then
    begin
      DrawKoof(ImageLarge, KoofList, AnalyzeResults[0].AnList, GraphTypes[KoofIndex], Value['ShowPoints']);
    end;
    LabelKoofDiscription.Caption := KoofDisc[KoofIndex];
  end;

  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.ButtonUpdateKoofClick(Sender: TObject);
{======================================================================================================================}
begin
  ButtonUpdateKoof.Enabled := False;
  UpdateKoofs();
  ButtonUpdateKoof.Enabled := True;
end;

{======================================================================================================================}
procedure TForm1.PageControl1Change(Sender: TObject);
{======================================================================================================================}
begin
  if (PageControl1.ActivePage = TabAnalyze)and
     (ComboKoof.ItemIndex = -1) then
  begin
    ComboKoof.ItemIndex := 0;
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

{======================================================================================================================}
procedure TForm1.ScrollToSelected;
{======================================================================================================================}
var
  n: integer;
begin
  n := DiaryView.GetSelectedRecordIndex;
  { вторая проверка, наверно, излишняя }
  if (n > -1)and(Length(DiaryView.CurrentPage) > 0) then
    ScrollBoxDiary.VertScrollBar.Position :=
      Round(n / Length(DiaryView.CurrentPage) * ScrollBoxDiary.VertScrollBar.Range);
end;

{======================================================================================================================}
procedure TForm1.ButtonAvgBSClick(Sender: TObject);
{======================================================================================================================}
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

  LastDate := Trunc(GetTimeUTC());
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

{======================================================================================================================}
procedure TForm1.ActionExitExecute(Sender: TObject);
{======================================================================================================================}
begin
  Close;
end;

{======================================================================================================================}
procedure TForm1.ActionAddRecordExecute(Sender: TObject);
{======================================================================================================================}
begin
  PageControl1.ActivePageIndex := 0;
  case TControl(Sender).Tag of
    1: ClickBlood(True);
    2: ClickIns(True);
    3: ClickMeal(True);
    4: ClickNote(True);
  end;
end;

{======================================================================================================================}
procedure TForm1.ActionExportDiaryExecute(Sender: TObject);
{======================================================================================================================}
begin
  FormExportText.ShowModal;
end;

{======================================================================================================================}
procedure TForm1.ActionSyncExecute(Sender: TObject);
{======================================================================================================================}
begin
  //ThreadExec.Execute(TaskSaveAndSync, tpIdle, 80000, TASK_SAVE_N_SYNC);
  TaskSaveAndSync(nil);
end;

{======================================================================================================================}
procedure TForm1.ActionSettingsExecute(Sender: TObject);
{======================================================================================================================}
begin
  FormSettings.OpenSettings;
end;

{======================================================================================================================}
procedure TForm1.ActionAboutExecute(Sender: TObject);
{======================================================================================================================}
begin
  FormAbout.ShowModal;
end;

{======================================================================================================================}
procedure TForm1.ActionExportKoofsExecute(Sender: TObject);
{======================================================================================================================}
begin
  WriteFile('temp/Koofs.json', ExportKoofs(True));
end;

{======================================================================================================================}
procedure TForm1.ActionExportFoodExecute(Sender: TObject);
{======================================================================================================================}
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

{======================================================================================================================}
procedure TForm1.PanelDiaryTopLeftDblClick(Sender: TObject);
{======================================================================================================================}
begin
  ImageMinMaxStatMouseDown(nil,mbLeft,[],0,0);
  ImageMinMaxGraphMouseDown(nil,mbLeft,[],0,0);
  ImageMinMaxTimesMouseDown(nil,mbLeft,[],0,0);
end;

{======================================================================================================================}
procedure TForm1.MyIdle(Sender: TObject; var Done: Boolean);
{======================================================================================================================}
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
      ProcessMealSelected(SelectedRecord() is TMealRecord);
      if WebClient.Online then
        Form1.StatusBar.Panels[2].Text := STATUS_RESULT_STATE_ONLINE
      else
        Form1.StatusBar.Panels[2].Text := STATUS_RESULT_STATE_OFFLINE;
    end;

    1: { базы }
    begin
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

    UpdateFoodTableHeaders();
    UpdateFoodTable();

    UpdateDishTableHeaders();
    UpdateDishTable();

    { после Maximize }
    //ListFood.Columns[0].Width := ListFood.Columns[0].Width - 30;
    //ListDish.Columns[0].Width := ListDish.Columns[0].Width - 30;

    Done := False;
    Exit;
  end;

  if IN_IDLE_CheckUpdates then
  begin
    IN_IDLE_CheckUpdates := False;

    // TODO: duplicate
    Value['LastUpdateCheck'] := GetTimeUTC();
    LatestVersion := GetLatestVersion(DiaryCore.WebClient);
    if LatestVersion > PROGRAM_VERSION_CODE then
    begin
      ShowBalloon(BALLOON_INFO_NEW_VERSION_AVAILABLE, bitInfo, BalloonAction_StartUpdate);
    end;

    Done := False;
    Exit;
  end;

  finally
    //FinishProc;
  end;
end;

{======================================================================================================================}
procedure TForm1.MyActivate(Sender: TObject);
{======================================================================================================================}
begin
  StartProc('TForm1.MyActivate');
  TimerTimeLeftTimer(nil);
  TimerTimeLeft.Interval := 1000;
  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.MyDeactivate(Sender: TObject);
{======================================================================================================================}
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
 { for i := Trunc(GetTimeUTC())-29 to Trunc(GetTimeUTC()) do
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

  for ToDate := Trunc(GetTimeUTC()) - LOOK_PERIOD to Trunc(GetTimeUTC()) do
  begin
    AnalyzeBS(LocalSource, ToDate - AVG_PERIOD, ToDate, Mean, StdDev, Targeted, Less, More);
    ListBS.Items.Add(Format('%s'#9'%.2f'#9'%.2f'#9'%.1f'#9'%.1f', [DateToStr(ToDate - (AVG_PERIOD div 2)), Mean, StdDev, Less * 100, Targeted * 100]));
  end;

  ListBS.Items.SaveToFile('temp\BS.txt');

  {for i := Trunc(GetTimeUTC()) - LOOK_PERIOD to Trunc(GetTimeUTC()) do
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

{======================================================================================================================}
procedure TForm1.ComboValueChange(Sender: TObject);
{======================================================================================================================}
type
  TRecItem = record
    Date: TDate;
    Value: real;
  end;

var
  List: array of TRecItem;
  //FirstDate: TDate;
  //LastDate: TDate;

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

  {FirstDate := Trunc(EncodeDate(2010, 01, 01));
  LastDate := Trunc(GetTimeUTC());
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
  end; }

  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.ProcMessage(var M: TMessage);
{======================================================================================================================}
begin
  TrayIcon.ShowMainForm;
end;

{======================================================================================================================}
procedure TForm1.BalloonAction_ShowForm;
{======================================================================================================================}
begin
  TrayIcon.ShowMainForm;
end;

{======================================================================================================================}
procedure TForm1.BalloonAction_ShowInternetSettings;
{======================================================================================================================}
begin
  TrayIcon.ShowMainForm;
  FormSettings.OpenSettings(spInternet);
end;

{======================================================================================================================}
procedure TForm1.BalloonAction_StartUpdate;
{======================================================================================================================}
begin
  SetupUpdate(LatestVersion);
end;

{======================================================================================================================}
procedure TForm1.TrayIconBalloonHintClick(Sender: TObject);
{======================================================================================================================}
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

{======================================================================================================================}
procedure TForm1.ShowBalloon(const Msg: string; MsgType: TBalloonHintIcon;
  AfterAction: TBalloonAction);
{======================================================================================================================}
var
  Time: integer;
begin
  case MsgType of
    bitInfo:              Time := 30;
    bitWarning, bitError: Time := 30; // max
    else                  Time := 30;
  end; // cute

  BalloonAction := AfterAction;
  TrayIcon.ShowBalloonHint(APPLICATION_TITLE, Msg, MsgType, Time);
end;

{======================================================================================================================}
procedure TForm1.ActionBalanceOnOffExecute(Sender: TObject);
{======================================================================================================================}
begin
  { изменяется после клика }
  Value['Balance'] := not Value['Balance'];
  ActionBalanceOnOff.Checked := Value['Balance'];

  if (not Value['Balance'])and(Value['ProcShow'] = 3) then
    Value['ProcShow'] := 1;

  UpdateDayInfo;
end;

{======================================================================================================================}
procedure TForm1.ActionEditBloodExecute(Sender: TObject);
{======================================================================================================================}
begin
  ClickBlood(False);
end;

{======================================================================================================================}
procedure TForm1.ActionEditInsExecute(Sender: TObject);
{======================================================================================================================}
begin
  ClickIns(False);
end;

{======================================================================================================================}
procedure TForm1.ActionEditMealExecute(Sender: TObject);
{======================================================================================================================}
begin
  ClickMeal(False);
end;  

{======================================================================================================================}
procedure TForm1.ActionEditNoteExecute(Sender: TObject);
{======================================================================================================================}
begin
  ClickNote(False);
end;

{======================================================================================================================}
procedure TForm1.ActionRemovePanelExecute(Sender: TObject);
{======================================================================================================================}
var
  Rec: TCustomRecord;
  MsgType: TMsgDlgType;
  Msg: string;
begin
  Rec := SelectedRecord();
  if (Rec is TBloodRecord) then
  begin
    MsgType := mtConfirmation;
    Msg := MESSAGE_CONF_REMOVE_DIARY_BLOOD;
  end else
  if (Rec is TInsRecord) then
  begin
    MsgType := mtConfirmation;
    Msg := MESSAGE_CONF_REMOVE_DIARY_INS;
  end else
  if (Rec is TMealRecord) then
  begin
    MsgType := mtWarning;
    Msg := MESSAGE_CONF_REMOVE_DIARY_MEAL;
  end else
  if (Rec is TNoteRecord) then
  begin
    MsgType := mtConfirmation;
    Msg := MESSAGE_CONF_REMOVE_DIARY_NOTE;
  end else
  begin
    // error
    MsgType := mtConfirmation;
    Msg := MESSAGE_CONF_REMOVE_DIARY_UNKNOWN;

    AutoLog.Log(ERROR, Format('Unsupported record type to delete (date: %s, index: %d)',
      [DateToStr(CalendarDiary.Date), DiaryView.GetSelectedRecordIndex]));
  end;

  if (MessageDlg(Msg, MsgType, [mbYes, mbNo], 0) = mrYes) then
  begin
    LocalSource.Delete(DiaryView.SelectedRecordID);
    DiaryView.SelectedRecordID := '';
    DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);

    UpdateMealDose;
    UpdateMealStatistics;
  end;
end;

{======================================================================================================================}
procedure TForm1.ActionBalanceMealExecute(Sender: TObject);
{======================================================================================================================}
{var
  i,n: integer;
  SCarbs: string;
  FullCarbs,TCarbs,TProts,TFats: extended;   }
begin
  //if (DiaryView.SelectedRecord is TMealRecord) then
  //begin
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
  //end;
end;

procedure TForm1.ActionCalcMassExecute(Sender: TObject);
var
  NewMass: real;
  Cant: boolean;
  meal: TMealRecord;
  Line: integer;
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

    Meal := TMealRecord(SelectedRecord());
    Meal[DiaryView.SelectedLine].Mass := Round(NewMass);
    Meal.Modified();
    LocalSource.Save(Meal);
    UpdateMealStatistics();
    UpdateMealDose();

    Line := DiaryView.SelectedLine;
    DiaryView.OpenPage(Diary[Trunc(Form1.CalendarDiary.Date)], True);
    DiaryView.SelectedRecordID := Meal.ID;
    DiaryView.SelectedLine := Line;
    EventDiaryChanged();
  end;
end;

procedure TForm1.ActionEditFoodExecute(Sender: TObject);
begin
  ClickMealFoodMassChange();
end;

procedure TForm1.ActionDeltaMassExecute(Sender: TObject);
begin
  ClickMealFoodMassDelta();
end;

{======================================================================================================================}
procedure TForm1.ActionRemoveFoodExecute(Sender: TObject);
{======================================================================================================================}
var
  Meal: TMealRecord;
  Food: TFoodMassed;
begin
  if (DiaryView.IsFoodSelected) then
  begin
    Meal := TMealRecord(SelectedRecord());
    Food := Meal[DiaryView.SelectedLine];

    if MessageDlg(Format(MESSAGE_CONF_REMOVE_DIARY_FOOD, [Food.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Meal.Remove(DiaryView.SelectedLine);
      Meal.Modified();
      LocalSource.Save(Meal);
      DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
      DiaryView.SelectedRecordID := Meal.ID;
      EventDiaryChanged();
    end;
  end;
end;

{======================================================================================================================}
procedure TForm1.ActionShowWindowExecute(Sender: TObject);
{======================================================================================================================}
begin
  TrayIcon.ShowMainForm;
end;

{======================================================================================================================}
procedure TForm1.ActionHideWindowExecute(Sender: TObject);
{======================================================================================================================}
begin
  TrayIcon.HideMainForm;
end;

{======================================================================================================================}
procedure TForm1.ActionIsMinToTrayExecute(Sender: TObject);
{======================================================================================================================}
begin
  Value['MinToTray'] := not Value['MinToTray'];
  TrayIcon.MinimizeToTray := Value['MinToTray'];
  ActionIsMinToTray.Checked := Value['MinToTray'];
end;

{======================================================================================================================}
procedure SetupUpdate(const ExpectedVersion: integer);
{======================================================================================================================}
begin
  Value['UpdatedVersion'] := ExpectedVersion;
  RunLoader(DiaryCore.WebClient, Form1.Handle);
end;

{======================================================================================================================}
procedure TForm1.ActionCheckUpdateExecute(Sender: TObject);
{======================================================================================================================}
var
  Version: integer;
begin
  // TODO: duplicate
  Value['LastUpdateCheck'] := GetTimeUTC();
  Version := GetLatestVersion(DiaryCore.WebClient);

  if (Version = -1) then
    ErrorMessage(MESSAGE_ERROR_NO_INTERNET) else
  if (Version = PROGRAM_VERSION_CODE) then
    InfoMessage(MESSAGE_INFO_NO_UPDATES) else
  if (Version > PROGRAM_VERSION_CODE) then
  begin
    // i18n
    if (MessageDlg('Найдена новая версия: '+ IntToStr(Version) + #13'Установить сейчас?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      SetupUpdate(Version);
  end else
    ShowMessage('Dev Mode!');
end;

{======================================================================================================================}
procedure TForm1.ActionSettingsImageExecute(Sender: TObject);
{======================================================================================================================}
begin
  FormSettings.OpenSettings(spView);
end;

{======================================================================================================================}
procedure TForm1.ActionSettingsAnalyzeExecute(Sender: TObject);
{======================================================================================================================}
begin
  FormSettings.OpenSettings(spAnalyze);
end;

{======================================================================================================================}
procedure TForm1.ActionSettingsStatExecute(Sender: TObject);
{======================================================================================================================}
begin
  FormSettings.OpenSettings(spNorms);
end;

{======================================================================================================================}
procedure TForm1.ItemCopyFoodClick(Sender: TObject);
{======================================================================================================================}
var
  Item: TFoodItem;
  n, i: integer;
  OldName: string;
  NewName: string;
begin
  n := ListFood.ItemIndex;
  if (n < 0) or (n > High(FoodList)) then
  begin
    Exit;
  end;

  Item := FoodBaseLocal.FindById(FoodList[n].ID) as TFoodItem;
  OldName := Item.Name;
  i := 1;
  repeat
    inc(i);
    NewName := Format('%s (%d)', [OldName, i]);
  until FoodBaseLocal.FindOne(NewName) = nil;

  Item.ID := CreateCompactGUID();
  Item.Name := NewName;
  Item.Modified();
  FoodBaseLocal.Save(Item);

  EventFoodbaseChanged(True);

  //ShowTableItem(ListFood, n);
  ListFood.SetFocus;
  // TODO 5: RF: selecting copied food disabled
end;

{======================================================================================================================}
procedure TForm1.ItemCopyDishClick(Sender: TObject);
{======================================================================================================================}
var
  Item: TDishItem;
  n, i: integer;
  OldName: string;
  NewName: string;
begin
  n := ListDish.ItemIndex;
  if (n < 0) or (n > High(DishList)) then
  begin
    Exit;
  end;

  Item := DishBaseLocal.FindByID(DishList[n].ID) as TDishItem;
  OldName := Item.Name;
  i := 1;
  repeat
    inc(i);
    NewName := Format('%s (%d)', [OldName, i]);
  until DishBaseLocal.FindOne(NewName) = nil;

  Item.ID := CreateCompactGUID();
  Item.Name := NewName;
  Item.Modified();
  DishBaseLocal.Save(Item);

  // TODO: rough
  EventDishbaseChanged(True, True);

  //ShowTableItem(ListDish, n);
  ListDish.SetFocus;
  // TODO 5: RF: selecting copied food disabled
end;

{======================================================================================================================}
procedure TForm1.ListBaseColumnRightClick(Sender: TObject; Column: TListColumn; Point: TPoint);
{======================================================================================================================}
begin
  GetCursorPos(Point);
  case TWinControl(Sender).Tag of
    1: PopupFoodCol.Popup(Point.X, Point.Y);
    2: PopupDishCol.Popup(Point.X, Point.Y);
  end;
end;

{======================================================================================================================}
procedure TForm1.ListBaseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{======================================================================================================================}
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

{======================================================================================================================}
procedure TForm1.Item_BaseColClick(Sender: TObject);
{======================================================================================================================}
var
  Val: boolean;
begin
  Val := not TMenuItem(Sender).Checked;

  TMenuItem(Sender).Checked := Val;
  case TMenuItem(Sender).Tag of
    1: Value['FoodP'] := Val;
    2: Value['FoodF'] := Val;
    3: Value['FoodC'] := Val;
    4: Value['FoodV'] := Val;
    5: Value['FoodD'] := Val;

    -1: Value['DishM'] := Val;
    -2: Value['DishP'] := Val;
    -3: Value['DishF'] := Val;
    -4: Value['DishC'] := Val;
    -5: Value['DishV'] := Val;
    -6: Value['DishD'] := Val;
  end;

  if TMenuItem(Sender).Tag in [1..5] then
  begin
    UpdateFoodTableHeaders();
    // make it resize
    ListFood.Height := ListFood.Height + 1;
    ListFood.Height := ListFood.Height - 1;
  end else
  if -TMenuItem(Sender).Tag in [1..6] then
  begin
    UpdateDishTableHeaders();
    // make it resize
    ListDish.Height := ListDish.Height + 1;
    ListDish.Height := ListDish.Height - 1;
  end;
end;

{======================================================================================================================}
procedure TForm1.ActionShortMealExecute(Sender: TObject);
{======================================================================================================================}
var
  Rec: TCustomRecord;
  Meal: TMealRecord;
begin
  //{ все проверки - внутри }
  //DiaryView.SetShortMeal(ActionShortMeal.Checked);

  Rec := SelectedRecord();

  if (Rec is TMealRecord) then
  begin
    Meal := TMealRecord(Rec);
    Meal.ShortMeal := ActionShortMeal.Checked;
    Meal.Modified();
    LocalSource.Save(Meal);

    UpdateTimeLeft();
    EventDiaryChanged();
  end;
end;

{======================================================================================================================}
procedure TForm1.ButtonInsListClick(Sender: TObject);
{======================================================================================================================}
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

 (* for i := Trunc(GetTimeUTC()) - LOOK_PERIOD + 1 to Trunc(GetTimeUTC()) do
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

{======================================================================================================================}
procedure TForm1.ThreadExec_Done(Sender: TObject; TaskID: Cardinal);
{======================================================================================================================}
begin
  // DONE

  // Если мы пришли сюда, то это значит только то, что задача не вылетела
  // по тайм-ауту. Не факт, что задача выполнена успешно.

  {case TaskID of
    TASK_SAVE_N_SYNC: StatusBar.Panels[3].Text := 'Дневник загружен на сервер';
    TASK_LOGIN: StatusBar.Panels[2].Text := 'Онлайн';
  end;   }
end;

{======================================================================================================================}
procedure TForm1.ThreadExec_TimeOut(Sender: TObject; TaskID: Cardinal);
{======================================================================================================================}
begin
  // TIME OUT
  case TaskID of
    TASK_SAVE_N_SYNC: StatusBar.Panels[3].Text := 'Ошибка загрузки [t/o]';
    //TASK_LOGIN: StatusBar.Panels[2].Text := 'Оффлайн [t/o]';
  end;
end;

{======================================================================================================================}
procedure TForm1.ComboDiaryNewDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
{======================================================================================================================}
begin
  DrawACItem(
    Control,
    Rect,
    DiaryMultiMap[ComboDiaryNew.ShowedIndex[Index]],
    MaxDiaryTag,
    odSelected in State,
    Value['CarbsInfo']
  );
end;

procedure TForm1.DiaryViewChange(Sender: TObject; EventType: TPageEventType;
  Page: TRecordList; RecClass: TClassCustomRecord; RecInstance: TCustomRecord);
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

{======================================================================================================================}
procedure TForm1.UpdateNextFinger;
{======================================================================================================================}

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

  LabelDiaryFinger.Caption := MAIN_DIARY_PANEL_TIME_FINGER;

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
    LabelDiaryFingerVal.Hint := MAIN_DIARY_PANEL_TIME_FINGER_NOT_FOUND;
  end;

  FinishProc;
end;

{======================================================================================================================}
procedure TForm1.UpdateTimeLeft;  
{======================================================================================================================}
var
  hour,min,sec,msec: word;
  i: integer;
  Found: boolean;
  Meal: TMealRecord;
  TempTime: integer;

  Recs: TRecordList;
begin
  //StartProc('UpdateTimeLeft(): search for meal');
                              
  LabelDiaryTimeLeftIns.Caption := MAIN_DIARY_PANEL_TIME_AFTER_INS;
  LabelDiaryTimeLeftMeal.Caption := MAIN_DIARY_PANEL_TIME_AFTER_MEAL;

  DecodeTime(SysUtils.Time, Hour, Min, Sec, msec);

  {=====================================================}

  Recs := LocalSource.FindPeriod(GetTimeUTC() - SEARCH_INTERVAL, GetTimeUTC());

  Found := False;
  for i := High(Recs) downto 0 do
  begin
    if (Recs[i].RecType = TMealRecord) then
    begin
      Meal := TMealRecord(Recs[i]);
      if (Meal.Count > 0) and (not Meal.ShortMeal) then
      begin
        TempTime := Round((GetTimeUTC() - Recs[i].Time) * SecPerDay);

        if (TempTime >= 0) then
        begin
          Found := True;
          Form1.LabelDiaryTimeLeftMealVal.Caption := TimeTrncSecToStr(TempTime);
          break;
        end;
      end;
    end;
    if Found then break;
  end;
  Form1.LabelDiaryTimeLeftMealVal.Font.Color := COLOR_HASDATA[Found];
  if not Found then
    Form1.LabelDiaryTimeLeftMealVal.Caption := '?';
  {=====================================================}

  Found := false;
  for i := High(Recs) downto 0 do
  begin
    if (Recs[i].RecType = TInsRecord) then
    begin
      TempTime := Round((GetTimeUTC() - Recs[i].Time) * SecPerDay);
      if (TempTime > 0) then
      begin
        Found := True;
        Form1.LabelDiaryTimeLeftInsVal.Caption := TimeTrncSecToStr(TempTime);
        break;
      end;
    end;
    if Found then Break;
  end;
  Form1.LabelDiaryTimeLeftInsVal.Font.Color := COLOR_HASDATA[Found];
  if not Found then
    Form1.LabelDiaryTimeLeftInsVal.Caption := '?';

  FreeRecords(Recs);

  //FinishProc;
end;

{======================================================================================================================}
procedure TForm1.ApplyInterfaceSettings;
{======================================================================================================================}

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
  Item_FoodD.Checked  := Value['FoodD'];

  Item_DishM.Checked := Value['DishM'];
  Item_DishP.Checked := Value['DishP'];
  Item_DishF.Checked := Value['DishF'];
  Item_DishC.Checked := Value['DishC'];
  Item_DishV.Checked := Value['DishV'];
  Item_DishD.Checked := Value['DishD'];

  { Localization }

  TabDiary.Caption := MAIN_DIARY;
  TabBase.Caption := MAIN_BASES;
  DiaryView.TextEmptyPage := MAIN_DIARY_VIEW_EMPTYPAGE;

  FinishProc;
end;

{======================================================================================================================}
procedure SetColumn(Columns: TListColumns; var Index: integer;
  Caption: String; AutoSize: boolean; MinWidth: integer; Width: integer);
{======================================================================================================================}
begin
  inc(Index);
  with Columns do
  begin
    while (Count < Index) do Add();

    Items[Index - 1].Caption := Caption;
    Items[Index - 1].AutoSize := AutoSize;
    Items[Index - 1].MinWidth := MinWidth;
    if (not AutoSize) then
      Items[Index - 1].Width := Width;
  end;
end;

{======================================================================================================================}
procedure TForm1.UpdateFoodTableHeaders();
{======================================================================================================================}
var
  n: integer;
begin
  n := 0;

  if True           then SetColumn(ListFood.Columns, n, 'Наименование', True, 150, 150);
  if Value['FoodP'] then SetColumn(ListFood.Columns, n, 'Б',            False, 50, 50);
  if Value['FoodF'] then SetColumn(ListFood.Columns, n, 'Ж',            False, 50, 50);
  if Value['FoodC'] then SetColumn(ListFood.Columns, n, 'У',            False, 50, 50);
  if Value['FoodV'] then SetColumn(ListFood.Columns, n, 'ккал',         False, 50, 50);
  if Value['FoodD'] then SetColumn(ListFood.Columns, n, 'Дата',         False, 80, 80);

  with ListFood.Columns do
  while (Count > n) do
    Delete(Count - 1);
end;

{======================================================================================================================}
procedure TForm1.UpdateDishTableHeaders();
{======================================================================================================================}
var
  n: integer;
begin
  n := 0;

  if True           then SetColumn(ListDish.Columns, n, 'Наименование', True, 150, 150);
  if Value['DishM'] then SetColumn(ListDish.Columns, n, 'Масса',        False, 50, 60);
  if Value['DishP'] then SetColumn(ListDish.Columns, n, 'Б',            False, 50, 50);
  if Value['DishF'] then SetColumn(ListDish.Columns, n, 'Ж',            False, 50, 50);
  if Value['DishC'] then SetColumn(ListDish.Columns, n, 'У',            False, 50, 50);
  if Value['DishV'] then SetColumn(ListDish.Columns, n, 'ккал',         False, 50, 50);
  if Value['DishD'] then SetColumn(ListDish.Columns, n, 'Дата',         False, 80, 80);

  with ListDish.Columns do
  while (Count > n) do
    Delete(Count - 1);
end;

  function MoreFoodName(n1, n2: integer): boolean;
  begin
    Result := AnsiUpperCase(FoodList[n1].Name) > AnsiUpperCase(FoodList[n2].Name);
  end;

  function MoreFoodDate(n1, n2: integer): boolean;
  begin
    Result := FoodList[n1].TimeStamp < FoodList[n2].TimeStamp;
  end;

  procedure ExchFood(n1, n2: integer);
  var
    temp: TFoodItem;
  begin
    temp := FoodList[n1];
    FoodList[n1] := FoodList[n2];
    FoodList[n2] := temp;
  end;

{======================================================================================================================}
procedure TForm1.UpdateFoodTable();
{======================================================================================================================}

  procedure UpdateFoodbaseFilter();
  var
    Filter: string;
  begin
    BusinessObjects.Free(FoodList);

    Filter := EditBaseFoodSearch.Text;
    if (Trim(Filter) = '') then
    begin
      FoodList := FoodBaseLocal.FindAll(false);
    end else
    begin
      FoodList := FoodBaseLocal.FindAny(Filter);
    end;

    case FoodBaseSort of
      smName:
        begin
          QuickSort(0, High(FoodList), ExchFood, MoreFoodName);
        end;
      smDate:
        begin
          QuickSort(0, High(FoodList), ExchFood, MoreFoodDate);
        end;
    end;
  end;

begin
  StartProc('UpdateFoodTable()');

  UpdateFoodbaseFilter();

  LabelFoodBase.Caption := Format(MAIN_BASES_FOOD_TITLE, [Length(FoodList)]);
  ListFood.Items.Count := Length(FoodList);

  if (Length(FoodList) <> 0) then
  begin
    ShowTableItem(ListFood, 0);
  end;

  ListFood.Repaint();
  FinishProc();
end;

  procedure ExchDish(n1, n2: integer);
  var
    temp: TDishItem;
  begin
    temp := DishList[n1];
    DishList[n1] := DishList[n2];
    DishList[n2] := temp;
  end;

  function MoreDishName(n1, n2: integer): boolean;
  begin
    Result := AnsiUpperCase(DishList[n1].Name) > AnsiUpperCase(DishList[n2].Name);
  end;

  function MoreDishDate(n1, n2: integer): boolean;
  begin
    Result := DishList[n1].TimeStamp < DishList[n2].TimeStamp;
  end;

{======================================================================================================================}
procedure TForm1.UpdateDishTable();
{======================================================================================================================}

  procedure UpdateDishbaseFilter();
  var
    Filter: string;
  begin
    BusinessObjects.Free(DishList);

    Filter := EditBaseFoodSearch.Text;
    if (Trim(Filter) = '') then
    begin
      DishList := DishBaseLocal.FindAll(false);
    end else
    begin
      DishList := DishBaseLocal.FindAny(Filter);
    end;

    case DishBaseSort of
      smName:
        begin
          QuickSort(0, High(DishList), ExchDish, MoreDishName);
        end;
      smDate:
        begin
          QuickSort(0, High(DishList), ExchDish, MoreDishDate);
        end;
    end;
  end;

begin
  StartProc('UpdateDishTable()');

  UpdateDishbaseFilter();

  LabelDishBase.Caption := Format(MAIN_BASES_DISH_TITLE, [Length(DishList)]);
  ListDish.Items.Count := Length(DishList);

  if (Length(DishList) <> 0) then
  begin
    ShowTableItem(ListDish, 0);
  end;

  ListDish.Repaint();
  FinishProc();
end;

procedure TForm1.EventDiaryChanged;
begin
  // TODO: i18n
  Form1.StatusBar.Panels[3].Text := 'Изменено';
end;

procedure TForm1.EventDishbaseChanged(CountChanged, NamesChanged: boolean);
begin
  StartProc('EventDishbaseChanged');

  // TODO: make it handler for DAO-generated event; don't call it manually

  // TODO: i18n
  Form1.StatusBar.Panels[3].Text := 'Изменено';

  UpdateDishTable();
  UpdateCombos;

  FinishProc;
end;

procedure TForm1.EventFoodbaseChanged(CountChanged: boolean);
begin
  StartProc('EventFoodbaseChenged');

  // TODO: make it handler for DAO-generated event; don't call it manually

  // TODO: i18n
  Form1.StatusBar.Panels[3].Text := 'Изменено';

  UpdateFoodTable();
  UpdateCombos;

  FinishProc;
end;

procedure TForm1.EventKoofChanged;
begin

end;

procedure TForm1.EventPageChanged;
begin

end;

{======================================================================================================================}
procedure TForm1.ButtonInsulinCalcClick(Sender: TObject);
{======================================================================================================================}
const
  PERIOD    = 30;
  CARTRIDGE = 300;
var
  Summ: real;
  i, Count: integer;
  Recs: TRecordList;
begin
  Summ := 0;
  Count := 0;

  Recs := LocalSource.FindPeriod(GetTimeUTC() - PERIOD, GetTimeUTC());

  for i := 0 to High(Recs) do
  begin
    if (Recs[i].RecType = TInsRecord) then
    begin
      inc(Count);
      Summ := Summ + TInsRecord(Recs[i]).Value;
    end;
  end;

  FreeRecords(Recs);

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

{======================================================================================================================}
procedure TForm1.PanelDevelopmentClick(Sender: TObject);
{======================================================================================================================}
begin
  FormMisc.ShowModal;
end;

{======================================================================================================================}
procedure TForm1.EditBaseFoodSearchChange(Sender: TObject);
{======================================================================================================================}
begin
  UpdateFoodTable();
  UpdateDishTable();
end;

{======================================================================================================================}
procedure TForm1.ButtonBasesFilterResetClick(Sender: TObject);
{======================================================================================================================}
begin
  // FIXME: unused method?

  if (not TSpeedButton(Sender).Down) then
    EditBaseFoodSearch.Text := '';

  UpdateFoodTable();
  if (ListFood.Items.Count > 0) then
  begin
    ListFood.Items[0].Selected := True;
    ListFood.Items[0].Focused := True;
  end;
end;

{======================================================================================================================}
procedure TForm1.EditBaseFoodSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{======================================================================================================================}
begin
  case Key of
    vk_Up:
      begin
        if (ListFood.ItemIndex > 0) then
          ListFood.ItemIndex := ListFood.ItemIndex - 1;
        Key := 0;
      end;
    vk_Down:
      begin
        if (ListFood.ItemIndex < ListFood.Items.Count - 1) then
          ListFood.ItemIndex := ListFood.ItemIndex + 1;
        Key := 0;
      end;
    vk_Return:
      begin
        ListFood.OnKeyDown(Sender, Key, Shift);
        Key := 0;
      end;
    vk_Escape:
      begin
        EditBaseFoodSearch.Text := '';
        {UpdateFoodbaseFilter();
        UpdateFoodTable(False, True, False);
        if (ListFood.Items.Count > 0) then
        begin
          ListFood.Items[0].Selected := True;
          ListFood.Items[0].Focused := True;
        end; }
        Key := 0;
      end;
  end;
end;

{======================================================================================================================}
procedure TForm1.EditBaseFoodSearchKeyPress(Sender: TObject; var Key: Char);
{======================================================================================================================}
begin
  // to prevent windows ding
  if ((Key = Char(vk_Return)) or (Key = Char(vk_Escape))) then
    Key := #0;
end;

{======================================================================================================================}
function MyTimeToStr(Date: TDateTime): string;
{======================================================================================================================}
begin
  if (Date <> 0) then
    Result := DateToStr(Date)
  else
    Result := '---';
end;

{======================================================================================================================}
procedure TForm1.ListFoodData(Sender: TObject; Item: TListItem);
{======================================================================================================================}
var
  i: integer;
begin
  i := Item.Index;
  if (i >= 0) and (i <= High(FoodList)) then
  with Item do
  begin
    Caption := FoodList[i].Name;
    ImageIndex := Byte(FoodList[i].FromTable);

    if Value['FoodP'] then SubItems.Add(RealToStr(FoodList[i].RelProts));
    if Value['FoodF'] then SubItems.Add(RealToStr(FoodList[i].RelFats));
    if Value['FoodC'] then SubItems.Add(RealToStr(FoodList[i].RelCarbs));
    if Value['FoodV'] then SubItems.Add(IntToStr(Round(FoodList[i].RelValue)));
    if Value['FoodD'] then SubItems.Add(MyTimeToStr(FoodList[i].TimeStamp));
  end;
end;

{======================================================================================================================}
procedure TForm1.ListDishData(Sender: TObject; Item: TListItem);
{======================================================================================================================}
var
  i: integer;
begin
  i := Item.Index;
  if (i >= 0) and (i <= High(DishList)) then
  with Item do
  begin
    Caption := DishList[i].Name;//+' ['+IntToStr(DishBase[i].Tag)+']';;
    ImageIndex := 2;

    if Value['DishM'] then SubItems.Add(RealToStr(DishList[i].RealMass));
    if Value['DishP'] then SubItems.Add(RealToStr(DishList[i].RelProts));
    if Value['DishF'] then SubItems.Add(RealToStr(DishList[i].RelFats));
    if Value['DishC'] then SubItems.Add(RealToStr(DishList[i].RelCarbs));
    if Value['DishV'] then SubItems.Add(IntToStr(Round(DishList[i].RelValue)));
    if Value['DishD'] then SubItems.Add(MyTimeToStr(DishList[i].TimeStamp));
  end;
end;

{======================================================================================================================}
procedure TForm1.ListFoodColumnClick(Sender: TObject; Column: TListColumn);
{======================================================================================================================}
begin
  case Column.Index of
    0: FoodBaseSort := smName;
    5: FoodBaseSort := smDate;
  end;

  UpdateFoodTable();
end;

{======================================================================================================================}
procedure TForm1.ListDishColumnClick(Sender: TObject; Column: TListColumn);
{======================================================================================================================}
begin
  case Column.Index of
    0: DishBaseSort := smName;
    6: DishBaseSort := smDate;
  end;

  UpdateDishTable();
end;

{======================================================================================================================}
function TForm1.SelectedRecord(): TCustomRecord;
{======================================================================================================================}
var
  ID: TCompactGUID;
begin
  ID := DiaryView.SelectedRecordID;
  if (ID <> '') then
    Result := LocalSource.FindById(ID) as TCustomRecord
  else
    Result := nil;
end;

{======================================================================================================================}
procedure TForm1.ActionViewLogsExecute(Sender: TObject);
{======================================================================================================================}
begin
  FormLogViewer.ShowModal;
end;

{======================================================================================================================}
procedure TForm1.FormShow(Sender: TObject);
{======================================================================================================================}
begin
  DiaryView.OpenPage(Diary[Trunc(CalendarDiary.Date)], True);
end;

{======================================================================================================================}
procedure TForm1.WarnBadCredentials();
{======================================================================================================================}
begin
  ShowBalloon('Авторизация не удалась [неверный логин/пароль]. Щёлкните это сообщение, чтобы открыть настройки.', bitError, Form1.BalloonAction_ShowInternetSettings);

  StatusBar.Panels[3].Text := 'Неверный логин / пароль';
  Application.ProcessMessages;
end;

end.

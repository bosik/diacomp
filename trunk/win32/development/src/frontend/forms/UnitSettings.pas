unit UnitSettings;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Spin, Grids,
  ValEdit, Math, DiaryRoutines,
  DiaryInterface, { PlaceCenter, BORD }
  TextInterface, { сообщения }
  DiaryView, { панельки }
  SettingsINI, ImgList,
  ShellAPI,

  DiaryCore;

type
  TSettingsPage = (spDefault, spPrivate, spAnalyze, spView, spInterface, spNorms, spInternet);

  TFormSettings = class(TAutosetupForm)
    PageSettings: TPageControl;

    TabPrivate: TTabSheet;
    TabView: TTabSheet;
    TabAnalyze: TTabSheet;
    TabNorms: TTabSheet;
    ListNorms: TValueListEditor;
    LabelNormsDiscription: TLabel;

    GroupShowProcents: TGroupBox;
    RadioProcMass: TRadioButton;
    RadioProcCal: TRadioButton;
    RadioProcNorm: TRadioButton;
    GroupFont: TGroupBox;
    MemoDemo: TMemo;
    GroupInterfaceAdditional: TGroupBox;
    CheckDoubleBuffered: TCheckBox;
    CheckShadow: TCheckBox;
    GroupBS: TGroupBox;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    GroupCalc: TGroupBox;
    Label5: TLabel;
    SpinSettingsMaxDose: TSpinEdit;
    Shape1: TShape;
    Shape2: TShape;
    CheckAnimate: TCheckBox;
    Shape3: TShape;
    PanelButtons: TPanel;
    ButtonSave: TBitBtn;
    ButtonCancel: TBitBtn;
    PanelTabs: TPanel;
    ButtonTabPrivate: TSpeedButton;
    ButtonTabAnalyze: TSpeedButton;
    ButtonTabInterface: TSpeedButton;
    ButtonTabView: TSpeedButton;
    ButtonTabNorms: TSpeedButton;
    TabInterface: TTabSheet;
    GroupSorting: TGroupBox;
    Shape4: TShape;
    Shape6: TShape;
    PanelCheckBalance: TPanel;
    CheckUseNorms: TCheckBox;
    LabelAnPeriod: TLabel;
    EditOptPeriod: TSpinEdit;
    GroupAnGeneral: TGroupBox;
    LabelDaysProcess: TLabel;
    EditAnPeriod: TSpinEdit;
    GroupKoofUpdating: TGroupBox;
    RadioUpdateKOnChange: TRadioButton;
    RadioUpdateKOnStartup: TRadioButton;
    Shape7: TShape;
    GroupColorScheme: TGroupBox;
    ImageColors: TImage;
    ColorDialog: TColorDialog;
    ButtonBackToDefaultColors: TButton;
    CheckColoredEditors: TCheckBox;
    GroupMisc: TGroupBox;
    CheckMealNotify: TCheckBox;
    CheckUpdates: TCheckBox;
    Shape8: TShape;
    GroupBox1: TGroupBox;
    CheckShowPoints: TCheckBox;
    EditTargetBS: TEditNumb;
    EditBS1: TEditNumb;
    EditBS2: TEditNumb;
    EditBS3: TEditNumb;
    TabAccount: TTabSheet;
    ButtonTabInternet: TSpeedButton;
    GroupAccountCommon: TGroupBox;
    LabelLogin: TLabel;
    EditLogin: TEdit;
    LabelPassword: TLabel;
    EditPassword: TEdit;
    Shape9: TShape;
    GroupServer: TGroupBox;
    LabelServer: TLabel;
    EditServerURL: TEdit;
    CheckAutoSync: TCheckBox;
    FontDialog: TFontDialog;
    ButtonFont: TButton;
    CheckCarbsInfo: TCheckBox;
    Shape11: TShape;
    GroupPostPeriods: TGroupBox;
    LabelPPTime: TLabel;
    EditPPTime: TSpinEdit;
    EditSPTime: TSpinEdit;
    LabelSPTime: TLabel;
    LabelAdSpeed: TLabel;
    EditAdSpeed: TSpinEdit;
    EditComp: TSpinEdit;
    LabelCompression: TLabel;
    GroupBox2: TGroupBox;
    Shape5: TShape;
    ButtonRegister: TButton;
    Label1: TLabel;
    LabelAutosaveInterval: TLabel;
    EditAutosaveInterval: TSpinEdit;

    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckUseNormsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckDoubleBufferedClick(Sender: TObject);
    procedure ButtonTabClick(Sender: TObject);
    procedure ImageColorsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonBackToDefaultColorsClick(Sender: TObject);
    procedure ButtonFontClick(Sender: TObject);
    procedure ButtonRegisterClick(Sender: TObject);
    procedure RadioProcNormClick(Sender: TObject);
  protected
    procedure Designer; override;
  private
    procedure UpdateTabButton;
    procedure OpenPage(Page: TSettingsPage);
    procedure ShowSettings;
    function CheckSettings: boolean;
    procedure ReadSettings;
    procedure DrawColors;
    procedure UpdateBackButtonEnabled;
  public
    procedure OpenSettings(Page: TSettingsPage = spDefault);
  end;

var
  FormSettings: TFormSettings;
  RecHeight: integer;
const
  {KC_COUNT = 8;
  KC_VALUES: array[0..KC_COUNT-1] of byte = (2,3,4,6,8,12,24,48);
  KC_DISCR: array[0..KC_COUNT-1] of string = (
    '12 часов',
    '8 часов',
    '6 часов',
    '4 часа',
    '3 часа',
    '2 часа',
    '1 час',
    '30 минут'); }
  {KC_HINT: array[0..KC_COUNT-1] of string = (
    'Позволяет быстро определить общий уровень коэффициентов, но имеет очень низкую точность. Начните с этого режима.',
    'Низкая точность. Перейдите на этот режим через несколько дней после начала ведения дневника.',
    'Удовлетворительная точность. Перейдите на этот режим примерно через неделю ведения дневника.',
    'Средняя точность',
    'Хорошая точность',
    'Достаточная точность',
    'Высокая точность при сохранении устойчивости к скачкам. Рекомендуется для ежедневного использования.',
    'Сверхвысокая точность, нестабильность к скачкам. Не рекомендуется использовать этот режим.');
     }
  LEFT_SPACE = 8;
  TOP_SPACE  = 3;

implementation

uses MainUnit;

//uses MainUnit;

{$R *.dfm}

var
  Temp_StdBlood: integer;
  Temp_StdIns: integer;
  Temp_StdMeal: integer;
  Temp_StdNote: integer;
  Temp_StdBloodPP: integer;
  Temp_SelBlood: integer;
  Temp_SelIns: integer;
  Temp_SelMeal: integer;
  Temp_SelNote: integer;
  Temp_SelBloodPP: integer;

{======================================================================================================================}
procedure TFormSettings.UpdateTabButton;
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to ComponentCount-1 do
  if (Components[i] is TSpeedButton) and
     (Components[i].Tag = PageSettings.ActivePageIndex + 1) then
  begin
    TSpeedButton(Components[i]).Down := True;
    Exit;
  end;
end;

{======================================================================================================================}
procedure TFormSettings.OpenPage(Page: TSettingsPage);
{======================================================================================================================}
begin
  case Page of
    spPrivate:    begin PageSettings.ActivePage := TabPrivate;   ButtonTabPrivate.Down := True;   end;
    spAnalyze:    begin PageSettings.ActivePage := TabAnalyze;   ButtonTabAnalyze.Down := True;   end;
    spInterface:  begin PageSettings.ActivePage := TabInterface; ButtonTabInterface.Down := True; end;
    spView:       begin PageSettings.ActivePage := TabView;      ButtonTabView.Down := True;      end;
    spNorms:      begin PageSettings.ActivePage := TabNorms;     ButtonTabNorms.Down := True;     end;
    spInternet:   begin PageSettings.ActivePage := TabAccount;   ButtonTabInternet.Down := True;  end;
    spDefault:    begin PageSettings.ActivePage := TabPrivate;   ButtonTabPrivate.Down := True;   end;
  end;
  // UpdateTabButton;
end;

{======================================================================================================================}
procedure TFormSettings.OpenSettings(Page: TSettingsPage);
{======================================================================================================================}
begin
  OpenPage(Page);
  ShowModal;
end;


{ * * * * * * ОСНОВНЫЕ СОБЫТИЯ ФОРМЫ * * * * * * }

{======================================================================================================================}
procedure TFormSettings.FormCreate(Sender: TObject);
{======================================================================================================================}
const
  BUTTON_WIDTH  = 90;
  BUTTON_HEIGHT = 68;
var
  i,j: integer;
begin
  { инициализация }
  {for i := 0 to KC_COUNT-1 do
    ComboKC.Items.Add(IntToStr(KC_VALUES[i])); }

  for i := 0 to PageSettings.PageCount-1 do
    PageSettings.Pages[i].TabVisible := false;

  MemoDemo.Text := 'Печенье овсяное (25)';

  ImageColors.Width := GroupColorScheme.Width - 2*ImageColors.Left;

  { кнопки }
  for i := 1 to PageSettings.PageCount do
  for j := 0 to ComponentCount-1 do
  if (Components[j] is TSpeedButton) and
     (Components[j].Tag = i) then
  with TSpeedButton(Components[j]) do
  begin
    Left := BORD;
    Top := i*BORD + (i-1)*BUTTON_HEIGHT;
    Width := BUTTON_WIDTH;
    Height := BUTTON_HEIGHT;
    break;
  end;
  PanelTabs.Width := BUTTON_WIDTH + 2*BORD;
  Height := Min(
    7 * (BUTTON_HEIGHT+BORD) + PanelButtons.Height + 2*BORD,
    Round(1.2 * Width)
  );
end;

{======================================================================================================================}
procedure TFormSettings.FormShow(Sender: TObject);
{======================================================================================================================}
begin
  ShowSettings;
  PlaceCenter(FormSettings);
end;

{======================================================================================================================}
procedure TFormSettings.ButtonSaveClick(Sender: TObject);
{======================================================================================================================}
begin
  if CheckSettings then
  begin
    ReadSettings;
    Hide; // чтобы создать у пользователя иллюзию скорости :Р

    // TODO: обновлять только то, что изменилось?

    Form1.ApplyInterfaceSettings;
    Form1.UpdateDayInfo;      // плохо, долго
    Form1.UpdateKoofs;        // плохо, долго
    Form1.UpdateCombos;       // плохо, долго

    SaveSettings;

    Close; // нужно, так как окно - модальное
  end;
end;


{ * * * * * ОТОБРАЖЕНИЕ / СОХРАНЕНИЕ НАСТРОЕК * * * * * }

{======================================================================================================================}
procedure TFormSettings.ShowSettings;
{======================================================================================================================}

  function Limit(const X, Max: integer): integer;
  begin
    if X>Max then Result := Max else Result := X;
  end;

begin
  // Личные
  EditTargetBS.Text := RealToStrZero(Value['TargetBS']);
  SpinSettingsMaxDose.Text := IntToStr(Value['MaxDose']);

  EditBS1.Text := RealToStr(Value['BS1']);
  EditBS2.Text := RealToStr(Value['BS2']);
  EditBS3.Text := RealToStr(Value['BS3']);

  // Анализ
  EditAdSpeed.Value := Round(Value['Adaptation']*200-100);
  EditAnPeriod.Value := Value['DaysProcess'];
  EditPPTime.Value := Value['PostPrandTime'];
  EditSPTime.Value := Value['ShortPostPrandTime'];
  CheckShowPoints.Checked := Value['ShowPoints'];

  case Value['UpdateKMode'] of
    1: RadioUpdateKOnChange.Checked := True;
    2: RadioUpdateKOnStartup.Checked := True;
    else RadioUpdateKOnChange.Checked := True;  // :)
  end;

  // Вид
  //ComboFontSize.ItemIndex := ComboFontSize.Items.IndexOf(IntToStr(Value_FontSize));
  MemoDemo.Font.Size := Value['FontSize'];
  MemoDemo.Font.Name := Value['FontName'];

  CheckShadow.Checked := Value['ShadowEditors'];
  CheckDoubleBuffered.Checked := Value['DoubleBuffered'];
  CheckAnimate.Checked := Value['AnimatePanels'];
  CheckAnimate.Enabled := Value['DoubleBuffered'];
  CheckColoredEditors.Checked := Value['ColoredEditors'];

  Temp_StdBlood   := Value['Color_StdBlood']  ;
  Temp_StdIns     := Value['Color_StdIns']    ;
  Temp_StdMeal    := Value['Color_StdMeal']   ;
  Temp_StdNote    := Value['Color_StdNote']   ;
  Temp_StdBloodPP := Value['Color_StdBloodPP'];
  Temp_SelBlood   := Value['Color_SelBlood']  ;
  Temp_SelIns     := Value['Color_SelIns']    ;
  Temp_SelMeal    := Value['Color_SelMeal']   ;
  Temp_SelNote    := Value['Color_SelNote']   ;
  Temp_SelBloodPP := Value['Color_SelBloodPP'];

  UpdateBackButtonEnabled;
  DrawColors;

  // Интерфейс
  //CheckOptimize.Checked := Value['OptimizeBases'];
  EditOptPeriod.Value := Value['AnUsingPeriod'];
  CheckMealNotify.Checked := Value['MealNotifies'];
  CheckUpdates.Checked := Value['CheckUpdates'];
  CheckCarbsInfo.Checked := Value['CarbsInfo'];

  // Нормы
  CheckUseNorms.Checked := Value['Balance'];
  ListNorms.Strings.Clear;
  ListNorms.InsertRow('Белки',IntToStr(Value['NormProts']),True);
  ListNorms.InsertRow('Жиры',IntToStr(Value['NormFats']),True);
  ListNorms.InsertRow('Углеводы',IntToStr(Value['NormCarbs']),True);

  case Value['ProcShow'] of
    1: RadioProcMass.Checked := true;
    2: RadioProcCal.Checked := true;
    3: RadioProcNorm.Checked := true;
    else RadioProcMass.Checked := true;
  end;

  // Интернет
  CheckAutoSync.Checked := Value['AutoSync'];
  EditAutosaveInterval.Value := Value['AutosaveInterval'];
  EditLogin.Text := Value['Login'];
  EditPassword.Text := Value['Password'];
  EditServerURL.Text := Value['ServerURL'];
end;

{======================================================================================================================}
function TFormSettings.CheckSettings: boolean;
{======================================================================================================================}

  procedure CheckDecSeparator(Edit: TEdit);
  begin
    Edit.Text := CheckDot(Edit.Text);
  end;

  {function IsParent(Parent, Child: TControl): boolean;
  var
    Temp: TControl;
  begin
    Temp := Child;
    while (Temp <> nil) do
    begin
      if (Temp = Parent) then
      begin
        Result := True;
        Exit;
      end else
        Temp := Temp.Parent;
    end;
    Result := False;
  end;

  function FindParentPage(Component: TControl): TTabSheet;
  var
    i: integer;
  begin
    for i := 0 to PageSettings.PageCount - 1 do
    //if PageSettings.Pages[i].FindComponent(ComponentName) <> nil then
    if IsParent(PageSettings.Pages[i], Component) then
    begin
      Result := PageSettings.Pages[i];
      Exit;
    end;
    Result := nil;
  end;   }

  procedure OpenTabWithComponent(Component: TControl);
  {var
    Tab: TTabSheet; }
  begin
    {Tab := FindParentPage(Component);
    if (Tab <> nil) then
    begin
      PageSettings.ActivePage := Tab;
      UpdateTabButton;
    end else
    if ADVANCED_MODE then
      ErrorMessage('Предупреждение: закладка, содержащая компонент "' + Component.Name + '", не найдена');    }
    Component.Show;
    UpdateTabButton;
  end;

  procedure ErrorOnEdit(Edit: TEdit; const Msg: string);
  begin
    OpenTabWithComponent(Edit);
    Edit.SetFocus;
    Edit.SelectAll;
    ErrorMessage(Msg);
  end;

var
  bs1,bs2,bs3: extended;
  TempInt: integer;
  i: integer;
begin
  CheckDecSeparator(EditTargetBS);
  CheckDecSeparator(EditBS1);
  CheckDecSeparator(EditBS2);
  CheckDecSeparator(EditBS3);

  Result := False;
  
  if not TryStrToFloat(CheckDot(EditTargetBS.Text),bs1) then
  begin
    //PageSettings.ActivePage := TabPrivate;
    ErrorOnEdit(EditTargetBS, MESSAGE_ERROR_INPUT_REAL);
    Exit;
  end;
  if not TryStrToFloat(CheckDot(EditBS1.Text),bs1) then
  begin
    //PageSettings.ActivePage := TabPrivate;
    ErrorOnEdit(EditBS1, MESSAGE_ERROR_INPUT_REAL);
    Exit;
  end;
  if not TryStrToFloat(CheckDot(EditBS2.Text),bs2) then
  begin
    //PageSettings.ActivePage := TabPrivate;
    ErrorOnEdit(EditBS2, MESSAGE_ERROR_INPUT_REAL);
    Exit;
  end;
  if not TryStrToFloat(CheckDot(EditBS3.Text),bs3) then
  begin
    //PageSettings.ActivePage := TabPrivate;
    ErrorOnEdit(EditBS3, MESSAGE_ERROR_INPUT_REAL);
    Exit;
  end;
  if (bs1 >= bs2) then
  begin
    //PageSettings.ActivePage := TabPrivate;
    ErrorOnEdit(EditBS1, 'Неверное значение: минимальный СК до еды должен быть меньше максимального СК до еды.');
    Exit;
  end;
  if (bs2 >= bs3) then
  begin
    //PageSettings.ActivePage := TabPrivate;
    ErrorOnEdit(EditBS2, 'Неверное значение: максимальный СК до еды должен быть меньше максимального СК после еды.');
    Exit;
  end;

  if (CheckUseNorms.Checked) then
  begin
    for i := 1 to 3 do
    if (not TryStrToInt(ListNorms.Cells[1,i],TempInt)) or (TempInt <= 0) then
    begin
      PageSettings.ActivePage := TabNorms;
      ListNorms.SetFocus;
      ListNorms.Row := i;
      ErrorMessage(MESSAGE_ERROR_INPUT_INT_POSITIVE);
      Exit;
    end
  end;

  Result := True;
end;

{======================================================================================================================}
procedure TFormSettings.ReadSettings;
{======================================================================================================================}
begin
  // Личные
  Value['MaxDose'] := SpinSettingsMaxDose.Value;
  Value['TargetBS'] := StrToFloat(CheckDot(EditTargetBS.Text));
  Value['BS1'] := StrToFloat(CheckDot(EditBS1.Text));
  Value['BS2'] := StrToFloat(CheckDot(EditBS2.Text));
  Value['BS3'] := StrToFloat(CheckDot(EditBS3.Text));

  // Анализ
  Value['Adaptation'] := (EditAdSpeed.Value+100)/200;
  Value['DaysProcess'] := EditAnPeriod.Value;
  Value['PostPrandTime'] := EditPPTime.Value;
  Value['ShortPostPrandTime'] := EditSPTime.Value;
  Value['ShowPoints'] := CheckShowPoints.Checked;

  if RadioUpdateKOnChange.Checked then Value['UpdateKMode'] := 1 else
  if RadioUpdateKOnStartup.Checked then Value['UpdateKMode'] := 2 else
    Value['UpdateKMode'] := Value.Rec['UpdateKMode'].Default;

  // Вид
  Value['FontSize'] := MemoDemo.Font.Size;
  Value['FontName'] := MemoDemo.Font.Name;

  Value['DoubleBuffered'] := CheckDoubleBuffered.Checked;
  Value['ShadowEditors'] := CheckShadow.Checked;
  Value['AnimatePanels'] := CheckAnimate.Checked;
  Value['ColoredEditors'] := CheckColoredEditors.Checked;

  Value['Color_StdBlood']   := Temp_StdBlood  ;
  Value['Color_StdIns']     := Temp_StdIns    ;
  Value['Color_StdMeal']    := Temp_StdMeal   ;
  Value['Color_StdNote']    := Temp_StdNote   ;
  Value['Color_StdBloodPP'] := Temp_StdBloodPP;
  Value['Color_SelBlood']   := Temp_SelBlood  ;
  Value['Color_SelIns']     := Temp_SelIns    ;
  Value['Color_SelMeal']    := Temp_SelMeal   ;
  Value['Color_SelNote']    := Temp_SelNote   ;
  Value['Color_SelBloodPP'] := Temp_SelBloodPP;

  // Интерфейс
  //Value['OptimizeBases'] := CheckOptimize.Checked;
  Value['AnUsingPeriod'] := EditOptPeriod.Value;
  Value['MealNotifies'] := CheckMealNotify.Checked;
  Value['CheckUpdates'] := CheckUpdates.Checked;
  Value['CarbsInfo'] := CheckCarbsInfo.Checked;

  // Нормы
  Value['Balance'] := CheckUseNorms.Checked;
  if Value['Balance'] then
  begin
    Value['NormProts'] := StrToInt(ListNorms.Cells[1,1]);
    Value['NormFats'] := StrToInt(ListNorms.Cells[1,2]);
    Value['NormCarbs'] := StrToInt(ListNorms.Cells[1,3]);
  end;

  if RadioProcMass.Checked then Value['ProcShow'] := 1 else
  if RadioProcCal.Checked then Value['ProcShow'] := 2 else
  if RadioProcNorm.Checked then Value['ProcShow'] := 3 else
    Value['ProcShow'] := Value.Rec['ProcShow'].Default;

  // Интернет
  Value['AutoSync'] := CheckAutoSync.Checked;
  Value['AutosaveInterval'] := EditAutosaveInterval.Value;
  Value['Login'] := EditLogin.Text;
  Value['Password'] := EditPassword.Text;

  if ((EditServerURL.Text <> '') and (EditServerURL.Text[Length(EditServerURL.Text)] <> '/')) then
    EditServerURL.Text := EditServerURL.Text + '/';
  Value['ServerURL'] := EditServerURL.Text;

  {*}WebClient.Username := Value['Login'];
  {*}WebClient.Password := Value['Password'];
  {*}WebClient.Server := Value['ServerURL'];
end;


{ * * * * * * * * ДЕТАЛИ ИНТЕРФЕЙСА * * * * * * * * }

{======================================================================================================================}
procedure TFormSettings.CheckUseNormsClick(Sender: TObject);
{======================================================================================================================}
begin
  if (not CheckUseNorms.Checked)and(RadioProcNorm.Checked) then
    RadioProcMass.Checked := true;

  ListNorms.Font.Color := FONT_COLOR[CheckUseNorms.Checked];
  ListNorms.Enabled := CheckUseNorms.Checked;
  //RadioProcNorm.Enabled := CheckUseNorms.Checked;
end;

{======================================================================================================================}
procedure TFormSettings.RadioProcNormClick(Sender: TObject);
{======================================================================================================================}
begin
  CheckUseNorms.Checked := True;
end;

{======================================================================================================================}
procedure TFormSettings.CheckDoubleBufferedClick(Sender: TObject);
{======================================================================================================================}
begin
  CheckAnimate.Enabled := CheckDoubleBuffered.Checked;
  CheckAnimate.Checked := CheckAnimate.Checked and CheckDoubleBuffered.Checked;
end;

{======================================================================================================================}
procedure TFormSettings.ButtonTabClick(Sender: TObject);
{======================================================================================================================}
begin
  PageSettings.ActivePageIndex := TWinControl(Sender).Tag - 1;
end;

{======================================================================================================================}
procedure TFormSettings.DrawColors;
{======================================================================================================================}
var
  TimeFont, RecsFont: TFont;
  R: TPanelRect;
  W, L: integer;

  procedure ShowPanel(Color: TColor; FS: TFontStyles; const Time, Text: string);
  begin
    RecsFont.Style := FS;
    DrawPanelExt(
      ImageColors.Canvas, Time, FmtArray(Text), L, R.Rect.Bottom-1, W,
      LEFT_SPACE, TOP_SPACE, Color, TimeFont, RecsFont, -1, clYellow, R);
  end;

begin
  ImageColors.Canvas.Brush.Color := Form1.DiaryView.Colors.Background;//GroupColorScheme.Color;
  ImageColors.Canvas.FillRect(ImageColors.ClientRect);

  W := (ImageColors.Width div 2) - LEFT_SPACE;

  TimeFont := TFont.Create;
  RecsFont := TFont.Create; 
  TimeFont.Charset := RUSSIAN_CHARSET;
  RecsFont.Charset := RUSSIAN_CHARSET;

  TimeFont.Style := [fsBold, fsUnderline];

  L := Form1.DiaryView.Border;
  R.Rect.Bottom := LEFT_SPACE + 1; // init
  ShowPanel(Temp_StdBlood,   [fsBold], '09:00', '5,0');
  ShowPanel(Temp_StdIns,     [],       '09:10', '[-6]');
  ShowPanel(Temp_StdMeal,    [],       '09:30', 'Каша перловая (230)');
  ShowPanel(Temp_StdBloodPP, [fsBold], '11:00', '6,2');
  ShowPanel(Temp_StdNote,    [],       '12:00', 'Заметка');
  ImageColors.Canvas.Brush.Color := Form1.DiaryView.Colors.Background;
  ImageColors.Canvas.FillRect(Rect(W-10,0, W+50, ImageColors.Height));

  L := W + Form1.DiaryView.Border;
  R.Rect.Bottom := LEFT_SPACE + 1; // init
  ShowPanel(Temp_SelBlood,   [fsBold], '09:00', '5,0');
  ShowPanel(Temp_SelIns,     [],       '09:10', '[-6]');
  ShowPanel(Temp_SelMeal,    [],       '09:30', 'Каша перловая (230)');
  ShowPanel(Temp_SelBloodPP, [fsBold], '11:00', '6,2');
  ShowPanel(Temp_SelNote,    [],       '12:00', 'Заметка');
  ImageColors.Canvas.Brush.Color := Form1.DiaryView.Colors.Background;
  ImageColors.Canvas.FillRect(Rect(L+W-10,0, ImageColors.Width, ImageColors.Height));

  RecHeight :=  R.Rect.Bottom - R.Rect.Top;
  TimeFont.Free;
  RecsFont.Free;
end;

procedure TFormSettings.ImageColorsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure Dialog(var Color: integer);
  begin
    ColorDialog.Color := Color;
    if ColorDialog.Execute then
    begin
      Color := ColorDialog.Color;
      UpdateBackButtonEnabled;
      DrawColors;
    end;
  end;

begin
  if X < (ImageColors.Width div 2) then
    case (Y - LEFT_SPACE) div RecHeight of
      0: Dialog(Temp_StdBlood);
      1: Dialog(Temp_StdIns);
      2: Dialog(Temp_StdMeal);
      3: Dialog(Temp_StdBloodPP);
      4: Dialog(Temp_StdNote);
    end
  else
    case (Y - LEFT_SPACE) div RecHeight of
      0: Dialog(Temp_SelBlood);
      1: Dialog(Temp_SelIns);
      2: Dialog(Temp_SelMeal);
      3: Dialog(Temp_SelBloodPP);
      4: Dialog(Temp_SelNote);
    end
end;

procedure TFormSettings.ButtonBackToDefaultColorsClick(Sender: TObject);
begin
  if MessageDlg('Вы уверены, что хотите вернуть стандартную схему? '+
    'Все текущие изменения будут утеряны.',
    mtConfirmation, [mbYes,mbNo],0) <> mrYes then exit;

  Temp_StdBlood   := Value.Rec['Color_StdBlood'].Default  ;
  Temp_StdIns     := Value.Rec['Color_StdIns'].Default    ;
  Temp_StdMeal    := Value.Rec['Color_StdMeal'].Default   ;
  Temp_StdNote    := Value.Rec['Color_StdNote'].Default   ;
  Temp_StdBloodPP := Value.Rec['Color_StdBloodPP'].Default;
  Temp_SelBlood   := Value.Rec['Color_SelBlood'].Default  ;
  Temp_SelIns     := Value.Rec['Color_SelIns'].Default    ;
  Temp_SelMeal    := Value.Rec['Color_SelMeal'].Default   ;
  Temp_SelNote    := Value.Rec['Color_SelNote'].Default   ;
  Temp_SelBloodPP := Value.Rec['Color_SelBloodPP'].Default;

  ButtonBackToDefaultColors.Enabled := false;
  DrawColors;
end;

procedure TFormSettings.UpdateBackButtonEnabled;
begin
  ButtonBackToDefaultColors.Enabled :=
    (Temp_StdBlood   <> Value.Rec['Color_StdBlood'].Default  ) or
    (Temp_StdIns     <> Value.Rec['Color_StdIns'].Default    ) or
    (Temp_StdMeal    <> Value.Rec['Color_StdMeal'].Default   ) or
    (Temp_StdNote    <> Value.Rec['Color_StdNote'].Default   ) or
    (Temp_StdBloodPP <> Value.Rec['Color_StdBloodPP'].Default) or
    (Temp_SelBlood   <> Value.Rec['Color_SelBlood'].Default  ) or
    (Temp_SelIns     <> Value.Rec['Color_SelIns'].Default    ) or
    (Temp_SelMeal    <> Value.Rec['Color_SelMeal'].Default   ) or
    (Temp_SelNote    <> Value.Rec['Color_SelNote'].Default   ) or
    (Temp_SelBloodPP <> Value.Rec['Color_SelBloodPP'].Default);
end;

procedure TFormSettings.Designer;
begin
  EditTargetBS.Width := MASS_EDIT_WIDTH;
  EditBS1.Width := MASS_EDIT_WIDTH;
  EditBS2.Width := MASS_EDIT_WIDTH;
  EditBS3.Width := MASS_EDIT_WIDTH;

  EditTargetBS.Left := GroupBS.Width - MASS_EDIT_WIDTH - BORD;
  EditBS1.Left := GroupBS.Width - MASS_EDIT_WIDTH - BORD;
  EditBS2.Left := GroupBS.Width - MASS_EDIT_WIDTH - BORD;
  EditBS3.Left := GroupBS.Width - MASS_EDIT_WIDTH - BORD;
end;

procedure TFormSettings.ButtonFontClick(Sender: TObject);
begin
  FontDialog.Font := MemoDemo.Font;
  if FontDialog.Execute then
    MemoDemo.Font := FontDialog.Font;
end;

procedure TFormSettings.ButtonRegisterClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('http://diacomp.net/register/'), '', '', SW_SHOW);
end;

end.



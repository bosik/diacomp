unit ACCombo2;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls, Graphics, Forms;

type
  { Класс, реализующий выпадающее окно автозавершения }
  TDropDownListBox = class(TListBox)
  private
    FBlockMouseOnce: boolean; // костыль
    {
      При введении текста выпадающее окошко меняется и получает сообщение
      WMMouseMove, при этом выделяется элемент, стоящий под курсором.
      Это очень неудобно, поскольку затрудняет работу стрелками
      (в стандартном ComboBox выделения не происходит).
    }
  protected
    procedure WMActivateApp(var M: TMessage); message WM_ACTIVATEAPP;
    procedure WMMouseMove(var M: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var M: TWMLButtonUp); message WM_LBUTTONUP;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateItemIndex;
  public
    constructor Create(AOwner: TComponent); override;
    property BlockMouseOnce: boolean write FBlockMouseOnce;
  end;

  { процедура проверки строки }
  TCheckStringEvent = procedure(Sender: TObject; const EditText, S: String; var AddString: Boolean) of object;

type
  { Комбо-бокс с выпадающем окном автозавершения }
  TACComboBox = class(TCustomComboBox)
  private
    FDropped: Boolean; // True, когда показано выпадающее окно
    FACItems: TStrings;
    FOldFormWndProc, FNewFormWndProc: Pointer; // Используется для подмены оконной процедуры родительской формы
    FParentFormWnd: hWnd; // Handle родительской формы
    FOnCheckString: TCheckStringEvent;

    FUserHint: string;
    FUserHintColor: TColor;
    FShowUserHint: boolean;
    FDefaultTextColor: TColor;
    FUserHintShowed: boolean;

    procedure SetACItems(const Value: TStrings);
    procedure ParentFormWndProc(var Message: TMessage); // Оконная процедура, которой подменяем оконную процедуру родительской формы
  protected
    FDropDown: TDropDownListBox; // Выпадающее окно автозавершения

    procedure CNCommand(var M: TWMCommand); message CN_COMMAND;
    procedure WMMouseWheel(var M: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure CMExit(var M: TCMExit); message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetParent(AParent: TWinControl); override;

    procedure DoEnter; override;
    procedure DoExit; override;

    { заполнение списка автозавершения }
    procedure PrepareACStrings({const }AText: String); virtual;
    procedure CheckStringSimple(Sender: TObject; const EditText, S: String; var AddString: Boolean);
    procedure CheckStringSubString(Sender: TObject; const EditText, S: String; var AddString: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowAC;
    procedure HideAC(ApplySelection: Boolean);
  published
    property ACItems: TStrings read FACItems write SetACItems;
    property OnCheckString: TCheckStringEvent read FOnCheckString write FOnCheckString;

    property UserHint: string read FUserHint write FUserHint;
    property UserHintColor: TColor read FUserHintColor write FUserHintColor default clSilver;
    property ShowUserHint: boolean read FShowUserHint write FShowUserHint default False;

    { скопировано из TComboBox }           
    property AutoComplete default True;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

procedure Register;

implementation

{$R *.dcr}

procedure Register;
begin
  RegisterComponents('Additional', [TACComboBox]);
end;

// Метод используется для подсвечивания в ListBox'е элемента, над которым находится курсор мыши
procedure TDropDownListBox.UpdateItemIndex;
var
  P: TPoint;
  I: Integer;
begin
  if FBlockMouseOnce then
  begin
    FBlockMouseOnce := False;
    Exit;
  end;
  GetCursorPos(P);
  P := ScreenToClient(P);
  I := ItemAtPos(P, True);
  if I<>-1 then ItemIndex := I;
end;

procedure TDropDownListBox.WMLButtonUp(var M: TWMLButtonUp);
begin
  inherited;
  // При выборе элемента из списка автозавершения помещаем его в поле ввода комбо-бокса и закрываем окно автозавершения.
  if ItemIndex<>-1 then TACComboBox(Owner).HideAC(True);
end;

procedure TDropDownListBox.WMMouseMove(var M: TWMMouseMove);
begin
  inherited;
  // При движении мыши над выпадающем окном подсвечиваем элемент, над которым находится курсор мыши.
  UpdateItemIndex;
end;

procedure TDropDownListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := WS_EX_TOOLWINDOW;
  Params.WndParent := GetDesktopWindow;
  Params.Style := WS_CHILD or WS_BORDER or WS_CLIPSIBLINGS or WS_OVERLAPPED or WS_VSCROLL or LBS_NOINTEGRALHEIGHT;
end;

procedure TDropDownListBox.WMActivateApp(var M: TMessage);
begin
  inherited;
  // При переключении на другое приложение (Alt+Tab, etc.) прячем окно автозавершения
  TACComboBox(Owner).HideAC(False);
end;

constructor TDropDownListBox.Create(AOwner: TComponent);
begin
  inherited;
  FBlockMouseOnce := False;
end;

{ TACEdit }

procedure TACComboBox.CheckStringSimple(Sender: TObject; const EditText,
  S: String; var AddString: Boolean);
begin
  AddString := AnsiUpperCase(copy(S, 1, Length(EditText))) = AnsiUpperCase(EditText);
end;

procedure TACComboBox.CheckStringSubString(Sender: TObject; const EditText,
  S: String; var AddString: Boolean);
begin
  AddString := pos(AnsiUpperCase(EditText), AnsiUpperCase(S))<>0;
end;

procedure TACComboBox.CMExit(var M: TCMExit);
begin
  inherited;
  // при потере фокуса комбо-боксом прячем список автозавершения
  HideAC(False);
end;

procedure TACComboBox.CNCommand(var M: TWMCommand);
begin
  if M.NotifyCode = CBN_DROPDOWN then
  // если пользователь раскрывает комбо-бокс, прячем наш список автозавершения
  begin
    HideAC(False);
  end else
  {if M.NotifyCode = CBN_CLOSEUP then
  begin

  end else  }
  if M.NotifyCode = CBN_EDITCHANGE then
    ShowAC // при вводе текста в поле комбо-бокса показываем список автозавершения
  else
    inherited;
end;

constructor TACComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACItems := TStringList.Create;
  FDropped := False;
  FParentFormWnd := 0;

  FUserHintShowed := False;
  FUserHint := '';
  FUserHintColor := clSilver;
  FShowUserHint := False; 

  // в Design-Mode список автозавершения нам не нужен, поэтому создаем его только в Run-Time
  if not (csDesigning in ComponentState) then
    FDropDown := TDropDownListBox.Create(Self);
end;

destructor TACComboBox.Destroy;
begin
  if not (csDesigning in ComponentState) then
    FDropDown.Free;
  FACItems.Free;
  inherited Destroy;
end;

procedure TACComboBox.DoEnter;
begin
  inherited;
  if FShowUserHint and FUserHintShowed then
  begin
    FUserHintShowed := False;
    Text := '';
    Font.Color := FDefaultTextColor;
  end;
end;

procedure TACComboBox.DoExit;
begin
  inherited;
  if FShowUserHint and (Trim(Text) = '') then
  begin
    FDefaultTextColor := Font.Color;
    Font.Color := FUserHintColor;
    FUserHintShowed := True;
    Text := FUserHint;
  end;
end;

procedure TACComboBox.HideAC(ApplySelection: Boolean);
var
  i: Integer;
begin
  ShowWindow(FDropDown.Handle, SW_HIDE); // прячем список автозавершения

  // если ApplySelection=True, то помещаем в поле редактирования комбо-бокса
  // выбранный пользователем элемент из списка автозавершения
  if ApplySelection then
  begin
    I := FDropDown.ItemIndex;
    if I<>-1 then
    begin
      Text := FDropDown.Items[I];
      SelectAll;
    end;
    CloseUp;
  end;
  FDropped := False;
end;

procedure TACComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  M: TWMKeyDown;
begin
  // если список автозавершения не раскрыт, то нажатия клавиш обрабатываем
  // обычным для комбо-бокса образом
  if not FDropped then
  begin
    inherited KeyDown(Key, Shift);
    exit;
  end;

  case Key of
    VK_ESCAPE: // по нажатию Esc прячем список автозавершения
      HideAC(False);

    // нажатия стрелок вверх/вниз, PgUp/PgDn перадаем окну автозавершения, чтобы перемещаться по списку вариантов
    VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR:
      begin
        FillChar(M, SizeOf(M), 0);
        M.Msg := WM_KEYDOWN;
        M.CharCode := Key;
        SendMessage(FDropDown.Handle, TMessage(M).Msg, TMessage(M).WParam, TMessage(M).LParam);
        FillChar(M, SizeOf(M), 0);
        M.Msg := WM_KEYUP;
        M.CharCode := Key;
        SendMessage(FDropDown.Handle, TMessage(M).Msg, TMessage(M).WParam, TMessage(M).LParam);

        // скрываем от системы факт нажатия стрелки/PgUp/PgDn, иначе, кроме перемещения по списку
        // автозавершения, будут выполняться действия для комбо-бокса по умолчанию, т.е.
        // в поле ввода комбобокса будут показываться строки из свойства Items
        Key := 0;
      end;

    // по нажатию Enter закрываем окно автозавершения и помещаем в поле редактирования
    // комбо-бокса выбранный элемент из списка автозавершения
    VK_RETURN:
      HideAC(True);
    else
      inherited KeyDown(Key, Shift);
  end; // case
end;

procedure TACComboBox.KeyPress(var Key: Char);
begin
  // Если окно автозавершения показано, возвращаем 0, иначе при нажатии клавиш Esc/Enter
  // будет издаваться звуковой сигнал (beep)
{  if FDropped then
  begin
  end; }

  { (вообще-то и без этого не пищит) }
  if (Ord(Key) in [VK_RETURN, VK_ESCAPE]) then
    Key := #0 else

  // нажатие клавиши BackSpace (в отличие от буквенно-цифровых клавиш и клавиши Del)
  // не генерит сообщения CBN_EDITCHANGE, и как следствие, обновление списка автозавершения
  // не происходит, поэтому приходится обрабатывать VK_BACK отдельно
  if (Ord(Key) = VK_BACK)and((Text <> '')or(not FDropped)) then
  begin
    inherited KeyPress(Key);
    ShowAC;
  end else
    inherited KeyPress(Key);
end;

// Этой процедурой подменяем оконную процедуру родительской формы (эта форма не обязательно
// является непосредственным Parent'ом комбо-бокса)
procedure TACComboBox.ParentFormWndProc(var Message: TMessage);

  procedure Default;
  begin
    with Message do
      Result := CallWindowProc(FOldFormWndProc, FParentFormWnd, Msg, wParam, lParam);
  end;

begin
  case Message.Msg of // при изменении положения родительской формы прячем окно автозавершения
    {WM_WINDOWPOSCHANGING, }WM_WINDOWPOSCHANGED: HideAC(False);
  end; // case
  Default;
end;

// этот метод заполняет список вариантов автозавершения. В данной реализации исходные данные
// берутся из списка значений, заранее определенных пользователем (свойство ACItems),
// но в принципе исходные данные могут браться из любого источника.
// Для этого нужно перекрыть (override) данный метод в наследнике класса TACComboBox,
// и заполнить FDropDown.Items любыми требуемыми значениями.
// В параметре AText передается введенный в поле редактирования комбобокса текст.
procedure TACComboBox.PrepareACStrings({const }AText: String);
var
  I: Integer;
  Starts, Contents: boolean;
  SecondList: TStrings;  
begin
  SecondList := TStringList.Create;

  //FDropDown.Items.BeginUpdate;
  FDropDown.Items.Clear;
  for i := 0 to FACItems.Count-1 do
  begin
    AText := Trim(AText);
    CheckStringSimple(Self, AText, FACItems[i], Starts);
    CheckStringSubString(Self, AText, FACItems[i], Contents);

    if Starts then
      FDropDown.Items.Add(FACItems[i]) else
    if Contents then
      SecondList.Add(FACItems[i]);
  end;

  FDropDown.Items.AddStrings(SecondList);
  //FDropDown.Items.EndUpdate;
  SecondList.Free;
end;

procedure TACComboBox.SetACItems(const Value: TStrings);
begin
  FACItems.Assign(Value);
end;

procedure TACComboBox.SetParent(AParent: TWinControl);
var
  Frm: TCustomForm;
begin
  // Если вдруг (что маловероятно :)) компонент переносится с одной формы на другую,
  // возвращаем форме ее "родную" оконную процедуру
  if not (csDesigning in ComponentState) and (FParentFormWnd<>0) then
    SetWindowLong(FParentFormWnd, GWL_WNDPROC, Integer(FOldFormWndProc));

  inherited SetParent(AParent);

  // Подменяем оконную процедуру родительской формы. Делаем это только в Run-Time,
  // т.к. в Design-Time список автозавершения не создается
  if not (csDesigning in ComponentState) then
  begin
    Frm := GetParentForm(Self);

    if Assigned(Frm) then
    begin
      FParentFormWnd := Frm.Handle;
      FNewFormWndProc := MakeObjectInstance(ParentFormWndProc);
      FOldFormWndProc := Pointer(GetWindowLong(FParentFormWnd, GWL_WNDPROC));
      SetWindowLong(FParentFormWnd, GWL_WNDPROC, Integer(FNewFormWndProc));
    end;
  end;
end;

procedure TACComboBox.ShowAC;
var
  P: TPoint; Cnt: Integer;
begin
  FDropDown.BlockMouseOnce := True;
  PrepareACStrings(Text); // заполняем список автозаверения вариантами, соответствующими введенному тексту
  Cnt := FDropDown.Items.Count;

  // задаем высоту окна автозавершения таким образом, чтобы в нем помещалось не более DropDownCount строк;
  // если вариантов заверешения более DropDownCount, будет показана вертикальная полоса прокрутки
  if Cnt>DropDownCount then Cnt := DropDownCount;
  FDropped := True;
  SendMessage(Handle, CB_SHOWDROPDOWN, 0, 0); // прячем "родное" выпадающее окно комбобокса

  // показываем окно автозавершения под комбобоксом
  if ClientToScreen(Point(1, Height-1)).Y + Cnt*FDropDown.ItemHeight+2 < Screen.Height then
  begin
    P.X := 0;
    P.Y := Height-1;
    P := ClientToScreen(P);
    SetWindowPos(
      FDropDown.Handle, HWND_TOPMOST,
      P.X, P.Y, Width{-GetSystemMetrics(SM_CXVSCROLL)-2},
      Cnt*FDropDown.ItemHeight+2,
      SWP_SHOWWINDOW);
  end else
  begin
    P.X := 0;
    P.Y := -Cnt*FDropDown.ItemHeight - 2;
    P := ClientToScreen(P);
    SetWindowPos(
      FDropDown.Handle, HWND_TOPMOST,
      P.X, P.Y, Width{-GetSystemMetrics(SM_CXVSCROLL)-2},
      Cnt*FDropDown.ItemHeight+2,
      SWP_SHOWWINDOW);
  end;
  {
  В оригинальном ComboBox выпадающее окно имеет полную ширину,
  поэтому вычет GetSystemMetrics(SM_CXVSCROLL) закомментирован
  }

  if (FDropDown.ItemIndex = -1)and(FDropDown.Items.Count>0) then
    FDropDown.ItemIndex := 0;
end;

// т.к. фокус остается в комбобоксе, сообщения от колеса мыши выпадающему окну не приходят,
// поэтому передаем их принудительно
procedure TACComboBox.WMMouseWheel(var M: TWMMouseWheel);
begin
  FDropDown.BlockMouseOnce := True;
  TMessage(M).Result := SendMessage(FDropDown.Handle, TMessage(M).Msg, TMessage(M).WParam, TMessage(M).LParam);
  FDropDown.UpdateItemIndex;     
end;

end.

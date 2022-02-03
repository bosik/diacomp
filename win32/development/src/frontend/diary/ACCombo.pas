unit ACCombo;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls, Math, Graphics, Forms,
  DiaryRoutines;

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
    procedure MyDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    constructor Create(AOwner: TComponent); override;
    property BlockMouseOnce: boolean read FBlockMouseOnce write FBlockMouseOnce;
  end;

  { процедура проверки строки }
  TCheckStringEvent = procedure(Sender: TObject; const EditText, S: String; var AddString: Boolean) of object;

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
    FManualDraw: boolean;

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

    function GetShowedItem(Index: integer): string;
    function GetShowedIndex(Index: integer): integer;

    { заполнение списка автозавершения }
    procedure PrepareACStrings({const} AText: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowAC;
    procedure HideAC(ApplySelection: Boolean);

    {!!!} procedure Debug(var R: TRect);
    
    property ShowedItems[Index: integer]: string read GetShowedItem;
    property ShowedIndex[Index: integer]: integer read GetShowedIndex;
  published
    property ACItems: TStrings read FACItems write SetACItems;
    property OnCheckString: TCheckStringEvent read FOnCheckString write FOnCheckString;
    property UserHint: string read FUserHint write FUserHint;
    property UserHintColor: TColor read FUserHintColor write FUserHintColor default clSilver;
    property ShowUserHint: boolean read FShowUserHint write FShowUserHint default False;
    property ManualDraw: boolean read FManualDraw write FManualDraw default False;

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

const
  RUSSIAN = ['А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ё', 'Ж', 'З', 'И',
             'Й', 'К', 'Л', 'М', 'Н', 'О', 'П', 'Р', 'С', 'Т',
             'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы', 'Ь',
             'Э', 'Ю', 'Я',
             'а', 'б', 'в', 'г', 'д', 'е', 'ё', 'ж', 'з', 'и',
             'й', 'к', 'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т',
             'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'ы', 'ь',
             'э', 'ю', 'я'];
  GOOD = RUSSIAN + ['a'..'z', 'A'..'Z', '0'..'9'];

{$R *.dcr}

procedure Register;
begin
  RegisterComponents('Компенсация', [TACComboBox]);
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
  if ItemIndex <> -1 then TACComboBox(Owner).HideAC(True);
end;

procedure TDropDownListBox.WMMouseMove(var M: TWMMouseMove);
begin
  inherited;
  // При движении мыши над выпадающем окном подсвечиваем элемент, над которым находится курсор мыши.
  UpdateItemIndex;
end;

constructor TDropDownListBox.Create(AOwner: TComponent);
begin
  inherited;
  Parent := TWinControl(AOwner);   
  DoubleBuffered := True;

  //if TACComboBox(Owner).ManualDraw then
  if True then
  begin
    Style := lbOwnerDrawFixed;
    OnDrawItem := MyDrawItem;
  end else
    OnDrawItem := nil;

  TabStop := False;
  FBlockMouseOnce := False;
end;

procedure TDropDownListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := WS_EX_TOOLWINDOW;

  //Params.WndParent := GetDesktopWindow;
  Params.WndParent := TWinControl(TACComboBox(Owner).Owner).Handle;


  {Params.Style :=
    WS_CHILD
    or WS_BORDER
    or WS_CLIPSIBLINGS
    or WS_OVERLAPPED
    or WS_VSCROLL
    or LBS_NOINTEGRALHEIGHT
    //or LBS_OWNERDRAWFIXED       /// !!!
     ;  }

  Params.Style := Params.Style or WS_BORDER {or LBS_OWNERDRAWFIXED};
end;    

procedure TDropDownListBox.WMActivateApp(var M: TMessage);
begin
  inherited;
  // При переключении на другое приложение (Alt+Tab, etc.) прячем окно автозавершения
  TACComboBox(Owner).HideAC(False);
end;

// ручная отрисовка
procedure TDropDownListBox.MyDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if Assigned(TACComboBox(Owner).OnDrawItem) then
   TACComboBox(Owner).OnDrawItem(Control, Index, rect, State);
end;

{ TACComboBox }

function CheckStringSimple(const EditText, S: String): boolean;
begin
  Result := AnsiUpperCase(copy(S, 1, Length(EditText))) = AnsiUpperCase(EditText);
end;

function CheckStringSubString(const EditText, S: String): boolean;
begin
  Result := pos(AnsiUpperCase(EditText), AnsiUpperCase(S))<>0;
end;

function CheckStringWord({const} EditText, S: String): boolean;
var
  i, j: integer;
  Ignore: boolean;
begin
  if (EditText = '') then
  begin
    Result := True;
    Exit;
  end;

  EditText := AnsiUpperCase(EditText);
  S := AnsiUpperCase(S);
  Ignore := False;

  j := 0;
  for i := 1 to Length(S) do
  if (S[i] in GOOD) then
  begin
    if (not Ignore) then
    if (S[i] = EditText[j + 1]) then
    begin
      inc(j);
      if (j = Length(EditText)) then
      begin
        Result := True;
        Exit;
      end;
    end else
    begin
      j := 0;
      Ignore := True;
    end;
  end else
  begin
    j := 0;
    Ignore := False;
  end;

  Result := False;
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

  FManualDraw := False;

  FUserHintShowed := False;
  FUserHint := '';
  FUserHintColor := clSilver;
  FShowUserHint := False;

  // в Design-Mode список автозавершения нам не нужен, поэтому создаем его только в Run-Time
  if not (csDesigning in ComponentState) then
  begin
    FDropDown := TDropDownListBox.Create(Self);
    ShowWindow(FDropDown.Handle, SW_HIDE)
  end;
end;

procedure TACComboBox.Debug(var R: TRect);
begin
  {ShowWindow(FDropDown.Handle, SW_SHOW);
  MoveWindow(
    FDropDown.Handle,
    100,100, 150, 100, True);
  SetForegroundWindow(FDropDown.Handle);  }

  GetWindowRect(FDropDown.Handle, R);

  //ShowWindow(FDropDown.Handle, SW_HIDE);

  //MoveWindow(FDropDown.Handle, 300, 300, 100, 100, True);
  //ShowWindow(FDropDown.Handle, SW_SHOW);
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
  if (FShowUserHint and FUserHintShowed) then
  begin
    FUserHintShowed := False;
    Text := '';
    Font.Color := FDefaultTextColor;
  end;
end;

procedure TACComboBox.DoExit;
begin
  inherited;
  if (FShowUserHint and (Trim(Text) = '')) then
  begin
    FDefaultTextColor := Font.Color;
    Font.Color := FUserHintColor;
    FUserHintShowed := True;
    Text := FUserHint;
  end;
end;

function TACComboBox.GetShowedIndex(Index: integer): integer;
begin
  Result := Integer(FDropDown.Items.Objects[Index]);
end;

function TACComboBox.GetShowedItem(Index: integer): string;
begin
  Result := FDropDown.Items[Index];
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

    // Home/End - клвиши для поля ввода, а не для списка
    VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR
    //, VK_HOME, VK_END
    :
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
      begin
        inherited KeyDown(Key, Shift);
        HideAC(True);
      end;
    else
      inherited KeyDown(Key, Shift);
  end;
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
    WM_WINDOWPOSCHANGING,
    WM_WINDOWPOSCHANGED:
    begin
      if (FDropped) then
        HideAC(False);
    end;
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

  function MatchesTerms(Query, S: String): boolean;
  var
    i: integer;
    Start: integer;
  begin
    Start := 0;
    Query := AnsiUpperCase(Query);
    S := AnsiUpperCase(S);

    for i := 1 to Length(Query) do
    begin
      if (Query[i] in GOOD) then
      begin
        if (Start = 0) then
        begin
          Start := i;
        end;
      end else
      begin
        if (Start <> 0) then
        begin
          if (pos(Copy(Query, Start, i - Start), S) = 0) then
          begin
            Result := False;
            Exit;
          end;

          Start := 0;
        end;
      end;
    end;

    if (Start <> 0) then
    begin
      i := Length(Query) + 1;
      if (pos(Copy(Query, Start, i - Start), S) = 0) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
  end;

  function Search(Query: String): TStrings;
  var
    i: integer;
  begin
    Result := TStringList.Create();
    for i := 0 to FACItems.Count - 1 do
    begin
      if (MatchesTerms(Query, FACItems[i])) then
        Result.AddObject(FACItems[i], TObject(i));
    end;
  end;

var
  FoundItems: TStrings;
begin
  AText := Trim(AText);

  FoundItems := Search(AText);
  try
    if (FoundItems.Count = 0) then
    begin
      FoundItems.Free();
      FoundItems := Search(SwitchLanguage(AText));
    end;

    FDropDown.Items.Assign(FoundItems);
  finally
    FoundItems.Free();
  end;
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
      FNewFormWndProc := Classes.MakeObjectInstance(ParentFormWndProc);
      FOldFormWndProc := Pointer(GetWindowLong(FParentFormWnd, GWL_WNDPROC));
      SetWindowLong(FParentFormWnd, GWL_WNDPROC, Integer(FNewFormWndProc));
    end;
  end;
end;

procedure TACComboBox.ShowAC;
var
  P: TPoint;
  DisplayCount, DisplayHeight: Integer;
  Control: TWinControl;
begin
  FDropDown.BlockMouseOnce := True;
  PrepareACStrings(Text);

  SendMessage(Handle, CB_SHOWDROPDOWN, 0, 0); // прячем "родное" выпадающее окно комбобокса
  Control := TWinControl(Owner);

  DisplayCount := Min(FDropDown.Items.Count, DropDownCount);
  DisplayHeight := DisplayCount * FDropDown.ItemHeight;

  //if ClientToScreen(Point(1, Height-1)).Y + DisplayCount*FDropDown.ItemHeight+2 < Screen.Height then
  if ClientToScreen(Point(1, Height - 1)).Y + DisplayHeight + 2 < Control.ClientOrigin.Y + Control.ClientHeight then
    P := Point(Left, Top + Height - 1)
  else
    P := Point(Left, Top - DisplayHeight - 1);

  P := ClientToScreen(P);
  P.X := P.X - Control.ClientOrigin.X - Left;
  P.Y := P.Y - Control.ClientOrigin.Y - Top;

  MoveWindow(
    FDropDown.Handle,
    P.X, P.Y, Width, DisplayHeight + 2, True);
  ShowWindow(FDropDown.Handle, SW_SHOW);

  FDropped := True;

  if (FDropDown.ItemIndex = -1) and (FDropDown.Items.Count > 0) then
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

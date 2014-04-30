unit DiaryView;

{$R+ O+}

interface

uses
  Graphics, {colors, etc.}
  Classes, {Rect()}
  Windows, {TRect}
  SysUtils, {IntToStr}
  Controls, {TMouseButton, Date, TColorSettings}
  DiaryRoutines,
  TextInterface, {finger names}
  BusinessObjects,
  DiaryRecords,
  DiaryPage,

  StdCtrls,
  Math, { TStatProgress }
  Menus; {PopupMenu}

type
  TClickPlace = (cpNoWhere, cpPanel, cpTime, cpRec);

  TClickInfo = record
    Place: TClickPlace;
    Index: integer;
    Line: integer;
  end;

  TColorSettings = class(TPersistent)
  private
    FControl          : TControl;
    FBackground       : TColor;

    FPanel_StdBlood   : TColor;
    FPanel_StdIns     : TColor;
    FPanel_StdMeal    : TColor;
    FPanel_StdNote    : TColor;
    FPanel_StdBloodPP : TColor;

    FPanel_SelBlood   : TColor;
    FPanel_SelIns     : TColor;
    FPanel_SelMeal    : TColor;
    FPanel_SelNote    : TColor;
    FPanel_SelBloodPP : TColor;

    FOnChange: TNotifyEvent;

    procedure SetColors(Index: Integer; Value: TColor);
  protected
    procedure Change; virtual;
    property Control: TControl read FControl;
  public
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(Control: TControl); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Background:        TColor index 0 read FBackground        write SetColors default $FFFFFF;

    property Panel_SelBlood:    TColor index -1 read FPanel_SelBlood    write SetColors default $FFD0D0;
    property Panel_SelIns:      TColor index -2 read FPanel_SelIns      write SetColors default $F0F0F0;
    property Panel_SelMeal:     TColor index -3 read FPanel_SelMeal     write SetColors default $A1FFFF;
    property Panel_SelNote:     TColor index -4 read FPanel_SelNote     write SetColors default $B0FFC0;
    property Panel_SelBloodPP:  TColor index -5 read FPanel_SelBloodPP  write SetColors default $F5C9F1;

    property Panel_StdBlood:    TColor index 1 read FPanel_StdBlood    write SetColors default $F8E4D8;
    property Panel_StdIns:      TColor index 2 read FPanel_StdIns      write SetColors default $FFFFFF;
    property Panel_StdMeal:     TColor index 3 read FPanel_StdMeal     write SetColors default $E1FFFF;
    property Panel_StdNote:     TColor index 4 read FPanel_StdNote     write SetColors default $E4F8D8;
    property Panel_StdBloodPP:  TColor index 5 read FPanel_StdBloodPP  write SetColors default $E6E6FF;
  end;

  TPanelRect = record
    Rect: TRect;
    TimeRect: TRect;
    Recs: array of TRect;    
  end;

  { события }
  TClickEvent = procedure(Sender: TObject; Index: integer; Place: TClickPlace) of object;
  TFoodShowEvent = procedure(Sender: TObject; Index,Line: integer; var Text: string) of object;
  TEventRecordChanged = procedure(Sender: TObject; EventType: TPageEventType; Page: TDiaryPage; RecClass: TClassCustomRecord; RecInstance: TCustomRecord) of object;

  TDiaryView = class(TGraphicControl)
  private
    { текущая страница }
    FBitMap: Graphics.TBitMap;
    FCurrentPage: TDiaryPage;

    PanelRects: array of TPanelRect;
    FSelLine: integer;
    FSelPanel: integer;

    { интерфейс }
    FLastClickedTime: cardinal;
    FLastX: integer;
    FLastY: integer;
    FMouseDowned: boolean;
    FTextEmptyPage: string;

    { оформление }
    FFont: TFont;
    FBorder: integer;
    FBorderRight: integer;
    FBorderTimeTop: integer;
    FBorderTimeLeft: integer;
    FColors: TColorSettings;

    { привязанные компоненты }
    FPopupBlood: TPopupMenu;
    FPopupIns: TPopupMenu;
    FPopupMeal: TPopupMenu;
    FPopupFood: TPopupMenu;
    FPopupNote: TPopupMenu;

    { события }
    FOnPage: TNotifyEvent;           // при открытии/смене страницы
    FOnPaint: TNotifyEvent;          // перерисовка
    FOnChange: TEventRecordChanged;  // при изменении страницы

    FOnClickBlood: TClickEvent;
    FOnClickIns: TClickEvent;
    FOnClickMeal: TClickEvent;
    FOnClickNote: TClickEvent;

    FOnDoubleClickBlood: TClickEvent;
    FOnDoubleClickIns: TClickEvent;
    FOnDoubleClickMeal: TClickEvent;
    FOnDoubleClickNote: TClickEvent;

    FOnFoodShow: TFoodShowEvent;

    function CalcHeight: integer;
    procedure DrawIntoBuffer;
    procedure GetMousePlace(x,y: integer; var ClickInfo: TClickInfo);
    procedure ProcessClick(X,Y: integer; Button: TMouseButton);

    { Get/Set }
    procedure SetBorder(const Value: integer);
    procedure SetBorderRight(const Value: integer);
    procedure SetBorderTimeTop(const Value: integer);
    procedure SetBorderTimeLeft(const Value: integer);
    procedure SetColors(Value: TColorSettings);
    procedure SetFont(Value: TFont);
    procedure SetPopupBlood(Value: TPopupMenu);
    procedure SetPopupIns(Value: TPopupMenu);
    procedure SetPopupMeal(Value: TPopupMenu);
    procedure SetPopupFood(Value: TPopupMenu);
    procedure SetPopupNote(Value: TPopupMenu);
  protected
    { обработчики событий }
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;

    { реакция }
    procedure HandleBaseChanged(EventType: TPageEventType; Page: TDiaryPage;
      RecClass: TClassCustomRecord; RecInstance: TCustomRecord); deprecated;
    procedure HandlePageChanged(EventType: TPageEventType; Page: TDiaryPage; // let it [Page] be
      RecClass: TClassCustomRecord; RecInstance: TCustomRecord);

    { инициаторы }
    procedure MyPage;
    procedure MyColorsChanged(Sender: TObject);
    function GetFoodInfo(Index, Line: integer): string;

    procedure ClickBlood(Index: integer; Place: TClickPlace; IsDouble: boolean);
    procedure ClickIns(Index: integer; Place: TClickPlace; IsDouble: boolean);
    procedure ClickMeal(Index: integer; Place: TClickPlace; IsDouble: boolean);
    procedure ClickNote(Index: integer; Place: TClickPlace; IsDouble: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenPage(Page: TDiaryPage; ForceRepaint: boolean = False);
    procedure Paint; override;
    procedure DrawCurrentPage;
    procedure DeselectAll;

    { текущая страница }

    function GetCurrentDate: TDate;

    function IsFoodSelected: boolean;

    function SelectedRecord: TCustomRecord;
    function SelectedFood: TFoodMassed;

    property SelectedRecordIndex: integer read FSelPanel write FSelPanel;
    property SelectedLine: integer read FSelLine;
    property CurrentPage: TDiaryPage read FCurrentPage;
    property CurrentDate: TDate read GetCurrentDate; // TODO: дописать write, использующий OpenPage?
  published
    property Align;
    property Anchors;
    property Border: integer read FBorder write SetBorder default 8;
    property BorderRight: integer read FBorderRight write SetBorderRight default 8;
    property BorderTimeTop: integer read FBorderTimeTop write SetBorderTimeTop default 3;
    property BorderTimeLeft: integer read FBorderTimeLeft write SetBorderTimeLeft default 10;
    property Colors: TColorSettings read FColors write SetColors;
    property Font: TFont read FFont write SetFont;
    property PopupBlood: TPopupMenu read FPopupBlood write SetPopupBlood;
    property PopupIns: TPopupMenu read FPopupIns write SetPopupIns;
    property PopupMeal: TPopupMenu read FPopupMeal write SetPopupMeal;
    property PopupFood: TPopupMenu read FPopupFood write SetPopupFood;
    property PopupNote: TPopupMenu read FPopupNote write SetPopupNote;
    property Visible;

    property TextEmptyPage: string read FTextEmptyPage write FTextEmptyPage;

    { события }
    property OnClickBlood:  TClickEvent read FOnClickBlood  write FOnClickBlood;
    property OnClickIns:    TClickEvent read FOnClickIns    write FOnClickIns;
    property OnClickMeal:   TClickEvent read FOnClickMeal   write FOnClickMeal;
    property OnClickNote:   TClickEvent read FOnClickNote   write FOnClickNote;
    property OnChange: TEventRecordChanged read FOnChange write FOnChange;
    property OnDoubleClickBlood:  TClickEvent read FOnDoubleClickBlood  write FOnDoubleClickBlood;
    property OnDoubleClickIns:    TClickEvent read FOnDoubleClickIns    write FOnDoubleClickIns;
    property OnDoubleClickMeal:   TClickEvent read FOnDoubleClickMeal   write FOnDoubleClickMeal;
    property OnDoubleClickNote:   TClickEvent read FOnDoubleClickNote   write FOnDoubleClickNote;
    property OnFoodShow: TFoodShowEvent read FOnFoodShow write FOnFoodShow;
    property OnMouseDown;
    property OnPage: TNotifyEvent read FOnPage write FOnPage;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TStatProgress = class(TGraphicControl)
  private
    FColorFill: TColor;
    FColorOver: TColor;
    FLighter: real;
    FMax: integer;
    FProgress: integer;
    FVolume: integer;
    FRotation: integer;
    procedure SetMax(Value: integer);
    procedure SetProgress(Value: integer);
    procedure SetColorFill(Value: TColor);
    procedure SetColorOver(Value: TColor);
    procedure SetRotation(Value: integer);
    procedure SetVolume(Value: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ColorFill: TColor read FColorFill write SetColorFill;
    property ColorOver: TColor read FColorOver write SetColorOver;
    property Max: integer read FMax write SetMax default 100;
    property Progress: integer read FProgress write SetProgress default 0;
    property Rotation: integer read FRotation write SetRotation default 50;
    property Volume: integer read FVolume write SetVolume default 100;
    property OnDblClick;
  end;

const
  DEFAULT_COLOR_WARNING = $F0F0FF;

type
  TCheckValueEvent = procedure (Sender: TObject; const Value: string; var Accept: boolean) of Object;

  TEditNumb = class(TEdit)
  private
    FDecimal: char;
    FAcceptColor: TColor;
    FAcceptNegative: boolean;
    FBlockKeys: boolean;
    FWarningColor: TColor;
    FWarningShow: boolean;
    FCheckFunction: TCheckValueEvent;
  protected
    procedure Change; override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AcceptColor: TColor read FAcceptColor write FAcceptColor default clWindow;
    property AcceptNegative: boolean read FAcceptNegative write FAcceptNegative default True;
    property BlockKeys: boolean read FBlockKeys write FBlockKeys default True;
    property Decimal: char read FDecimal write FDecimal;
    property WarningColor: TColor read FWarningColor write FWarningColor default DEFAULT_COLOR_WARNING;
    property WarningShow: boolean read FWarningShow write FWarningShow default False;

    property OnCheckValue: TCheckValueEvent read FCheckFunction write FCheckFunction;
  end;

  procedure Register;

  { рисует основу панели }
  //procedure DrawPanelBack(Canvas: TCanvas; R: TRect; Color: TColor);

  { рисует текст, записывает в ATextRect прямоугольник вывода }
  procedure DrawText(
    Canvas: TCanvas;
    x,y: integer;
    const Text: string;
    var ATextRect: TRect;
    StdColor: TColor;
    Selected: boolean = False;
    SelColor: TColor = 0;
    FrameColor: TColor = 0); overload;

  { рисует текст }
  {...}

  { определяет цвет панели }
  function PanelColor(ARec: TCustomRecord; Selected: boolean;
    Colors: TColorSettings): TColor;

  { рисует панель }
  procedure DrawPanelExt(
    { на чём рисовать }
    Canvas: TCanvas;
    { что рисовать }
    const Time: string;
    const Recs: TStringArray;
    { где рисовать }
    Left, Top, Width: integer;
    LeftSpacing: integer;
    TopSpacing: integer;
    { как рисовать }
    BackColor: TColor;
    TimeFont: TFont;
    RecsFont: TFont;
    SelItem: integer;
    SelColor: TColor;
    { дополнительно }
    out Rects: TPanelRect
  ); 

  procedure DrawPanel(
    Canvas: TCanvas;
    const Time: string;
    const Recs: TStringArray;
    Left, Top, Width: integer;
    LeftSpacing: integer;
    TopSpacing: integer;
    BackColor: TColor;
    TimeFont: TFont;
    RecsFont: TFont;
    SelItem: integer;
    SelColor: TColor
    { без Rects }
  );

const
  DoubleClickTime  = 350;
  DoubleClickShift = 10;

  EmptyTextBorder = 30;

  FONTSIZE_EMPTYPAGE = 30;

  PANEL_LIGHT_BORD = clWhite;
  PANEL_DARK_BORD  = clGray;
  SELECTED_LINE    = clYellow;
  LINES_COLOR      = $FBF1EB;//$F8E4D8;

implementation

{$R *.dcr}

{==============================================================================}
procedure Register;
{==============================================================================}
begin
  RegisterComponents('Компенсация', [TDiaryView]);
  RegisterComponents('Компенсация', [TStatProgress]);
  RegisterComponents('Компенсация', [TEditNumb]);

  //RegisterComponents('Компенсация', [TFoodBase]);
  //RegisterComponents('Компенсация', [TDishBase]);

  //RegisterComponents('Компенсация', [TCompensationServer]);
  //RegisterComponents('Компенсация', [TCompensationClient]);
  //RegisterComponents('Компенсация', [TWebShell]);
end;

{==============================================================================}
procedure DrawText(
{==============================================================================}
  Canvas: TCanvas;
  x,y: integer;
  const Text: string;
  var ATextRect: TRect;
  StdColor: TColor;
  Selected: boolean = False; 
  SelColor: TColor = 0;
  FrameColor: TColor = 0); overload;
begin
  with Canvas do
  begin
    { Font должен быть настроен }
    Brush.Style := bsSolid;
    if Selected then
    begin
      Brush.Color := SelColor;
      Pen.Color := FrameColor;
      Pen.Style := psSolid;
      Rectangle(
        x-3,
        y-1,
        x + TextWidth(Text) + 2,
        y + TextHeight(Text) + 1
      );
    end else
    begin
      Brush.Color := StdColor;
    end;

    //Brush.Color := clLime;
    TextOut(x,y,text);

    ATextRect := Rect(
      x,
      y,
      x + TextWidth(text),
      y + TextHeight(text)
    );
  end;
end;

{==============================================================================}
function PanelColor(ARec: TCustomRecord; Selected: boolean;
  Colors: TColorSettings): TColor;
{==============================================================================}
begin
  if (ARec.RecType = TBloodRecord) and
     (TBloodRecord(ARec).PostPrand) then
  if Selected then
    Result := Colors.Panel_SelBloodPP
  else
    Result := Colors.Panel_StdBloodPP

  else

  if Selected then
  begin
    if (ARec.RecType = TBloodRecord) then Result := Colors.Panel_SelBlood else
    if (ARec.RecType = TInsRecord)   then Result := Colors.Panel_SelIns else
    if (ARec.RecType = TMealRecord)  then Result := Colors.Panel_SelMeal else
    if (ARec.RecType = TNoteRecord)  then Result := Colors.Panel_SelNote else
      Result := clRed; {!}
  end else
  begin
    if (ARec.RecType = TBloodRecord) then Result := Colors.Panel_StdBlood else
    if (ARec.RecType = TInsRecord)   then Result := Colors.Panel_StdIns else
    if (ARec.RecType = TMealRecord)  then Result := Colors.Panel_StdMeal else
    if (ARec.RecType = TNoteRecord)  then Result := Colors.Panel_StdNote else
      Result := clRed; {!}
  end;
end;

{==============================================================================}
procedure DrawPanelExt(
{==============================================================================}
  { на чём рисовать }
  Canvas: TCanvas;
  { что рисовать }
  const Time: string;
  const Recs: TStringArray;
  { где рисовать }
  Left, Top, Width: integer;
  LeftSpacing: integer;
  TopSpacing: integer;
  { как рисовать }
  BackColor: TColor;
  TimeFont: TFont;
  RecsFont: TFont;
  SelItem: integer;
  SelColor: TColor;
  { дополнительно }
  out Rects: TPanelRect
);

  procedure DrawPanelBack(Canvas: TCanvas; R: TRect; Color: TColor);
  begin
    with Canvas do
    begin
      Brush.Color := Color;
      Brush.Style := bsSolid;
      Pen.Color := PANEL_LIGHT_BORD;
      Pen.Width := 1;
      Rectangle(R);

      Pen.Color := PANEL_DARK_BORD;
      MoveTo(R.Left + 1,R.Bottom-2);
      LineTo(R.Left + 1,R.Top + 1);
      LineTo(R.Right-1,R.Top + 1);
      MoveTo(R.Left,R.Bottom);
      LineTo(R.Right,R.Bottom);
      LineTo(R.Right,R.Top-1);
    end;
  end;

var
  RecsCount: integer;
  i,cnt: integer;
  StandartTH: integer;
  RecsLeftBord: integer;
begin
  RecsCount := length(Recs);

  with Canvas do
  begin
    { определить высоту панели }
    Font.Assign(TimeFont);
    StandartTH := TextHeight('0123456789') + 2;
    RecsLeftBord := 2*LeftSpacing + TextWidth(Time);

    cnt := max(1, RecsCount);
    Rects.Rect := Rect(
      Left, Top, Left + Width,
      Top + 2*TopSpacing + StandartTH*cnt
    );

    { нарисовать основу }
    DrawPanelBack(Canvas, Rects.Rect, BackColor);

    { нарисовать время }
    Brush.Color := BackColor;
    // Font.Assign(TimeFont); - уже сделано
    TextOut(
      Rects.Rect.Left + LeftSpacing,
      Rects.Rect.Top + TopSpacing + 1,
      Time
    );

    {Rects.TimeRect.Left := Rects.Rect.Left + LeftSpacing;
    Rects.TimeRect.Top := Rects.Rect.Top + TopSpacing + 1;
    Rects.TimeRect.Right := Rects.Rect.Left + LeftSpacing + TextWidth(Time);
    Rects.TimeRect.Bottom := Rects.Rect.Top + TopSpacing + 1 + TextHeight(Time);
     }

    { }

    { нарисовать записи }
    Font.Assign(RecsFont);
    SetLength(Rects.Recs, length(Recs));

    for i := 0 to high(Recs) do
      DrawText(
        Canvas,
        Rects.Rect.Left + RecsLeftBord,
        Rects.Rect.Top + TopSpacing + StandartTH*i + 1,
        recs[i],
        Rects.Recs[i],
        BackColor,
        SelItem = i,
        SelColor,
        PANEL_DARK_BORD
      );
  end;
end;

procedure DrawPanel(
  Canvas: TCanvas;
  const Time: string;
  const Recs: TStringArray;
  Left, Top, Width: integer;
  LeftSpacing: integer;
  TopSpacing: integer;
  BackColor: TColor;
  TimeFont: TFont;
  RecsFont: TFont;
  SelItem: integer;
  SelColor: TColor
  { без Rects }
);
var
  R: TPanelRect;
begin
  DrawPanelExt(
    Canvas, Time, Recs, Left, Top, Width,
    LeftSpacing, TopSpacing, BackColor,
    TimeFont, RecsFont, SelItem, SelColor,
    R
  );
end;

{ TColorSettings }

procedure TColorSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TColorSettings then
    with TColorSettings(Dest) do
    begin
      FBackground        := Self.FBackground;

      FPanel_StdBlood    := Self.FPanel_StdBlood;
      FPanel_StdIns      := Self.FPanel_StdIns;
      FPanel_StdMeal     := Self.FPanel_StdMeal;
      FPanel_StdNote     := Self.FPanel_StdNote;
      FPanel_StdBloodPP  := Self.FPanel_StdBloodPP;
      FPanel_SelBlood    := Self.FPanel_SelBlood;
      FPanel_SelIns      := Self.FPanel_SelIns;
      FPanel_SelMeal     := Self.FPanel_SelMeal;
      FPanel_SelNote     := Self.FPanel_SelNote;
      FPanel_SelBloodPP  := Self.FPanel_SelBloodPP;

      {...}
      Change;
    end
  else inherited AssignTo(Dest);
end;

procedure TColorSettings.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TColorSettings.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;

  FBackground       :=  $FFFFFF;

  FPanel_StdBlood   :=  $F8E4D8;
  FPanel_StdIns     :=  $FFFFFF;
  FPanel_StdMeal    :=  $E1FFFF;
  FPanel_StdNote    :=  $E4F8D8;
  FPanel_StdBloodPP :=  $E6E6FF;

  FPanel_SelBlood   :=  $FFD0D0;
  FPanel_SelIns     :=  $F0F0F0;
  FPanel_SelMeal    :=  $A1FFFF;
  FPanel_SelNote    :=  $B0FFC0;
  FPanel_SelBloodPP :=  $F5C9F1;
end;

procedure TColorSettings.SetColors(Index: Integer; Value: TColor);

  procedure MicroSet(var Field: TColor);
  begin
    if Value <> Field then
    begin
      Field := Value;
      Change;
    end;
  end;

begin
  case Index of
    0:  MicroSet(FBackground);
    
    +1: MicroSet(FPanel_StdBlood);
    +2: MicroSet(FPanel_StdIns);
    +3: MicroSet(FPanel_StdMeal);
    +4: MicroSet(FPanel_StdNote);
    +5: MicroSet(FPanel_StdBloodPP);

    -1: MicroSet(FPanel_SelBlood);
    -2: MicroSet(FPanel_SelIns);
    -3: MicroSet(FPanel_SelMeal);
    -4: MicroSet(FPanel_SelNote);
    -5: MicroSet(FPanel_SelBloodPP);
  end;
end;

{ TDiary }

(*
{==============================================================================}
function TDiaryView.AddBlood(Time: integer; const Value: real; Finger: integer): integer;
{==============================================================================}
begin
  if not CurrentIsEmpty then
    Result := CurrentPage.AddBlood(Time, Value, Finger)
  else
    Result := -1;

  if Result >- 1 then
  begin
    FSelPanel := Result;
    FSelLine := -1;
    RecBloodChange(Result);
    Change;
  end;
end;

{==============================================================================}
function TDiaryView.AddFoodToMeal(Index: integer; Food: TFoodMassed): integer;
{==============================================================================}
begin
  if not CurrentIsEmpty then
    Result := CurrentPage.AddFoodToMeal(Index,Food)
  else
    Result := -1;
  if Result > -1 then
  begin
    FSelPanel := Index;
    FSelLine := Result;
    RecMealChange(Result);
    Change;
  end;
end;

{==============================================================================}
function TDiaryView.AddFoodToMeal(Food: TFoodMassed): integer;
{==============================================================================}
begin
  Result := AddFoodToMeal(FSelPanel,Food);
end;

{==============================================================================}
function TDiaryView.AddIns(Time: integer; const Value: real): integer;
{==============================================================================}
begin
  if not CurrentIsEmpty then
    Result := CurrentPage.AddIns(Time,Value)
  else
    Result := -1;
  if Result>-1 then
  begin
    FSelPanel := Result;
    FSelLine := -1;
    RecInsChange(Result);
    Change;
  end;
end;

{==============================================================================}
function TDiaryView.AddMealBlank(Time: integer; ShortMeal: boolean): integer;
{==============================================================================}
begin
  if not CurrentIsEmpty then
    Result := CurrentPage.AddMealBlank(Time, ShortMeal)
  else
    Result := -1;
  if Result>-1 then
  begin
    FSelPanel := Result;
    FSelLine := -1;
    RecMealChange(Result);
    Change;
  end;
end;

{==============================================================================}
function TDiaryView.AddNote(Time: integer; const Value: string): integer;
{==============================================================================}
begin
  if not CurrentIsEmpty then
    Result := CurrentPage.AddNote(Time,Value)
  else
    Result := -1;
  if Result>-1 then
  begin
    FSelPanel := Result;
    FSelLine := -1;
    Change;
  end;
end;
*)

{==============================================================================}
function TDiaryView.CalcHeight: integer;
{==============================================================================}

  function F(n: integer): integer;
  begin
    if (n <= 0) then
      Result := 1
    else
      Result := n;
  end;

var
  SavedFontSize: integer;
  StandartTH: integer;
  i: integer;
begin
  with FBitMap.Canvas do
  begin
    if (CurrentPage = nil) or
       (CurrentPage.Count = 0) then
    begin
      SavedFontSize := Font.Size;
      Font.Style := [];
      Font.Size := FONTSIZE_EMPTYPAGE;
      Result := TextHeight(FTextEmptyPage) + 2 * EmptyTextBorder;
      Font.Size := SavedFontSize;
    end else
    begin
      Font.Style := [fsBold];
      StandartTH := TextHeight('0123456789')+2;
      Result := (FBorder * 2) + 2;
      for i := 0 to CurrentPage.Count - 1 do
      begin
        if (CurrentPage[i].RecType = TBloodRecord) then Result := Result + 2 * FBorderTimeTop + StandartTH - 1 else
        if (CurrentPage[i].RecType = TInsRecord)   then Result := Result + 2 * FBorderTimeTop + StandartTH - 1 else
        if (CurrentPage[i].RecType = TMealRecord)  then Result := Result + 2 * FBorderTimeTop + StandartTH * F(TMealRecord(CurrentPage[i]).Count) - 1 else
        if (CurrentPage[i].RecType = TNoteRecord)  then Result := Result + 2 * FBorderTimeTop + StandartTH - 1;
      end;
    end;
  end;
end;

{==============================================================================}
procedure TDiaryView.ClickBlood(Index: integer; Place: TClickPlace; IsDouble: boolean);
{==============================================================================}
begin
  if (not IsDouble) and Assigned(FOnClickBlood) then
    FOnClickBlood(Self,Index,Place) else
  if (IsDouble) and Assigned(FOnDoubleClickBlood) then
    FOnDoubleClickBlood(Self,Index,Place);
end;

{==============================================================================}
procedure TDiaryView.ClickIns(Index: integer; Place: TClickPlace; IsDouble: boolean);
{==============================================================================}
begin
  if (not IsDouble) and Assigned(FOnClickIns) then
    FOnClickIns(Self,Index,Place) else
  if (IsDouble) and Assigned(FOnDoubleClickIns) then
    FOnDoubleClickIns(Self,Index,Place);
end;

{==============================================================================}
procedure TDiaryView.ClickMeal(Index: integer; Place: TClickPlace; IsDouble: boolean);
{==============================================================================}
begin
  if (not IsDouble) and Assigned(FOnClickMeal) then
    FOnClickMeal(Self,Index,Place) else
  if (IsDouble) and Assigned(FOnDoubleClickMeal) then
    FOnDoubleClickMeal(Self,Index,Place);
end;

{==============================================================================}
procedure TDiaryView.ClickNote(Index: integer; Place: TClickPlace; IsDouble: boolean);
{==============================================================================}
begin
  if (not IsDouble) and Assigned(FOnClickNote) then
    FOnClickNote(Self,Index,Place) else
  if (IsDouble) and Assigned(FOnDoubleClickNote) then
    FOnDoubleClickNote(Self,Index,Place);
end;

{==============================================================================}
constructor TDiaryView.Create(AOwner: TComponent);
{==============================================================================}
begin
  inherited Create(AOwner);

  { внутренняя часть }

  { компонентно-графическая часть }
  Width := 400;
  Height := 300;
  FBitMap := Graphics.TBitMap.Create;
  FBitMap.Width := Width;
  FBitMap.Height := Height;

  FColors := TColorSettings.Create(Self);
  FColors.OnChange := MyColorsChanged;
  FFont := TFont.Create;

  {
  FColors.FBackground      := $FFFFFF;

  FColors.FPanel_StdBlood   := $F8E4D8;
  FColors.FPanel_StdIns     := $FFFFFF;
  FColors.FPanel_StdMeal    := $E1FFFF;
  FColors.FPanel_StdNote    := $E4F8D8;//$EEFFEE;
  FColors.FPanel_StdBloodPP := $E6E6FF;//$F8E4FA;
  FColors.FPanel_SelBlood   := $FFD0D0;
  FColors.FPanel_SelIns     := $F0F0F0;
  FColors.FPanel_SelMeal    := $A1FFFF;
  FColors.FPanel_SelNote    := $B0FFC0;//$CCFFCC;
  FColors.FPanel_SelBloodPP := $F5C9F1;
  }

  FBorder := 8;
  FBorderRight := 8;
  FBorderTimeTop := 3;
  FBorderTimeLeft := 10;

  FCurrentPage := nil;
  FSelPanel := -1;
  FSelLine := -1;
  FMouseDowned := false;
end;

{==============================================================================}
procedure TDiaryView.DeselectAll;
{==============================================================================}
begin
  FSelPanel := -1;
  FSelLine := -1;
  DrawCurrentPage;
end;

{==============================================================================}
destructor TDiaryView.Destroy;
{==============================================================================}
begin
  FBitMap.Free;
  FColors.Free;
  FFont.Free;
  inherited;
end;

{==============================================================================}
procedure TDiaryView.DblClick;
{==============================================================================}
begin
  inherited;
  // TODO: remove?
end;

{==============================================================================}
procedure TDiaryView.DrawCurrentPage;
{==============================================================================}
var
  NewHeight: integer;
begin
  { определение необходимого места }
  if  (((not (akTop in Anchors))or
      (not (akBottom in Anchors))))and
      (CurrentPage <> nil)
  then
  begin
    NewHeight := CalcHeight;
    if NewHeight <> Height then
      Height := NewHeight;
  end;

  DrawIntoBuffer;
  Canvas.Draw(0, 0, FBitMap);
end;

{==============================================================================}
procedure TDiaryView.DrawIntoBuffer;
{==============================================================================}
var
  w: integer;
  i: integer;
  CurTop: integer;

  { добавление панельки в список }
  function AddPanelRect(const R: TPanelRect): integer;
  begin
    Result := Length(PanelRects);
    SetLength(PanelRects, Result + 1);
    PanelRects[Result] := R;
  end;

  { очистка экрана }
  procedure BackFill;
  begin
    FBitMap.Width := Width;
    FBitMap.Height := Height;
    with FBitMap.Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColors.Background;
      FillRect(Rect(0,0,Width,Height));
                                                                  
      {Brush.Style := bsCross;
      Brush.Color := LINES_COLOR;
      Pen.Color := FColors.Background;
      Rectangle(0,0,Width,Height);}
    end;
  end;

  procedure AddPanel(
    Index: integer;
    Time: integer;
    Recs: TStringArray;
    RecsFontStyle: TFontStyles);
  { рисует панель со временем, добавляет прямоугольник }
  var
    //i,n,cnt: integer;
    //r: TRect;
    Color: TColor;

    TimeFont: TFont;
    RecsFont: TFont;

    NewRect: TPanelRect;
    Sel: integer;
  begin
    with FBitMap.Canvas do
    begin
      Color := PanelColor(CurrentPage[Index], FSelPanel = Index, Colors);

      TimeFont := TFont.Create;
      RecsFont := TFont.Create;

      TimeFont.Assign(FBitmap.Canvas.Font);
      RecsFont.Assign(FBitmap.Canvas.Font);

      TimeFont.Color := clBlack;  {magic!}
      TimeFont.Style := [fsBold{,fsUnderline}]; {magic!}

      RecsFont.Style := RecsFontStyle;
      if FSelPanel = Index then
        Sel := FSelLine
      else
        Sel := -1;

      {!!!}
      //RecsFont.Name := 'Tahoma';
      //RecsFont.Name := 'Futura';
      //RecsFont.Name := 'Frutiger';
      //RecsFont.Name := 'Calibri';
      //RecsFont.Name := 'Candara';
      //RecsFont.Name := 'Bookman Old Style';
      //RecsFont.Name := 'Helvetica';
      //RecsFont.Name := 'Arial';  

      DrawPanelExt(
        FBitMap.Canvas,
        DiaryRoutines.MTimeToStrColon(time),
        Recs,
        Border,
        CurTop,
        w-FBorderRight-Border,
        FBorderTimeLeft,
        FBorderTimeTop,
        Color,
        TimeFont,
        RecsFont,
        Sel,
        SELECTED_LINE,
        NewRect
      );

      TimeFont.Free;     
      RecsFont.Free;

      AddPanelRect(NewRect);

      { ВРЕМЯ }
      (*Brush.Color := Color;
      Font.Color := clBlack;  {magic!}
      Font.Style := [fsBold,fsUnderline]; {magic!}
      DrawText(
        FBitMap.Canvas,
        r.Left+FBorderTimeLeft,
        r.Top+FBorderTimeTop+1,
        TimeToStr(time),
        PanelRects[n].TimeRect,
        Color
      );  *)

      { ЗАПИСИ }
      //Font.Style := RecsFontStyle;
      //SetLength(PanelRects[n].Recs,length(Recs));

      {if (FSelPanel=n) then
      for i := 0 to high(recs) do
      begin
        if FSelLine = i then // не слишком быстро, но лаконично
          Brush.Color := SELECTED_LINE
        else
          Brush.Color := Color;
        DrawText(
          FBitMap.Canvas,
          r.Left+RecsLeftBord,
          r.Top+FBorderTimeTop+StandartTH*i+1,
          recs[i],
          FSelLine=i,
          PanelRects[n].Recs[i]
        );
      end else
      for i := 0 to high(recs) do
      DrawText(
        FBitMap.Canvas,
        r.Left+RecsLeftBord,
        r.Top+FBorderTimeTop+StandartTH*i+1,
        recs[i],
        false,
        PanelRects[n].Recs[i]
      );  }
    end;
    CurTop := CurTop+(NewRect.Rect.Bottom-NewRect.Rect.Top)-1;
  end;

  function CreateRecsArray(Index: integer): TStringArray;
  var
    i: integer;
  begin
    SetLength(Result,TMealRecord(CurrentPage[Index]).Count);
    for i := 0 to high(Result) do
      Result[i] := TMealRecord(CurrentPage[Index]).Food[i].Name + ' (' +
        RealToStr(TMealRecord(CurrentPage[Index]).Food[i].Mass) + ')' +
        GetFoodInfo(Index, i);
  end;

const
  BEST_CHARSET = RUSSIAN_CHARSET;
  //BEST_CHARSET = CHINESEBIG5_CHARSET;
var
  Msg: string;
begin
  if (CurrentPage = nil) or (CurrentPage.Count = 0) then
  with FBitMap.Canvas do
  begin
    {if (csDesigning in ComponentState) then
    begin
      BackFill;
    end else   }

    begin
      {if (FBase = nil) then
        Msg := TEXT_NODATABASE
      else }
        Msg := FTextEmptyPage;

      Font.Assign(FFont);
      Font.Color := clSilver;
      Font.Size := FONTSIZE_EMPTYPAGE;
      Font.Style := [];
      Font.Charset := BEST_CHARSET;

      if (not ((akTop in Anchors) and (akBottom in Anchors))) then
        Height := TextHeight(Msg) + 2 * EmptyTextBorder;

      BackFill;
      TextOut(
        (Width-TextWidth(Msg)) div 2,
        EmptyTextBorder,
        Msg
      );
    end;
    SetLength(PanelRects, 0);
    Exit;
  end;

  { очистка регионов }
  SetLength(PanelRects,0);
  with FBitMap.Canvas do
  begin
    //StandartTH := TextHeight('0123456789')+2;

    { рисование }
    BackFill;

    w := Width;
    FBitmap.Canvas.Font.Assign(FFont);
    Font.Style := [fsBold];
    FBitmap.Canvas.Font.Charset := BEST_CHARSET;

    //RecsLeftBord := 2*FBorderTimeLeft+TextWidth('88:88');
    CurTop := Border;
    Pen.Style := psSolid;

    { заполнение }
    for i := 0 to CurrentPage.Count - 1 do
    begin
      if (CurrentPage[i].RecType = TBloodRecord) then
      begin
          if (TBloodRecord(CurrentPage[i]).Finger > -1) then
            AddPanel(
                   i,
                   CurrentPage[i].Time,
                   FmtArray(RealToStrZero(TBloodRecord(CurrentPage[i]).Value) + '  ' +
                   ShortFingerNames[TBloodRecord(CurrentPage[i]).Finger]),
                   [{fsBold}]
                 )
          else
            AddPanel(
                   i,
                   CurrentPage[i].Time,
                   FmtArray(RealToStrZero(TBloodRecord(CurrentPage[i]).Value)),
                   [{fsBold}]
                 )
      end else

      if (CurrentPage[i].RecType = TInsRecord) then
        AddPanel(
                   i,
                   CurrentPage[i].Time,
                   FmtArray(RealToStr(TInsRecord(CurrentPage[i]).Value)+' ед'),
                   []
                 ) else
      if (CurrentPage[i].RecType = TMealRecord) then
        AddPanel(
                    i,
                    CurrentPage[i].Time,
                    CreateRecsArray(i),
                    []
                  ) else
      if (CurrentPage[i].RecType = TNoteRecord) then
         AddPanel(
                   i,
                   CurrentPage[i].Time,
                   FmtArray(TNoteRecord(CurrentPage[i]).Text),
                   [{fsItalic}]
                 );
    end;

    TextOut(0,0, IntToStr(CurrentPage.Version));

    {if FSelPanel<>-1 then
    begin
      Brush.Style := bsSolid;
      Pen.Style := psClear;
      Brush.Color := clNavy;

      Rectangle(
        PanelRects[FSelPanel].Rect.Left-10,
        PanelRects[FSelPanel].Rect.Top,
        PanelRects[FSelPanel].Rect.Left-1,
        PanelRects[FSelPanel].Rect.Bottom
      );

      Rectangle(
        PanelRects[FSelPanel].Rect.Right+1,
        PanelRects[FSelPanel].Rect.Top,
        PanelRects[FSelPanel].Rect.Right+10,
        PanelRects[FSelPanel].Rect.Bottom
      );

      Pen.Style := psSolid;
      Pen.Width := 4;

      MoveTo(
        PanelRects[FSelPanel].Rect.Left+2,
        PanelRects[FSelPanel].Rect.Bottom
      );

      LineTo(
        PanelRects[FSelPanel].Rect.Right+1,
        PanelRects[FSelPanel].Rect.Bottom
      );

      LineTo(
        PanelRects[FSelPanel].Rect.Right+1,
        PanelRects[FSelPanel].Rect.Top+2
      );
    end;   }
  end;
end;

(*
{==============================================================================}
function TDiaryView.EditFoodMass(Index, Line: integer; const NewMass: real): boolean;
{==============================================================================}
begin
  if (not CurrentIsEmpty) then
    Result := CurrentPage.EditFoodMass(Index,Line,NewMass)
  else
    Result := false;
  if Result then
  begin
    RecMealChange(Index);
    Change;
  end;
end;

{==============================================================================}
function TDiaryView.EditFoodMass(Line: integer; const NewMass: real): boolean;
{==============================================================================}
begin
  Result := EditFoodMass(FSelPanel,Line,NewMass);
end;

{==============================================================================}
function TDiaryView.EditFoodMass(const NewMass: real): boolean;
{==============================================================================}
begin
  Result := EditFoodMass(FSelPanel,FSelLine,NewMass);
end;

{==============================================================================}
function TDiaryView.EditTime(Index, NewTime: integer): integer;
{==============================================================================}
begin
  if (not CurrentIsEmpty) then
    Result := CurrentPage.EditTime(Index,NewTime)
  else
    Result := -1;

  if Result>-1 then
  begin
    if Index=FSelPanel then
      FSelPanel := Result else
    if (Index<FSelPanel)and(Result>FSelPanel) then
      dec(FSelPanel) else
    if (Index>FSelPanel)and(Result<FSelPanel) then
      inc(FSelPanel);

    case CurrentPage[Result].TagType of
      rtBlood: RecBloodChange(Result);
      rtIns: RecInsChange(Result);
      rtMeal: RecMealChange(Result);
    end;
    Change;
    DrawCurrentPage;
  end;
end;

{==============================================================================}
function TDiaryView.EditBloodValue(Index: integer;
  const NewValue: real): boolean;
{==============================================================================}
begin
  if (not CurrentIsEmpty) then
    Result := CurrentPage.EditBloodValue(Index,NewValue)
  else
    Result := false;
  if Result then
  begin
    RecBloodChange(Index);
    Change;
  end;
end;

{==============================================================================}
function TDiaryView.EditBloodValue(const NewValue: real): boolean;
{==============================================================================}
begin
  Result := EditBloodValue(FSelPanel,NewValue);
end;

{==============================================================================}
function TDiaryView.EditInsValue(Index: integer; const NewValue: real): boolean;
{==============================================================================}
begin
  if (not CurrentIsEmpty) then
    Result := CurrentPage.EditInsValue(Index,NewValue)
  else
    Result := false;
  if Result then
  begin
    RecInsChange(Index);
    Change;
  end;
end;

{==============================================================================}
function TDiaryView.EditInsValue(const NewValue: real): boolean;
{==============================================================================}
begin
  Result := EditInsValue(FSelPanel,NewValue);
end;

{==============================================================================}
function TDiaryView.EditNoteValue(Index: integer; const NewValue: string): boolean;
{==============================================================================}
begin
  if (not CurrentIsEmpty) then
    Result := CurrentPage.EditNoteValue(Index,NewValue)
  else
    Result := false;
  if Result then
  begin
    Change;
  end;
end;

{==============================================================================}
function TDiaryView.EditNoteValue(const NewValue: string): boolean;
{==============================================================================}
begin
  Result := EditNoteValue(FSelPanel,NewValue);
end;

{==============================================================================}
function TDiaryView.EditTime(NewTime: integer): integer;
{==============================================================================}
begin
  Result := EditTime(FSelPanel,NewTime);
end;
*)

{==============================================================================}
function TDiaryView.GetFoodInfo(Index, Line: integer): string;
{==============================================================================}
begin
  if Assigned(FOnFoodShow) then
    FOnFoodShow(Self,Index,Line,Result)
  else
    Result := '';
end;
     
{==============================================================================}
procedure TDiaryView.GetMousePlace(x, y: integer; var ClickInfo: TClickInfo);
{==============================================================================}
var
  i,j: integer;
begin
  ClickInfo.Place := cpNoWhere;
  ClickInfo.Index := -1;
  ClickInfo.Line := -1;

  { определяем место курсора }
  for i := 0 to high(PanelRects) do
  if PtInRect(PanelRects[i].Rect,Point(x,y)) then
  begin
    ClickInfo.Index := i;
    ClickInfo.Place := cpPanel;

    if (PtInRect(PanelRects[i].TimeRect,Point(x,y)))then
    begin
      ClickInfo.Place := cpTime;
      exit;
    end else

    for j := 0 to high(PanelRects[i].Recs) do
    if PtInRect(PanelRects[i].Recs[j],Point(x,y)) then
    begin
      ClickInfo.Line := j;
      ClickInfo.Place := cpRec;
      exit;
    end;
  end;
end;

{==============================================================================}
function TDiaryView.IsFoodSelected: boolean;
{==============================================================================}
begin
  Result :=
    (SelectedRecord is TMealRecord)and
    (FSelLine >= 0)and
    (FSelLine < TMealRecord(CurrentPage.Recs[FSelPanel]).Count);
end;

{==============================================================================}
procedure TDiaryView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
{==============================================================================}
begin
  FMouseDowned := (Button = mbLeft);
  ProcessClick(x,y,Button);
  inherited; { после ProcessClick }
end;

{==============================================================================}
procedure TDiaryView.MouseMove(Shift: TShiftState; X, Y: Integer);
{==============================================================================}
var
  ClickInfo: TClickInfo;
  Meal: TMealRecord;
begin
  inherited;

  if (FMouseDowned)and
     (FSelLine > -1)and
     (SelectedRecord <> nil) and
     (SelectedRecord is TMealRecord) then
  begin
    GetMousePlace(90, y, ClickInfo);

    Meal := TMealRecord(SelectedRecord);

    if (ClickInfo.Place = cpRec)and
       //(ClickInfo.Index = FSelPanel)and
       (ClickInfo.Line <> FSelLine)and
       (ClickInfo.Line > -1)and
       (ClickInfo.Line < Meal.Count) then
    begin
      // чтобы края не блокировались
      if (ClickInfo.Index < FSelPanel) then ClickInfo.Line := 0 else
      if (ClickInfo.Index > FSelPanel) then ClickInfo.Line := Meal.Count - 1;

      // поехали
      while (FSelLine < ClickInfo.Line) do
      begin
        Meal.Exchange(FSelLine, FSelLine + 1);
        inc(FSelLine);
      end;

      while (FSelLine > ClickInfo.Line) do
      begin
        Meal.Exchange(FSelLine, FSelLine - 1);
        dec(FSelLine);
      end;

      // рисуем
      DrawCurrentPage;
    end;
  end;
end;

{==============================================================================}
procedure TDiaryView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
{==============================================================================}
begin
  FMouseDowned := false;
  inherited;
end;

{==============================================================================}
procedure TDiaryView.MyColorsChanged(Sender: TObject);
{==============================================================================}
begin
  Paint;
end;

{==============================================================================}
procedure TDiaryView.MyPage;
{==============================================================================}
begin
  if Assigned(FOnPage) then FOnPage(Self);
end;

(*
{==============================================================================}
procedure TDiaryView.OpenPage(Date: TDateTime; ForceRepaint: boolean = False);
{==============================================================================}

  procedure RedrawIt;
  begin
    FSelPanel := -1;
    FSelLine := -1;
    DrawCurrentPage;
    MyPage;
  end;

begin
  if FBase <> nil then

  if (CurrentPage = nil) or (CurrentDate <> Date) then
  begin
    FCurrentPage := FBase[Date];
    RedrawIt;
  end else
  if ForceRepaint then
  begin
    RedrawIt;
  end;
end;
*)

{==============================================================================}
procedure TDiaryView.OpenPage(Page: TDiaryPage; ForceRepaint: boolean = False);
{==============================================================================}

  procedure RedrawIt;
  begin
    FSelPanel := -1;
    FSelLine := -1;
    DrawCurrentPage;
    MyPage;
  end;

begin
  if (FCurrentPage <> Page) then
  begin
    FCurrentPage := Page;
    Page.AddChangeListener(HandlePageChanged);
    RedrawIt;
  end else
  if ForceRepaint then
  begin
    RedrawIt;
  end;
end;

{==============================================================================}
procedure TDiaryView.Paint;
{==============================================================================}
begin
  {if csDesigning in ComponentState then
	begin
    Canvas.Pen.Style := psDash;
	  Canvas.Brush.Style := bsClear;
	  Canvas.Rectangle(0,0,Width,Height);
	end else   }
  if Visible then
  begin
    DrawCurrentPage;
    if Assigned(FOnPaint) then FOnPaint(Self);
  end;
end;

{==============================================================================}
procedure TDiaryView.ProcessClick(X, Y: integer; Button: TMouseButton);
{==============================================================================}
type
  TPopup = (pNothing, pBlood, pIns, pMeal, pFood, pNote);
var
  DoubleClick: boolean;
  Cursor: TPoint;
  ClickInfo: TClickInfo;
  TagType: TClassCustomRecord;
  Popup: TPopup;
begin
  { определим, является ли клик двойным }
  // TODO: отказаться от ручного определения двойного клика
  DoubleClick := 
    ((GetTickCount-FLastClickedTime)<DoubleClickTime)and
    (abs(X-FLastX)+abs(Y-FLastY)<DoubleClickShift)and
    (Button=mbLeft);
  FLastClickedTime := GetTickCount;
  FLastX := x;
  FLastY := y;

  GetMousePlace(x,y,ClickInfo);

  GetCursorPos(Cursor); // для popup'ов
  Popup := pNothing;
  if ClickInfo.Index <> -1 then
    TagType := CurrentPage[ClickInfo.Index].RecType
  else
    TagType := nil;

  case ClickInfo.Place of

    cpPanel:  case Button of
    { предупреждение компилятора можно игнорировать }
                mbLeft:
                begin
                  if (TagType = TBloodRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickBlood(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TInsRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickIns(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TMealRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickMeal(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TNoteRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickNote(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end;
                end;
                mbRight:
                begin
                  if (TagType = TBloodRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickBlood(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pBlood;
                  end else

                  if (TagType = TInsRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickIns(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pIns;
                  end else

                  if (TagType = TMealRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickMeal(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pMeal;
                  end else

                  if (TagType = TNoteRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickNote(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pNote;
                  end;
                end;
              end;
              
    cpTime:   case Button of
                mbLeft:
                begin
                  if (TagType = TBloodRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickBlood(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TInsRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickIns(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TMealRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickMeal(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TNoteRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickNote(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end;
                end;

                mbRight:
                begin
                  if (TagType = TBloodRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickBlood(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pBlood;
                  end else

                  if (TagType = TInsRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickIns(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pIns;
                  end else

                  if (TagType = TMealRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickMeal(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pMeal;
                  end else

                  if (TagType = TNoteRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickNote(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pNote;
                  end;
                end;
              end;

    cpRec:  case Button of
                mbLeft:
                begin
                  if (TagType = TBloodRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickBlood(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TInsRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickIns(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TMealRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := ClickInfo.Line;
                    ClickMeal(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end else

                  if (TagType = TNoteRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickNote(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                  end;
                end;


                mbRight:
                begin
                  if (TagType = TBloodRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickBlood(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pBlood;
                  end else

                  if (TagType = TInsRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickIns(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pIns;
                  end else

                  if (TagType = TMealRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := ClickInfo.Line;
                    ClickMeal(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pFood;
                  end else

                  if (TagType = TNoteRecord) then
                  begin
                    FSelPanel := ClickInfo.Index;
                    FSelLine := -1;
                    ClickNote(ClickInfo.Index,ClickInfo.Place,DoubleClick);
                    Popup := pNote;
                  end;
                end;
              end;

    cpNoWhere:begin
                FSelPanel := -1;
                FSelLine := -1;
              end;
  end;

  DrawCurrentPage;

  case Popup of
    pBlood: if FPopupBlood <> nil then
              FPopupBlood.Popup(Cursor.x,Cursor.y);
    pIns:   if FPopupIns <> nil then
              FPopupIns.Popup(Cursor.x,Cursor.y);
    pMeal:  if FPopupMeal <> nil then
              FPopupMeal.Popup(Cursor.x,Cursor.y);
    pFood:  if FPopupFood <> nil then
              FPopupFood.Popup(Cursor.x,Cursor.y);
    pNote:  if FPopupNote <> nil then
              FPopupNote.Popup(Cursor.x,Cursor.y);
  end;
end;

{==============================================================================}
function TDiaryView.SelectedFood: TFoodMassed;
{==============================================================================}
begin
  //Log('SelectedFood()');

  if IsFoodSelected then
    Result := TMealRecord(CurrentPage.Recs[FSelPanel]).Food[FSelLine]
  else
    //Result := nil;
    raise Exception.Create('SelectedFood: not such record selected');
end;

{==============================================================================}
function TDiaryView.SelectedRecord: TCustomRecord;
{==============================================================================}
begin
  //Log('SelectedRecord()');

  if ((CurrentPage <> nil) and (FSelPanel >= 0) and (FSelPanel < CurrentPage.Count)) then
    Result := CurrentPage.Recs[FSelPanel]
  else
    Result := nil;
end;

{==============================================================================}
procedure TDiaryView.SetBorder(const Value: integer);
{==============================================================================}
begin
  if (Value>=0)and(Value<>FBorder) then
  begin
    FBorder := Value;
    if not(csDesigning in ComponentState) then
      DrawCurrentPage;
  end;
end;

{==============================================================================}
procedure TDiaryView.SetBorderRight(const Value: integer);
{==============================================================================}
begin
  if (Value>=0)and(Value<>FBorderRight) then
  begin
    FBorderRight := Value;
    if not(csDesigning in ComponentState) then
      DrawCurrentPage;
  end;
end;

{==============================================================================}
procedure TDiaryView.SetBorderTimeLeft(const Value: integer);
{==============================================================================}
begin
  if (Value>=0)and(Value<>FBorderTimeLeft) then
  begin
    FBorderTimeLeft := Value;
    if not(csDesigning in ComponentState) then
      DrawCurrentPage;
  end;
end;

{==============================================================================}
procedure TDiaryView.SetBorderTimeTop(const Value: integer);
{==============================================================================}
begin
  if (Value>=0)and(Value<>FBorderTimeTop) then
  begin
    FBorderTimeTop := Value;
    if not(csDesigning in ComponentState) then
      DrawCurrentPage;
  end;
end;

{==============================================================================}
procedure TDiaryView.SetColors(Value: TColorSettings);
{==============================================================================}
begin
  FColors.AssignTo(Value);
end;

{==============================================================================}
procedure TDiaryView.SetPopupBlood(Value: TPopupMenu);
{==============================================================================}
begin
  FPopupBlood := Value;
  if Value <> nil then
  begin
    Value.ParentBiDiModeChanged(Self);
    Value.FreeNotification(Self);
  end;
end;

{==============================================================================}
procedure TDiaryView.SetPopupFood(Value: TPopupMenu);
{==============================================================================}
begin
  FPopupFood := Value;
  if Value <> nil then
  begin
    Value.ParentBiDiModeChanged(Self);
    Value.FreeNotification(Self);
  end;
end;

{==============================================================================}
procedure TDiaryView.SetPopupIns(Value: TPopupMenu);
{==============================================================================}
begin
  FPopupIns := Value;
  if Value <> nil then
  begin
    Value.ParentBiDiModeChanged(Self);
    Value.FreeNotification(Self);
  end;
end;

{==============================================================================}
procedure TDiaryView.SetPopupMeal(Value: TPopupMenu);
{==============================================================================}
begin
  FPopupMeal := Value;
  if Value <> nil then
  begin
    Value.ParentBiDiModeChanged(Self);
    Value.FreeNotification(Self);
  end;
end;

{==============================================================================}
procedure TDiaryView.SetPopupNote(Value: TPopupMenu);
{==============================================================================}
begin
  FPopupNote := Value;
  if Value <> nil then
  begin
    Value.ParentBiDiModeChanged(Self);
    Value.FreeNotification(Self);
  end;
end;

{ TStatProgress }

constructor TStatProgress.Create(AOwner: TComponent);
begin
  inherited;
  Width := 50;
  Height := 15;
  FMax := 100;
  FProgress := 0;
  FVolume := 100;
  FRotation := 50;
  FColorFill := clGreen;
  FColorOver := clRed;
end;

procedure TStatProgress.Paint;

  function F(x: real): real;
  begin
    Result := Sqrt(1-Sqr(x-1));
  end;
  { f(0) = 0; }
  { f(1) = 1; }
  { f(2) = 0; }

  function F_Shifted(x: real): real;
  {const
    K = 0.5; // from (0..1]
  begin
    x := x*k + 2*(1-k);
    //x := 1-x*0.5;
    Result := (F(x));
  end;}
  const
    P = 0.5; { [0..1] }
  var
    k: real;
  begin
    //k := 1;
    k := 1 / (2-P);
    x := (x-2)*k + 2;
    Result := F(x);
  end;

  function WeightColor(c1, c2: TColor; K: real): TColor;
  begin
    Result := RGB(
      Round((1-k)*GetRValue(c1) + k*GetRValue(c2)),
      Round((1-k)*GetGValue(c1) + k*GetGValue(c2)),
      Round((1-k)*GetBValue(c1) + k*GetBValue(c2))
    );
  end;

  procedure DrawStick(x1,x2,d: integer; c1,c2: TColor);
  var
    i, s: integer;
    k: real;
  begin
    if Height > 0 then {f(i/hh), hh = Height/2 }
    with Canvas do
    begin
      { окружность слева }
      Pen.Color := c1;
      Brush.Color :=  c1;
      Ellipse(x1-d, 1, x1+d, Height-1);

      { линии }
      {hh := Height div 2;
      for i := 0 to hh do
      begin
        k := F_Shifted(i/hh);
        s := Round(k*d);
        Pen.Color := WeightColor(c1,c2,k);

        MoveTo(x1-s+1, i);
        LineTo(x2-s+1, i);
        MoveTo(x1-s+1, Height-i-1);
        LineTo(x2-s+1, Height-i-1);
      end;}
      for i := 1 to Height-2 do
      begin
        k := F_Shifted(2*i/Height);
        s := Round(F(2*i/Height)*d);
        Pen.Color := WeightColor(c1,c2,k);

        MoveTo(x1-s+1, i);
        LineTo(x2-s+2, i);
        //MoveTo(x1-s+1, Height-i-1);
        //LineTo(x2-s+1, Height-i-1);
      end;

      { окружность справа }
      Pen.Color := c2;
      Brush.Color :=  c2;
      Ellipse(x2-d, 1, x2+d, Height-1);
    end;
  end;

const
  K = 0.7; { положение разделителя }
var
  MaxPos: integer;
  MainPart: integer;
  OverPart: integer;
  Over: real;
  D: integer;
begin
  with inherited Canvas do
  begin
    D := Round((FRotation/100)*0.5*Height);

    { определение позиции }
    MaxPos := Round(K*Width);
    if FProgress<=FMax then
      MainPart := D+Round(FProgress/FMax*(MaxPos-D))
    else
      MainPart := MaxPos;

    { контур }
    Pen.Color := RGB(
      Round(FLighter*GetRValue(FColorFill)),
      Round(FLighter*GetGValue(FColorFill)),
      Round(FLighter*GetBValue(FColorFill))
    );
    Pen.Color := WeightColor(clBlack,FColorFill,FLighter);
    Brush.Color := clWhite;
    Rectangle(0,0,Width,Height);

    { основной прогресс }
    // Brush.Color := FColorFill;
    // FillRect(Rect(1,1,MainPart,Height-1));

    //hh := Round(Height/2);
    {for i := 0 to hh do
    begin
      r := Sqrt(Sqr(hh)-Sqr(i-hh))/hh;
      r := (1-Lighter)*r + Lighter;
      Pen.Color := RGB(
        Round(r*GetRValue(FColorFill)),
        Round(r*GetGValue(FColorFill)),
        Round(r*GetBValue(FColorFill))
      );
      MoveTo(1,i);
      LineTo(MainPart+1,i);
      MoveTo(1,Height-i-1);
      LineTo(MainPart+1,Height-i-1);
    end;  }

    DrawStick(D, MainPart, D, WeightColor(clBlack,FColorFill,FLighter), FColorFill);

    { сверхпрогресс }
    if FProgress>FMax then
    begin
      Over := 1-Power(0.5,(FProgress-FMax)/FMax);
      OverPart := Round(Over*(1-K)*Width);

      DrawStick(MaxPos, MaxPos+OverPart, D, WeightColor(clBlack,FColorOver,FLighter), FColorOver);

      {for i := 0 to hh do
      begin
        r := Sqrt(Sqr(hh)-Sqr(i-hh))/hh;
        r := (1-Lighter)*r + Lighter;
        Pen.Color := RGB(
          Round(r*GetRValue(FColorOver)),
          Round(r*GetGValue(FColorOver)),
          Round(r*GetBValue(FColorOver))
        );
        MoveTo(MaxPos, i);
        LineTo(MaxPos+OverPart+1, i);
        MoveTo(MaxPos, Height-i-1);
        LineTo(MaxPos+OverPart+1, Height-i-1);
      end; }
    end;

    if Rotation = 0 then
    begin
      { разделитель }
      Pen.Color := clBlack;
      MoveTo(MaxPos,0);
      LineTo(MaxPos,Height);
    end;
  end;
end;

procedure TStatProgress.SetColorFill(Value: TColor);
begin
  if (Value<>FColorFill) then
  begin
    FColorFill := Value;
    Paint;
  end;
end;

procedure TStatProgress.SetColorOver(Value: TColor);
begin
  if (Value<>FColorOver) then
  begin
    FColorOver := Value;
    Paint;
  end;
end;

procedure TStatProgress.SetRotation(Value: integer);
begin
  if Value < 0 then Value := 0 else
  if Value > 100 then Value := 100;

  if FRotation <> Value then
  begin
    FRotation := Value;
    Paint;
  end;
end;

procedure TStatProgress.SetVolume(Value: integer);
begin
  if Value < 0 then Value := 0 else
  if Value > 100 then Value := 100;

  if FVolume <> Value then
  begin
    FVolume := Value;
    FLighter := 1-FVolume/100;
    Paint;
  end;
end;

procedure TStatProgress.SetMax(Value: integer);
begin
  if (Value>0)and(Value<>FMax) then
  begin
    FMax := Value;
    Paint;
  end;
end;

procedure TStatProgress.SetProgress(Value: integer);
begin
  if (Value>=0)and(Value<>FProgress) then
  begin
    FProgress := Value;
    Paint;
  end;
end;

{ TEditNumb }

procedure TEditNumb.Change;
var
  Accept: boolean;
  X: Extended;
begin
  inherited;
  if FWarningShow then
  begin
    //if TryStrToFloat(Text, X) then
    // TODO: hardcoded check

    Accept := True;

    if (Assigned(OnCheckValue)) then
      OnCheckValue(Self, Text, Accept)
    else
      Accept := (Text = '') or (TryToCalculate(Text, X) and (X >= 0));

    if (Accept) then
      Color := FAcceptColor
    else
      Color := FWarningColor;
  end;
end;

constructor TEditNumb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {if TryStrToFloat('1,2', X) then FDecimal := ',' else
  if TryStrToFloat('1.2', X) then FDecimal := '.' else
    FDecimal := #0;   }
  FDecimal := DiaryRoutines.Decimal;

  FAcceptColor := Color;
  FAcceptNegative := True;
  FBlockKeys := True;
  FWarningColor := DEFAULT_COLOR_WARNING;
  FWarningShow := False;
end;

procedure TEditNumb.KeyPress(var Key: Char);
var
  Accept: set of char;
begin
  if Key in ['.', ','] then Key := FDecimal else
  
  if FBlockKeys then
  begin
    Accept := ['0'..'9', '+', char(VK_BACK), char(VK_RETURN), char(VK_ESCAPE)];
    if FAcceptNegative then
      Accept := Accept + ['-'];

    if not (Key in Accept) then Key := #0;
  end;

  inherited KeyPress(Key);
end;

function TDiaryView.GetCurrentDate: TDate;
begin
  if CurrentPage <> nil then
    Result := CurrentPage.Date
  else
    raise Exception.Create('Нельзя обращаться к GetCurrentDate до открытия страницы');
end;

procedure TDiaryView.SetFont(Value: TFont);
begin
  if (Value <> nil) then
  begin
    FFont := Value;
    FFont.Charset := RUSSIAN_CHARSET;
    Paint;
  end;
end;

procedure TDiaryView.HandleBaseChanged(EventType: TPageEventType; Page: TDiaryPage;
  RecClass: TClassCustomRecord; RecInstance: TCustomRecord);
//var
//  Index: integer;
begin
  //if (Page <> nil) then // типа лишних проверок не бывает?
  //begin
    (*if (RecInstance <> nil) then
    begin
      Index := Page.FindRecord(RecInstance);

      // DONE: вызывать OnChange
      {if (CurrentPage = Page) then
      case RecInstance.TagType of
        rtBlood: RecBloodChange(Index);        // а где IF ASSIGNED() ???
        rtIns: RecInsChange(Index);            // а где IF ASSIGNED() ???
        rtMeal: RecMealChange(Index);          // а где IF ASSIGNED() ???
        //rtNote: RecNoteChange(Index);        // а где IF ASSIGNED() ???
      end; }
      //if FBase <> nil then
      //  FBase.UpdatePostprand(Page.Date - 1);
    end else
    begin
      // TODO: хак, но работать будет
      if (CurrentPage = Page) then
      begin
        RecBloodChange(-1);  // а где IF ASSIGNED() ???
        RecInsChange(-1);    // а где IF ASSIGNED() ???
        RecMealChange(-1);   // а где IF ASSIGNED() ???
      end;
    end;
      *)
    // TODO: добавить условия перерисовки
    // TODO: что делать с выделениями?

    if (EventType = etRemove) then
    begin
      FSelPanel := -1;
      FSelLine := -1;
    end else
    if (EventType = etAdd){or(EventType = etModify)} then
    begin
      {
      Если по событию etModify менять FSelLine, То возникает косвенная рекурсия
      при перетаскивании продуктов в приёме пищи
      }

      if (Page <> nil) and (Page = CurrentPage) then
      begin
        FSelPanel := Page.FindRecord(RecInstance);
        FSelLine := -1;
      end;
    end;

    DrawCurrentPage; // после всего, чтобы рисовались уже новые значения
    if Assigned(FOnChange) then FOnChange(Self, EventType, Page, RecClass, RecInstance);
//  end;
end;

{procedure TDiaryView.SetBase(Value: TDiary);
begin
  FBase := Value;
  if FBase <> nil then
  begin
    FBase.OnChange := HandleBaseChanged;
  end;
end;}

{==============================================================================}
procedure TDiaryView.HandlePageChanged(EventType: TPageEventType;
  Page: TDiaryPage; RecClass: TClassCustomRecord; RecInstance: TCustomRecord);
{==============================================================================}
begin
  if (EventType = etRemove) then
  begin
    // TODO: а если удалили не то, что было выделено?
    FSelPanel := -1;
    FSelLine := -1;
  end else
  if (EventType = etAdd){or(EventType = etModify)} then
  begin
    {
    Если по событию etModify менять FSelLine, то возникает косвенная рекурсия
    при перетаскивании продуктов в приёме пищи
    }

    // TODO: аккуратнее обработать разные типы записей и событий

    //if (Page <> nil) and (Page = CurrentPage) then - здесь всегда так
    begin
      FSelPanel := Page.FindRecord(RecInstance);
      FSelLine := -1;
    end;
  end;

  DrawCurrentPage; // после всего, чтобы рисовались уже новые значения
  if Assigned(FOnChange) then FOnChange(Self, EventType, CurrentPage, RecClass, RecInstance);
end;

end.
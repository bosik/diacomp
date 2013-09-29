unit DiaryInterface;

{ Модуль для интерфейса дневника }

interface

uses
  Windows, //
  //Messages,
  SysUtils,
  Math,
  SettingsINI,
  Classes, //
  Controls, //
  Dialogs, //
  Buttons, //
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Graphics,
  Forms,
  ACCombo,
  DiaryRoutines,
  ActnMenus,
  AutoLog,
  DiaryCore,
  UnitDataInterface;

type
  TMultiItem = record
    ItemType: TItemType;
    Index: integer;
    Tag: real;
  end;
  TMultimap = array of TMultiItem;

  {@}TSmallTimer = class
  private
    FStartTime: cardinal;
    FCheckTime: cardinal;
  public
    constructor Create;
    procedure Reset;
    function Time: cardinal;
    function FullTime: cardinal;
  end;

  {@}TAutosetupForm = class(TForm)
  private
    FInterfaceReady: boolean;
  protected
    procedure DoShow; override;
    procedure Designer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetupInterface;
    property InterfaceReady: boolean read FInterfaceReady;
  end;

  TFoodDialogType = (dtChangeMass, dtDeltaMass);

  { сообщения и диалоги }
  {@}procedure ErrorMessage(const Text: string);
  {@}procedure WarningMessage(const Text: string);
  {@}procedure InfoMessage(const Text: string);
  {@}function DialogFoodMass(DialogType: TFoodDialogType; const AHint: string; var Mass: real): boolean;

  { Мелочь }
  {@}procedure PlaceCenter(TheForm: TForm);
  {@}procedure FormatBevel(Bevel: TBevel);
  {@}procedure FocusEdit(Edit: TEdit);

  { Кодировки }
  {@}procedure CheckComponentCharset(C: TComponent);
  {@}procedure CheckAllComponentsCharset(F: TForm);

  {@}procedure ShowTableItem(Table: TListView; Index: integer; FreshScroll: boolean = False);

  { работа с панельками главной формы }
  {@}procedure MinMaxBox(Box: TGroupBox; Image: TImage; Icons: TImageList;
    Animated: boolean; OpenHeight,CloseHeight,OpenTime,CloseTime: integer);
   procedure DrawACItem(Control: TControl; Rect: TRect; const Item: TMultiItem; Selected: Boolean);

  { технические }
  {@}function CodeToRus(Key: byte): char;
  {@}procedure Wait(Time: cardinal);

// TODO: подумать, куда можно перенести эти переменные и константы  
var
  DGroup: integer;
  
const
  BORD = 8;
  MASS_EDIT_WIDTH = 75;

  clNoData     = $F8E4D8;
  clHasData    = $000000;
  COLOR_HASDATA: array[Boolean] of TColor = (clNoData, clHasData);
  FONT_COLOR: array[Boolean] of TColor = (clInactiveBorder, clWindowText);

  BEST_CHARSET = RUSSIAN_CHARSET;
  //BEST_CHARSET = CHINESEBIG5_CHARSET;
  //BEST_CHARSET = SYMBOL_CHARSET;

implementation

{ TSmallTimer }

{==============================================================================}
constructor TSmallTimer.Create;
{==============================================================================}
begin
  Reset;
end;

{==============================================================================}
function TSmallTimer.FullTime: cardinal;
{==============================================================================}
begin
  Result := GetTickCount - FStartTime;
end;

{==============================================================================}
procedure TSmallTimer.Reset;
{==============================================================================}
begin
  FStartTime := GetTickCount;
  FCheckTime := FStartTime;
end;

{==============================================================================}
function TSmallTimer.Time: cardinal;
{==============================================================================}
begin
  Result := GetTickCount - FCheckTime;
  FCheckTime := FCheckTime + Result;
end;

{ TAutosetupForm }

{==============================================================================}
constructor TAutosetupForm.Create(AOwner: TComponent);
{==============================================================================}
begin
  inherited;
  FInterfaceReady := False;
end;

{==============================================================================}
procedure TAutosetupForm.SetupInterface;
{==============================================================================}
begin
  if not FInterfaceReady then
  begin
    CheckAllComponentsCharset(Self);
    Designer;
    FInterfaceReady := True;
  end;
end;

{==============================================================================}
procedure TAutosetupForm.Designer;
{==============================================================================}
begin
  // для переопределения в потомках
  WarningMessage('В классе ' + Self.ClassName + ' не реализован метод Designer()');
end;

{==============================================================================}
procedure TAutosetupForm.DoShow;
{==============================================================================}
begin
  inherited;   
  SetupInterface;
end;

{ Misc }

{==============================================================================}
procedure ErrorMessage(const Text: string);
{==============================================================================}
begin
  MessageDlg(Text,mtError,[mbOK],0);
end;

{==============================================================================}
procedure WarningMessage(const Text: string);
{==============================================================================}
begin
  MessageDlg(Text,mtWarning,[mbOK],0);
end;

{==============================================================================}
procedure InfoMessage(const Text: string);
{==============================================================================}
begin
  MessageDlg(Text,mtInformation,[mbOK],0);
end;

{==============================================================================}
procedure FormatBevel(Bevel: TBevel);
{==============================================================================}
begin
  Bevel.Height := 2;
  Bevel.Left := 0;
  Bevel.Width := Bevel.Parent.ClientWidth;
end;

{==============================================================================}
procedure CheckComponentCharset(C: TComponent);
{==============================================================================}
begin
  if (C is TACComboBox) then        TACComboBox(C).Font.Charset := BEST_CHARSET else
  if (C is TActionMainMenuBar) then TActionMainMenuBar(C).Font.Charset := BEST_CHARSET else
  if (C is TBitBtn) then            TBitBtn(C).Font.Charset := BEST_CHARSET else
  if (C is TButton) then            TButton(C).Font.Charset := BEST_CHARSET else
  if (C is TCheckBox) then          TCheckBox(C).Font.Charset := BEST_CHARSET else
  if (C is TComboBox) then          TComboBox(C).Font.Charset := BEST_CHARSET else
  if (C is TEdit) then              TEdit(C).Font.Charset := BEST_CHARSET else
  if (C is TForm) then              TForm(C).Font.Charset := BEST_CHARSET else
  if (C is TGroupBox) then          TGroupBox(C).Font.Charset := BEST_CHARSET else
  if (C is TLabel) then             TLabel(C).Font.Charset := BEST_CHARSET else
  if (C is TListView) then          TListView(C).Font.Charset := BEST_CHARSET else
  if (C is TMemo) then              TMemo(C).Font.Charset := BEST_CHARSET else
  if (C is TMonthCalendar) then     TMonthCalendar(C).Font.Charset := BEST_CHARSET else
  if (C is TPageControl) then       TPageControl(C).Font.Charset := BEST_CHARSET else
  if (C is TRadioGroup) then        TRadioGroup(C).Font.Charset := BEST_CHARSET else
  if (C is TRadioButton) then       TRadioButton(C).Font.Charset := BEST_CHARSET else
  if (C is TSpeedButton) then       TSpeedButton(C).Font.Charset := BEST_CHARSET else
  if (C is TStatusBar) then         TStatusBar(C).Font.Charset := BEST_CHARSET else
  if (C is TTabSheet) then          TTabSheet(C).Font.Charset := BEST_CHARSET else
  {if (C is sometype) then       sometype(C).Font.Charset := BEST_CHARSET else}
end;

{==============================================================================}
procedure CheckAllComponentsCharset(F: TForm);
{==============================================================================}
var
  i: integer;
begin
  CheckComponentCharset(F);
  for i := 0 to F.ComponentCount - 1 do
    CheckComponentCharset(F.Components[i]);
end;

{==============================================================================}
procedure PlaceCenter(TheForm: TForm);
{==============================================================================}
begin
  TheForm.Left := (Screen.Width-TheForm.Width) div 2;
  TheForm.Top := (Screen.Height-TheForm.Height) div 2;
end;

{==============================================================================}
procedure FocusEdit(Edit: TEdit);
{==============================================================================}
begin
  Edit.SetFocus;
  Edit.SelectAll;
end;

{==============================================================================}
function DialogFoodMass(DialogType: TFoodDialogType; const AHint: string; var Mass: real): boolean;
{==============================================================================}
const
  DIALOG_CHANGE_MASS = 'Изменить массу';
  DIALOG_DELTA_MASS  = 'Прибавить/вычесть';
  CAPTIONS: array[TFoodDialogType] of string = (DIALOG_CHANGE_MASS, DIALOG_DELTA_MASS);
var
  S: string;
  NewMassExt: extended;
begin
  if DialogType = dtDeltaMass then
    S := '+'
  else
    S := RealToStr(Mass);

  if not InputQuery(CAPTIONS[DialogType], AHint, S) then
    Result := False
  else
  begin
    //s := InputBox(CAPTIONS[DialogType], AHint, Old);
    
    {if Length(S) > MAX_MASS_LENGTH then
      S := Copy(S, 1, MAX_MASS_LENGTH);}

    if TryToCalculate(CheckDot(s), NewMassExt) then
    begin
      Mass := NewMassExt;
      Result := True;
    end else
    begin
      ErrorMessage('Неверная масса');
      Result := False;
    end;
  end;
end;

{==============================================================================}
procedure ShowTableItem(Table: TListView; Index: integer; FreshScroll: boolean = False);
{==============================================================================}
begin
  with Table do
  if (Index >= 0)and(Index < Items.Count) then
  begin
    if Table.ItemIndex = Index then Exit;

    if FreshScroll then
    begin
      Items[Items.Count - 1].MakeVisible(False);
      if Index > 0 then
        Items[Index - 1].MakeVisible(False)
      else
        Items[Index].MakeVisible(False);
    end else
      Items[Index].MakeVisible(False);

    Items.Item[Index].Focused := True;
    Items.Item[Index].Selected := True;
  end;
end;

{==============================================================================}
function CodeToRus(Key: byte): char;
{==============================================================================}
const
  RS: array[1..33] of byte = (
    70,188,68,85,76,84,192,186,80,66,81,
    82,75,86,89,74,71,72,67,78,69,65,
    219,87,88,73,79,221,83,77,222,190,90);
  RC: array[1..33] of char = (
    'А','Б','В','Г','Д','Е','Ё','Ж','З','И','Й',
    'К','Л','М','Н','О','П','Р','С','Т','У','Ф',
    'Х','Ц','Ч','Ш','Щ','Ъ','Ы','Ь','Э','Ю','Я');
var
  i: integer;
begin
  for i := 1 to 33 do
  if (RS[i] = Key) then
  begin
    Result := RC[i];
    Exit;
  end;
  Result := #0;
end;

(*
{==============================================================================}
function Min(a,b: integer): integer;
{==============================================================================}
begin
  if a<b then Result := a
         else Result := b;
end;

{==============================================================================}
function Max(a,b: integer): integer;
{==============================================================================}
begin
  if a>b then Result := a
         else Result := b;
end;
*)

{==============================================================================}
procedure MinMaxBox(Box: TGroupBox; Image: TImage; Icons: TImageList;
  Animated: boolean; OpenHeight,CloseHeight,OpenTime,CloseTime: integer);
{==============================================================================}

  function F(Time: real): real;
  { Time приводится к [0..1] }
  { Result из [0..1] }
  begin
    //Log('F('+FloatToStr(Time)+')');

    if Time <= 0 then Result := 0 else
    if Time >= 1 then Result := 1 else

    //Result := Time;
    Result := (sin(pi * ((Time)-0.5))+1) * 0.5;
    //Result := Sqr(Time);

    //Log('F('+FloatToStr(Time)+') = ' + FloatToStr(Result));
  end;

  function R(Time: real): real;
  begin
    Result := 1-F(Time);
  end;

  procedure WaitABit;
  var
    h: THandle;
  begin
    h := CreateEvent(nil,True,false,'et');
    WaitForSingleObject(h, 10);
    CloseHandle(h);
  end;

  procedure Apply(NewHeight: real);
  var
    RoundedHeight: integer;
  begin
    //Log('Apply('+FloatToStr(NewHeight)+')');
    RoundedHeight := Round(NewHeight);
    if RoundedHeight <> Box.Height then
    begin
      //Log('Apply('+FloatToStr(NewHeight)+') modifying...');
      Box.Height := RoundedHeight;
      //Box.Repaint;
      WaitABit;
      Application.ProcessMessages;
    end;
    //Log('Apply('+FloatToStr(NewHeight)+') done');
  end;

var
  tick: cardinal;

  procedure ResetTimer;
  begin
    tick := GetTickCount;
  end;

  function LeftTime: integer;
  begin
    Result := GetTickCount - tick;
  end;

var
  n: integer;
  Dif: integer;
begin
  //Log('Поехали...');

  n := Image.Tag;
  Image.Tag := -10; // not ready
  if (OpenTime <= 0) then OpenTime := 1;
  if (CloseTime <= 0) then CloseTime := 1;


  Dif := abs(OpenHeight - CloseHeight);
  case n of
    0: begin
         if Animated then
         begin
           ResetTimer;
           while LeftTime <= OpenTime do
             Apply(CloseHeight + Round(Dif * F(LeftTime/OpenTime)));
         end;

         Apply(OpenHeight);

         Icons.Draw(Image.Canvas,0,0,n);
         Image.Hint := 'Свернуть';
       end;
    1: begin
         if Animated then
         begin
           ResetTimer;
           while LeftTime <= CloseTime do
             Apply(CloseHeight + Round(Dif * R(LeftTime/CloseTime)));
         end;

         Apply(CloseHeight);

         Icons.Draw(Image.Canvas,0,0,n);
         Image.Hint := 'Развернуть';
       end;
  end;
  Image.Repaint;
  Image.Tag := 1 - n; // ready
end;

{procedure QuickSort(More: TMoreFunction; Exch: TExchangeProc;
  StartN, FinishN: integer);

  procedure qsort(l,r: integer);
  var
    i,j,x: integer;
  begin
    i := l;
    j := r;
    x := (l+r) div 2;
    repeat
      while More(x, i) do inc(i);
      while More(j, x) do dec(j);
      if i<=j then
      begin
        if More(i, j) or More(j, i) then
          Exch(i,j);
        inc(i);
        dec(j);
      end;
    until i>j;
    if l<j then qsort(l,j);
    if i<r then qsort(i,r);
  end;

begin
  if FinishN >= StartN then
    qsort(StartN, FinishN);
end;     }

{==============================================================================}
procedure DrawACItem(Control: TControl; Rect: TRect; const Item: TMultiItem; Selected: Boolean);
{==============================================================================}

  procedure GetColors(Tag: Real; Selected: Boolean; out FontColor, BrushColor: TColor);
  var
    FC: integer;
  begin
    FC := Round(190 / IntPower(Tag + 1, 5));

    if (Selected) then
    begin
      FontColor := RGB(255 - FC div 4, 255 - FC div 4, 255 - FC div 4); //clHighlightText;
      BrushColor := RGB(FC div 2, FC div 2, FC div 2); //clMenuHighlight;
    end else
    begin
      FontColor := RGB(FC, FC, FC); //clWindowText;
      BrushColor := clWindow;
    end;
  end;

var
  Caption, Help1, Help2: string;
  IconIndex: integer;
  FontColor, BrushColor: TColor;
begin
  GetColors(Item.Tag, Selected, FontColor, BrushColor);
  IconIndex := 3;

  case Item.ItemType of
    itFood:
    begin
      Caption := FoodBase[Item.Index].Name;
      IconIndex := Byte(FoodBase[Item.Index].FromTable);
      Help1 := '';
      if Boolean(Value['CarbsInfo']) then
        Help2 := Format('  %.1f', [FoodBase[Item.Index].RelCarbs])
      else
        Help2 := '';
    end;
    itDish:
    begin
      Caption := DishBase[Item.Index].Name;
      IconIndex := 2;
      if Boolean(Value['CarbsInfo']) then
      begin
        Help1 := DateToStr(DishBase[Item.Index].ModifiedTime);
        Help2 := Format('  %.1f', [DishBase[Item.Index].RelCarbs]);
      end else
      begin
        Help1 := '';
        Help2 := '';
      end;
    end;
  end;

  with TListBox(Control).Canvas do
  begin
    Brush.Color := BrushColor;
    Font.Color := FontColor;
    Font.Charset := BEST_CHARSET;
    Windows.FillRect(Handle, Rect, Brush.Handle);
    TextOut(17 + Rect.Left, Rect.Top, Caption);
    TextOut(Control.ClientWidth - TextWidth(Help2) - 5, Rect.Top, Help2);
    TextOut(Control.ClientWidth - 2*TextWidth(Help1) - 5, Rect.Top, Help1);
    DataInterface.Images_BaseContent.Draw(TListBox(Control).Canvas, Rect.Left, Rect.Top, IconIndex);
  end;
end;

procedure Wait(Time: cardinal);
var
  h: THandle;
begin
  h := CreateEvent(nil, True, False, 'cmp');
  WaitForSingleObject(h, Time);
  CloseHandle(h);
end;

end.

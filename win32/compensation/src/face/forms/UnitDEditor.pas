unit UnitDEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask, ExtCtrls, UnitShadow,
  DiaryInterface {for BORD}, TextInterface{finger names}, DiaryRoutines {TimeToStr}, Math,
  DiaryView {Max};

type
  TEditorsList = (edTime, edTimeValue, edTimeValueFinger);
  TFocusMode = (fmTime, fmValue);
  TCheckValue = function(const Value: string): boolean;

  // not bad...
  TDialogParams = record
    Image: TBitMap; // иконка
    Color: TColor;  // цвет окна

    Caption: string;           // заголовок окна
    CaptionTime: string;       // подпись в лейбле "Время"
    CaptionValue: string;      // подпись в лейбле "Значение"
    CaptionFinger: string;     // подпись в лейбле "Палец"
    CaptionOK: string;         // подпись к кнопке "ОК"
    CaptionCancel: string;     // подпись к кнопке "Отмена"

    FocusMode: TFocusMode;     // фокус после отображения
    ShowEditors: TEditorsList; // какие поля отображать
    CheckValue: TCheckValue;   // функция проверки значения
  end;

  TFormEditor = class(TForm)
    Image: TImage;
    LabelTime: TLabel;
    LabelValue: TLabel;
    Bevel1: TBevel;
    EditTime: TMaskEdit;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    LabelFinger: TLabel;
    ComboFinger: TComboBox;
    EditValue: TEditNumb;
    procedure FormShow(Sender: TObject);
    procedure EditTimeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOKClick(Sender: TObject);
    procedure EditValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboFingerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private

  public
    FocusMode: TFocusMode;
    ShowEditors: TEditorsList;
    CheckValue: TCheckValue;
    OK: boolean;
    procedure SmallDesigner(ShowEditors: TEditorsList);
  end;

  function ShowEditor(var Time: integer; var Value: string;
    const Params: TDialogParams): boolean; overload;
  function ShowEditor(var Time: integer; var Value: string; var Finger: integer;
    const Params: TDialogParams): boolean; overload;


implementation

{$R *.dfm}

{ TFormEditor }

{==============================================================================}
function ShowEditor(var Time: integer; var Value: string; var Finger: integer;
  const Params: TDialogParams): boolean;
{==============================================================================}
var
  Dialog: TFormEditor;
  i: integer;
begin
  Dialog := TFormEditor.Create(nil);

  { подготовка }
  Dialog.Caption := Params.Caption;
  Dialog.Color := Params.Color;

  // TODO: FIX IT
  if (Params.Image <> nil) then
  begin
    //Dialog.Image.Picture.Bitmap.Assign(Params.Image);
    Dialog.Image.Picture.Bitmap.Width := Params.Image.Width;
    Dialog.Image.Picture.Bitmap.Height := Params.Image.Height;
    Dialog.Image{.Picture.Bitmap}.Canvas.Draw(0,0, Params.Image);
    Dialog.Image.Transparent := True;
  end;

  Dialog.LabelTime.Caption := Params.CaptionTime;
  Dialog.EditTime.Text := TimeToStr(Time);
  Dialog.LabelValue.Caption := Params.CaptionValue;
  Dialog.EditValue.Text := Value;
  Dialog.LabelFinger.Caption := Params.CaptionFinger;

  if Params.ShowEditors = edTimeValueFinger then
  with Dialog.ComboFinger.Items do
  begin
    Clear;
    {Add('[Левая]: большой');
    Add('[Левая]: указательный');
    Add('[Левая]: средний');
    Add('[Левая]: безымянный');
    Add('[Левая]: мизинец');
    Add('[Правая]: мизинец');
    Add('[Правая]: безымянный');
    Add('[Правая]: средний');
    Add('[Правая]: указательный');
    Add('[Правая]: большой');  }
    for i := 0 to 9 do
      Add(LongFingerNames[i]);

    Dialog.ComboFinger.ItemIndex := Finger;
  end;

  Dialog.ButtonOK.Caption := Params.CaptionOK;
  Dialog.ButtonCancel.Caption := Params.CaptionCancel;
  Dialog.FocusMode := Params.FocusMode;
  Dialog.ShowEditors := Params.ShowEditors;
  Dialog.CheckValue := Params.CheckValue;
  Dialog.EditValue.BlockKeys := False;

  Dialog.SmallDesigner(Params.ShowEditors);

  { запускаем пользователя }
  Shadow(true, Params.Color);
  Dialog.ShowModal;

  if Dialog.OK then
  begin
    TryStrToTime(Dialog.EditTime.Text, Time);
    Value := Dialog.EditValue.Text;
    Finger := Dialog.ComboFinger.ItemIndex;
    Result := True;
  end else
    Result := False;

  Shadow(false);

  Dialog.Free;
end;

{==============================================================================}
function ShowEditor(var Time: integer; var Value: string;
  const Params: TDialogParams): boolean;
{==============================================================================}
var
  f: integer;
begin
  f := -1;
  if Params.ShowEditors = edTimeValueFinger then
    ShowMessage('Некорректный вызов ShowEditor');
  Result := ShowEditor(Time, Value, f, Params);
end;

{ TFormEditor }

{==============================================================================}
procedure TFormEditor.SmallDesigner(ShowEditors: TEditorsList);
{==============================================================================}
var
  BottomLine: integer;

  procedure AlignTop(Control: TControl);
  begin
    Control.Top := BottomLine;
    inc(BottomLine, Control.Height);
  end;

  procedure BorderTop(Size: integer);
  begin
    inc(BottomLine, Size);
  end;

begin
  ClientWidth := 320;
  BottomLine := 2*BORD;

  Image.Left := 2*BORD;
  Image.Top := 2*BORD;
  Image.Width := 10*BORD;
  Image.Height := 10*BORD;

  AlignTop(LabelTime);
  AlignTop(EditTime);
  BorderTop(2*BORD);

  LabelTime.Left := 14*BORD;
  EditTime.Left := 14*BORD;
  EditTime.Width := ClientWidth - 16*BORD;

  {*****}

  // если нужно поле "Значение"

  if ShowEditors in [edTimeValue, edTimeValueFinger] then
  begin
    LabelValue.Left := 14*BORD;
    EditValue.Left := 14*BORD;
    AlignTop(LabelValue);
    AlignTop(EditValue);
    BorderTop(2*BORD);
    EditValue.Width := ClientWidth - 16*BORD;
    LabelValue.Show;
    EditValue.Show;
  end else
  begin
    LabelValue.Hide;
    EditValue.Hide;
  end;

  // если нужно поле "Пальцы"

  if ShowEditors in [edTimeValueFinger] then
  begin
    LabelFinger.Left := 14*BORD;
    ComboFinger.Left := 14*BORD;
    AlignTop(LabelFinger);
    AlignTop(ComboFinger);
    BorderTop(2*BORD);
    ComboFinger.Width := ClientWidth - 16*BORD;
    LabelFinger.Show;
    ComboFinger.Show;
  end else
  begin
    LabelFinger.Hide;
    ComboFinger.Hide;
  end;


  {*****}
  BottomLine := Max(BottomLine, 14*BORD);

  FormatBevel(Bevel1);
  AlignTop(Bevel1);
  BorderTop(2*BORD);

  ButtonOK.Top := BottomLine;
  AlignTop(ButtonCancel);

  ButtonOK.Left := 2 * BORD;
  ButtonCancel.Left := ClientWidth - ButtonCancel.Width - 2*BORD;

  ClientHeight := BottomLine + 2*BORD;
  {!!!}
end;

{==============================================================================}
procedure TFormEditor.FormShow(Sender: TObject);
{==============================================================================}
begin
  PlaceCenter(Self);
  case FocusMode of
    fmTime: EditTime.SetFocus;
    fmValue: EditValue.SetFocus;
  end;
  OK := False;
end;

{==============================================================================}
procedure TFormEditor.EditTimeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if Key = vk_Return then
  case ShowEditors of
    edTime:              ButtonOKClick(nil);
    edTimeValue:         EditValue.SetFocus;
    edTimeValueFinger:   EditValue.SetFocus;
  end;
end;

{==============================================================================}
procedure TFormEditor.EditValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if Key = vk_Return then
  case ShowEditors of
    edTime:              ;// LOLWHAT
    edTimeValue:         ButtonOKClick(nil);
    edTimeValueFinger:   ComboFinger.SetFocus;
  end;
end;

{==============================================================================}
procedure TFormEditor.ComboFingerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if Key = vk_Return then
  case ShowEditors of
    edTime:              ;// LOLWHAT
    edTimeValue:         ;// LOLWHAT
    edTimeValueFinger:   ButtonOKClick(nil);
  end;
end;

{==============================================================================}
procedure TFormEditor.ButtonOKClick(Sender: TObject);
{==============================================================================}
var
  T: integer;
begin
  if not TryStrToTime(EditTime.Text, T) then
  begin
    ErrorMessage('Неверное время');
    EditTime.SetFocus;
    Exit;
  end;

  // not bad...
  if Assigned(CheckValue) and (not CheckValue(EditValue.Text)) then
  begin
    ErrorMessage('Неверное значение');
    EditValue.SetFocus;
  end else
  begin
    OK := True;
    Close;
  end;
end;

procedure TFormEditor.FormPaint(Sender: TObject);
begin
 // Image.Transparent := True;
end;

procedure TFormEditor.FormCreate(Sender: TObject);
begin
  CheckAllComponentsCharset(Self);
end;

end.

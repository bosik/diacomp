unit UnitDEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask, ExtCtrls, UnitShadow,
  DiaryInterface {for BORD}, TextInterface{finger names}, DiaryRoutines {TimeToStr}, Math,
  DiaryView {Max}, UnitEditor, BusinessObjects, DiaryRecords, DiaryPageSerializer,
  JsonSerializer, uLkJSON, ComCtrls, SettingsINI;

type
  TFocusMode = (fmTime, fmValue);

  // not bad...
  // Nope, it's bad.
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
  end;

  TFormEditorBlood = class(TFormCommonEditor)
    Image: TImage;
    LabelTime: TLabel;
    LabelValue: TLabel;
    Bevel1: TBevel;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    LabelFinger: TLabel;
    ComboFinger: TComboBox;
    EditValue: TEditNumb;
    TimePicker: TDateTimePicker;
    DatePicker: TDateTimePicker;
    procedure FormShow(Sender: TObject);
    procedure EditTimeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboFingerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOKClick(Sender: TObject);
  private
    function Entity(): TBloodRecord;
  protected
    class function Clone(X: TVersioned): TVersioned; override;
    class function CreateEditorForm(): TFormCommonEditor; override;
    function ReadEntityFromGUI(): boolean; override;
    procedure ShowEntityInGUI(CreateMode: boolean); override;
  public
    //FocusMode: TFocusMode;
    OK: boolean;
    procedure SmallDesigner();
  end;

  function ShowEditor(var Time_: TDateTime; var Value: string;
    const Params: TDialogParams): boolean; overload;
  function ShowEditor(var Time_: TDateTime; var Value: string; var Finger: integer;
    const Params: TDialogParams): boolean; overload;

implementation

{$R *.dfm}

{ TFormEditorBlood }

{==============================================================================}
function ShowEditor(var Time_: TDateTime; var Value: string; var Finger: integer;
  const Params: TDialogParams): boolean;
{==============================================================================}
var
  Dialog: TFormEditorBlood;
  i: integer;
  Minutes: integer;
begin
 
end;

{==============================================================================}
function ShowEditor(var Time_: TDateTime; var Value: string;
  const Params: TDialogParams): boolean;
{==============================================================================}
var
  f: integer;
begin
  f := -1;
  Result := ShowEditor(Time_, Value, f, Params);
end;

{==============================================================================}
procedure TFormEditorBlood.SmallDesigner();
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
  ClientWidth := 350;
  BottomLine := 2 * BORD;

  Image.Left := 2 * BORD;
  Image.Top := 2 * BORD;
  Image.Width := 10 * BORD;
  Image.Height := 10 * BORD;

  AlignTop(LabelTime);
  AlignTop(TimePicker);
  DatePicker.Top := TimePicker.Top;

  BorderTop(2 * BORD);

  LabelTime.Left := 14 * BORD;
  TimePicker.Left := 14 * BORD;
  DatePicker.Left := TimePicker.Left + TimePicker.Width + 2 * BORD;


  { * * * * * }

  // поле "Значение"
  LabelValue.Left := 14 * BORD;
  EditValue.Left := 14 * BORD;
  AlignTop(LabelValue);
  AlignTop(EditValue);
  BorderTop(2 * BORD);
  EditValue.Width := ClientWidth - 16 * BORD;

  // поле "Пальцы"
  LabelFinger.Left := 14 * BORD;
  ComboFinger.Left := 14 * BORD;
  AlignTop(LabelFinger);
  AlignTop(ComboFinger);
  BorderTop(2 * BORD);
  ComboFinger.Width := ClientWidth - 16 * BORD;

  { * * * * * }
  BottomLine := Max(BottomLine, 14 * BORD);

  FormatBevel(Bevel1);
  AlignTop(Bevel1);
  BorderTop(2 * BORD);

  ButtonOK.Top := BottomLine;
  AlignTop(ButtonCancel);

  ButtonOK.Left := 2 * BORD;
  ButtonCancel.Left := ClientWidth - ButtonCancel.Width - 2 * BORD;

  ClientHeight := BottomLine + 2 * BORD;
  {!!!}
end;

{==============================================================================}
procedure TFormEditorBlood.FormShow(Sender: TObject);
{==============================================================================}
begin
  PlaceCenter(Self);
  {case FocusMode of
    fmTime: EditTime.SetFocus;
    fmValue: EditValue.SetFocus;
  end;
  OK := False; }
  // TODO 5: RF (?): focusing modes in editor
end;

{==============================================================================}
procedure TFormEditorBlood.EditTimeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if Key = vk_Return then
    EditValue.SetFocus;
end;

{==============================================================================}
procedure TFormEditorBlood.EditValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if Key = vk_Return then
    ComboFinger.SetFocus;
end;

{==============================================================================}
procedure TFormEditorBlood.ComboFingerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if Key = vk_Return then
  begin
    ModalResult := mrOk;
  end;
end;

procedure TFormEditorBlood.FormPaint(Sender: TObject);
begin
 // Image.Transparent := True;
end;

{==============================================================================}
procedure TFormEditorBlood.FormCreate(Sender: TObject);
{==============================================================================}
var
  i: integer;
begin
  CheckAllComponentsCharset(Self);

  Caption := 'Замер СК';
  if Value['ColoredEditors'] then
    Color := Value['Color_SelBlood']
  else
    Color := clBtnFace;

  LabelTime.Caption := 'Время';
  LabelValue.Caption := 'Значение';
  LabelFinger.Caption := 'Палец';

  with ComboFinger.Items do
  begin
    Clear;
    for i := 0 to 9 do
      Add(LongFingerNames[i]);
  end;

  ButtonCancel.Caption := 'Отмена';
  EditValue.BlockKeys := True;

  SmallDesigner();
end;

{==============================================================================}
class function TFormEditorBlood.Clone(X: TVersioned): TVersioned;
{==============================================================================}
var
  S: string;
begin
  S := JsonWrite(SerializeVersionedDiaryRecord(X as TCustomRecord));
  Result := ParseVersionedDiaryRecord(JsonRead(S) as TlkJSONobject);
end;

{==============================================================================}
class function TFormEditorBlood.CreateEditorForm: TFormCommonEditor;
{==============================================================================}
begin
  Result := TFormEditorBlood.Create(nil);
end;

{==============================================================================}
function TFormEditorBlood.ReadEntityFromGUI: boolean;
{==============================================================================}
var
  T: integer;
  X: Extended;
begin
  if (not TryStrToFloat(CheckDot(EditValue.Text), X) or (X <= 0)) then
  begin
    ErrorMessage('Неверное значение');
    EditValue.SetFocus;
    Result := False;
  end else
  begin
    Entity.NativeTime := LocalToUTC(Trunc(DatePicker.Date) + Frac(TimePicker.Time));
    Entity.Finger := ComboFinger.ItemIndex;
    Entity.Value := X;
    Result := True;
  end;
end;

{==============================================================================}
procedure TFormEditorBlood.ShowEntityInGUI(CreateMode: boolean);
{==============================================================================}
var
  LocalTime: TDateTime;
begin
  if (CreateMode) then
  begin
    LocalTime := Now();
    TimePicker.Time := LocalTime;
    DatePicker.Date := LocalTime;
    EditValue.Text := '';
    ComboFinger.ItemIndex := Entity.Finger;
  end else
  begin
    LocalTime := UTCToLocal(Entity.NativeTime);
    TimePicker.Time := LocalTime;
    DatePicker.Date := LocalTime;
    EditValue.Text := FloatToStr(Entity.Value);
    ComboFinger.ItemIndex := Entity.Finger;
  end;

  ButtonOK.Caption := SAVE_CAPTION[CreateMode];
end;

{==============================================================================}
function TFormEditorBlood.Entity: TBloodRecord;
{==============================================================================}
begin
  Result := TBloodRecord(inherited Entity);
end;

procedure TFormEditorBlood.ButtonOKClick(Sender: TObject);
begin
  if (ReadEntityFromGUI()) then
  begin
    ModalResult := mrOK;
  end;
end;

end.


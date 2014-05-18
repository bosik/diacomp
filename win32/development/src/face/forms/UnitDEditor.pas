unit UnitDEditor;

interface

uses
  Windows, // vk_Return
  SysUtils, // TryStrToFloat
  Classes, //
  Graphics, // clBtnFace
  Controls,
  Forms,
  StdCtrls,
  Buttons,
  ExtCtrls, //
  DiaryInterface {for BORD},
  TextInterface{finger names},
  DiaryRoutines {TimeToStr},
  Math, // Max
  UnitEditor,
  BusinessObjects, //
  DiaryRecords,
  DiaryPageSerializer,
  JsonSerializer,
  uLkJSON,
  ComCtrls,
  SettingsINI, DiaryView;

type
  // not bad...
  // Nope, it's bad.
  TDialogParams = record
    Image: TBitMap; // иконка

    Caption: string;           // заголовок окна
    CaptionTime: string;       // подпись в лейбле "Время"
    CaptionValue: string;      // подпись в лейбле "Значение"
    CaptionFinger: string;     // подпись в лейбле "Палец"
    Color: TColor;
  end;

  TFormEditorCommon = class(TFormCommonEditor)
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
    procedure FieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOKClick(Sender: TObject);
  private
    function Entity(): TBloodRecord;
  protected
    class function Clone(X: TVersioned): TVersioned; override;
    class function CreateEditorForm(CreateMode: boolean): TFormCommonEditor; override;
    function ReadEntityFromGUI(): boolean; override;
    procedure ShowEntityInGUI(CreateMode: boolean); override;
    procedure Designer(); override;
  end;

  function ShowEditor(var Time_: TDateTime; var Value: string;
    const Params: TDialogParams): boolean; overload; deprecated;
  function ShowEditor(var Time_: TDateTime; var Value: string; var Finger: integer;
    const Params: TDialogParams): boolean; overload; deprecated;

implementation

uses UnitEditorBlood;

{$R *.dfm}

{ TFormEditorBlood }

{==============================================================================}
class function TFormEditorCommon.Clone(X: TVersioned): TVersioned;
{==============================================================================}
var
  S: string;
begin
  S := JsonWrite(SerializeVersionedDiaryRecord(X as TCustomRecord));
  Result := ParseVersionedDiaryRecord(JsonRead(S) as TlkJSONobject);
end;

{==============================================================================}
class function TFormEditorCommon.CreateEditorForm(CreateMode: boolean): TFormCommonEditor;
{==============================================================================}
var
  Dialog: TFormEditorBlood;
  i: integer;
begin
  Dialog := TFormEditorBlood.Create(nil);

  Dialog.Caption := 'Замер СК';
  if Value['ColoredEditors'] then
    Dialog.Color := Value['Color_SelBlood']
  else
    Dialog.Color := clBtnFace;

  Dialog.LabelTime.Caption := 'Время';
  Dialog.LabelValue.Caption := 'Значение';
  Dialog.LabelFinger.Caption := 'Палец';

  with Dialog.ComboFinger.Items do
  begin
    Clear;
    for i := 0 to High(LongFingerNames) do
      Add(LongFingerNames[i]);
  end;

  Dialog.ButtonOK.Caption := SAVE_CAPTION[CreateMode];
  Dialog.ButtonCancel.Caption := 'Отмена';
  Dialog.EditValue.BlockKeys := True;

  Result := Dialog;
end;

{==============================================================================}
function TFormEditorCommon.ReadEntityFromGUI(): boolean;
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
procedure TFormEditorCommon.ShowEntityInGUI(CreateMode: boolean);
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
end;

{==============================================================================}
function TFormEditorCommon.Entity: TBloodRecord;
{==============================================================================}
begin
  Result := TBloodRecord(inherited Entity);
end;

{==============================================================================}
procedure TFormEditorCommon.ButtonOKClick(Sender: TObject);
{==============================================================================}
begin
  Submit();
end;

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
function ShowEditor(var Time_: TDateTime; var Value: string; const Params: TDialogParams): boolean;
{==============================================================================}
var
  f: integer;
begin
  f := -1;
  Result := ShowEditor(Time_, Value, f, Params);
end;

{==============================================================================}
procedure TFormEditorCommon.Designer();
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

  PlaceCenter(Self);
end;

{==============================================================================}
procedure TFormEditorCommon.FieldKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if (Key = vk_Return) then
  begin
    if (Sender = TimePicker) then
      EditValue.SetFocus() else
    if (Sender = EditValue) then
      ComboFinger.SetFocus() else
    if (Sender = ComboFinger) then
      Submit();
  end;
end;

end.

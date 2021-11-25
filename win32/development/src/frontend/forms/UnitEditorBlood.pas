unit UnitEditorBlood;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, StdCtrls, ExtCtrls, DiaryRoutines, Buttons, DiaryInterface,
  UnitShadow, SettingsINI, DiaryView, ComCtrls, DiaryRecords, BusinessObjects,
  UnitEditor, JsonSerializer, uLKjson, DiaryPageSerializer, TextInterface, Math,
  DiaryCore;

type
  TFormEditorBlood = class(TFormEditor)
    Image: TImage;
    LabelTime: TLabel;
    LabelValue: TLabel;
    Bevel1: TBevel;
    LabelFinger: TLabel;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    ComboFinger: TComboBox;
    EditValue: TEditNumb;
    TimePicker: TDateTimePicker;
    DatePicker: TDateTimePicker;
    procedure FieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOKClick(Sender: TObject);
    procedure EditValueKeyPress(Sender: TObject; var Key: Char);
  private
    function Entity(): TBloodRecord;
  protected
    class function Clone(X: TVersioned): TVersioned; override;
    class function CreateEditorForm(CreateMode: boolean): TFormEditor; override;
    function ReadEntityFromGUI(): boolean; override;
    procedure ShowEntityInGUI(CreateMode: boolean); override;
    procedure Designer(); override;
  end;

implementation

uses MainUnit;

{$R *.dfm}

{ TFormEditorBlood }

{======================================================================================================================}
class function TFormEditorBlood.Clone(X: TVersioned): TVersioned;
{======================================================================================================================}
var
  S: string;
begin
  S := JsonWrite(SerializeVersionedDiaryRecord(X));
  Result := ParseVersionedDiaryRecord(JsonRead(S) as TlkJSONobject);
end;

{======================================================================================================================}
class function TFormEditorBlood.CreateEditorForm(CreateMode: boolean): TFormEditor;
{======================================================================================================================}
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

{======================================================================================================================}
function TFormEditorBlood.ReadEntityFromGUI(): boolean;
{======================================================================================================================}
var
  X: Extended;
begin
  if (not TryStrToFloat(CheckDot(EditValue.Text), X) or (X <= 0)) then
  begin
    ErrorMessage('Неверное значение');
    EditValue.SetFocus;
    Result := False;
  end else
  begin
    Entity.Time := LocalToUTC(Trunc(DatePicker.Date) + Frac(TimePicker.Time));
    Entity.Finger := ComboFinger.ItemIndex;
    Entity.Value := X;
    Result := True;
  end;
end;

{======================================================================================================================}
procedure TFormEditorBlood.ShowEntityInGUI(CreateMode: boolean);
{======================================================================================================================}
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
    LocalTime := UTCToLocal(Entity.Time);
    TimePicker.Time := LocalTime;
    DatePicker.Date := LocalTime;
    EditValue.Text := FloatToStr(Entity.Value);
    ComboFinger.ItemIndex := Entity.Finger;
  end;
end;

{======================================================================================================================}
function TFormEditorBlood.Entity: TBloodRecord;
{======================================================================================================================}
begin
  Result := TBloodRecord(inherited Entity.Data);
end;

{======================================================================================================================}
procedure TFormEditorBlood.ButtonOKClick(Sender: TObject);
{======================================================================================================================}
begin
  Submit();
end;

{======================================================================================================================}
procedure TFormEditorBlood.Designer();
{======================================================================================================================}
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

  if (FileExists(WORK_FOLDER + IMAGE_DIARY_NEW_BLOOD)) then
    Image.Picture.LoadFromFile(WORK_FOLDER + IMAGE_DIARY_NEW_BLOOD);

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

{======================================================================================================================}
procedure TFormEditorBlood.FieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{======================================================================================================================}
begin
  if (Key = vk_Return) then
  begin
    if (Sender = TimePicker) then
      EditValue.SetFocus() else
    if (Sender = DatePicker) then
      EditValue.SetFocus() else
    if (Sender = EditValue) then
      Submit() else
    if (Sender = ComboFinger) then
      Submit();
  end;
end;

{======================================================================================================================}
procedure TFormEditorBlood.EditValueKeyPress(Sender: TObject; var Key: Char);
{======================================================================================================================}
begin
  if (Key = #13) then
    Key := #0;
end;

end.

unit UnitEditorIns;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, StdCtrls, ExtCtrls, DiaryRoutines, Buttons, DiaryInterface,
  UnitShadow, SettingsINI, DiaryView, ComCtrls, DiaryRecords, BusinessObjects,
  UnitEditor, JsonSerializer, uLKjson, DiaryPageSerializer, TextInterface, Math,
  DiaryCore;

type
  TFormEditorIns = class(TFormEditor)
    Image: TImage;
    LabelTime: TLabel;
    LabelValue: TLabel;
    Bevel1: TBevel;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    EditValue: TEditNumb;
    TimePicker: TDateTimePicker;
    DatePicker: TDateTimePicker;
    procedure FieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOKClick(Sender: TObject);
  private
    function Entity(): TInsRecord;
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

{======================================================================================================================}
class function TFormEditorIns.Clone(X: TVersioned): TVersioned;
{======================================================================================================================}
var
  S: string;
begin
  S := JsonWrite(SerializeVersionedDiaryRecord(X as TCustomRecord));
  Result := ParseVersionedDiaryRecord(JsonRead(S) as TlkJSONobject);
end;

{======================================================================================================================}
class function TFormEditorIns.CreateEditorForm(CreateMode: boolean): TFormEditor;
{======================================================================================================================}
var
  Dialog: TFormEditorIns;
begin
  Dialog := TFormEditorIns.Create(nil);

  Dialog.Caption := 'Инъекция';
  if Value['ColoredEditors'] then
    Dialog.Color := Value['Color_SelIns']
  else
    Dialog.Color := clBtnFace;

  Dialog.LabelTime.Caption := 'Время';
  Dialog.LabelValue.Caption := 'Значение';

  Dialog.ButtonOK.Caption := SAVE_CAPTION[CreateMode];
  Dialog.ButtonCancel.Caption := 'Отмена';
  Dialog.EditValue.BlockKeys := True;

  Result := Dialog;
end;

{======================================================================================================================}
function TFormEditorIns.ReadEntityFromGUI(): boolean;
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
    Entity.Value := X;
    Result := True;
  end;
end;

{======================================================================================================================}
procedure TFormEditorIns.ShowEntityInGUI(CreateMode: boolean);
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
  end else
  begin
    LocalTime := UTCToLocal(Entity.Time);
    TimePicker.Time := LocalTime;
    DatePicker.Date := LocalTime;
    EditValue.Text := FloatToStr(Entity.Value);
  end;
end;

{======================================================================================================================}
function TFormEditorIns.Entity: TInsRecord;
{======================================================================================================================}
begin
  Result := TInsRecord(inherited Entity);
end;

{======================================================================================================================}
procedure TFormEditorIns.ButtonOKClick(Sender: TObject);
{======================================================================================================================}
begin
  Submit();
end;

{======================================================================================================================}
procedure TFormEditorIns.Designer();
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

  if (FileExists(WORK_FOLDER + IMAGE_DIARY_NEW_INS)) then
    Image.Picture.LoadFromFile(WORK_FOLDER + IMAGE_DIARY_NEW_INS);

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
procedure TFormEditorIns.FieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{======================================================================================================================}
begin
  if (Key = vk_Return) then
  begin
    if (Sender = TimePicker) then
      EditValue.SetFocus() else
    if (Sender = DatePicker) then
      EditValue.SetFocus() else
    if (Sender = EditValue) then
      Submit();
  end;
end;

end.

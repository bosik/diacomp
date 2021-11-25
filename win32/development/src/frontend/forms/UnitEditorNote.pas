unit UnitEditorNote;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, StdCtrls, ExtCtrls, DiaryRoutines, Buttons, DiaryInterface,
  UnitShadow, SettingsINI, DiaryView, ComCtrls, DiaryRecords, BusinessObjects,
  UnitEditor, JsonSerializer, uLKjson, DiaryPageSerializer, TextInterface, Math,
  DiaryCore;

type
  TFormEditorNote = class(TFormEditor)
    Image: TImage;
    LabelTime: TLabel;
    LabelText: TLabel;
    Bevel1: TBevel;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    TimePicker: TDateTimePicker;
    DatePicker: TDateTimePicker;
    EditText: TMemo;
  procedure FieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOKClick(Sender: TObject);
  private
    function Entity(): TNoteRecord;
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
class function TFormEditorNote.Clone(X: TVersioned): TVersioned;
{======================================================================================================================}
var
  S: string;
begin
  S := JsonWrite(SerializeVersionedDiaryRecord(X));
  Result := ParseVersionedDiaryRecord(JsonRead(S) as TlkJSONobject);
end;

{======================================================================================================================}
class function TFormEditorNote.CreateEditorForm(CreateMode: boolean): TFormEditor;
{======================================================================================================================}
var
  Dialog: TFormEditorNote;
begin
  Dialog := TFormEditorNote.Create(nil);

  Dialog.Caption := 'Заметка';
  if Value['ColoredEditors'] then
    Dialog.Color := Value['Color_SelNote']
  else
    Dialog.Color := clBtnFace;

  Dialog.LabelTime.Caption := 'Время';
  Dialog.LabelText.Caption := 'Текст';

  Dialog.ButtonOK.Caption := SAVE_CAPTION[CreateMode];
  Dialog.ButtonCancel.Caption := 'Отмена';

  Result := Dialog;
end;

{======================================================================================================================}
function TFormEditorNote.ReadEntityFromGUI(): boolean;
{======================================================================================================================}
begin
  Entity.Time := LocalToUTC(Trunc(DatePicker.Date) + Frac(TimePicker.Time));
  Entity.Text := EditText.Text;
  Result := True;
end;

{======================================================================================================================}
procedure TFormEditorNote.ShowEntityInGUI(CreateMode: boolean);
{======================================================================================================================}
var
  LocalTime: TDateTime;
begin
  if (CreateMode) then
  begin
    LocalTime := Now();
    TimePicker.Time := LocalTime;
    DatePicker.Date := LocalTime;
    EditText.Text := '';
  end else
  begin
    LocalTime := UTCToLocal(Entity.Time);
    TimePicker.Time := LocalTime;
    DatePicker.Date := LocalTime;
    EditText.Text := Entity.Text;
  end;
end;

{======================================================================================================================}
function TFormEditorNote.Entity: TNoteRecord;
{======================================================================================================================}
begin
  Result := TNoteRecord(inherited Entity.Data);
end;

{======================================================================================================================}
procedure TFormEditorNote.ButtonOKClick(Sender: TObject);
{======================================================================================================================}
begin
  Submit();
end;

{======================================================================================================================}
procedure TFormEditorNote.Designer();
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

  if (FileExists(WORK_FOLDER + IMAGE_DIARY_NEW_NOTE)) then
    Image.Picture.LoadFromFile(WORK_FOLDER + IMAGE_DIARY_NEW_NOTE);

  AlignTop(LabelTime);
  AlignTop(TimePicker);
  DatePicker.Top := TimePicker.Top;

  BorderTop(2 * BORD);

  LabelTime.Left := 14 * BORD;
  TimePicker.Left := 14 * BORD;
  DatePicker.Left := TimePicker.Left + TimePicker.Width + 2 * BORD;

  { * * * * * }

  // поле "Значение"
  LabelText.Left := 14 * BORD;
  EditText.Left := 14 * BORD;
  AlignTop(LabelText);
  AlignTop(EditText);
  BorderTop(2 * BORD);
  EditText.Width := ClientWidth - 16 * BORD;
  EditText.Height := ClientHeight - EditText.Top - ButtonOK.Height - 3 * BORD;

  { * * * * * }
  BottomLine := Max(BottomLine, 14 * BORD);

  FormatBevel(Bevel1);
  Bevel1.Top := ClientHeight - ButtonOK.Height - 2 * BORD;

  ButtonOK.Top := ClientHeight - ButtonOK.Height - BORD;
  ButtonCancel.Top := ClientHeight - ButtonOK.Height - BORD;

  ButtonOK.Left := 2 * BORD;
  ButtonCancel.Left := ClientWidth - ButtonCancel.Width - 2 * BORD;

  {!!!}

  PlaceCenter(Self);
end;

{======================================================================================================================}
procedure TFormEditorNote.FieldKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{======================================================================================================================}
begin
  if (Key = vk_Return) then
  begin
    if (Sender = TimePicker) then
      EditText.SetFocus() else
    if (Sender = DatePicker) then
      EditText.SetFocus() else
    if (Sender = EditText) then
      Submit();
  end;
end;

end.

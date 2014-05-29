unit UnitEditorMeal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, StdCtrls, ExtCtrls, DiaryRoutines, Buttons, DiaryInterface,
  UnitShadow, SettingsINI, DiaryView, ComCtrls, DiaryRecords, BusinessObjects,
  UnitEditor, JsonSerializer, uLKjson, DiaryPageSerializer, TextInterface, Math;

type
  TFormEditorMeal = class(TFormEditor)
    Image: TImage;
    LabelTime: TLabel;
    Bevel1: TBevel;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    TimePicker: TDateTimePicker;
    DatePicker: TDateTimePicker;
    CheckShortMeal: TCheckBox;
    procedure FieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOKClick(Sender: TObject);
  private
    function Entity(): TMealRecord;
  protected
    class function Clone(X: TVersioned): TVersioned; override;
    class function CreateEditorForm(CreateMode: boolean): TFormEditor; override;
    function ReadEntityFromGUI(): boolean; override;
    procedure ShowEntityInGUI(CreateMode: boolean); override;
    procedure Designer(); override;
  end;

implementation

{$R *.dfm}

{ TFormEditorMeal }

{==============================================================================}
class function TFormEditorMeal.Clone(X: TVersioned): TVersioned;
{==============================================================================}
var
  S: string;
begin
  S := JsonWrite(SerializeVersionedDiaryRecord(X as TCustomRecord));
  Result := ParseVersionedDiaryRecord(JsonRead(S) as TlkJSONobject);
end;

{==============================================================================}
class function TFormEditorMeal.CreateEditorForm(CreateMode: boolean): TFormEditor;
{==============================================================================}
var
  Dialog: TFormEditorMeal;
begin
  Dialog := TFormEditorMeal.Create(nil);

  Dialog.Caption := 'Приём пищи';
  if Value['ColoredEditors'] then
    Dialog.Color := Value['Color_SelMeal']
  else
    Dialog.Color := clBtnFace;

  Dialog.LabelTime.Caption := 'Время';
  Dialog.CheckShortMeal.Caption := 'Сокращённый постпрандиал';

  Dialog.ButtonOK.Caption := SAVE_CAPTION[CreateMode];
  Dialog.ButtonCancel.Caption := 'Отмена';

  Result := Dialog;
end;

{==============================================================================}
function TFormEditorMeal.ReadEntityFromGUI(): boolean;
{==============================================================================}
begin
  Entity.NativeTime := LocalToUTC(Trunc(DatePicker.Date) + Frac(TimePicker.Time));
  Entity.ShortMeal := CheckShortMeal.Checked;
  Result := True;
end;

{==============================================================================}
procedure TFormEditorMeal.ShowEntityInGUI(CreateMode: boolean);
{==============================================================================}
var
  LocalTime: TDateTime;
begin
  if (CreateMode) then
  begin
    LocalTime := Now();
    TimePicker.Time := LocalTime;
    DatePicker.Date := LocalTime;
    CheckShortMeal.Checked := False;
  end else
  begin
    LocalTime := UTCToLocal(Entity.NativeTime);
    TimePicker.Time := LocalTime;
    DatePicker.Date := LocalTime;
    CheckShortMeal.Checked := Entity.ShortMeal;
  end;
end;

{==============================================================================}
function TFormEditorMeal.Entity: TMealRecord;
{==============================================================================}
begin
  Result := TMealRecord(inherited Entity);
end;

{==============================================================================}
procedure TFormEditorMeal.ButtonOKClick(Sender: TObject);
{==============================================================================}
begin
  Submit();
end;

{==============================================================================}
procedure TFormEditorMeal.Designer();
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

  // field "Short meal"
  CheckShortMeal.Left := 14 * BORD;
  AlignTop(CheckShortMeal);
  BorderTop(2 * BORD);
  CheckShortMeal.Width := ClientWidth - 16 * BORD;

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
procedure TFormEditorMeal.FieldKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if (Key = vk_Return) then
  begin
    //if (Sender = TimePicker) then
    Submit()
  end;
end;

end.

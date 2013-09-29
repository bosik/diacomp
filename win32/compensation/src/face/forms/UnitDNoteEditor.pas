unit UnitDNoteEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask, ExtCtrls, DiaryInterface, DiaryRoutines,
  UnitShadow, SettingsINI;

type
  TDNoteEditor = class(TAutosetupForm)
    Image: TImage;
    LabelTime: TLabel;
    LabelValue: TLabel;
    Bevel1: TBevel;
    EditValue: TEdit;
    EditTime: TMaskEdit;
    ButtonSave: TBitBtn;
    ButtonCancel: TBitBtn;
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditTimeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    procedure Designer; override;
  public
    CurMode: integer;
  end;

  function ShowNoteEditor(var Time: integer;  var Value: string; New: boolean;
    FocusMode: integer): boolean;

var
  DNoteEditor: TDNoteEditor;

const
  NO_TEXT = '  .  ';
  fmTime  = 0;
  fmValue = 1;
  SAVE_CAPTION: array[Boolean] of string = ('Сохранить','Добавить');

implementation

{$R *.dfm}

{ TDNoteEditor }

{==============================================================================}
function ShowNoteEditor(var Time: integer; var Value: string; New: boolean;
  FocusMode: integer): boolean;
{==============================================================================}
begin
  { подготовка к редактированию }
  if New then
  begin
    DNoteEditor.EditTime.Text := GetCurrentTime;
    DNoteEditor.EditValue.Text := '';
  end else
  begin
    DNoteEditor.EditTime.Text := TimeToStr(Time);
    DNoteEditor.EditValue.Text := Value;
  end;

  DNoteEditor.CurMode := FocusMode;
  DNoteEditor.ButtonSave.Caption := SAVE_CAPTION[New];

  { запускаем пользователя }
  Shadow(true, DNoteEditor.Color);
  if DNoteEditor.ShowModal = mrOK then
  begin
    TryStrToTime(DNoteEditor.EditTime.Text, Time);
    Value := DNoteEditor.EditValue.Text;
  end;

  Shadow(false);
end;

procedure TDNoteEditor.ButtonSaveClick(Sender: TObject);
var
  T: integer;
begin
  if not TryStrToTime(EditTime.Text, T) then
  begin
    ErrorMessage('Неверное время');
    EditTime.SetFocus;
  end else
  begin
    ModalResult := mrOK;
    Close;
  end;
end;

procedure TDNoteEditor.FormShow(Sender: TObject);
begin
  {if Value['ColoredEditors'] then
    Color := Value['Color_SelNote']
  else
    Color := clBtnFace;
  case FocusMode of
    fmTime: EditTime.SetFocus;
    fmValue: EditValue.SetFocus;
  end;
  PlaceCenter(Self);}
end;

procedure TDNoteEditor.EditTimeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
    EditValue.SetFocus;
end;

procedure TDNoteEditor.EditValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
    ButtonSaveClick(nil);
end;

procedure TDNoteEditor.Designer;
begin
  FormatBevel(Bevel1, Self);
  {!!!}
end;

end.

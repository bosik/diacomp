unit UnitDInsEditor;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, StdCtrls, ExtCtrls, DiaryRoutines, Buttons, DiaryInterface;

type
  TDInsEditor = class(TAutosetupForm)
    Image: TImage;
    LabelTime: TLabel;
    LabelValue: TLabel;
    EditValue: TEdit;
    EditTime: TMaskEdit;
    Bevel1: TBevel;
    ButtonSave: TBitBtn;
    ButtonCancel: TBitBtn;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditTimeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    procedure Designer; override;
  public
    function ShowInsEditor(var DefaultTime: integer;
      var DefaultValue: real; New: boolean; FocusMode: integer): boolean;
  end;

var
  DInsEditor: TDInsEditor;
  OK: boolean;
  Time: integer;
  Value: extended; {for TryStrToFloat}
  CurMode: integer;
const
  NO_TEXT = '  .  ';
  fmTime  = 0;
  fmValue = 1;
  SAVE_CAPTION: array[Boolean] of string = ('Сохранить','Добавить');
implementation

uses UnitShadow, SettingsINI;

{$R *.dfm}

{========================================================}
function TDInsEditor.ShowInsEditor(var DefaultTime: integer;
  var DefaultValue: real; New: boolean; FocusMode: integer): boolean;
{========================================================}
begin
  { подготовка к редактированию }
  Time:=DefaultTime;
  Value:=DefaultValue;
  if New then
  begin
    EditTime.Text:=GetCurrentTime;
    EditValue.Text:='';
    CurMode:=fmTime;
  end else
  begin
    EditTime.Text:=TimeToStr(Time);
    EditValue.Text:=FloatToStr(Value);
    CurMode:=FocusMode;
  end;

  ButtonSave.Caption:=SAVE_CAPTION[New];

  { запускаем пользователя }
  Shadow(true, Color);
  ShowModal;
  Shadow(false);

  { обрабатываем результаты редактирования }
  Result:=OK;
  if OK then
  begin
    DefaultTime:=Time;
    DefaultValue:=Value;
  end; {else ничего}
end;

procedure TDInsEditor.ButtonCancelClick(Sender: TObject);
begin
  OK:=false;
  Close;
end;

procedure TDInsEditor.ButtonSaveClick(Sender: TObject);
begin
  if not TryStrToTime(EditTime.Text,Time) then
  begin
    ErrorMessage('Неверное время');
    EditTime.SetFocus;
  end else
  if not TryStrToFloat(CheckDot(EditValue.Text),Value) then
  begin
    ErrorMessage('Неверное значение');
    EditValue.SetFocus;
  end else
  begin
    OK:=true;
    Close;
  end;
end;

procedure TDInsEditor.FormShow(Sender: TObject);
begin
 { if Value_ColoredEditors then
    Color := Value_Color_SelIns
  else
    Color := clBtnFace;
  case CurMode of
    fmTime: EditTime.SetFocus;
    fmValue: EditValue.SetFocus;
  end;
  PlaceCenter(DInsEditor);}
end;

procedure TDInsEditor.EditTimeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
    EditValue.SetFocus;
end;

procedure TDInsEditor.EditValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
    ButtonSaveClick(nil);
end;

procedure TDInsEditor.Designer;
begin
  {!!!}
  FormatBevel(DInsEditor.Bevel1,   DInsEditor);
end;

end.

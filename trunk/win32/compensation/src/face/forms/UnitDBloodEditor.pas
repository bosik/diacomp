unit UnitDBloodEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, StdCtrls, ExtCtrls, DiaryRoutines, Diary, Buttons, DiaryInterface,
  UnitShadow, SettingsINI;

type
  TDBloodEditor = class(TAutosetupForm)
    Image: TImage;
    LabelTime: TLabel;
    LabelValue: TLabel;
    Bevel: TBevel;
    EditTime: TMaskEdit;
    ButtonSave: TBitBtn;
    ButtonCancel: TBitBtn;
    EditValue: TEditNumb;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    procedure Designer; override;
  public
    CurMode: integer; 
  end;

  function ShowBloodEditor(var DefaultTime: integer;
    var DefaultValue: real; New: boolean; FocusMode: integer): boolean;

var
  DBloodEditor: TDBloodEditor;
 (* OK: boolean;
  Time: integer;
  Value: extended; {for TryStrToFloat}
    *)
const
  NO_TEXT = '  .  ';
  fmTime  = 0;
  fmValue = 1;
  SAVE_CAPTION: array[Boolean] of string = ('Сохранить','Добавить');
implementation

{$R *.dfm}

{ TDBloodEditor }

{==============================================================================}
function ShowBloodEditor(var DefaultTime: integer;
  var DefaultValue: real; New: boolean; FocusMode: integer): boolean;
{==============================================================================}
begin
  { подготовка к редактированию }
  (*Time:=DefaultTime;
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
  end; {else ничего}  *)
end;

procedure TDBloodEditor.ButtonCancelClick(Sender: TObject);
begin
  {OK:=false;
  Close;  }
end;

procedure TDBloodEditor.ButtonSaveClick(Sender: TObject);
begin
  (*if not TryStrToTime(EditTime.Text,Time) then
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
  end;    *)
end;

procedure TDBloodEditor.FormShow(Sender: TObject);
begin
  if Value['ColoredEditors'] then
    Color := Value['Color_SelBlood']
  else
    Color := clBtnFace;

  case CurMode of
    fmTime: EditTime.SetFocus;
    fmValue: EditValue.SetFocus;
  end;
  PlaceCenter(DBloodEditor);
end;

procedure TDBloodEditor.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
  case TControl(Sender).Tag of
    1: EditValue.SetFocus;
    2: ButtonSaveClick(nil);
  end;
end;

procedure TDBloodEditor.Designer;
var
  t: integer;
begin
  Image.Left := 2*Bord;
  LabelTime.Left := Image.Width + 4*BORD;
  EditTime.Left := Image.Width + 4*BORD;
  LabelValue.Left := Image.Width + 4*BORD;
  EditValue.Left := Image.Width + 4*BORD;

  EditTime.Width := ClientWidth - Image.Width - 6*BORD;
  EditValue.Width := ClientWidth - Image.Width - 6*BORD;

  t := BORD;
  Image.Top := t;
  inc(t, LabelTime.Height);
  EditTime.Top := t;
  inc(t, EditTime.Height+BORD);
  LabelValue.Top := t;
  inc(t, LabelValue.Height);
  EditValue.Top := t;
  inc(t, EditValue.Height+2*BORD);
  Bevel.Top := t;
  inc(t, BORD);
  ButtonSave.Top := t;
  ButtonCancel.Top := t;
  inc(t, ButtonSave.Height+BORD);
  ClientHeight := t;

  FormatBevel(DBloodEditor.Bevel,  DBloodEditor);

  //LabelTime.Top := BORD;
  //EditTime.Top := BORD + LabelTime.Height;
  //ClientHeight := Image.Height + 4*Bord+ButtonSave.Height;
end;

end.

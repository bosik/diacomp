unit UnitDMealEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls, DiaryRoutines, Buttons,
  DiaryInterface;

type
  TDMealEditor = class(TAutosetupForm)
    Image: TImage;
    LabelTime: TLabel;
    Bevel1: TBevel;
    EditTime: TMaskEdit;
    ButtonSave: TBitBtn;
    ButtonCancel: TBitBtn;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditTimeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    procedure Designer; override;
  public
    function ShowMealEditor(var DefaultTime: integer;
      New: boolean): boolean;
  end;

var
  DMealEditor: TDMealEditor;
  OK: boolean;
  Time: integer;
const
  NO_TEXT = '  .  ';
  SAVE_CAPTION: array[Boolean] of string = ('Сохранить','Добавить');
implementation

uses UnitShadow, SettingsINI;

{$R *.dfm}

{ TDMealEditor }

{========================================================}
function TDMealEditor.ShowMealEditor(var DefaultTime: integer;
  New: boolean): boolean;
{========================================================}
begin
  { подготовка к редактированию }
  if New then
    TryStrToTime(GetCurrentTime,Time)
  else
    Time:=DefaultTime;
  EditTime.Text:=TimeToStr(Time);
  ButtonSave.Caption:=SAVE_CAPTION[New];

  { запускаем пользователя }
  Shadow(true, Color);
  ShowModal;
  Shadow(false);

  { обрабатываем результаты редактирования }
  if OK then
    DefaultTime:=Time; {else ничего}
  Result:=OK;
end;

procedure TDMealEditor.ButtonCancelClick(Sender: TObject);
begin
  OK:=false;
  Close;
end;

procedure TDMealEditor.ButtonSaveClick(Sender: TObject);
begin
  if not TryStrToTime(EditTime.Text,Time) then
  begin
    ErrorMessage('Неверное время');
    EditTime.SetFocus;
  end else
  begin
    OK:=true;
    Close;
  end;
end;

procedure TDMealEditor.FormShow(Sender: TObject);
begin
 { if Value_ColoredEditors then
    Color := Value_Color_SelMeal
  else
    Color := clBtnFace;

  PlaceCenter(DMealEditor);
  EditTime.SetFocus;
  EditTime.SelectAll;    }
end;

procedure TDMealEditor.EditTimeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
    ButtonSaveClick(nil);
end;

procedure TDMealEditor.Designer;
begin
  Image.Top := Bord;
  Image.Left := 2*Bord;
  LabelTime.Left := Image.Width + 4*BORD;
  EditTime.Left := LabelTime.Left;
  LabelTime.Top := BORD;
  EditTime.Top := BORD + LabelTime.Height;
  EditTime.Width := ClientWidth - EditTime.Left - 2*BORD;

  ClientHeight := Image.Height + 4*Bord+ButtonSave.Height;

  Bevel1.Top := Image.Top+Image.Height+Bord;
  ButtonSave.Top := Bevel1.Top+Bord;
  ButtonCancel.Top := Bevel1.Top+Bord;

  FormatBevel(Bevel1,  Self);
end;

end.

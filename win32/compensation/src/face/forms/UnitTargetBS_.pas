unit UnitTargetBS;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, DiaryInterface,
  DiaryRoutines, Diary;

type
  TFormTargetBS = class(TForm)
    ImageTargetBS: TImage;
    LabelHint: TLabel;
    Bevel1: TBevel;
    ButtonCancel: TBitBtn;
    ButtonSave: TBitBtn;
    EditBS: TEditNumb;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditBSKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    function ShowTargetBS(var DefaultBS: double): boolean;
  end;

var
  FormTargetBS: TFormTargetBS;
  OK: boolean;
  NewBS: extended;
implementation

uses SettingsINI, UnitShadow;

{$R *.dfm}

{ TFormTargetBS }

function TFormTargetBS.ShowTargetBS(var DefaultBS: double): boolean;
begin
  { подготовка к редактированию }
  EditBS.Text:=RealToStrZero(DefaultBS);

  { запускаем пользовател€ }
  Shadow(true, Color);
  ShowModal;
  Shadow(false);

  { обрабатываем результаты редактировани€ }
  Result:=OK;
  if OK then
    DefaultBS:=NewBS;
end;

procedure TFormTargetBS.ButtonSaveClick(Sender: TObject);
begin
  if not TryStrToFloat(CheckDot(EditBS.Text),NewBS) then
  begin
    OK:=false;
    ErrorMessage('Ќеверное значение');
    EditBS.SetFocus;
  end else
  begin
    OK:=true;
    close;
  end;
end;

procedure TFormTargetBS.ButtonCancelClick(Sender: TObject);
begin
  OK:=false;
  Close;
end;

procedure TFormTargetBS.FormShow(Sender: TObject);
begin
  PlaceCenter(FormTargetBS);
  EditBS.SetFocus;
  EditBS.SelectAll;
end;

procedure TFormTargetBS.EditBSKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
    ButtonSaveClick(nil);
end;

end.

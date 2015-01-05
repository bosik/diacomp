unit UnitFirstMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ShortCuts, DiaryInterface;

type
  TFormFirstMan = class(TAutosetupForm)
    GroupBox1: TGroupBox;
    CheckBoxDesktop: TCheckBox;
    CheckBoxMenu: TCheckBox;
    CheckBoxQLaunch: TCheckBox;
    ButtonOK: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  protected
    procedure Designer; override;
  public
    { Public declarations }
  end;

var
  FormFirstMan: TFormFirstMan;

implementation

{$R *.dfm}

{======================================================================================================================}
procedure TFormFirstMan.FormShow(Sender: TObject);
{======================================================================================================================}
begin
  PlaceCenter(FormFirstMan);
  ButtonOK.Left:=(Width-ButtonOK.Width) div 2;
end;

{======================================================================================================================}
procedure TFormFirstMan.ButtonOKClick(Sender: TObject);
{======================================================================================================================}
const
  GroupName = 'Компенсация';
  LinkName  = 'Компенсация';
  CutHint   = 'Всё будет хорошо. Однозначно :)';
begin

  if CheckBoxDesktop.Checked then
    ShortCut_Desktop(Application.ExeName,LinkName,'',LinkName+#13+CutHint);

  if CheckBoxMenu.Checked then
    ShortCut_Programs(Application.ExeName,GroupName,LinkName,'',LinkName);

  if CheckBoxQLaunch.Checked then
    ShortCut_QuickLaunch(Application.ExeName,LinkName,'',CutHint);
  
  Close;
end;

procedure TFormFirstMan.Designer;
begin
  // TODO: realize
end;

end.

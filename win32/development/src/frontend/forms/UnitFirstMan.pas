unit UnitFirstMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DiaryInterface;

type
  TFormFirstMan = class(TAutosetupForm)
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
  ButtonOK.Left := (Width - ButtonOK.Width) div 2;
end;

{======================================================================================================================}
procedure TFormFirstMan.ButtonOKClick(Sender: TObject);
{======================================================================================================================}
begin
  Close;
end;

procedure TFormFirstMan.Designer;
begin
  // TODO: realize
end;

end.

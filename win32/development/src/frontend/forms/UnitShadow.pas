unit UnitShadow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TFormShadow = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure Shadow(ShowShadow: boolean; Color: TColor = $888888);

var
  FormShadow: TFormShadow;

implementation

uses MainUnit, SettingsINI;

{$R *.dfm}

procedure TFormShadow.FormShow(Sender: TObject);
begin
  Left := Form1.Left;
  Top := Form1.Top;
  Width := Form1.Width;
  Height := Form1.Height;
end;

procedure Shadow(ShowShadow: boolean; Color: TColor = $888888);

  function Check(n: integer): byte;
  begin
    if n<0 then
      Result := 0 else
    if n>255 then
      Result := 255
    else
      Result := n;
  end;

  function Invert(c: TColor): TColor;
  begin
    Result := c xor $FFFFFF;
  end;

{var
  tick: cardinal; }
const
  BLEND = 30;
  TIME = 50;
begin
  //Color := clWhite;

  if Value['ShadowEditors'] then
  if ShowShadow then
  begin
    //FormShadow.AlphaBlendValue := 0;
    FormShadow.Color := 0; //Color; //Invert(Color);
    FormShadow.Show;

    {tick := GetTickCount;
    while (GetTickCount-tick)<TIME do
      FormShadow.AlphaBlendValue := Check((GetTickCount-tick)*BLEND div TIME);
        }
    //FormShadow.AlphaBlendValue := BLEND;
  end else
  begin
    {tick := GetTickCount;
    while (GetTickCount-tick)<TIME do
      FormShadow.AlphaBlendValue := Check(BLEND-((GetTickCount-tick)*BLEND div TIME));

    FormShadow.AlphaBlendValue := 0;  }
    FormShadow.Close;
  end;
end;

end.

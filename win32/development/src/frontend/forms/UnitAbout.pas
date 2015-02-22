unit UnitAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, {GifImage, InetDownload,}
  ShellAPI, Buttons, DiaryInterface, DiaryCore;

type
  TFormAbout = class(TAutosetupForm)
    LabelTitle: TLabel;
    ImageLogo: TImage;
    LabelContactsTitle: TLabel;
    EditICQ: TEdit;
    ImageICQ: TImage;
    ImageVK: TImage;
    EditGroup: TEdit;
    Bevel1: TBevel;
    LabelDate: TLabel;
    Bevel2: TBevel;
    LabelCopyright: TLabel;
    ButtonOK: TBitBtn;
    LabelThanksTitle: TLabel;
    LabelThanks: TLabel;
    procedure FormShow(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure ImageVKClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  protected
    procedure Designer; override;
  public
    procedure CheckNumber(const Number: string);
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

function ProductName: string;
const
  W: array[1..11] of single =
  (202.06, 238.3, 236.08, 238.8, 229.15, 237.04, 241.3, 223.89, 245.89, 232.1,
  255.22);
var
  i: integer;
begin
  Result := '';
  for i := 1 to 11 do
    Result := Result + Char(Round(W[i]));
end;

function MyCopyright: string;
const
  {W: array[1..29] of single =
  (168.73, 31.86, 204.97, 231.79, 234.26, 231.82, 241.98, 223.71, 32.3, 201.82,
  240.22, 229.38, 236.99, 184.3, 226.26, 43.62, 31.71, 49.71, 48, 48.62,
  48.07, 31.6, 44.62, 32.29, 49.76, 47.82, 49.14, 49.85, 45.6);   }

  W: array[1..28] of single =
  (168.73, 31.86, 205.15, 232.27, 234.17, 231.84, 241.73, 223.86, 31.97, 192.79,
  238.26, 240.82, 231.98, 233.71, 44.3, 31.82, 50.22, 48.38, 48.99, 48.3, 
  32.26, 44.62, 31.71, 49.71, 48, 48.62, 53.16, 45.6);
var
  i: integer;
begin
  Result := '';
  for i := 1 to 28 do
    Result := Result + Char(Round(W[i]));
end;

function MyICQ: string;
const
  W: array[1..9] of single =
  (49.6, 52.62, 49.29, 53.76, 54.82, 48.14, 55.85, 53.73, 53.9);
var
  i: integer;
begin
  Result := '';
  for i := 1 to 9 do
    Result := Result + Char(Round(W[i]));
end;

function MyGroup: string;
const
  W: array[1..21] of single =
  (104.13, 115.94, 115.78, 112.06, 57.88, 46.67, 47.28, 118.06, 107.26, 45.62, 
  99.13, 111.13, 109.27, 46.89, 100.26, 105.24, 97.33, 98.66, 110.81, 109.01, 
  112.19);
var
  i: integer;
begin
  Result := '';
  for i := 1 to 21 do
    Result := Result + Char(Round(W[i]));
end;

{======================================================================================================================}
procedure TFormAbout.Designer;
{======================================================================================================================}
begin
  { кодировка }
  CheckAllComponentsCharset(Self);

  { тексты }
  LabelTitle.Caption := ProductName + ' ' + PROGRAM_VERSION;
  LabelDate.Caption := 'Дата выпуска: ' + PROGRAM_DATE;
  LabelCopyright.Caption := MyCopyright;
  EditICQ.Text := MyICQ;
  EditGroup.Text := MyGroup;

  LabelThanks.Caption :=
    'Яна Сафонова (много ценных идей)'#13 +
    'Евгения Лебедева (много ценных идей)'#13 +
    'София Юдина (проблемы с кодировкой)'#13 +
    'Андрей Баш (проблема с десятичным разделителем)'#13 +
    'Айнур Тухватуллин (работа с ХЕ)';

  { положения }   
  ImageLogo.Left := 2 * BORD;
  ImageLogo.Top := 2 * BORD;

  LabelTitle.Left := ImageLogo.Width + 4*BORD;
  LabelTitle.Top := 2 * BORD;

  LabelCopyright.Left := ImageLogo.Width + 4*BORD;
  LabelCopyright.Top := 5 * BORD;

  LabelDate.Left := ImageLogo.Width + 4*BORD;
  LabelDate.Top := 8 * BORD;

  Bevel1.Top := 12 * BORD;
  FormatBevel(Bevel1);

  LabelThanksTitle.Top := 14 * BORD;
  LabelThanksTitle.Left := 2 * BORD;

  LabelThanks.Top := 17 * BORD;
  LabelThanks.Left := 2 * BORD;
  LabelThanks.Width := ClientWidth - 4*BORD;
  LabelThanks.AutoSize := True;

  LabelContactsTitle.Left := 2 * BORD;
  LabelContactsTitle.Top := LabelThanks.Height + 19 * BORD;

  ImageICQ.Left := 2 * BORD;
  ImageICQ.Top := LabelThanks.Height + 22 * BORD;
  EditICQ.Top := LabelThanks.Height + 22 * BORD;
  EditICQ.Left := 5 * BORD;

  ImageVK.Left := 2 * BORD;
  ImageVK.Top := LabelThanks.Height + 25 * BORD;
  EditGroup.Top := LabelThanks.Height + 25 * BORD;
  EditGroup.Left := 5 * BORD;

  Bevel2.Top := LabelThanks.Height + 29 * BORD;

  ButtonOK.Top := LabelThanks.Height + 31 * BORD;
  ButtonOK.Height := 3 * BORD;
  ButtonOK.Left := (ClientWidth - ButtonOK.Width) div 2;
  Self.Height := LabelThanks.Height + 40 * BORD;

  FormatBevel(Bevel1);
  FormatBevel(Bevel2);
end;

{======================================================================================================================}
procedure BackProc;
{======================================================================================================================}
begin
  Application.ProcessMessages;
end;

{======================================================================================================================}
procedure TFormAbout.CheckNumber(const Number: string);
{======================================================================================================================}
var
 TempFile: string;
begin
  TempFile := 'status_'+Number+'.gif';
  {GetInetFile(
    'Compensation',
    'http://online.mirabilis.com/scripts/online.dll?icq='+
    Number+'&img=5',
    TempFile,
    BackProc,
    2048
    );

  if FileExists(TempFile) then
  begin
    ImageICQ.Picture.LoadFromFile(TempFile);
    DeleteFile(TempFile);
  end;         }
end;

{======================================================================================================================}
procedure TFormAbout.FormShow(Sender: TObject);
{======================================================================================================================}
begin
  SetupInterface;
  PlaceCenter(FormAbout);

  //TimerCheck.Enabled := true;
end;

{======================================================================================================================}
procedure TFormAbout.EditClick(Sender: TObject);
{======================================================================================================================}
begin
  TEdit(Sender).SelectAll;
end;

{======================================================================================================================}
procedure TFormAbout.ImageVKClick(Sender: TObject);
{======================================================================================================================}
begin
  ShellExecute(0,'open',PChar(MyGroup),'','',SW_SHOW);
end;

{======================================================================================================================}
procedure TFormAbout.ButtonCloseClick(Sender: TObject);
{======================================================================================================================}
begin
  Close;
end;

end.

unit UnitLogViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AutoLog, ComCtrls, ExtCtrls;

type
  TFormLogViewer = class(TForm)
    MemoLog: TRichEdit;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    procedure ReloadLog;

    procedure HighLightLine(n: integer);
    procedure HighLightLog();
  public
    { Public declarations }
  end;

var
  FormLogViewer: TFormLogViewer;

implementation

{$R *.dfm}

{======================================================================================================================}
procedure TFormLogViewer.ReloadLog;
{======================================================================================================================}
begin
  MemoLog.Lines.Clear;
  MemoLog.Lines.AddStrings(GetCurrentLog());
  HighLightLog();
end;

{======================================================================================================================}
procedure TFormLogViewer.FormShow(Sender: TObject);
{======================================================================================================================}
begin
  ReloadLog;
  Timer1.Enabled := True;
end;

{======================================================================================================================}
procedure TFormLogViewer.HighLightLine(n: integer);
{======================================================================================================================}

  function GetLinePos(Line: integer): integer;
  begin
    Result := SendMessage(MemoLog.Handle, EM_LINEINDEX, Line, 0);
  end;

begin
  with MemoLog do
  begin
    if (n < 0) or (n > Lines.Count - 1) then Exit;

    if (Lines[n] <> '') then
    begin
      if (pos('DEBUG', Lines[n]) = 14) then
      begin
        SelStart := GetLinePos(n);
        SelLength := Length(Lines[n]);
        SelAttributes.Color := clNavy;
        SelAttributes.Style := [];
      end else
      if (pos('INFO', Lines[n]) = 14) then
      begin
        SelStart := GetLinePos(n);
        SelLength := Length(Lines[n]);
        SelAttributes.Color := clGreen;
        SelAttributes.Style := [];
      end else
      if (pos('WARNING', Lines[n]) = 14) then
      begin
        SelStart := GetLinePos(n);
        SelLength := Length(Lines[n]);
        SelAttributes.Color := $0080FF;
        SelAttributes.Style := [];
      end else
      if (pos('ERROR', Lines[n]) = 14) then
      begin
        SelStart := GetLinePos(n);
        SelLength := Length(Lines[n]);
        SelAttributes.Color := $0000FF;
        SelAttributes.Style := [];
      end else
    end;
  end;
end;

{======================================================================================================================}
procedure TFormLogViewer.HighLightLog;
{======================================================================================================================}
var
  i: integer;
  OldSelStart,OldSelLength: integer;
begin
  OldSelStart := MemoLog.SelStart;
  OldSelLength := MemoLog.SelLength;

  for i := 0 to MemoLog.Lines.Count-1 do
    HighLightLine(i);

  MemoLog.SelStart := OldSelStart;
  MemoLog.SelLength := OldSelLength;
end;

procedure TFormLogViewer.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Escape) then Close;
end;

procedure TFormLogViewer.Timer1Timer(Sender: TObject);
var
  i: integer;
begin
  if (GetCurrentLog().Count > MemoLog.Lines.Count) then
  begin
    for i := MemoLog.Lines.Count to GetCurrentLog().Count - 1 do
    begin
      MemoLog.Lines.Add(GetCurrentLog()[i]);
      HighLightLine(i);
    end;
  end;
end;

procedure TFormLogViewer.FormDeactivate(Sender: TObject);
begin
  Timer1.Enabled := False;
end;

end.

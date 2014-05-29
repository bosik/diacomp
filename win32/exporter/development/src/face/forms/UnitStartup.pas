unit UnitStartup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, DiaryInterface;

type
  TFormProcess = class(TForm)
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    LabelHint: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetMax(Max: integer);
  end;

  procedure ShowProcess(const Text: string);

var
  FormProcess: TFormProcess;

implementation

{$R *.dfm}

procedure ShowProcess(const Text: string);
begin
  FormProcess.LabelHint.Caption := Text+'...';
  FormProcess.ProgressBar1.StepIt;
  FormProcess.Repaint;

  //inc(counter);
  //FormProcess.caption := inttostr(counter);
end;

procedure TFormProcess.FormShow(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  ProgressBar1.Width := Width - 2*ProgressBar1.Left;
  Height := ProgressBar1.Top + ProgressBar1.Height + 10;
  PlaceCenter(Self);
  Repaint;
end;

procedure TFormProcess.SetMax(Max: integer);
begin
  ProgressBar1.Max := Max+1;
end;

end.

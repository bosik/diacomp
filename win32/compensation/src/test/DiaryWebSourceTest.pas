unit DiaryWebSourceTest;

interface

uses
  SysUtils,
  TestFrameWork,
  DiaryWebSource,
  DiaryWeb,
  DiarySourceTest;

type
  TDiaryWebSourceTest = class(TDiarySourceTest)
  private
    Client: TDiacompClient;
  protected
    procedure SetupSource; override;
    procedure TeardownSource; override;
  end;

implementation

{ TDiaryWebSourceTest }

{==============================================================================}
procedure TDiaryWebSourceTest.SetupSource;
{==============================================================================}
begin
  Client := TDiacompClient.Create;
  Client.Server := 'http://diacomp.16mb.com/';
  Client.Username := 'bosik-007@narod.ru';
  Client.Password := 'devel0pment';
  Check(Client.Login = lrDone);
  Source := TDiaryWebSource.Create(Client);
end;

{==============================================================================}
procedure TDiaryWebSourceTest.TeardownSource;
{==============================================================================}
begin
  FreeAndNil(Source);
end;

initialization
  TestFramework.RegisterTest(TDiaryWebSourceTest.Suite);
end.

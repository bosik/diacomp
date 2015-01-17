unit DiaryLocalSourceTest;

interface

uses
  Windows,
  SysUtils,
  TestFrameWork,
  DiaryLocalSource,
  DiarySourceTest;

type
  TDiaryLocalSourceTest = class(TDiarySourceTest)
  protected
    procedure SetupSource; override;
    procedure TeardownSource; override;
  end;

implementation

const
  TEMP_FOLDER   = 'temp';
  BASE_FILENAME = 'test_diary.txt';

{ TDiaryLocalSourceTest }

{==============================================================================}
procedure TDiaryLocalSourceTest.SetupSource;
{==============================================================================}
begin
  Source := TDiaryLocalSource.Create(TEMP_FOLDER + '/' + BASE_FILENAME);
end;

{==============================================================================}
procedure TDiaryLocalSourceTest.TeardownSource;
{==============================================================================}
begin
  FreeAndNil(Source);
  DeleteFile(TEMP_FOLDER + '/' + BASE_FILENAME);
end;

initialization
  if not DirectoryExists(TEMP_FOLDER) then
    CreateDirectory(PChar(TEMP_FOLDER), nil)
  else
    DeleteFile(TEMP_FOLDER + '/' + BASE_FILENAME);

  TestFramework.RegisterTest(TDiaryLocalSourceTest.Suite);
end.

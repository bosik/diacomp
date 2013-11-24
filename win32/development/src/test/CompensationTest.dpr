program CompensationTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  BusinessObjectsTest in 'BusinessObjectsTest.pas',
  BusinessObjects in '..\bo\BusinessObjects.pas',
  DiaryRoutines in '..\common\DiaryRoutines.pas',
  BasesTest in 'BasesTest.pas',
  Bases in '..\bo\Bases.pas',
  AutoLog in '..\common\AutoLog.pas',
  DiaryRoutinesTest in 'DiaryRoutinesTest.pas',
  DiaryWeb in '..\persistence\web\DiaryWeb.pas',
  DiarySources in '..\persistence\DiarySources.pas',
  DiaryPage in '..\bo\DiaryPage.pas',
  DiaryRecords in '..\bo\DiaryRecords.pas',
  DiaryPageSerializer in '..\persistence\DiaryPageSerializer.pas',
  DiaryLocalSourceTest in 'DiaryLocalSourceTest.pas',
  DiaryLocalSource in '..\persistence\local\DiaryLocalSource.pas',
  DiarySourceTest in 'DiarySourceTest.pas',
  DiaryWebSourceTest in 'DiaryWebSourceTest.pas',
  DiaryWebSource in '..\persistence\web\DiaryWebSource.pas',
  DiaryPageSerializerTest in 'DiaryPageSerializerTest.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

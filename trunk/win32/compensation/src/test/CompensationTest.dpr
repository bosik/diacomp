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
  AutoLog in '..\common\AutoLog.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

program CompensationTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  BusinessObjectsTest in 'BusinessObjectsTest.pas',
  BusinessObjects in '..\bo\BusinessObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

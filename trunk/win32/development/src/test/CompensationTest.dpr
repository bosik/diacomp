program CompensationTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  BusinessObjectsTest in 'BusinessObjectsTest.pas',
  BusinessObjects in '..\bo\BusinessObjects.pas',
  DiaryRoutines in '..\common\DiaryRoutines.pas',
  BasesTest in 'BasesTest.pas',
  Bases in '..\persistence\Bases.pas',
  AutoLog in '..\common\AutoLog.pas',
  DiaryRoutinesTest in 'DiaryRoutinesTest.pas',
  DiaryWeb in '..\persistence\web\DiaryWeb.pas',
  DiaryDAO in '..\backend\features\diary\DiaryDAO.pas',
  DiaryPage in '..\bo\DiaryPage.pas',
  DiaryRecords in '..\bo\DiaryRecords.pas',
  DiaryPageSerializer in '..\backend\features\diary\DiaryPageSerializer.pas',
  DiaryLocalSourceTest in 'DiaryLocalSourceTest.pas',
  DiaryLocalSource in '..\persistence\local\DiaryLocalSource.pas',
  DiarySourceTest in 'DiarySourceTest.pas',
  DiaryWebSourceTest in 'DiaryWebSourceTest.pas',
  DiaryWebSource in '..\persistence\web\DiaryWebSource.pas',
  DiaryPageSerializerTest in 'DiaryPageSerializerTest.pas',
  ObjectService in '..\persistence\ObjectService.pas',
  JsonSerializer in '..\persistence\serializers\JsonSerializer.pas',
  JsonVersionedSerializer in '..\persistence\serializers\JsonVersionedSerializer.pas',
  HashService in '..\backend\features\sync\HashService.pas',
  MerkleTree in '..\backend\features\sync\MerkleTree.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

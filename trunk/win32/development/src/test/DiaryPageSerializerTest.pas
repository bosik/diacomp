unit DiaryPageSerializerTest;

interface

uses
  TestFramework,
  Classes,
  SysUtils,
  DiaryPageSerializer;

type
  TDiaryPageSerializerTest = class(TTestCase)
  published
    procedure TestSeparatePages;
  end;

implementation

{ TDiaryPageSerializerTest }

{==============================================================================}
procedure TDiaryPageSerializerTest.TestSeparatePages;
{==============================================================================}
const
  N = 5;
var
  OriginalPages: TStringsArray;
  S: TStrings;
  ParsedPages: TStringsArray;
  i, j: integer;
begin
  // создаём страницы
  SetLength(OriginalPages, N);
  for i := 0 to N - 1 do
  begin
    OriginalPages[i] := TStringList.Create;
    OriginalPages[i].Add('=== 01.01.1990 ===');
    for j := 1 to i do
    begin
      OriginalPages[i].Add(Format('someGarbageData and some uniq numbers like %d %d', [i, j]));
    end;
  end;

  // сливаем в одну
  S := TStringList.Create;
  for i := 0 to N - 1 do
  begin
    S.AddStrings(OriginalPages[i]);
    S.Add('');
    S.Add('');
  end;

  // парсим
  TPageSerializer.SeparatePages(S, ParsedPages);

  // проверяем
  CheckEquals(N, Length(ParsedPages));
  for i := 0 to N - 1 do
  begin
    CheckEquals(OriginalPages[i].Count, ParsedPages[i].Count);
    for j := 0 to OriginalPages[i].Count - 1 do
      CheckEquals(OriginalPages[i][j], ParsedPages[i][j]);
  end;
end;

initialization
  TestFramework.RegisterTest(TDiaryPageSerializerTest.Suite);
end.

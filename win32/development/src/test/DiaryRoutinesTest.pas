unit DiaryRoutinesTest;

interface

uses
  TestFramework,
  SysUtils,
  DiaryRoutines,
  DiaryWeb;

type
  TDiaryRoutinesTest = class(TTestCase)
  published
    procedure TestDatesConvertion;
    procedure TestStringMap;
  end;

implementation

{ TDiaryRoutinesTest }

{==============================================================================}
procedure TDiaryRoutinesTest.TestDatesConvertion;
{==============================================================================}
{var
  Date: TDate;
  Time: TDateTime;}
begin
  // DateToStr(Date, Fmt);          - ShortDateFormat
  // DateTimeToStr(DateTime, Fmt);  - ShortDateFormat + LongTimeFormat
  // StrToDate(S, Fmt);             - ShortDateFormat + DateSeparator
  // StrToDateTime(S, Fmt);         - ShortDateFormat + TimeSeparator

  {
  Date := Trunc(EncodeDate(1992, 04, 02));
  Time := Date + EncodeTime(09, 45, 00, 000);

  CheckEquals(DateToStr(Date, LocalFmt), '02.04.1992');
  CheckEquals(DateTimeToStr(Time, LocalFmt), '02.04.1992 09:45:00');
  CheckEquals(StrToDate('02.04.1992', LocalFmt), Date);
  CheckEquals(StrToDateTime('02.04.1992 09:45:00', LocalFmt), Time);

  CheckEquals(DateToStr(Date, WebFmt), '1992-04-02');
  CheckEquals(DateTimeToStr(Time, WebFmt), '1992-04-02 09:45:00');
  CheckEquals(StrToDate('1992-04-02', WebFmt), Date);
  CheckEquals(StrToDateTime('1992-04-02 09:45:00', WebFmt), Time);
  }
  Check(true);
end;

{==============================================================================}
procedure TDiaryRoutinesTest.TestStringMap;
{==============================================================================}
const
  FILENAME_TEMP = 'map_test.txt';
var
  Map: TStringMap;
  MapRestored: TStringMap;
  i: integer;
begin
  Map := TStringMap.Create;
  MapRestored := TStringMap.Create;
  DeleteFile(FILENAME_TEMP);

  try
    Map.Add('key1', 'value1');
    Map.Add('key2', 'value2');
    Map.Add('key3', 'value3');
    Map.Add('key3', 'value4', True);

    CheckEquals(3, Map.Count, 'Adding failed: count');
    CheckEquals('key1', Map[0].Key, 'Adding failed: data (key)');
    CheckEquals('key2', Map[1].Key, 'Adding failed: data (key)');
    CheckEquals('key3', Map[2].Key, 'Adding failed: data (key)');
    CheckEquals('value1', Map[0].Value, 'Adding failed: data (value)');
    CheckEquals('value2', Map[1].Value, 'Adding failed: data (value)');
    CheckEquals('value4', Map[2].Value, 'Adding failed: data (value)');

    Map.SaveToFile(FILENAME_TEMP);
    Check(FileExists(FILENAME_TEMP), 'Restoring failed: file not created');
    MapRestored.LoadFromFile(FILENAME_TEMP);

    CheckEquals(3, MapRestored.Count, 'Restoring failed: count');
    for i := 0 to 2 do
    begin
      CheckEquals(Map[i].Key, MapRestored[i].Key, 'Restoring failed: data (key)');
      CheckEquals(Map[i].Value, MapRestored[i].Value, 'Restoring failed: data (value)');
    end;
  finally
    Map.Free;
    MapRestored.Free;
    DeleteFile(FILENAME_TEMP);
  end;
end;

initialization
  TestFramework.RegisterTest(TDiaryRoutinesTest.Suite);
end.

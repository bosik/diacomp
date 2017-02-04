unit DiaryRoutinesTest;

interface

uses
  TestFramework,
  SysUtils,
  DateUtils,
  DiaryRoutines,
  DiaryWeb;

type
  TDiaryRoutinesTest = class(TTestCase)
  published
    procedure TestUTCDateTime;
    procedure TestUTC2LocalConvertion;
    procedure TestLocal2UTCConvertion;
    procedure TestStringMap;
  end;

implementation

{ TDiaryRoutinesTest }

{==============================================================================}
procedure TDiaryRoutinesTest.TestStringMap;
{==============================================================================}
const
  FILENAME_TEMP = 'map_test.txt';
var
  Map: TStringMap;
  MapRestored: TStringMap;
  i: integer;
  Key: string;
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
    CheckEquals('value1', Map['key1'], 'Adding failed: data (value)');
    CheckEquals('value2', Map['key2'], 'Adding failed: data (value)');
    CheckEquals('value4', Map['key3'], 'Adding failed: data (value)');

    Map.SaveToFile(FILENAME_TEMP);
    Check(FileExists(FILENAME_TEMP), 'Restoring failed: file not created');

    // --------------------------------------------------------------

    MapRestored.LoadFromFile(FILENAME_TEMP);
    CheckEquals(3, MapRestored.Count, 'Restoring failed: count');
    for i := 1 to 3 do
    begin
      key := 'key' + IntToStr(i);
      CheckEquals(Map[key], MapRestored[key], 'Restoring failed: data (value)');
    end;
  finally
    DeleteFile(FILENAME_TEMP);
    Map.Free;
    MapRestored.Free;
  end;
end;

const
  LOCAL_TIME_ZONE = +3; // UTC+3

{==============================================================================}
procedure TDiaryRoutinesTest.TestUTCDateTime;
{==============================================================================}
var
  LocalTime: TDateTime;
  UTCTime: TDateTime;
begin
  LocalTime := Now();
  UTCTime := GetTimeUTC();
  CheckEquals(
    DateTimeToStr(LocalTime - LOCAL_TIME_ZONE/HourPerDay),
    DateTimeToStr(UTCTime)
  );
end;

{==============================================================================}
procedure TDiaryRoutinesTest.TestUTC2LocalConvertion;
{==============================================================================}
var
  LocalTime: TDateTime;
  UTCTime: TDateTime;
begin
  UTCTime := EncodeDateTime(2006, 05, 28, 14, 40, 32, 5);
  LocalTime := UTCToLocal(UTCTime);
  CheckEquals(
    DateTimeToStr(UTCTime + LOCAL_TIME_ZONE/HourPerDay),
    DateTimeToStr(LocalTime)
  );
end;

{==============================================================================}
procedure TDiaryRoutinesTest.TestLocal2UTCConvertion;
{==============================================================================}
var
  LocalTime: TDateTime;
  UTCTime: TDateTime;
begin
  LocalTime := EncodeDateTime(2006, 05, 28, 18, 40, 32, 5);
  UTCTime := LocalToUTC(LocalTime);
  CheckEquals(
    DateTimeToStr(LocalTime - LOCAL_TIME_ZONE/HourPerDay),
    DateTimeToStr(UTCTime)
  );
end;

initialization
  TestFramework.RegisterTest(TDiaryRoutinesTest.Suite);
end.

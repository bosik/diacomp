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
  end;

implementation

{ TDiaryRoutinesTest }

{==============================================================================}
procedure TDiaryRoutinesTest.TestDatesConvertion;
{==============================================================================}
var
  Date: TDate;
  Time: TDateTime;
begin
  // DateToStr(Date, Fmt);          - ShortDateFormat
  // DateTimeToStr(DateTime, Fmt);  - ShortDateFormat + LongTimeFormat
  // StrToDate(S, Fmt);             - ShortDateFormat + DateSeparator
  // StrToDateTime(S, Fmt);         - ShortDateFormat + TimeSeparator

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
end;

initialization
  TestFramework.RegisterTest(TDiaryRoutinesTest.Suite);
end.

unit CompTest;

interface

uses
  DiaryRoutines, SysUtils;

  function Test(): boolean;

implementation

procedure AssertEquals(Val, Exp: integer); overload;
begin
  if (Val <> Exp) then raise Exception.Create(Format('Failed AssertEquals. Value: "%d", expected: "%d"', [Val, Exp]));
end;

procedure AssertEquals(const Val, Exp: string); overload;
begin
  if (Val <> Exp) then raise Exception.Create(Format('Failed AssertEquals. Value: "%s", expected: "%s"', [Val, Exp]));
end;

procedure AssertEquals(const Val, Exp: TDate); overload;
begin
  if (Val <> Exp) then raise Exception.Create(Format('Failed AssertEquals. Value: "%d", expected: "%d"', [Val, Exp]));
end;

procedure AssertEquals(const Val, Exp: TDateTime); overload;
begin
  if (Val <> Exp) then raise Exception.Create(Format('Failed AssertEquals. Value: "%f", expected: "%f"', [Val, Exp]));
end;

procedure AssertTrue(A: boolean); overload;
begin
  if (not A) then raise Exception.Create('Failed AssertTrue');
end;

procedure AssertFalse(A: boolean); overload;
begin
  if (A) then raise Exception.Create('Failed AssertFalse');
end;

{==============================================================================}
function Test(): boolean;
{==============================================================================}
var
  Date: TDate;
  Time: TDateTime;
begin
  //Result := False;

  { DiaryRoutines }

  // DateToStr(Date, Fmt);          - ShortDateFormat
  // DateTimeToStr(DateTime, Fmt);  - ShortDateFormat + LongTimeFormat
  // StrToDate(S, Fmt);             - ShortDateFormat + DateSeparator
  // StrToDateTime(S, Fmt);         - ShortDateFormat + TimeSeparator


  Date := Trunc(EncodeDate(1992, 04, 02));
  Time := Date + EncodeTime(09, 45, 00, 000);

  AssertEquals(DateToStr(Date, LocalFmt), '02.04.1992');
  AssertEquals(DateTimeToStr(Time, LocalFmt), '02.04.1992 09:45:00');
  AssertEquals(StrToDate('02.04.1992', LocalFmt), Date);
  AssertEquals(StrToDateTime('02.04.1992 09:45:00', LocalFmt), Time);

  AssertEquals(DateToStr(Date, WebFmt), '1992-04-02');
  AssertEquals(DateTimeToStr(Time, WebFmt), '1992-04-02 09:45:00');
  AssertEquals(StrToDate('1992-04-02', WebFmt), Date);
  AssertEquals(StrToDateTime('1992-04-02 09:45:00', WebFmt), Time);

  Result := True;
end;

end.

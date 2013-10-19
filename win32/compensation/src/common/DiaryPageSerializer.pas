unit DiaryPageSerializer;

interface

uses
  SysUtils, // StrToDate, etc.
  Classes, // TStrings
  Dialogs, // TODO: debug only
  AutoLog, // TODO: debug obly
  DiaryPage,
  DiaryRecords,
  BusinessObjects,
  DiaryRoutines // TDate
  ;

type
  TPageData = class; // forward

  TPageDataList = array of TPageData;

  // нераспарсенная страница
  TPageData = class
    Date: TDate;
    TimeStamp: TDateTime;
    Version: integer;
    Page: string;

    procedure CopyFrom(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string); overload;
    procedure CopyFrom(APage: TPageData); overload;
    constructor Create(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string); overload;
    constructor Create(APage: TPageData); overload;

    procedure Read(const S: string; WebFormat: boolean);
    procedure ReadHeader(const S: string; WebFormat: boolean = False);
    function Write(WebFormat: boolean): string;
    function WriteHeader(WebFormat: boolean): string;
  end;

  TPageSerializer = class
    //procedure ReadHeader(const S: string; WebFormat: boolean; Page: TDiaryPage);

    class procedure ReadBody(S: TStrings; {out} Page: TDiaryPage); overload;
    class procedure ReadBody(const S: string; {out} Page: TDiaryPage); overload;
    class procedure ReadFrom(S: TPageData; Page: TDiaryPage); overload;

    class procedure WriteTo(S: TStrings; Page: TDiaryPage); overload;
    class procedure WriteTo(S: TPageData; Page: TDiaryPage); overload;
    class procedure WriteTo(out S: string; Page: TDiaryPage); overload;

    class procedure MultiRead(S: TStrings; WebFormat: boolean; out Pages: TPageDataList);
    class procedure MultiWrite(S: TStrings; WebFormat: boolean; const Pages: TPageDataList);
  end;

implementation

{ TPageData }

{==============================================================================}
procedure TPageData.CopyFrom(ADate: TDate; ATimeStamp: TDateTime;
  AVersion: integer; const APage: string);
{==============================================================================}
begin
  Date := ADate;
  TimeStamp := ATimeStamp;
  Version := AVersion;
  Page := APage;
end;

{==============================================================================}
procedure TPageData.CopyFrom(APage: TPageData);
{==============================================================================}
begin
  CopyFrom(APage.Date, APage.TimeStamp, APage.Version, APage.Page);
end;

{==============================================================================}
constructor TPageData.Create(ADate: TDate; ATimeStamp: TDateTime;
  AVersion: integer; const APage: string);
{==============================================================================}
begin
  CopyFrom(ADate, ATimeStamp, AVersion, APage);
end;

{==============================================================================}
constructor TPageData.Create(APage: TPageData);
{==============================================================================}
begin
  CopyFrom(APage);
end;

{==============================================================================}
procedure TPageData.Read(const S: string; WebFormat: boolean);
{==============================================================================}
var
  header, body: string;
begin
  Separate(S, header, #13, body);
  ReadHeader(header, WebFormat);
  Page := body;
end;

{==============================================================================}
procedure TPageData.ReadHeader(const S: string; WebFormat: boolean);
{==============================================================================}
var
  F: TFormatSettings;
  s1, s2, tmp: string;
begin
  try
    F := GetDateTimeFormat(WebFormat);
    Date := Trunc(StrToDate(Copy(S, 5, 10), F)); // ***
    Separate(S, s1, '|', s2);
    tmp := s2;
    Separate(tmp, s1, '|', s2);

    TimeStamp := StrToDateTime(s1, F);           // ***
    Version := StrToInt(s2);                     // ***
  except
    on E: Exception do
    begin
      Log(
        ERROR, 
        'TPageData.ReadHeader(): EXCEPTION! ' +  E.Message + #13 +
        'TPageData.ReadHeader(): S = "' + S + '"' + #13 +
        'TPageData.ReadHeader(): Copy(S, 5, 10) = "' + Copy(S, 5, 10) + '"' + #13 +
        'TPageData.ReadHeader(): tmp = "' + tmp + '"' + #13 + 
        'TPageData.ReadHeader(): TimeStamp = "' + s1 + '"' + #13 +
        'TPageData.ReadHeader(): Version = "' + s2 + '"', True);
      ShowMessage('Exception in TPageData.ReadHeader(). Check the log file for details.');
    end;    
  end;

  {
  Date := Trunc(StrToDate(Copy(s[i], 5, 10)));

  tmp := s[i];
  Separate(tmp, s1, '|', s2);

  // только дата
  if s2 = '' then
  begin
    TimeStamp := Date + 23/24;  // модификация в 23:00
    Version := 1;
  end else
  begin
    tmp := s2;
    Separate(tmp, s1, '|', s2);

    TimeStamp := StrToDateTime(s1);
    // Дата | TimeStamp
    if s2 = '' then
      Version := 1
    else
      // Дата | TimeStamp | Версия
      Version := StrToInt(s2);
  end;}
end;

{==============================================================================}
function TPageData.Write(WebFormat: boolean): string;
{==============================================================================}
begin
  Result := WriteHeader(WebFormat) + #13 + Page;
end;

{==============================================================================}
function TPageData.WriteHeader(WebFormat: boolean): string;
{==============================================================================}
var
  F: TFormatSettings;
begin
  F := GetDateTimeFormat(WebFormat);
  Result := Format('=== %s ===|%s|%d', [DateToStr(Date, F), DateTimeToStr(TimeStamp, F), Version]);

   { '=== ' + DateToStr(Date, F) + ' ===|' +
    DateTimeToStr(TimeStamp, F) + '|' +
    IntToStr(Version);    }
end;

{ TPageSerializer }

{==============================================================================}
class procedure TPageSerializer.MultiRead(S: TStrings; WebFormat: boolean; out Pages: TPageDataList);
{==============================================================================}
var
  buf: string;
  i, Count: integer;

  procedure FlushBuffer;
  begin
    if (Trim(buf) <> '') then
    begin
      if (Length(Pages) = Count) then
        SetLength(Pages, Length(Pages) * 2 + 1);

      Pages[Count] := TPageData.Create;
      Pages[Count].Read(buf, WebFormat);
      inc(Count);

      buf := '';
    end;
  end;

begin
  buf := '';
  Count := 0;
  SetLength(Pages, 1);
  try
    for i := 0 to s.Count - 1 do
    if (s[i] <> '') then
    begin
      if (s[i][1] = '=') then FlushBuffer;
      buf := buf + s[i] + #13;
    end;
    FlushBuffer;
  finally
    SetLength(Pages, Count);
  end;
end;

{==============================================================================}
class procedure TPageSerializer.MultiWrite(S: TStrings; WebFormat: boolean; const Pages: TPageDataList);
{==============================================================================}
var
  i: integer;
begin
  // пустые страницы могут появляться после интенсивного тыкания по календарю,
  // поэтому сохраняем только страницы с записями - МОЛОДЕЦ! :-)
  // ---
  // No. С таким подходом я не смогу перезаписать страницу на пустую. Пустое содержание - тоже содержание
  // ---
  /// Yes. Проверять нужно номер версии, а не содержание.

  s.Clear;
  for i := 0 to High(Pages) do
  // (Trim(Pages[i].Page) <> '') then
  if (Pages[i].Version > 0) then
      s.Add(Pages[i].Write(WebFormat));
end;

{==============================================================================}
class procedure TPageSerializer.ReadBody(S: TStrings; Page: TDiaryPage);
{==============================================================================}
var
  i,k: integer;
  CurStr: string;
  TempStr: string;
  TempTime: integer;
  TempValue: real;
  TempFinger: integer;
  TempShort: boolean;
  Meal: TMealRecord;
  TempFood: TFoodMassed;
begin
  with Page do
  begin
    // TODO: протестировать скорость загрузки дневника из XML
    //Log('TDiaryPage.ReadFrom() started');

    if S = nil then
    begin
      //Log('TDiaryPage.ReadFrom() error: S=nil');
      raise Exception.Create('TDiaryPage.ReadFrom(): поток для чтения не может быть nil');
    end;

    SilentChange := True;

    try
      Clear;
      Meal := nil;

      for i := 0 to S.Count - 1 do
      if (S[i] <> '') then
      begin
        CurStr := S[i];
        case CurStr[1] of
          '*':
          begin
            TempTime := StrToTimeQuick(Copy(CurStr,2,5));

            k := pos('|', curStr);
            if k > 0 then
            begin
              TempValue := StrToFloat(CheckDot( Copy(CurStr, 8, k-8) ));
              TempFinger := StrToInt( Copy(CurStr, k+1, Length(CurStr)-k) );
            end else
            begin
              TempValue := StrToFloat(CheckDot(Copy(CurStr,8,Length(CurStr)-7)));
              TempFinger := -1;
            end;                                                            
            Add(TBloodRecord.Create(TempTime, TempValue, TempFinger));
          end;
          '-':
          begin
            TempTime := StrToTimeQuick(Copy(CurStr,2,5));
            TempValue := StrToFloat(CheckDot(Copy(CurStr,8,Length(CurStr)-7)));
            Add(TInsRecord.Create(TempTime, TempValue));
          end;
          ' ':
          begin
            TempTime := StrToTimeQuick(Copy(CurStr,2,5));
            TempShort := (CurStr[Length(CurStr)] = 's');
            Meal := TMealRecord.Create(TempTime, TempShort); // save it for further modifications
            Add(Meal);
          end;
          '#':
          begin
            if (Meal <> nil) then
            begin
              TempFood := TFoodMassed.Create();
              TempFood.Read(Copy(CurStr, 2, Length(CurStr) - 1));
              Meal.Add(TempFood);
            end;
          end;
          '%':
          begin
            TempTime := StrToTimeQuick(Copy(CurStr, 2, 5));
            TempStr := Copy(CurStr, 8, Length(CurStr) - 7);
            Add(TNoteRecord.Create(TempTime, TempStr));
          end;
          {else
            // и что, из-за одного символа вся база полетит?
            raise ELoadingError.Create('TDiaryPage.ReadFrom: Некорректные данные'#13+
              'Строка :'+#13+
              CurStr);   }
        end;
      end;
    finally
      //Log('TDiaryPage.ReadFrom() finished');
      //FUpdateStampOnChange := True;
    end;
    //Log('TDiaryPage.ReadFrom() done ok');

    SilentChange := False;
  end;
end;

{==============================================================================}
class procedure TPageSerializer.ReadBody(const S: string; Page: TDiaryPage);
{==============================================================================}
var
  Temp: TStringList;
begin
  Temp := TStringList.Create;
  try
    Temp.Text := S;
    ReadBody(Temp, Page);
  finally
    Temp.Free;
  end;
end;

{==============================================================================}
class procedure TPageSerializer.ReadFrom(S: TPageData; Page: TDiaryPage);
{==============================================================================}
begin
  with Page do
  begin
    Date := S.Date;
    TimeStamp := S.TimeStamp;
    Version := S.Version;
    ReadBody(S.Page, Page);
  end;


  {

  Switch: boolean;

  ------

  Switch := Switch and (not SilentMode);
  SilentMode := SilentMode or Switch;

  ...

  SilentMode := SilentMode and (not Switch);

  ------

  OldMode := SilentMode;
  if Switch then SilentMode := True;

  ...

  SilentMode := OldMode;

  }
end;

{==============================================================================}
class procedure TPageSerializer.WriteTo(S: TPageData; Page: TDiaryPage);
{==============================================================================}
begin
  with Page do
  begin
    S.Date := Date;
    S.TimeStamp := TimeStamp;
    S.Version := Version;
    WriteTo(S.Page, Page);
  end;
end;

{==============================================================================}
class procedure TPageSerializer.WriteTo(S: TStrings; Page: TDiaryPage);
{==============================================================================}
var
  j, n: integer;
begin
  if (S = nil) then
    raise Exception.Create('TDiaryPage.WriteTo(): поток для записи не может быть nil');

  with Page do
  begin
    for j := 0 to Count - 1 do
    begin
      if (Recs[j].RecType = TBloodRecord) then
      begin
        // TODO: use Format() instead
        s.Add(
          '*' + TimeToStr(Recs[j].Time) +
          ' ' + FloatToStr(TBloodRecord(Recs[j]).Value) +
          '|' + IntToStr(TBloodRecord(Recs[j]).Finger)
        )
      end else

      if (Recs[j].RecType = TInsRecord) then
      begin
        s.Add(
          '-' + TimeToStr(Recs[j].Time) +
          ' ' + FloatToStr(TInsRecord(Recs[j]).Value)
        );
      end else

      if (Recs[j].RecType = TMealRecord) then
      begin
        if TMealRecord(Recs[j]).ShortMeal then
          s.Add(' ' + TimeToStr(Recs[j].Time) + 's')
        else
          s.Add(' ' + TimeToStr(Recs[j].Time));

        for n := 0 to TMealRecord(Recs[j]).Count - 1 do
          s.Add('#' + TMealRecord(Recs[j])[n].Write);
      end else

      if (Recs[j].RecType = TNoteRecord) then
      begin
        s.Add(
          '%' + TimeToStr(Recs[j].Time) +
          ' ' + TNoteRecord(Recs[j]).Text
        );
      end;
    end;
  end;
end;

{==============================================================================}
class procedure TPageSerializer.WriteTo(out S: string; Page: TDiaryPage);
{==============================================================================}
var
  Temp: TStringList;
begin
  Temp := TStringList.Create;
  try
    WriteTo(Temp, Page);
    S := Temp.Text;

   { if (S <> '') and (S[Length(S)] = #13) then
      Delete(S, Length(S), 1);}
  finally
    Temp.Free;
  end;
end;

end.

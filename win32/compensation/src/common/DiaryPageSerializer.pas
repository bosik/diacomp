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
  // частично распарсенная страница
  TPageData = class
    Date: TDate;
    TimeStamp: TDateTime;
    Version: integer;
    Page: string;

    procedure CopyFrom(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string); overload;
    procedure CopyFrom(APage: TPageData); overload;
    constructor Create(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string); overload;
    constructor Create(APage: TPageData); overload;

    function Write(F: TFormatSettings): string;
    function WriteHeader(F: TFormatSettings): string; deprecated;
  end;

  TPageDataList = array of TPageData;

  TPageSerializer = class
  private
    // basics
    class procedure ReadHeader(const S: string; F: TFormatSettings; out Date: TDate; out TimeStamp: TDateTime; out Version: integer); overload;
    class procedure ReadBody(S: TStrings; Page: TDiaryPage); overload;
    class procedure ReadBody(const S: string; Page: TDiaryPage); overload;
    class procedure WriteHeader(Page: TDiaryPage; F: TFormatSettings; out S: string);
    class procedure WriteBody(Page: TDiaryPage; S: TStrings); overload;
    class procedure WriteBody(Page: TDiaryPage; out S: string); overload;

    // derivatives

    class procedure Read(const S: string; Page: TPageData; F: TFormatSettings); overload;
    class procedure ReadHeader(const S: string; PageData: TPageData; F: TFormatSettings); overload;
  public
{L} class procedure Read(PageData: TPageData; Page: TDiaryPage); overload;
{L} class procedure Read(S: TStrings; F: TFormatSettings; out Pages: TPageDataList); overload;
{W} class procedure Read(S: TStrings; F: TFormatSettings; out Pages: TDiaryPageList); overload;
{L} class procedure Write(Page: TDiaryPage; PageData: TPageData); overload;
{L} class procedure Write(const Pages: TPageDataList; S: TStrings; F: TFormatSettings); overload;
{W} class procedure Write(Page: TDiaryPage; S: TStrings; F: TFormatSettings); overload;

    {

    LocalSource
      TStrings <--> TPageDataList
      TPageData <--> TDiaryPage

    WebSource
      TStrings --> TDiaryPageList
      TDiaryPage --> TStrings

    }
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
function TPageData.Write(F: TFormatSettings): string;
{==============================================================================}
var
  Header: string;
  Body: string;
begin
  //TPageSerializer.WriteHeader(Self, F, Header);
  Header := WriteHeader(F);
  Body := Page;
  Result := Header + #13 + Body;
end;

{==============================================================================}
function TPageData.WriteHeader(F: TFormatSettings): string;
{==============================================================================}
begin
  Result := Format('=== %s ===|%s|%d', [DateToStr(Date, F), DateTimeToStr(TimeStamp, F), Version]);
end;

{ TPageSerializer }

{==============================================================================}
class procedure TPageSerializer.Read(S: TStrings; F: TFormatSettings; out Pages: TPageDataList);
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
      Read(buf, Pages[Count], F);
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
class procedure TPageSerializer.Read(S: TStrings; F: TFormatSettings; out Pages: TDiaryPageList);
{==============================================================================}
var
  PageDataList: TPageDataList;
  i: integer;
begin
  // TODO: optimize
  Read(S, F, PageDataList);

  SetLength(Pages, Length(PageDataList));
  for i := 0 to High(Pages) do
  begin
    Pages[i] := TDiaryPage.Create;
    Read(PageDataList[i], Pages[i]);
  end;
end;

{==============================================================================}
class procedure TPageSerializer.Read(const S: string; Page: TPageData; F: TFormatSettings);
{==============================================================================}
var
  header, body: string;
begin
  Separate(S, header, #13, body);
  ReadHeader(header, Page, F);
  Page.Page := body;
end;

{==============================================================================}
class procedure TPageSerializer.Read(PageData: TPageData; Page: TDiaryPage);
{==============================================================================}
begin
  with Page do
  begin
    Date := PageData.Date;
    TimeStamp := PageData.TimeStamp;
    Version := PageData.Version;
    ReadBody(PageData.Page, Page);
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
class procedure TPageSerializer.ReadHeader(const S: string; PageData: TPageData; F: TFormatSettings);
{==============================================================================}
begin
  ReadHeader(S, F, PageData.Date, PageData.TimeStamp, PageData.Version);
end;

{==============================================================================}
class procedure TPageSerializer.ReadHeader(const S: string; F: TFormatSettings; out Date: TDate; out TimeStamp: TDateTime; out Version: integer);
{==============================================================================}
var
  s1, s2, tmp: string;
begin
  try
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
end;

{==============================================================================}
class procedure TPageSerializer.Write(Page: TDiaryPage; PageData: TPageData);
{==============================================================================}
begin
  with Page do
  begin
    PageData.Date := Date;
    PageData.TimeStamp := TimeStamp;
    PageData.Version := Version;
    WriteBody(Page, PageData.Page);
  end;
end;

{==============================================================================}
class procedure TPageSerializer.Write(const Pages: TPageDataList; S: TStrings; F: TFormatSettings);
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

  for i := 0 to High(Pages) do
  // (Trim(Pages[i].Page) <> '') then
  if (Pages[i].Version > 0) then
      s.Add(Pages[i].Write(F));
end;

{==============================================================================}
class procedure TPageSerializer.WriteBody(Page: TDiaryPage; S: TStrings);
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
class procedure TPageSerializer.Write(Page: TDiaryPage; S: TStrings; F: TFormatSettings);
{==============================================================================}
begin
  S.Add(Format('=== %s ===|%s|%d', [DateToStr(Page.Date, F), DateTimeToStr(Page.TimeStamp, F), Page.Version]));
  WriteBody(Page, S);
end;

{==============================================================================}
class procedure TPageSerializer.WriteBody(Page: TDiaryPage; out S: string);
{==============================================================================}
var
  Temp: TStringList;
begin
  Temp := TStringList.Create;
  try
    WriteBody(Page, Temp);
    S := Temp.Text;

   { if (S <> '') and (S[Length(S)] = #13) then
      Delete(S, Length(S), 1);}
  finally
    Temp.Free;
  end;
end;

{==============================================================================}
class procedure TPageSerializer.WriteHeader(Page: TDiaryPage;
  F: TFormatSettings; out S: string);
{==============================================================================}
begin
  S := Format('=== %s ===|%s|%d', [DateToStr(Page.Date, F), DateTimeToStr(Page.TimeStamp, F), Page.Version]);
end;

end.

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
  DiaryRoutines, // TDate
  Bases,
  uLkJSON;

type
  TStringsArray = array of TStrings;

  TPageSerializer = class
  public
    {routine} class procedure SeparatePages(S: TStrings; out Pages: TStringsArray);

    {basic} class procedure ReadHeader(const S: string; F: TFormatSettings; out Date: TDate; out TimeStamp: TDateTime; out Version: integer); overload;
    {basic} class procedure WriteHeader(const Date: TDate; const TimeStamp: TDateTime; const Version: integer; F: TFormatSettings; out S: string); overload;
    {basic} class procedure ReadBody(S: TStrings; Page: TDiaryPage); overload;
    {basic} class procedure WriteBody(Page: TDiaryPage; S: TStrings); overload;

    {sugar} class procedure WriteHeader(Page: TDiaryPage; F: TFormatSettings; out S: string); overload;
    {sugar} class procedure WriteBody(Page: TDiaryPage; out S: string); overload;
    {sugar} class procedure ReadBody(const S: string; Page: TDiaryPage); overload;

    {sugar} class procedure ReadPage(S: TStrings; F: TFormatSettings; Page: TDiaryPage);
    {sugar} {W} class procedure WritePage(Page: TDiaryPage; S: TStrings; F: TFormatSettings); overload;

    {sugar} {W} class procedure ReadPages(S: TStrings; F: TFormatSettings; out Pages: TDiaryPageList); overload;
  end;

  {
    LocalSource
      TStrings <--> TPageDataList
      TPageData <--> TDiaryPage

    WebSource
      TStrings --> TDiaryPageList
      TDiaryPage --> TStrings
  }

implementation

{ TPageSerializer }

{==============================================================================}
class procedure TPageSerializer.ReadPages(S: TStrings; F: TFormatSettings; out Pages: TDiaryPageList);
{==============================================================================}
var
  PageList: TStringsArray;
  i: integer;
begin
  TPageSerializer.SeparatePages(S, PageList);

  SetLength(Pages, Length(PageList));
  for i := 0 to High(PageList) do
  begin
    Pages[i] := TDiaryPage.Create;
    TPageSerializer.ReadPage(PageList[i], F, Pages[i]);
  end;
end;

{==============================================================================}
class procedure TPageSerializer.ReadBody(S: TStrings; Page: TDiaryPage);
{==============================================================================}

  function ParseBlood(json: TlkJSONobject): TBloodRecord;
  begin
    Result := TBloodRecord.Create();
    Result.NativeTime := StrToDateTime((json['time'] as TlkJSONstring).Value);
    Result.Value := (json['value'] as TlkJSONnumber).Value;
    Result.Finger := (json['finger'] as TlkJSONnumber).Value;
  end;

  function ParseIns(json: TlkJSONobject): TInsRecord;
  begin
    Result := TInsRecord.Create();
    Result.NativeTime := StrToDateTime((json['time'] as TlkJSONstring).Value);
    Result.Value := (json['value'] as TlkJSONnumber).Value;
  end;

  function ParseNote(json: TlkJSONobject): TNoteRecord;
  begin
    Result := TNoteRecord.Create();
    Result.NativeTime := StrToDateTime((json['time'] as TlkJSONstring).Value);
    Result.Text := (json['text'] as TlkJSONnumber).Value;
  end;

  function ParseFoodMassed(json: TlkJSONobject): TFoodMassed;
  begin
    Result := TFoodMassed.Create();
    Result.Name     := (json['name']  as TlkJSONstring).Value;
    Result.RelProts := (json['prots'] as TlkJSONnumber).Value;
    Result.RelFats  := (json['fats']  as TlkJSONnumber).Value;
    Result.RelCarbs := (json['carbs'] as TlkJSONnumber).Value;
    Result.RelValue := (json['value'] as TlkJSONnumber).Value;
    Result.Mass     := (json['mass']  as TlkJSONnumber).Value;
  end;

  function ParseMeal(json: TlkJSONobject): TMealRecord;
  var
    content: TlkJSONlist;
    i: integer;
    Food: TFoodMassed;
  begin
    Result := TMealRecord.Create();
    Result.NativeTime := StrToDateTime((json['time'] as TlkJSONstring).Value);
    Result.ShortMeal := (json['short'] as TlkJSONstring).Value = 'true';

    content := (json['content'] as TlkJSONlist);
    for i := 0 to content.Count - 1 do
    begin
      Food := ParseFoodMassed((content.Child[i] as TlkJSONobject));
      Result.Add(food);
    end;
  end;

  function ParseRecord(S: string): TCustomRecord;
  var
    STime: string;
    STimeStamp: string;
    SID: string;
    SVersion: string;
    SDeleted: string;

    json: TlkJSONobject;
    ID: string;
    Stamp: TDateTime;
    Deleted: boolean;
    Version: integer;
    RecType: string;
    Time: TDateTime;
  begin
    Separate(S, STime, #9, S);
    Separate(S, STimeStamp, #9, S);
    Separate(S, SID, #9, S);
    Separate(S, SVersion, #9, S);
    Separate(S, SDeleted, #9, S);

    json := TlkJSON.ParseText(S) as TlkJSONobject;

    if Assigned(json) then
    begin
      RecType := (json['type'] as TlkJSONstring).Value;

      if (RecType = 'blood') then Result := ParseBlood(json) else
      if (RecType = 'ins')   then Result := ParseIns(json) else
      if (RecType = 'meal')  then Result := ParseMeal(json) else
      if (RecType = 'note')  then Result := ParseNote(json) else

      Result.NativeTime := StrToDateTime(STime);
      Result.TimeStamp := StrToDateTime(STimeStamp);
      Result.ID := SID;
      Result.Version := StrToInt(SVersion);
      Result.Deleted := (SDeleted = 'true');
    end else
    begin
      raise Exception.Create('Invalid JSON: ' + S);
    end;
  end;

var
  i,k: integer;
  CurStr: string;
  TempStr: string;
  TempTime: TDateTime;
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

    if (S = nil) then
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

        Add(ParseRecord(CurStr));

        {case CurStr[1] of

          '*':
          begin
            //TempTime := StrToDateTime(Copy(CurStr, 2, 19), STD_DATETIME_FMT);
            TempTime := ReadTime(Copy(CurStr, 2, 5));

            k := pos('|', curStr);

            if k > 0 then
            begin
              TempValue := StrToFloat(CheckDot( Copy(CurStr, 8, k - 8) ));
              TempFinger := StrToInt( Copy(CurStr, k + 1, Length(CurStr) - k) );
            end else
            begin
              TempValue := StrToFloat(CheckDot(Copy(CurStr, 8, Length(CurStr) - 7)));
              TempFinger := -1;
            end;
            Add(TBloodRecord.Create(TempTime, TempValue, TempFinger));
          end;
          '-':
          begin
            TempTime := ReadTime(Copy(CurStr, 2, 5));
            TempValue := StrToFloat(CheckDot(Copy(CurStr, 8, Length(CurStr) - 7)));
            Add(TInsRecord.Create(TempTime, TempValue));
          end;
          ' ':
          begin
            TempTime := ReadTime(Copy(CurStr, 2, 5));
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
            TempTime := ReadTime(Copy(CurStr, 2, 5));
            TempStr := Copy(CurStr, 8, Length(CurStr) - 7);
            Add(TNoteRecord.Create(TempTime, TempStr));
          end;
        end; }
      end;
    except
      ShowMessage('Error reading line: ' + CurStr);
    //end;
    //finally
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
        'TPageSerializer.ReadHeader(): EXCEPTION! ' +  E.Message + #13 +
        'TPageSerializer.ReadHeader(): S = "' + S + '"' + #13 +
        'TPageSerializer.ReadHeader(): Copy(S, 5, 10) = "' + Copy(S, 5, 10) + '"' + #13 +
        'TPageSerializer.ReadHeader(): tmp = "' + tmp + '"' + #13 +
        'TPageSerializer.ReadHeader(): TimeStamp = "' + s1 + '"' + #13 +
        'TPageSerializer.ReadHeader(): Version = "' + s2 + '"', True);
      ShowMessage('Exception in TPageSerializer.ReadHeader(). Check the log file for details.');
    end;
  end;
end;

{==============================================================================}
class procedure TPageSerializer.WriteBody(Page: TDiaryPage; S: TStrings);
{==============================================================================}

  procedure RemoveAll(var S: string; c: char);
  var
    k: integer;
  begin
    k := pos(c, S);
    while (k > 0) do
    begin
      Delete(S, k, 1);
      k := pos(c, S);
    end;
  end;

  function Generate(json: TlkJSONobject): string;
  var
    i: integer;
  begin
    Result := GenerateReadableText(json, i);
    RemoveAll(Result, #13);
    RemoveAll(Result, #10);
  end;

  function SerializeNote(Note: TNoteRecord): string;
  var
    json: TlkJSONobject;
  begin
    json := TlkJSONobject.Create();
    try
      json.Add('time', DateTimeToStr(Note.NativeTime, STD_DATETIME_FMT));
      json.Add('text', Note.Text);
      Result := Generate(json);
    finally
      FreeAndNil(json);
    end;
  end;

var
  j, n: integer;
  tmp: string;
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
          '*' + DateTimeToStr(Recs[j].Time, STD_DATETIME_FMT) +
          ' ' + FloatToStr(TBloodRecord(Recs[j]).Value) +
          '|' + IntToStr(TBloodRecord(Recs[j]).Finger)
        )
      end else

      if (Recs[j].RecType = TInsRecord) then
      begin
        s.Add(
          '-' + DateTimeToStr(Recs[j].Time, STD_DATETIME_FMT) +
          ' ' + FloatToStr(TInsRecord(Recs[j]).Value)
        );
      end else

      if (Recs[j].RecType = TMealRecord) then
      begin
        if TMealRecord(Recs[j]).ShortMeal then
          s.Add(' ' + DateTimeToStr(Recs[j].Time, STD_DATETIME_FMT) + 's')
        else
          s.Add(' ' + DateTimeToStr(Recs[j].Time, STD_DATETIME_FMT));

        for n := 0 to TMealRecord(Recs[j]).Count - 1 do
          s.Add('#' + TMealRecord(Recs[j])[n].Write);
      end else

      if (Recs[j].RecType = TNoteRecord) then
      begin
        tmp := '%' + DateTimeToStr(Recs[j].NativeTime, STD_DATETIME_FMT) + ' ' + TNoteRecord(Recs[j]).Text;
        //tmp := '%' + SerializeNote(Recs[j] as TNoteRecord);
        s.Add(
          tmp
        );
      end;
    end;
  end;
end;

{==============================================================================}
class procedure TPageSerializer.WritePage(Page: TDiaryPage; S: TStrings; F: TFormatSettings);
{==============================================================================}
var
  Header: string;
begin
  TPageSerializer.WriteHeader(Page, F, Header);
  S.Add(Header);
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
class procedure TPageSerializer.WriteHeader(const Date: TDate;
  const TimeStamp: TDateTime; const Version: integer; F: TFormatSettings;
  out S: string);
{==============================================================================}
begin
  S := Format('=== %s ===|%s|%d', [DateToStr(Date, F), DateTimeToStr(TimeStamp, F), Version]);
end;

{==============================================================================}
class procedure TPageSerializer.WriteHeader(Page: TDiaryPage;
  F: TFormatSettings; out S: string);
{==============================================================================}
begin
  WriteHeader(Page.Date, Page.TimeStamp, Page.Version, F, S);
end;

{==============================================================================}
class procedure TPageSerializer.SeparatePages(S: TStrings; out Pages: TStringsArray);
{==============================================================================}
var
  buf: TStrings;
  i, Count: integer;

  procedure FlushBuffer;
  begin
    if (buf.Count > 0) then
    begin
      if (Length(Pages) = Count) then
        SetLength(Pages, Length(Pages) * 2 + 1);

      Pages[Count] := TStringList.Create;
      Pages[Count].AddStrings(buf);
      inc(Count);

      buf.Clear;
    end;
  end;

begin
  buf := TStringList.Create;;
  Count := 0;
  SetLength(Pages, 1);
  try
    for i := 0 to s.Count - 1 do
    if (s[i] <> '') then
    begin
      if (s[i][1] = '=') then FlushBuffer;
      buf.Add(s[i]);
    end;
    FlushBuffer;
  finally
    SetLength(Pages, Count);
    buf.Free;
  end;
end;

{==============================================================================}
class procedure TPageSerializer.ReadPage(S: TStrings; F: TFormatSettings; Page: TDiaryPage);
{==============================================================================}
var
  T: TStrings;
  Date: TDate;
  Timestamp: TDateTime;
  Version: integer;
begin
  T := TStringList.Create;
  T.AddStrings(S);
  //T.Delete(0);

  //TPageSerializer.ReadHeader(S[0], F, Date, Timestamp, Version);
  Page.Date := Date;
  Page.TimeStamp := Timestamp;
  Page.Version := Version;

  ReadBody(T, Page);
end;

end.

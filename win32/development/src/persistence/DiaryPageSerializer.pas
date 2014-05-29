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
  uLkJSON,
  JsonSerializer,
  JsonVersionedSerializer;

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

  function ParseBlood(json: TlkJSONobject): TBloodRecord;
  function ParseIns(json: TlkJSONobject): TInsRecord;
  function ParseFoodMassed(json: TlkJSONobject): TFoodMassed;
  function ParseMeal(json: TlkJSONobject): TMealRecord;
  function ParseNote(json: TlkJSONobject): TNoteRecord;
  function ParseDiaryRecord(json: TlkJSONobject): TCustomRecord;
  function ParseVersionedDiaryRecord(json: TlkJSONbase): TCustomRecord;
  function ParseVersionedDiaryRecords(json: TlkJSONlist): TRecordList;

  function SerializeBlood(R: TBloodRecord): TlkJSONobject;
  function SerializeIns(R: TInsRecord): TlkJSONobject;
  function SerializeFoodMassed(R: TFoodMassed): TlkJSONobject;
  function SerializeMeal(R: TMealRecord): TlkJSONobject;
  function SerializeNote(R: TNoteRecord): TlkJSONobject;
  function SerializeDiaryRecord(R: TCustomRecord): TlkJSONobject;
  function SerializeVersionedDiaryRecord(R: TCustomRecord): TlkJSONobject;
  function SerializeVersionedDiaryRecords(List: TRecordList): TlkJSONlist;

const
  REC_TYPE            = 'type';
  REC_TYPE_BLOOD      = 'blood';
  REC_TYPE_INS        = 'ins';
  REC_TYPE_MEAL       = 'meal';
  REC_TYPE_NOTE       = 'note';

  REC_TIME            = 'time';
  REC_BLOOD_VALUE     = 'value';
  REC_BLOOD_FINGER    = 'finger';
  REC_INS_VALUE       = 'value';
  REC_MEAL_SHORT      = 'short';
  REC_MEAL_CONTENT    = 'content';
  REC_MEAL_FOOD_NAME  = 'name';
  REC_MEAL_FOOD_PROTS = 'prots';
  REC_MEAL_FOOD_FATS  = 'fats';
  REC_MEAL_FOOD_CARBS = 'carbs';
  REC_MEAL_FOOD_VALUE = 'value';
  REC_MEAL_FOOD_MASS  = 'mass';
  REC_NOTE_TEXT       = 'text';

implementation

{==============================================================================}
function ParseBlood(json: TlkJSONobject): TBloodRecord;
{==============================================================================}
begin
  Result := TBloodRecord.Create();
  Result.NativeTime := StrToDateTime((json[REC_TIME] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Result.Value := (json[REC_BLOOD_VALUE] as TlkJSONnumber).Value;
  Result.Finger := (json[REC_BLOOD_FINGER] as TlkJSONnumber).Value;
end;

{==============================================================================}
function ParseIns(json: TlkJSONobject): TInsRecord;
{==============================================================================}
begin
  Result := TInsRecord.Create();
  Result.NativeTime := StrToDateTime((json[REC_TIME] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Result.Value := (json[REC_INS_VALUE] as TlkJSONnumber).Value;
end;

{==============================================================================}
function ParseFoodMassed(json: TlkJSONobject): TFoodMassed;
{==============================================================================}
begin
  Result := TFoodMassed.Create();
  Result.Name     := (json[REC_MEAL_FOOD_NAME]  as TlkJSONstring).Value;
  Result.RelProts := (json[REC_MEAL_FOOD_PROTS] as TlkJSONnumber).Value;
  Result.RelFats  := (json[REC_MEAL_FOOD_FATS]  as TlkJSONnumber).Value;
  Result.RelCarbs := (json[REC_MEAL_FOOD_CARBS] as TlkJSONnumber).Value;
  Result.RelValue := (json[REC_MEAL_FOOD_VALUE] as TlkJSONnumber).Value;
  Result.Mass     := (json[REC_MEAL_FOOD_MASS]  as TlkJSONnumber).Value;
end;

{==============================================================================}
function ParseMeal(json: TlkJSONobject): TMealRecord;
{==============================================================================}
var
  content: TlkJSONlist;
  i: integer;
  Food: TFoodMassed;
begin
  Result := TMealRecord.Create();
  Result.NativeTime := StrToDateTime((json[REC_TIME] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Result.ShortMeal := (json[REC_MEAL_SHORT] as TlkJSONboolean).Value;

  content := (json[REC_MEAL_CONTENT] as TlkJSONlist);
  for i := 0 to content.Count - 1 do
  begin
    Food := ParseFoodMassed((content.Child[i] as TlkJSONobject));
    Result.Add(food);
  end;
end;

{==============================================================================}
function ParseNote(json: TlkJSONobject): TNoteRecord;
{==============================================================================}
begin
  Result := TNoteRecord.Create();

  if (Assigned(json[REC_TIME])) then
    Result.NativeTime := StrToDateTime((json[REC_TIME] as TlkJSONstring).Value, STD_DATETIME_FMT)
  else
    raise Exception.Create('Failed to read JSON: missing field: ' + REC_TIME);

  if (Assigned(json[REC_NOTE_TEXT])) then
    Result.Text := (json[REC_NOTE_TEXT] as TlkJSONstring).Value
  else
    raise Exception.Create('Failed to read JSON: missing field: ' + REC_NOTE_TEXT);
end;

{==============================================================================}
function ParseDiaryRecord(json: TlkJSONobject): TCustomRecord;
{==============================================================================}
var
  RecType: string;
begin
  RecType := (json[REC_TYPE] as TlkJSONstring).Value;

  if (RecType = REC_TYPE_BLOOD) then Result := ParseBlood(json as TlkJSONobject) else
  if (RecType = REC_TYPE_INS)   then Result := ParseIns  (json as TlkJSONobject) else
  if (RecType = REC_TYPE_MEAL)  then Result := ParseMeal (json as TlkJSONobject) else
  if (RecType = REC_TYPE_NOTE)  then Result := ParseNote (json as TlkJSONobject) else
    raise Exception.Create('Unsupported record type: ' + RecType);
end;

{==============================================================================}
function ParseVersionedDiaryRecord(json: TlkJSONbase): TCustomRecord;
{==============================================================================}
var
  JsonObj: TlkJSONobject;
begin
  JsonObj := json as TlkJSONobject;

  Result := ParseDiaryRecord(JsonObj[REC_DATA] as TlkJSONobject);
  Result.ID := (JsonObj[REC_ID] as TlkJSONstring).Value;
  Result.TimeStamp := StrToDateTime((JsonObj[REC_TIMESTAMP] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Result.Version := (JsonObj[REC_VERSION] as TlkJSONnumber).Value;
  Result.Deleted := (JsonObj[REC_DELETED] as TlkJSONboolean).Value;
end;

{==============================================================================}
function ParseVersionedDiaryRecords(json: TlkJSONlist): TRecordList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, json.Count);
  for i := 0 to json.Count - 1 do
    Result[i] := ParseVersionedDiaryRecord(json.Child[i] as TlkJSONobject);
end;

{==============================================================================}
function SerializeBlood(R: TBloodRecord): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_TYPE, REC_TYPE_BLOOD);
  Result.Add(REC_TIME, DateTimeToStr(R.NativeTime, STD_DATETIME_FMT));
  Result.Add(REC_BLOOD_VALUE, R.Value);
  Result.Add(REC_BLOOD_FINGER, R.Finger);
end;

{==============================================================================}
function SerializeIns(R: TInsRecord): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_TYPE, REC_TYPE_INS);
  Result.Add(REC_TIME, DateTimeToStr(R.NativeTime, STD_DATETIME_FMT));
  Result.Add(REC_INS_VALUE, R.Value);
end;

{==============================================================================}
function SerializeFoodMassed(R: TFoodMassed): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_MEAL_FOOD_NAME, R.Name);
  Result.Add(REC_MEAL_FOOD_PROTS, R.RelProts);
  Result.Add(REC_MEAL_FOOD_FATS, R.RelFats);
  Result.Add(REC_MEAL_FOOD_CARBS, R.RelCarbs);
  Result.Add(REC_MEAL_FOOD_VALUE, R.RelValue);
  Result.Add(REC_MEAL_FOOD_MASS, R.Mass);
end;

{==============================================================================}
function SerializeMeal(R: TMealRecord): TlkJSONobject;
{==============================================================================}
var
  Content: TlkJSONlist;
  i: integer;
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_TYPE, REC_TYPE_MEAL);
  Result.Add(REC_TIME, DateTimeToStr(R.NativeTime, STD_DATETIME_FMT));
  Result.Add(REC_MEAL_SHORT, R.ShortMeal);

  Content := TlkJSONlist.Create();
  for i := 0 to R.Count - 1 do
    Content.Add(SerializeFoodMassed(R[i]));

  Result.Add(REC_MEAL_CONTENT, Content);
end;

{==============================================================================}
function SerializeNote(R: TNoteRecord): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_TYPE, REC_TYPE_NOTE);
  Result.Add(REC_TIME, DateTimeToStr(R.NativeTime, STD_DATETIME_FMT));
  Result.Add(REC_NOTE_TEXT, R.Text);
end;

{==============================================================================}
function SerializeDiaryRecord(R: TCustomRecord): TlkJSONobject;
{==============================================================================}
begin
  if (R.RecType = TBloodRecord) then Result := SerializeBlood(R as TBloodRecord) else
  if (R.RecType = TInsRecord)   then Result := SerializeIns(R as TInsRecord) else
  if (R.RecType = TMealRecord)  then Result := SerializeMeal(R as TMealRecord) else
  if (R.RecType = TNoteRecord)  then Result := SerializeNote(R as TNoteRecord) else
    raise Exception.Create('Unsupported record type: ' + R.ClassName);
end;

{==============================================================================}
function SerializeVersionedDiaryRecord(R: TCustomRecord): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_ID, R.ID);
  Result.Add(REC_TIMESTAMP, DateTimeToStr(R.TimeStamp, STD_DATETIME_FMT));
  Result.Add(REC_VERSION, R.Version);
  Result.Add(REC_DELETED, R.Deleted);
  Result.Add(REC_DATA, SerializeDiaryRecord(R));
end;


{==============================================================================}
function SerializeVersionedDiaryRecords(List: TRecordList): TlkJSONlist;
{==============================================================================}
var
  i: integer;
begin
  Result := TlkJSONlist.Create;
  for i := Low(List) to High(List) do
    Result.Add(SerializeVersionedDiaryRecord(List[i]));
end;

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

  {function ParseRecord(S: string): TCustomRecord;
  var
    STime: string;
    STimeStamp: string;
    SID: string;
    SVersion: string;
    SDeleted: string;

    json: TlkJSONobject;
    RecType: string;
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
        raise Exception.Create('Unsupported record type: ' + RecType);

      Result.NativeTime := StrToDateTime(STime);
      Result.TimeStamp := StrToDateTime(STimeStamp);
      Result.ID := SID;
      Result.Version := StrToInt(SVersion);
      Result.Deleted := (SDeleted = 'true');
    end else
    begin
      raise Exception.Create('Invalid JSON: ' + S);
    end;
  end;     }

{var
  i: integer;}
begin
  {if (S = nil) then
  begin
    raise Exception.Create('TDiaryPage.ReadFrom(): поток для чтения не может быть nil');
  end;

  Page.SilentChange := True;
  Page.Clear;

  for i := 0 to S.Count - 1 do
  begin
    if (S[i] <> '') then
    try
      Page.Add(ParseRecord(S[i]));
    except
     ShowMessage('Error reading line: ' + S[i]);
    end;
  end;

  Page.SilentChange := False;}

  raise Exception.Create('Unsupported operation');
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

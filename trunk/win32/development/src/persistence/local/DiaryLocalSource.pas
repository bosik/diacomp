unit DiaryLocalSource;

interface

uses
  Windows,
  SysUtils, // FileExists(), Now()
  Classes,
  Dialogs {update warnings},
  DiaryRoutines,
  DiaryDAO,
  Bases,
  DiaryPage,
  DiaryRecords,
  DiaryPageSerializer,
  uLkJSON,
  BusinessObjects,

  AutoLog {TODO: debug only};

type
  // частично распарсенна€ запись
  TRecordData = class (TCustomRecord)
  public
    Data: string;

    procedure Read(S: string);
    function Write(): string;
    procedure Serialize(Rec: TCustomRecord);
    function Deserialize(): TCustomRecord;
  end;

  TRecordDataList = array of TRecordData;

  TDiaryLocalSource = class (TDiaryDAO)
  private
    FRecords: TRecordDataList;
    FModified: boolean;
    FFileName: string;

    function AddToInternal(Rec: TCustomRecord): integer;
    procedure Clear;
    function GetRecordIndex(ID: TCompactGUID): integer;
    function Trace(Index: integer): integer;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure Delete(ID: TCompactGUID); override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    procedure Save(const Recs: TVersionedList); override;

    // свойства
    // TODO: think about it
    //property Modified: boolean read FModified {write FModified};
 end;

implementation

var
  LocalFmt: TFormatSettings;
  
const
  ShowUpdateWarning = True;

{ TRecordData }

{==============================================================================}
function TRecordData.Deserialize: TCustomRecord;
{==============================================================================}

 { function ParseBlood(json: TlkJSONobject): TBloodRecord;
  begin
    Result := TBloodRecord.Create();
    Result.Value := StrToFloat(CheckDot((json['value'] as TlkJSONstring).Value));
    Result.Finger := StrToInt((json['finger'] as TlkJSONstring).Value);
  end;

  function ParseIns(json: TlkJSONobject): TInsRecord;
  begin
    Result := TInsRecord.Create();
    Result.Value := StrToFloat(CheckDot((json['value'] as TlkJSONstring).Value));
  end;

  function ParseFoodMassed(json: TlkJSONobject): TFoodMassed;
  begin
    Result := TFoodMassed.Create();
    Result.Name     := (json['name'] as TlkJSONstring).Value;
    Result.RelProts := StrToFloat(CheckDot((json['prots'] as TlkJSONstring).Value));
    Result.RelFats  := StrToFloat(CheckDot((json['fats']  as TlkJSONstring).Value));
    Result.RelCarbs := StrToFloat(CheckDot((json['carbs'] as TlkJSONstring).Value));
    Result.RelValue := StrToFloat(CheckDot((json['value'] as TlkJSONstring).Value));
    Result.Mass     := StrToFloat(CheckDot((json['mass']  as TlkJSONstring).Value));
  end;

  function ParseMeal(json: TlkJSONobject): TMealRecord;
  var
    content: TlkJSONlist;
    i: integer;
    Food: TFoodMassed;
  begin
    Result := TMealRecord.Create();
    Result.ShortMeal := (json['short'] as TlkJSONstring).Value = 'true';

    content := (json['content'] as TlkJSONlist);
    for i := 0 to content.Count - 1 do
    begin
      Food := ParseFoodMassed(content.Child[i] as TlkJSONobject);
      Result.Add(food);
    end;
  end;

  function ParseNote(json: TlkJSONobject): TNoteRecord;
  begin
    Result := TNoteRecord.Create();
    Result.Text := (json['text'] as TlkJSONstring).Value;
  end;   }

var
  json: TlkJSONobject;
begin
  json := TlkJSON.ParseText(Data) as TlkJSONobject;

  if Assigned(json) then
  try
    Result := DiaryPageSerializer.ParseDiaryRecord(json);

    {
    RecType  := (json['type'] as TlkJSONstring).Value;
    //SDeleted := (json['text'] as TlkJSONstring).Value;

    //if (SDeleted <> 'x123') then

    if (RecType = 'blood') then Result := ParseBlood(json) else
    if (RecType = 'ins')   then Result := ParseIns(json) else
    if (RecType = 'meal')  then Result := ParseMeal(json) else
    if (RecType = 'note')  then Result := ParseNote(json) else
      raise Exception.Create('Unsupported record type: ' + RecType);  }

    Result.NativeTime := NativeTime;
    Result.TimeStamp := TimeStamp;
    Result.ID := ID;
    Result.Version := Version;
    Result.Deleted := Deleted;
  finally
    json.Free;
  end else
  begin
    raise Exception.Create('Invalid JSON: ' + Data);
  end;
end;

{==============================================================================}
procedure TRecordData.Serialize(Rec: TCustomRecord);
{==============================================================================}

  {function SerializeBlood(Rec: TBloodRecord): string;
  var
    json: TlkJSONobject;
    Temp: string;
  begin
    json := TlkJSONobject.Create();
    try
      json.Add('type', 'blood');
      json.Add('value', Rec.Value);
      json.Add('finger', Rec.Finger);
      Temp := Generate(json);
      Result := Temp;
    finally
      FreeAndNil(json);
    end;
  end;

  function SerializeIns(Rec: TInsRecord): string;
  var
    json: TlkJSONobject;
  begin
    json := TlkJSONobject.Create();
    try
      json.Add('type', 'ins');
      json.Add('value', Rec.Value);
      Result := Generate(json);
    finally
      FreeAndNil(json);
    end;
  end;

  function SerializeMeal(Rec: TMealRecord): string;

    function SerializeFood(Rec: TFoodMassed): TlkJSONobject;
    begin
      Result := TlkJSONobject.Create();
      Result.Add('name', Rec.Name);
      Result.Add('prots', Rec.RelProts);
      Result.Add('fats', Rec.RelFats);
      Result.Add('carbs', Rec.RelCarbs);
      Result.Add('value', Rec.RelValue);
      Result.Add('mass', Rec.Mass);
    end;

  var
    json: TlkJSONobject;
    items: TlkJSONlist;
    i: integer;
  begin
    json := TlkJSONobject.Create();
    try
      json.Add('type', 'meal');
      json.Add('short', WriteBoolean(Rec.ShortMeal));

      items := TlkJSONlist.Create;

      for i := 0 to Rec.Count - 1 do
        items.Add(SerializeFood(Rec[i]));

      json.Add('content', items);
      Result := Generate(json);
    finally
      FreeAndNil(json);
    end;
  end;

  function SerializeNote(Rec: TNoteRecord): string;
  var
    json: TlkJSONobject;
  begin
    json := TlkJSONobject.Create();
    try
      json.Add('type', 'note');
      json.Add('text', Rec.Text);
      Result := Generate(json);
    finally
      FreeAndNil(json);
    end;
  end; }

var
  Json: TlkJSONobject;
begin
  Json := DiaryPageSerializer.SerializeDiaryRecord(Rec);

  ID := Rec.ID;
  TimeStamp := Rec.TimeStamp;
  Version := Rec.Version;
  Deleted := Rec.Deleted;
  Data := TlkJSON.GenerateText(Json);

  // cache
  NativeTime := Rec.NativeTime;
end;

{==============================================================================}
procedure TRecordData.Read(S: string);
{==============================================================================}
var
  STime: string;
  STimeStamp: string;
  SID: string;
  SVersion: string;
  SDeleted: string;
begin
  STime := TextBefore(S, #9);      S := TextAfter(S, #9);
  STimeStamp := TextBefore(S, #9); S := TextAfter(S, #9);
  SID := TextBefore(S, #9);        S := TextAfter(S, #9);
  SVersion := TextBefore(S, #9);   S := TextAfter(S, #9);
  SDeleted := TextBefore(S, #9);   S := TextAfter(S, #9);

  NativeTime := StrToDateTime(STime, STD_DATETIME_FMT);
  TimeStamp := StrToDateTime(STimeStamp, STD_DATETIME_FMT);
  ID := SID;
  Version := StrToInt(SVersion);
  Deleted := ReadBoolean(SDeleted);
  Data := S;
end;

{==============================================================================}
function TRecordData.Write: string;
{==============================================================================}
begin
  Result := Format('%s'#9'%s'#9'%s'#9'%d'#9'%s'#9'%s',
    [
      DateTimeToStr(NativeTime, STD_DATETIME_FMT),
      DateTimeToStr(TimeStamp, STD_DATETIME_FMT),
      ID,
      Version,
      WriteBoolean(Deleted),
      Data
    ]
  );
end;

{ TDiaryLocalSource }

{==============================================================================}
function TDiaryLocalSource.AddToInternal(Rec: TCustomRecord): integer;
{==============================================================================}
begin
  // 1. ѕроверка дублей
  // 2. —ортировка

  Result := GetRecordIndex(Rec.ID);
  if (Result = -1) then
  begin
    Result := Length(FRecords);
    SetLength(FRecords, Length(FRecords) + 1);
    FRecords[Result] := TRecordData.Create;
  end;

  FRecords[Result].Serialize(Rec);
  Result := Trace(Result);
end;

{==============================================================================}
procedure TDiaryLocalSource.Clear;
{==============================================================================}
var
  i: integer;
begin
  { вызываетс€ в деструкторе и перед загрузкой из файла }

  if (Length(FRecords) > 0) then
    FModified := True;

  for i := 0 to High(FRecords) do
    FRecords[i].Free;
  SetLength(FRecords, 0);
end;

{==============================================================================}
constructor TDiaryLocalSource.Create(const FileName: string);
{==============================================================================}
begin
  FModified := False;
  FFileName := FileName;

  if (FileExists(FileName)) then
    LoadFromFile(FileName);
end;

{==============================================================================}
destructor TDiaryLocalSource.Destroy;
{==============================================================================}
begin
  Clear;
  inherited;
end;

{==============================================================================}
function TDiaryLocalSource.GetRecordIndex(ID: TCompactGUID): integer;
{==============================================================================}
var
  // L,R: integer;
  i: integer;
begin
  {L := 0;
  R := High(FRecords);
  while (L <= R) do
  begin
    Result := (L + R) div 2;
    if (FPages[Result].Date < Date) then L := Result + 1 else
    if (FPages[Result].Date > Date) then R := Result - 1 else
      Exit;
  end;
  Result := -1;  }

  for i := 0 to High(FRecords) do
  if (FRecords[i].ID = ID) then
  begin
    Result := i;
    Exit;
  end;

  Result := -1;
end;

{==============================================================================}
procedure TDiaryLocalSource.LoadFromFile(const FileName: string);
{==============================================================================}

  {procedure Load_v1(S: TStrings);
  var
    Pages: TPageDataList;
    i: integer;
  begin
    s.Delete(0);
    s.Delete(0);
    TPageData.Read(S, LocalFmt, Pages);

    // дл€ проверки сортировки и дублей используем вспомогательный список
    for i := 0 to High(Pages) do
      Add(Pages[i]);
  end;

  procedure Load_v2(S: TStrings);
  var
    Pages: TPageDataList;
    i: integer;
  begin
    TPageData.Read(S, LocalFmt, Pages);
    for i := 0 to High(Pages) do
      Pages[i].TimeStamp := LocalToUTC(Pages[i].TimeStamp);

    // дл€ проверки сортировки и дублей используем вспомогательный список
    for i := 0 to High(Pages) do
      Add(Pages[i]);
  end;

  procedure Load_v3(S: TStrings);
  var
    Pages: TPageDataList;
    i: integer;
  begin
    s.Delete(0);
    TPageData.Read(S, LocalFmt, Pages);

    // дл€ проверки сортировки и дублей используем вспомогательный список
    for i := 0 to High(Pages) do
      Add(Pages[i]);
  end;     }

  procedure Load_v4(S: TStrings);
  var
    i, k: integer;
  begin
    s.Delete(0);

    SetLength(FRecords, S.Count);
    k := 0;

    for i := 0 to S.Count - 1 do
    if (S[i] <> '') then
    begin
      FRecords[k] := TRecordData.Create;
      FRecords[k].Read(S[i]);
      inc(k);
    end;

    SetLength(FRecords, k);
  end;

var
  s: TStrings;
  BaseVersion: integer;
begin
  Clear;

  s := TStringList.Create;

  try
    s.LoadFromFile(FileName);

    // самый старый формат
   { if (s.Count >= 2) and (s[0] = 'DIARYFMT') then
    begin
      Load_v1(s);
    end else }

    // формат с указанием версии
    if (s.Count >= 1) and (pos('VERSION=', s[0]) = 1) then
    begin
      BaseVersion := StrToInt(TextAfter(s[0], '='));
      case BaseVersion of
        //3:    Load_v3(s);
        4:    Load_v4(s);
        else raise Exception.Create('Unsupported database format');
      end;
    end else

      raise Exception.Create('Unsupported database format');

    // формат без версии - форматируем
    {begin
      Load_v2(s);
    end; }

  finally
    s.Free;
  end;

  FModified := False;  // да)
end;

{==============================================================================}
procedure TDiaryLocalSource.SaveToFile(const FileName: string);
{==============================================================================}

  (*function Escape(S: string): string;
  var
    i: integer;
  begin
    Result := '';
    for i := 1 to Length(S) do
    begin
      if (S[i] = '"') then
        Result := Result + '\"'
      else
        Result := Result + S[i];
    end; 
  end;

  function BlockBlood(R: TBloodRecord): string;
  begin
    Result := Format('{"type":"blood","value":"%.1f","finger":"%d"}', [R.Value, R.Finger]);
  end;

  function BlockIns(R: TInsRecord): string;
  begin
    Result := Format('{"type":"insulin","value":"%.1f"}', [R.Value]);
  end;

  function BlockFood(R: TFoodMassed): string;
  begin
    Result := Format('{"name":"%s","prots":"%.1f","fats":"%.1f","carbs":"%.1f","value":"%.1f","mass":"%.1f"}',
      [Escape(R.Name), R.RelProts, R.RelFats, R.RelCarbs, R.RelValue, R.Mass]);
  end;

  function BlockMeal(R: TMealRecord): string;

    function FmtBoolean(f: boolean): string;
    begin
      if (f) then
        Result := 'true'
      else
        Result := 'false';
    end;

  var
    i: integer;
  begin
    Result := Format('{"type":"meal","short":"%s","content":[',
      [FmtBoolean(R.ShortMeal)]);
    for i := 0 to R.Count - 1 do
    begin
      Result := Result + BlockFood(R[i]);
      if (i < R.Count - 1) then
        Result := Result + ',';
    end;
    Result := Result + ']}';
  end;

  function BlockNote(R: TNoteRecord): string;
  begin
    Result := Format('{"type":"note","text":"%s"}', [Escape(R.Text)]);
  end;

  function BlockRecord(R: TCustomRecord): string;
  var
    Header, Data: string;
  begin
    Header := Format('%s'#9'%s'#9'%s'#9'%d'#9'-'#9,
      [DateTimeToStr(R.NativeTime, STD_DATETIME_FMT),
       DateTimeToStr(R.NativeTime, STD_DATETIME_FMT),
       R.ID,
       R.Version]);

    if (R.RecType = TBloodRecord) then Data := BlockBlood(TBloodRecord(R)) else
    if (R.RecType = TInsRecord)   then Data := BlockIns(TInsRecord(R)) else
    if (R.RecType = TMealRecord)  then Data := BlockMeal(TMealRecord(R)) else
    if (R.RecType = TNoteRecord)  then Data := BlockNote(TNoteRecord(R)) else
      raise Exception.Create('Unknown record type: ' + R.RecTYpe.ClassName);

    Result := Header + Data;
  end; *)

  function BlockRecordData(R: TRecordData): string;
  begin
    Result := Format('%s'#9'%s'#9'%s'#9'%d'#9'%s'#9'%s',
      [DateTimeToStr(R.NativeTime, STD_DATETIME_FMT),
       DateTimeToStr(R.TimeStamp, STD_DATETIME_FMT),
       R.ID,
       R.Version,
       DiaryRoutines.WriteBoolean(R.Deleted),
       R.Data]);
  end;

  (*function BlockPage(P: TDiaryPage): string;
  var
    i: integer;
  begin
    Result := '';

    {Result := Format('{date: "%s", stamp: "%s", version: %d, content: [',
      [DateToStr(P.Date),
      DateTimeToStr(P.TimeStamp),
      P.Version]);
    for i := 0 to P.Count - 1 do
    begin
      if (P[i].RecType = TBloodRecord) then Result := Result + BlockBlood(TBloodRecord(P[i])) else
      if (P[i].RecType = TInsRecord)   then Result := Result + BlockIns(TInsRecord(P[i])) else
      if (P[i].RecType = TMealRecord)  then Result := Result + BlockMeal(TMealRecord(P[i])) else
      if (P[i].RecType = TNoteRecord)  then Result := Result + BlockNote(TNoteRecord(P[i]));

      if (i < P.Count - 1) then
        Result := Result + ', ';
    end;
    Result := Result + ']';
    }

    for i := 0 to P.Count - 1 do
      Result := Result + BlockRecord(P[i]) + #13;
  end; *)

var
  DataLine: string;
  i: integer;
  DS: char;
  S: TStrings;
begin
  DS := SysUtils.DecimalSeparator;
  SysUtils.DecimalSeparator := '.';

  S := TStringList.Create;
  try
    s.Add('VERSION=4');

    for i := 0 to High(FRecords) do
    begin
      //SysUtils.DecimalSeparator := '.';
      DataLine := BlockRecordData(FRecords[i]);
      S.Add(DataLine);
      //SysUtils.DecimalSeparator := DS;
    end;

    S.SaveToFile(FileName);
    FModified := False;
  finally
    SysUtils.DecimalSeparator := DS;
    S.Free;
  end;
end;

{==============================================================================}
function TDiaryLocalSource.Trace(Index: integer): integer;
{==============================================================================}
var
  Temp: TRecordData;
  Changed: boolean;
begin
  Result := Index;
  if (Index >= 0) and (Index <= High(FRecords)) then
  begin
    Temp := FRecords[Result];
    Changed := False;

    { прогон вверх }
    while (Result > 0)and(FRecords[Result - 1].NativeTime > Temp.NativeTime) do
    begin
      FRecords[Result] := FRecords[Result - 1];
      dec(Result);
      Changed := True;
    end;

    { прогон вниз }
    while (Result < High(FRecords))and(FRecords[Result + 1].NativeTime < Temp.NativeTime) do
    begin
      FRecords[Result] := FRecords[Result + 1];
      inc(Result);
      Changed := True;
    end;

    { запись }
    if Changed then
      FRecords[Result] := Temp;
  end;
end;


{==============================================================================}
function TDiaryLocalSource.FindById(ID: TCompactGUID): TVersioned;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FRecords) do
  if (FRecords[i].ID = ID) then
  begin
    Result := FRecords[i].Deserialize;
    Exit;
  end;

  Result := nil;
end;

{==============================================================================}
function TDiaryLocalSource.FindChanged(Since: TDateTime): TVersionedList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, 0);

  for i := 0 to High(FRecords) do
  if (FRecords[i].TimeStamp >= Since) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := FRecords[i].Deserialize;
  end;
end;

{==============================================================================}
function TDiaryLocalSource.FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, 0);

  for i := 0 to High(FRecords) do
  if (not FRecords[i].Deleted) and
     (FRecords[i].NativeTime >= TimeFrom) and
     (FRecords[i].NativeTime <= TimeTo) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := FRecords[i].Deserialize;
  end;

  Log(DEBUG, Format('Extracting diary records between %s and %s; %d items found', [
    DateTimeToStr(TimeFrom, STD_DATETIME_FMT),
    DateTimeToStr(TimeTo, STD_DATETIME_FMT),
    Length(Result)]));
end;

{==============================================================================}
procedure TDiaryLocalSource.Delete(ID: TCompactGUID);
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FRecords) do
  if (FRecords[i].ID = ID) then
  begin
    FRecords[i].Deleted := True;
    FRecords[i].Modified;
    FModified := True;
    SaveToFile(FFileName);
    Exit;
  end;
end;

{==============================================================================}
procedure TDiaryLocalSource.Save(const Recs: TVersionedList);
{==============================================================================}

 function PureLength(const S: string): integer;
  var
    i: integer;
  begin
    Result := Length(S);
    for i := 1 to Length(S) do
      if (S[i] = #10) or (S[i] = #13) then
        dec(Result);
  end;

  {function Worry(PageOld, PageNew: TPageData): boolean;
  var
    Msg: string;
    MsgType: TMsgDlgType;
  begin
    Msg := '';
    MsgType := mtConfirmation;

    if (PageOld.Version > PageNew.Version) then
    begin
      MsgType := mtWarning;
      Msg := Msg + '* ¬ерси€: ' + IntToStr(PageOld.Version) + ' --> ' + IntToStr(PageNew.Version) + #13;
    end;

    if (PageOld.TimeStamp > PageNew.TimeStamp) then
      Msg := Msg + '* ¬рем€: ' + DateTimeToStr(PageOld.TimeStamp) + ' --> ' + DateTimeToStr(PageNew.TimeStamp) + #13;

    if (PureLength(PageNew.Page) - PureLength(PageOld.Page) < -20) then
      Msg := Msg + '* –азмер: ' + IntToStr(Length(PageOld.Page)) + ' --> ' + IntToStr(Length(PageNew.Page)) + #13;

    Result :=
      (Msg <> '') and
      (MessageDlg(
        '—траница ' + DateToStr(PageOld.Date)+' будет перезаписана.'+#13+
        'ќбнаружены подозрени€:' + #13 +
        Msg + #13+
        'ѕродолжить?',
         MsgType, [mbYes,mbNo],0) <> 6); // mrYes
  end; }

var
  i: integer;
begin
  if (Length(Recs) > 0) then
  begin
    for i := 0 to High(Recs) do
      AddToInternal(Recs[i] as TCustomRecord);

    FModified := True;
    SaveToFile(FFileName);
  end;
end;

initialization
  // 02.04.1992 09:45:00
  GetLocaleFormatSettings(GetThreadLocale, LocalFmt);
  LocalFmt.DateSeparator := '.';
  LocalFmt.TimeSeparator := ':';
  LocalFmt.ShortDateFormat := 'dd.mm.yyyy';
  LocalFmt.LongTimeFormat := 'hh:nn:ss';
end.


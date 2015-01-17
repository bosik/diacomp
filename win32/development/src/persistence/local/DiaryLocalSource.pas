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
var
  json: TlkJSONobject;
begin
  json := TlkJSON.ParseText(Data) as TlkJSONobject;

  if Assigned(json) then
  try
    Result := DiaryPageSerializer.ParseDiaryRecord(json);

    Result.Time := Time;
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
  Time := Rec.Time;
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

  Time := StrToDateTime(STime, STD_DATETIME_FMT);
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
      DateTimeToStr(Time, STD_DATETIME_FMT),
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

  function BlockRecordData(R: TRecordData): string;
  begin
    Result := Format('%s'#9'%s'#9'%s'#9'%d'#9'%s'#9'%s',
      [DateTimeToStr(R.Time, STD_DATETIME_FMT),
       DateTimeToStr(R.TimeStamp, STD_DATETIME_FMT),
       R.ID,
       R.Version,
       DiaryRoutines.WriteBoolean(R.Deleted),
       R.Data]);
  end;

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
    while (Result > 0)and(FRecords[Result - 1].Time > Temp.Time) do
    begin
      FRecords[Result] := FRecords[Result - 1];
      dec(Result);
      Changed := True;
    end;

    { прогон вниз }
    while (Result < High(FRecords))and(FRecords[Result + 1].Time < Temp.Time) do
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
     (FRecords[i].Time >= TimeFrom) and
     (FRecords[i].Time <= TimeTo) then
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


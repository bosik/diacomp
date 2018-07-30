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

  AutoLog {TODO: debug only},
  HashService,
  MerkleTree;

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
    FBaseFileName: string;

    function AddToInternal(Rec: TCustomRecord): integer;
    procedure Clear;
    function GetRecordIndex(ID: TCompactGUID): integer;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Trace(Index: integer): integer;
  public
    function Count(Prefix: TCompactGUID): integer; override;
    constructor Create(const BaseFileName: string);
    destructor Destroy; override;
    procedure Delete(ID: TCompactGUID); override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; override;
    function FindById(ID: TCompactGUID): TVersioned; override;
    function FindByIdPrefix(Prefix: TCompactGUID): TVersionedList; override;
    function GetHashTree(): THashTree; override;
    procedure Save(const Recs: TVersionedList); override;

    // свойства
    // TODO: think about it
    //property Modified: boolean read FModified {write FModified};
 end;

implementation

uses ObjectService;

var
  LocalFmt: TFormatSettings;
  
const
  ShowUpdateWarning = True;
  CURRENT_VERSION   = 5;

{ TRecordData }

{======================================================================================================================}
function TRecordData.Deserialize(): TCustomRecord;
{======================================================================================================================}
var
  json: TlkJSONobject;
begin
  json := TlkJSON.ParseText(Data) as TlkJSONobject;

  if Assigned(json) then
  try
    Result := DiaryPageSerializer.ParseDiaryRecord(json);

    Result.Time := Time;
    Result.TimeStamp := TimeStamp;
    Result.Hash := Hash;
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

{======================================================================================================================}
procedure TRecordData.Serialize(Rec: TCustomRecord);
{======================================================================================================================}
var
  Json: TlkJSONobject;
begin
  try
    Json := DiaryPageSerializer.SerializeDiaryRecord(Rec);

    ID := Rec.ID;
    Hash := Rec.Hash;
    TimeStamp := Rec.TimeStamp;
    Version := Rec.Version;
    Deleted := Rec.Deleted;
    Data := TlkJSON.GenerateText(Json);

    // cache
    Time := Rec.Time;
  finally
    Json.Free;
  end;
end;

{======================================================================================================================}
procedure TRecordData.Read(S: string);
{======================================================================================================================}

  // TODO 1: remove this magic after migration
  function Count(c: char; const s: string): integer;
  var
    i: integer;
  begin
    Result := 0;
    for i := 1 to Length(s) do
    if (s[i] = c) then
      inc(Result);
  end;
  
var
  STime: string;
  STimeStamp: string;
  SHash: string;
  SID: string;
  SVersion: string;
  SDeleted: string;
begin
  STime := TextBefore(S, #9);      S := TextAfter(S, #9);
  STimeStamp := TextBefore(S, #9); S := TextAfter(S, #9);

  // TODO 1: remove this magic after migration
  if (Count(#9, S) = 4) then
  begin
    SHash := TextBefore(S, #9);      S := TextAfter(S, #9);
  end else
  begin
    SHash := CreateCompactGUID();
  end;

  SID := TextBefore(S, #9);        S := TextAfter(S, #9);
  SVersion := TextBefore(S, #9);   S := TextAfter(S, #9);
  SDeleted := TextBefore(S, #9);   S := TextAfter(S, #9);

  Time := StrToDateTime(STime, STD_DATETIME_FMT);
  TimeStamp := StrToDateTime(STimeStamp, STD_DATETIME_FMT);
  Hash := SHash;
  ID := SID;
  Version := StrToInt(SVersion);
  Deleted := ReadBoolean(SDeleted);
  Data := S;
end;

{======================================================================================================================}
function TRecordData.Write(): string;
{======================================================================================================================}
begin
  Result := Format('%s'#9'%s'#9'%s'#9'%s'#9'%d'#9'%s'#9'%s',
    [
      DateTimeToStr(Time, STD_DATETIME_FMT),
      DateTimeToStr(TimeStamp, STD_DATETIME_FMT),
      Hash,
      ID,
      Version,
      WriteBoolean(Deleted),
      Data
    ]
  );
end;

{ TDiaryLocalSource }

{======================================================================================================================}
function TDiaryLocalSource.AddToInternal(Rec: TCustomRecord): integer;
{======================================================================================================================}
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

  {* tree outdated *}
end;

{======================================================================================================================}
procedure TDiaryLocalSource.Clear;
{======================================================================================================================}
var
  i: integer;
begin
  { вызываетс€ в деструкторе и перед загрузкой из файла }

  if (Length(FRecords) > 0) then
    FModified := True;

  for i := 0 to High(FRecords) do
    FreeAndNil(FRecords[i]);
  SetLength(FRecords, 0);
end;

{======================================================================================================================}
function TDiaryLocalSource.Count(Prefix: TCompactGUID): integer;
{======================================================================================================================}
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(FRecords) do
  if (StartsWith(FRecords[i].ID, Prefix)) then
    inc(Result);
end;

{======================================================================================================================}
constructor TDiaryLocalSource.Create(const BaseFileName: string);
{======================================================================================================================}
begin
  FModified := False;
  FBaseFileName := BaseFileName;

  if (FileExists(BaseFileName)) then
    LoadFromFile(BaseFileName);
end;

{======================================================================================================================}
destructor TDiaryLocalSource.Destroy;
{======================================================================================================================}
begin
  Clear;
  inherited;
end;

{======================================================================================================================}
function TDiaryLocalSource.GetRecordIndex(ID: TCompactGUID): integer;
{======================================================================================================================}
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

{======================================================================================================================}
procedure TDiaryLocalSource.LoadFromFile(const FileName: string);
{======================================================================================================================}

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

    function InsertHash(S: string): string;
    var
      i, k: integer;
    begin
      i := 1;
      k := 0;
      while (i <= Length(S)) do
      begin
        if (S[i] = #9) then
        begin
          inc(k);
          if (k = 2) then
            break;
        end;
        inc(i);
      end;

      Insert(CreateCompactGUID() + #9, S, i + 1);
      Result := S;
    end;

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
      FRecords[k].Read(InsertHash(S[i]));
      inc(k);
    end;

    SetLength(FRecords, k);
  end;

  procedure Load_v5(S: TStrings);
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
        5:    Load_v5(s);
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

  if (baseVersion < CURRENT_VERSION) then
    SaveToFile(FileName);

  FModified := False;  // да)

  {* tree outdated *}
end;

{======================================================================================================================}
procedure TDiaryLocalSource.SaveToFile(const FileName: string);
{======================================================================================================================}
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
    s.Add('VERSION=' + IntToStr(CURRENT_VERSION));

    for i := 0 to High(FRecords) do
    begin
      //SysUtils.DecimalSeparator := '.';
      DataLine := FRecords[i].Write();
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

{======================================================================================================================}
function TDiaryLocalSource.Trace(Index: integer): integer;
{======================================================================================================================}
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

{======================================================================================================================}
function TDiaryLocalSource.FindById(ID: TCompactGUID): TVersioned;
{======================================================================================================================}
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

{======================================================================================================================}
function TDiaryLocalSource.FindChanged(Since: TDateTime): TVersionedList;
{======================================================================================================================}
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

{======================================================================================================================}
function TDiaryLocalSource.FindByIdPrefix(Prefix: TCompactGUID): TVersionedList;
{======================================================================================================================}
var
  i: integer;
begin
  SetLength(Result, 0);

  for i := 0 to High(FRecords) do
  if (StartsWith(FRecords[i].ID, Prefix)) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := FRecords[i].Deserialize;
  end;
end;

{======================================================================================================================}
function TDiaryLocalSource.FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList;
{======================================================================================================================}
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

  Log(VERBOUS, Format('Extracting diary records between %s and %s; %d items found', [
    DateTimeToStr(TimeFrom, STD_DATETIME_FMT),
    DateTimeToStr(TimeTo, STD_DATETIME_FMT),
    Length(Result)]));
end;

{======================================================================================================================}
procedure TDiaryLocalSource.Delete(ID: TCompactGUID);
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to High(FRecords) do
  if (FRecords[i].ID = ID) then
  begin
    FRecords[i].Deleted := True;
    FRecords[i].Modified();
    FModified := True;
    SaveToFile(FBaseFileName);

    {* tree outdated *}
    Exit;
  end;
end;

{======================================================================================================================}
function TDiaryLocalSource.GetHashTree(): THashTree;
{======================================================================================================================}
var
  Tree: TMerkleTree;
  i: integer;
begin
  Tree := TMerkleTree.Create();

  for i := 0 to High(FRecords) do
  begin
    Tree.Put(FRecords[i].ID, FRecords[i].Hash, MAX_PREFIX_SIZE, False);
  end;

  Tree.UpdateHash();
  Result := Tree;
end;

{======================================================================================================================}
procedure TDiaryLocalSource.Save(const Recs: TVersionedList);
{======================================================================================================================}

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
    SaveToFile(FBaseFileName);

    {* tree outdated *}
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


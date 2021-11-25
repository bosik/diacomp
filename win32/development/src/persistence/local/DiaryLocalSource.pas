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
  TDiaryEntity = record
    ID: TCompactGUID;
    TimeStamp: TDateTime;
    Hash: TCompactGUID;
    Version: integer;
    Deleted: boolean;

    Data: String;
    TimeCache: TDateTime;
  end;

  TDiaryEntityList = array of TDiaryEntity;

  TDiaryLocalSource = class (TDiaryDAO)
  private
    FRecords: TDiaryEntityList;
    FBaseFileName: string;

    function AddToInternal(Rec: TVersioned): integer;
    procedure Clear;
    function GetRecordIndex(ID: TCompactGUID): integer;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Trace(Index: integer): integer;
  public
    function Count(Prefix: TCompactGUID): integer; override;
    constructor Create(const BaseFileName: string);
    destructor Destroy; override;
    function FindChanged(Since: TDateTime): TVersionedList; override;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TVersionedList; override;
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
function Deserialize(Entity: TDiaryEntity): TVersioned;
{======================================================================================================================}
var
  json: TlkJSONobject;
begin
  json := TlkJSON.ParseText(Entity.Data) as TlkJSONobject;

  if Assigned(json) then
  try
    Result := TVersioned.Create();

    Result.ID := Entity.ID;
    Result.TimeStamp := Entity.TimeStamp;
    Result.Hash := Entity.Hash;
    Result.Version := Entity.Version;
    Result.Deleted := Entity.Deleted;
    Result.Data := DiaryPageSerializer.ParseDiaryRecord(json);
  finally
    json.Free;
  end else
  begin
    raise Exception.Create('Invalid JSON: ' + Entity.Data);
  end;
end;

{======================================================================================================================}
function Serialize(Rec: TVersioned): TDiaryEntity;
{======================================================================================================================}
var
  Json: TlkJSONobject;
begin
  try
    Json := DiaryPageSerializer.SerializeDiaryRecord(Rec.Data as TCustomRecord);

    Result.ID := Rec.ID;
    Result.TimeStamp := Rec.TimeStamp;
    Result.Hash := Rec.Hash;
    Result.Version := Rec.Version;
    Result.Deleted := Rec.Deleted;
    Result.Data := TlkJSON.GenerateText(Json);

    // cache
    Result.TimeCache := (Rec.Data as TCustomRecord).Time;
  finally
    Json.Free;
  end;
end;

{======================================================================================================================}
function Read_v4(S: string): TDiaryEntity;
{======================================================================================================================}
var
  Columns: TStringArray;
begin
  Columns := Split(S, #9);

  Result.TimeCache := ParseDateTime(Columns[0]);
  Result.TimeStamp := ParseDateTime(Columns[1]);
  Result.Hash := CreateCompactGUID(); // generate new hash
  Result.ID := Columns[2];
  Result.Version := StrToInt(Columns[3]);
  Result.Deleted := ReadBoolean(Columns[4]);
  Result.Data := Columns[5];
end;

{======================================================================================================================}
function Read_v5(S: string): TDiaryEntity;
{======================================================================================================================}
var
  Columns: TStringArray;
begin
  Columns := Split(S, #9);

  Result.TimeCache := ParseDateTime(Columns[0]);
  Result.TimeStamp := ParseDateTime(Columns[1]);
  Result.Hash := Columns[2];
  Result.ID := Columns[3];
  Result.Version := StrToInt(Columns[4]);
  Result.Deleted := ReadBoolean(Columns[5]);
  Result.Data := Columns[6];
end;

{======================================================================================================================}
function Read_v6(S: string): TDiaryEntity;
{======================================================================================================================}
var
  Columns: TStringArray;
begin
  Columns := Split(S, #9);

  // represents next format (normalized order), not in use
  Result.TimeCache := ParseDateTime(Columns[0]);
  Result.ID := Columns[1];
  Result.TimeStamp := ParseDateTime(Columns[2]);
  Result.Hash := Columns[3];
  Result.Version := StrToInt(Columns[4]);
  Result.Deleted := ReadBoolean(Columns[5]);
  Result.Data := Columns[6];
end;

{======================================================================================================================}
function Write(Entity: TDiaryEntity): string;
{======================================================================================================================}
begin
  Result := Format('%s'#9'%s'#9'%s'#9'%s'#9'%d'#9'%s'#9'%s',
    [
      FormatDateTime(Entity.TimeCache),
      FormatDateTime(Entity.TimeStamp),
      Entity.Hash,
      Entity.ID,
      Entity.Version,
      WriteBoolean(Entity.Deleted),
      Entity.Data
    ]
  );
end;

{ TDiaryLocalSource }

{======================================================================================================================}
function TDiaryLocalSource.AddToInternal(Rec: TVersioned): integer;
{======================================================================================================================}
begin
  // 1. Проверка дублей
  // 2. Сортировка

  Result := GetRecordIndex(Rec.ID);
  if (Result = -1) then
  begin
    Result := Length(FRecords);
    SetLength(FRecords, Length(FRecords) + 1);
  end;

  FRecords[Result] := Serialize(Rec);
  Result := Trace(Result);

  {* tree outdated *}
end;

{======================================================================================================================}
procedure TDiaryLocalSource.Clear;
{======================================================================================================================}
begin
  { вызывается в деструкторе и перед загрузкой из файла }
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

  procedure Load_v4(S: TStrings);
  var
    i, k: integer;
  begin
    SetLength(FRecords, S.Count - 1); // skip header
    k := 0;

    for i := 1 to S.Count - 1 do
    if (S[i] <> '') then
    begin
      FRecords[k] := Read_v4(S[i]);
      inc(k);
    end;

    SetLength(FRecords, k);
  end;

  procedure Load_v5(S: TStrings);
  var
    i, k: integer;
  begin
    SetLength(FRecords, S.Count - 1);  // skip header
    k := 0;

    for i := 1 to S.Count - 1 do
    if (S[i] <> '') then
    begin
      FRecords[k] := Read_v5(S[i]);
      inc(k);
    end;

    SetLength(FRecords, k);
  end;

  procedure Load_v6(S: TStrings);
  var
    i, k: integer;
  begin
    SetLength(FRecords, S.Count - 1);  // skip header
    k := 0;

    for i := 1 to S.Count - 1 do
    if (S[i] <> '') then
    begin
      FRecords[k] := Read_v6(S[i]);
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

    // формат с указанием версии
    if (s.Count >= 1) and (pos('VERSION=', s[0]) = 1) then
    begin
      BaseVersion := StrToInt(TextAfter(s[0], '='));
      case BaseVersion of
        //3:    Load_v3(s);
        4:    Load_v4(s);
        5:    Load_v5(s);
        6:    Load_v6(s);
        else raise Exception.Create('Unsupported database format: VERSION=' + IntToStr(BaseVersion));
      end;
    end else
      raise Exception.Create('Unsupported database format');

  finally
    s.Free;
  end;

  if (baseVersion < CURRENT_VERSION) then
    SaveToFile(FileName); // reformat

  {* tree outdated *}
end;

{======================================================================================================================}
procedure TDiaryLocalSource.SaveToFile(const FileName: string);
{======================================================================================================================}
var
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
      S.Add(Write(FRecords[i]));
      //SysUtils.DecimalSeparator := DS;
    end;

    S.SaveToFile(FileName);
  finally
    SysUtils.DecimalSeparator := DS;
    S.Free;
  end;
end;

{======================================================================================================================}
function TDiaryLocalSource.Trace(Index: integer): integer;
{======================================================================================================================}

  procedure Swap(var A, B: TDiaryEntity);
  var
    C: TDiaryEntity;
  begin
    C := A;
    A := B;
    B := C;
  end;

begin
  Result := Index;

  { прогон вверх }
  while (Result > 0)and(FRecords[Result - 1].TimeCache > FRecords[Result].TimeCache) do
  begin
    Swap(FRecords[Result], FRecords[Result - 1]);
    dec(Result);
  end;

  { прогон вниз }
  while (Result < High(FRecords))and(FRecords[Result + 1].TimeCache < FRecords[Result].TimeCache) do
  begin
    Swap(FRecords[Result], FRecords[Result + 1]);
    inc(Result);
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
    Result := Deserialize(FRecords[i]);
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
    Result[High(Result)] := Deserialize(FRecords[i]);
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
    Result[High(Result)] := Deserialize(FRecords[i]);
  end;
end;

{======================================================================================================================}
function TDiaryLocalSource.FindPeriod(TimeFrom, TimeTo: TDateTime): TVersionedList;
{======================================================================================================================}
var
  i: integer;
begin
  SetLength(Result, 0);

  for i := 0 to High(FRecords) do
  if (not FRecords[i].Deleted) and
     (FRecords[i].TimeCache >= TimeFrom) and
     (FRecords[i].TimeCache <= TimeTo) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Deserialize(FRecords[i]);
  end;

  Log(VERBOUS, Format('Extracting diary records between %s and %s; %d items found', [
    FormatDateTime(TimeFrom),
    FormatDateTime(TimeTo),
    Length(Result)]));
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
  Result := Tree; // TMerkleTree is child of THashTree
end;

{======================================================================================================================}
procedure TDiaryLocalSource.Save(const Recs: TVersionedList);
{======================================================================================================================}
var
  i: integer;
begin
  if (Length(Recs) > 0) then
  begin
    for i := 0 to High(Recs) do
      AddToInternal(Recs[i]);

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


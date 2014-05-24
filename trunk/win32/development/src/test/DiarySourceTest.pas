unit DiarySourceTest;

interface

uses
  Windows,
  SysUtils,
  DateUtils,
  TestFrameWork,
  DiaryDAO,
  DiaryPage,
  DiaryRecords,
  DiaryRoutines,
  BusinessObjects;

type
  TDiarySourceTest = class(TTestCase)
  protected
    Source: TDiaryDAO;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CompareRecs(ExpRec, ActRec: TCustomRecord); overload;
    procedure CompareRecs(const ExpRecs, ActRecs: TRecordList); overload;

    procedure SetupSource; virtual; abstract;
    procedure TeardownSource; virtual; abstract;
  published
    procedure TestFindChanged;
    procedure TestFindPeriod;
    procedure TestModifying;
  end;

implementation

uses Math;

var
  DemoRecs: TRecordList;

{==============================================================================}
procedure CreateDemoRecords;
{==============================================================================}

  function BuildBloodRecord(): TBloodRecord;
  begin
    Result := TBloodRecord.Create(EncodeDateTime(2002, 03, 15, 23, 15, 20, 128), 4.9, 3);
    Result.ID := CreateCompactGUID;
    Result.Deleted := False;
    Result.Modified;
  end;

  function BuildInsRecord(): TInsRecord;
  begin
    Result := TInsRecord.Create(EncodeDateTime(2002, 03, 15, 23, 20, 47, 319), 12);
    Result.ID := CreateCompactGUID;
    Result.Deleted := False;
    Result.Modified;
  end;

  function BuildMealRecord(): TMealRecord;
  begin
    Result := TMealRecord.Create(EncodeDateTime(2002, 03, 15, 23, 40, 0, 699), False);
    Result.Add(TFoodMassed.Create('Карбонат "Восточный" (Черн)', 9.9, 26.3, 0, 276, 90));
    Result.Add(TFoodMassed.Create('Хлеб', 5.5, 0.9, 44.1, 206.3, 42));
    Result.ID := CreateCompactGUID;
    Result.Deleted := False;
    Result.Modified;
  end;

  function BuildNoteRecord(): TNoteRecord;
  begin
    Result := TNoteRecord.Create(EncodeDateTime(2002, 03, 15, 23, 55, 01, 981), 'Just a "тестовая" {record} with single '' quote');
    Result.ID := CreateCompactGUID;
    Result.Deleted := False;
    Result.Modified;
  end;

  // TODO: check deleting
  
begin
  SetLength(DemoRecs, 4);

  DemoRecs[0] := BuildBloodRecord();
  DemoRecs[1] := BuildInsRecord();
  DemoRecs[2] := BuildMealRecord();
  DemoRecs[3] := BuildNoteRecord();
end;

{ TDiarySourceTest }

{==============================================================================}
procedure TDiarySourceTest.SetUp;
{==============================================================================}
begin
  inherited;
  CreateDemoRecords;

  if (Source <> nil) then
    FreeAndNil(Source);
  SetupSource;

  Source.Save(DemoRecs);

  FreeAndNil(Source);
  SetupSource;
end;

{==============================================================================}
procedure TDiarySourceTest.TearDown;
{==============================================================================}
begin
  inherited;
  TeardownSource;
end;

{==============================================================================}
procedure TDiarySourceTest.CompareRecs(ExpRec, ActRec: TCustomRecord);
{==============================================================================}
var
  ExpMeal, ActMeal: TMealRecord;
  k: integer;
begin
  // common
  CheckEquals(DateTimeToStr(ExpRec.NativeTime), DateTimeToStr(ActRec.NativeTime), 'Time differs');
  CheckEquals(ExpRec.ID, ActRec.ID, 'ID differs');
  CheckEquals(DateTimeToStr(ExpRec.TimeStamp), DateTimeToStr(ActRec.TimeStamp), 'TimeStamp differs');
  CheckEquals(ExpRec.Version, ActRec.Version, 'Version differs');
  CheckEquals(ExpRec.Deleted, ActRec.Deleted, 'Deleted flag differs');

  // specific
  if (ExpRec.RecType = TBloodRecord) then
  begin
    CheckEquals(TBloodRecord(ExpRec).Value, TBloodRecord(ActRec).Value);
    CheckEquals(TBloodRecord(ExpRec).Finger, TBloodRecord(ActRec).Finger);
  end else
  if (ExpRec.RecType = TInsRecord) then
  begin
    CheckEquals(TInsRecord(ExpRec).Value, TInsRecord(ActRec).Value);
  end else
  if (ExpRec.RecType = TMealRecord) then
  begin
    ExpMeal := TMealRecord(ExpRec);
    ActMeal := TMealRecord(ExpRec);

    CheckEquals(ExpMeal.ShortMeal, ActMeal.ShortMeal);
    CheckEquals(ExpMeal.Count, ActMeal.Count);

    for k := 0 to ExpMeal.Count - 1 do
    begin
      CheckEquals(ExpMeal[k].Write, ActMeal[k].Write);

      CheckEquals(ExpMeal[k].Name, ActMeal[k].Name);
      CheckEquals(ExpMeal[k].RelProts, ActMeal[k].RelProts);
      CheckEquals(ExpMeal[k].RelFats, ActMeal[k].RelFats);
      CheckEquals(ExpMeal[k].RelCarbs, ActMeal[k].RelCarbs);
      CheckEquals(ExpMeal[k].RelValue, ActMeal[k].RelValue);
      CheckEquals(ExpMeal[k].Mass, ActMeal[k].Mass);
    end;
  end else
  if (ExpRec.RecType = TNoteRecord) then
  begin
    CheckEquals(TNoteRecord(ExpRec).Text, TNoteRecord(ActRec).Text);
  end else;
end;

{==============================================================================}
procedure TDiarySourceTest.CompareRecs(const ExpRecs, ActRecs: TRecordList);
{==============================================================================}
var
  i: integer;
begin
  CheckEquals(Length(ExpRecs), Length(ActRecs), 'Count of records differs');
  for i := 0 to High(ExpRecs) do
  begin
    CompareRecs(ExpRecs[i], ActRecs[i]);
  end;
end;

{==============================================================================}
procedure TDiarySourceTest.TestFindChanged;
{==============================================================================}
var
  Recs: TRecordList;
begin
  // This test checks which records was modified last 5 seconds;
  // as far as it is run right after posting records, expected result
  // is all records persisted

  Recs := VersionedToRecord(Source.FindChanged(GetTimeUTC() - 1));
  CompareRecs(DemoRecs, Recs);
end;

{==============================================================================}
procedure TDiarySourceTest.TestFindPeriod;
{==============================================================================}
var
  Recs: TRecordList;
begin
  Recs := Source.FindPeriod(EncodeDate(1990, 1, 1), EncodeDate(2020, 1, 1));
  CompareRecs(DemoRecs, Recs);
end;

{==============================================================================}
procedure TDiarySourceTest.TestModifying;
{==============================================================================}

  function BuildNoteRecord(): TNoteRecord;
  begin
    Result := TNoteRecord.Create(EncodeDateTime(2003, 08, 16, 03, 10, 46, 001), 'Temp note');
    Result.ID := '933dee2557ebbb1a548e4e75074744b2';
    Result.Deleted := False;
    Result.Version := 18;
    Result.TimeStamp := EncodeDateTime(2003, 08, 17, 18, 28, 30, 183)
  end;

var
  OrgRec: TCustomRecord;
  RestoredRec: TCustomRecord;
  RestoredRecs: TRecordList;

  Recs: TRecordList;
begin
  OrgRec := BuildNoteRecord();
  Source.Add(OrgRec);
  OrgRec.Free;

  // -------------------------
  Source.Free;
  SetupSource;

  // find by ID
  OrgRec := BuildNoteRecord();
  RestoredRec := VersionedToRecord(Source.FindById(OrgRec.ID));
  CompareRecs(OrgRec, RestoredRec);
  RestoredRec.Free;

  // find by period
  OrgRec := BuildNoteRecord();
  RestoredRecs := Source.FindPeriod(OrgRec.NativeTime - 5 / SecPerDay, OrgRec.NativeTime + 5 / SecPerDay);
  CheckEquals(1, Length(RestoredRecs));
  CompareRecs(OrgRec, RestoredRecs[0]);
  RestoredRecs[0].Free;

  // -------------------------

  sleep(1200);

  TNoteRecord(OrgRec).Text := 'modified';
  OrgRec.Modified;
  SetLength(Recs, 1);
  Recs[0] := OrgRec;
  Source.Save(Recs);

  // -------------------------
  Source.Free;
  SetupSource;

  RestoredRecs := VersionedToRecord(Source.FindChanged(OrgRec.TimeStamp - 1 / SecPerDay));
  CheckEquals(1, Length(RestoredRecs), 'Count of records differs');
  CompareRecs(OrgRec, RestoredRecs[0]);
  RestoredRecs[0].Free;

  // -------------------------

  RestoredRecs := Source.FindPeriod(OrgRec.NativeTime - 5 / SecPerDay, OrgRec.NativeTime + 5 / SecPerDay);
  CheckEquals(1, Length(RestoredRecs));
  Source.Delete(OrgRec.ID);

  // -------------------------
  Source.Free;
  SetupSource;
  
  RestoredRecs := Source.FindPeriod(OrgRec.NativeTime - 5 / SecPerDay, OrgRec.NativeTime + 5 / SecPerDay);
  CheckEquals(0, Length(RestoredRecs));
end;

end.

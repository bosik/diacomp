unit DiarySourceTest;

interface

uses
  Windows,
  SysUtils,
  TestFrameWork,
  DiarySources,
  DiaryPage,
  DiaryRecords,
  DiaryRoutines,
  BusinessObjects;

type
  TDiarySourceTest = class(TTestCase)
  protected
    Source: IDiarySource;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ComparePages(ExpPage, ActPage: TDiaryPage); overload;
    procedure ComparePages(const ExpPages, ActPages: TDiaryPageList); overload;

    procedure SetupSource; virtual; abstract;
    procedure TeardownSource; virtual; abstract;
  published
    procedure TestGetModified;
    procedure TestGetVersions;
    procedure TestGetPages;
    procedure TestModifying;
  end;

implementation

uses Math;

var
  DemoPages: TDiaryPageList;
  Dates: TDateList;

{==============================================================================}
procedure CreateDemoPages;
{==============================================================================}
var
  Meal: TMealRecord;
  i: integer;
begin
  SetLength(DemoPages, 2);
  SetLength(Dates, 2);

  DemoPages[0] := TDiaryPage.Create;
  DemoPages[0].Date := Trunc(EncodeDate(2002, 03, 15));
  DemoPages[0].Add(TBloodRecord.Create(127, 4.9, 3));
  DemoPages[0].Add(TInsRecord.Create(135, 12));
    Meal := TMealRecord.Create(201, True);
    Meal.Add(TFoodMassed.Create('Карбонат "Восточный" (Черн)', 9.9, 26.3, 0, 276, 90));
    Meal.Add(TFoodMassed.Create('Хлеб', 5.5, 0.9, 44.1, 206.3, 42));
  DemoPages[0].Add(Meal);
  DemoPages[0].Add(TNoteRecord.Create(230, 'Demo'));

  DemoPages[1] := TDiaryPage.Create;
  DemoPages[1].Date := Trunc(EncodeDate(2002, 03, 16));
  DemoPages[1].Add(TBloodRecord.Create(820, 6.8, 7));
  DemoPages[1].Add(TInsRecord.Create(829, 16));
  DemoPages[1].Add(TMealRecord.Create(850, False));
  DemoPages[1].Add(TNoteRecord.Create(1439, 'DTest'));

  // to remove accuracy errors
  for i := 0 to High(DemoPages) do
  begin
    DemoPages[i].TimeStamp := StrToDateTime(DateTimeToStr(DemoPages[i].TimeStamp));
    Dates[i] := DemoPages[i].Date;
  end;
end;

{ TDiarySourceTest }

{==============================================================================}
procedure TDiarySourceTest.SetUp;
{==============================================================================}
begin
  inherited;
  CreateDemoPages;

  if (Source <> nil) then
    FreeAndNil(Source);
  SetupSource;

  Check(Source.PostPages(DemoPages), 'PostPages() failed');

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
procedure TDiarySourceTest.ComparePages(ExpPage, ActPage: TDiaryPage);
{==============================================================================}
var
  ExpMeal, ActMeal: TMealRecord;
  j, k: integer;
begin
  CheckEquals(Trunc(ExpPage.Date), Trunc(ActPage.Date));
  //CheckEquals(DateTimeToStr(ExpPage.TimeStamp), DateTimeToStr(ActPage.TimeStamp));

  Check(
    SameValue(
      ExpPage.TimeStamp,
      ActPage.TimeStamp,
      2/SecPerDay),
    //'Exp: ' + DateTimeToStr(ExpPage.TimeStamp) + ', Act: ' + DateTimeToStr(ActPage.TimeStamp)
    'Timestamp error, sec: ' + FloatToStr(abs(ExpPage.TimeStamp - ActPage.TimeStamp) * SecPerDay)
  );                                                                                         

  CheckEquals(ExpPage.Version, ActPage.Version);
  CheckEquals(ExpPage.Count, ActPage.Count);

  for j := 0 to ExpPage.Count - 1 do
  begin
    CheckEquals(ExpPage[j].Time, ActPage[j].Time);
    if (ExpPage[j].RecType = TBloodRecord) then
    begin
      CheckEquals(TBloodRecord(ExpPage[j]).Value, TBloodRecord(ActPage[j]).Value);
      CheckEquals(TBloodRecord(ExpPage[j]).Finger, TBloodRecord(ActPage[j]).Finger);
    end else
    if (ExpPage[j].RecType = TInsRecord) then
    begin
      CheckEquals(TInsRecord(ExpPage[j]).Value, TInsRecord(ActPage[j]).Value);
    end else
    if (ExpPage[j].RecType = TMealRecord) then
    begin
      ExpMeal := TMealRecord(ExpPage[j]);
      ActMeal := TMealRecord(ExpPage[j]);

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
    if (ExpPage[j].RecType = TNoteRecord) then
    begin
      CheckEquals(TNoteRecord(ExpPage[j]).Text, TNoteRecord(ActPage[j]).Text);
    end else;
  end;
end;

{==============================================================================}
procedure TDiarySourceTest.ComparePages(const ExpPages, ActPages: TDiaryPageList);
{==============================================================================}
var
  i: integer;
begin
  CheckEquals(Length(ExpPages), Length(ActPages));
  for i := 0 to High(ExpPages) do
  begin
    ComparePages(ExpPages[i], ActPages[i]);
  end;
end;

{==============================================================================}
procedure TDiarySourceTest.TestGetModified;
{==============================================================================}
var
  ModList: TModList;
  i: integer;
begin
  Source.GetModified(Now() - 5/SecPerDay, ModList); // what changed in last 5 secs?
  CheckEquals(Length(DemoPages), Length(ModList));

  for i := 0 to High(DemoPages) do
  begin
    CheckEquals(DemoPages[i].Date, ModList[i].Date);
    CheckEquals(DemoPages[i].Version, ModList[i].Version);
  end;
end;

{==============================================================================}
procedure TDiarySourceTest.TestGetPages;
{==============================================================================}
var
  Pages: TDiaryPageList;
begin
  Check(Source.GetPages(Dates, Pages), 'GetPages() failed');
  ComparePages(DemoPages, Pages);
end;

{==============================================================================}
procedure TDiarySourceTest.TestGetVersions;
{==============================================================================}
var
  ModList: TModList;
  i: integer;
begin
  Source.GetVersions(Dates, ModList);
  CheckEquals(Length(DemoPages), Length(ModList));

  for i := 0 to High(DemoPages) do
  begin
    CheckEquals(DemoPages[i].Date, ModList[i].Date);
    CheckEquals(DemoPages[i].Version, ModList[i].Version);
  end;
end;

{==============================================================================}
procedure TDiarySourceTest.TestModifying;
{==============================================================================}
var
  Page: TDiaryPage;
  OldRecCount: integer;
  OldVersion: integer;
begin
  Page := Source.GetPage(Trunc(EncodeDate(2013, 10, 23)));
  Check(Page <> nil, 'Failed to get page (1)');
  OldRecCount := Page.Count;
  OldVersion := Page.Version;

  Page.Add(TNoteRecord.Create(1439, 'Temp note'));
  Check(Source.PostPage(Page), 'Failed to post page (1)');
  Page.Free;

  // -------------------------

  Page := Source.GetPage(Trunc(EncodeDate(2013, 10, 23)));
  Check(Page <> nil, 'Failed to get page (2)');
  CheckEquals(OldRecCount + 1, Page.Count, 'Count check failed (1)');
  CheckEquals(OldVersion + 1, Page.Version, 'Version check failed (1)');
  CheckEquals(TNoteRecord, Page[Page.Count - 1].RecType, 'RecType check failed');
  CheckEquals(1439, TNoteRecord(Page[Page.Count - 1]).Time, 'Time check failed');
  CheckEquals('Temp note', TNoteRecord(Page[Page.Count - 1]).Text, 'Text check failed');

  Page.Remove(Page.Count - 1);
  Check(Source.PostPage(Page), 'Failed to post page (2)');
  Page.Free;

  // -------------------------

  Page := Source.GetPage(Trunc(EncodeDate(2013, 10, 23)));
  Check(Page <> nil, 'Failed to get page (3)');
  CheckEquals(OldRecCount, Page.Count, 'Count check failed (2)');
  CheckEquals(OldVersion + 2, Page.Version, 'Version check failed (2)');
  Page.Free;
end;

end.

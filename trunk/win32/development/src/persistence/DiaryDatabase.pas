unit DiaryDatabase;

{$R+ O+}

interface

uses
  SysUtils, // Exception
  Classes, // TStrings
  Math, // Max
  DiaryRoutines, // TDate
  DiaryDAO,
  DiaryRecords,
  ActiveX,
  XMLDoc,
  XMLIntf,
  BusinessObjects,
  DiaryPage,
  DiaryPageSerializer;

type
  TSearchDirection = (sdBack, sdForward, sdAround);

  TDiary = class
  private
    FSource: TDiaryDAO;

    FPostPrandIns: integer;
    FPostPrandStd: integer;
    FPostPrandShort: integer;

    function GetPage(Date: TDate): TRecordList;
    procedure SetPostPrand(Index, Value: integer);
  public
    constructor Create(Source: TDiaryDAO);
    function FindRecord(RecType: TClassCustomRecord; TimeUTC: TDateTime;
      DeltaTime: integer; Direction: TSearchDirection): TCustomRecord;
    function GetLastBloodRecord: TBloodRecord;
    function GetNextFinger: integer;
    procedure SaveToXML(const FileName: string);
    procedure SaveToJSON(const FileName: string);

    // свойства
    property Pages[Index: TDate]: TRecordList read GetPage; default;
    property PostPrandStd: integer    index 1 read FPostPrandStd   write SetPostPrand default 210;
    property PostPrandShort: integer  index 2 read FPostPrandShort write SetPostPrand default 30;
    property PostPrandIns: integer    index 3 read FPostPrandStd   write SetPostPrand default 210;
 end;

implementation

(*
{==============================================================================}
function GetNextDayFreeTime(Page: TDiaryPage; StdMealPeriod, ShortMealPeriod, InsPeriod: integer): integer;
{==============================================================================}
var
  CurTime, i: integer;
begin
  //Log('TPageCache.GetNextDayFreeTime()');

  Result := 0;

  // TODO: дублирующийся код (UpdatePostprand)
  for i := 0 to Page.Count - 1 do
  begin
    CurTime := Page.Recs[i].Time_;

    if (Page.Recs[i].RecType = TInsRecord) then
      Result := Max(Result, CurTime + InsPeriod) else
    if (Page.Recs[i].RecType = TMealRecord) then
      if (TMealRecord(Page.Recs[i]).Carbs > 0) then
         if TMealRecord(Page.Recs[i]).ShortMeal then
           Result := Max(Result, CurTime + ShortMealPeriod)
         else
           Result := Max(Result, CurTime + StdMealPeriod);
  end;

  Result := Result - MinPerDay;
end;    *)

{ TDiary }

{==============================================================================}
constructor TDiary.Create(Source: TDiaryDAO);
{==============================================================================}
begin
  if (Source = nil) then
    raise Exception.Create('Source can''t be nil')
  else
    FSource := Source;

  FPostPrandStd := 210;    // TODO: fix hardcode
  FPostPrandShort := 30;
  FPostPrandIns := 210;
end;

{==============================================================================}
function TDiary.GetPage(Date: TDate): TRecordList;
{==============================================================================}
var
  //PrevPage: TDiaryPage;
  TimeFrom, TimeTo: TDateTime;
begin
  TimeFrom := LocalToUTC(Date);
  TimeTo := LocalToUTC(Date + 1);
  Result := FSource.FindPeriod(TimeFrom, TimeTo);

  {Result := FSource.GetPage(Date);
  PrevPage := FSource.GetPage(Date - 1);
  Result.FreeTime := GetNextDayFreeTime(PrevPage, PostPrandStd, PostPrandShort, PostPrandIns); // TODO: INS time, not meal
  Result.PostprandMealStd := PostPrandStd;
  Result.PostprandMealShort := PostPrandShort;
  Result.PostprandIns := PostPrandIns;
  Result.UpdatePostprand();
  Result.AddChangeListener(PageChangeListener);  }

  // TODO: выделить эти параметры в рекорд/класс и сделать его отдельным полем
end;

{==============================================================================}
function TDiary.FindRecord(RecType: TClassCustomRecord;
  TimeUTC: TDateTime; DeltaTime: integer;
  Direction: TSearchDirection): TCustomRecord;
{==============================================================================}

  {

  08.30                           ||
  08.40                           ||
    09.00   <--  FirstMoreEq  --- \/ ---
    09.00
    09.00
    09.00   <--  LastLessEq   --- /\ ---
  09.20                           ||
  09.30                           ||
  }

var
  //StartDate: TDate;
  //StartTime: integer;
  //FinishDate: TDate;
  //FinishTime: integer;
  //Date: TDate;

  TimeFrom, TimeTo: TDateTime;

  Recs: TRecordList;
  CurDist, MinDist: real;
  i: integer;
begin
  { проверяем аргументы }
  if (DeltaTime <= 0) then
    raise Exception.Create('FindRecord: некорректное значение DeltaTime (' + IntToStr(DeltaTime) + ')');


  if (Direction = sdForward) then
  begin
    TimeFrom := TimeUTC;
    TimeTo := TimeUTC + DeltaTime / MinPerDay;
    Recs := FSource.FindPeriod(TimeFrom, TimeTo);
    for i := 0 to High(Recs) do
    if (RecType = nil) or (RecType = Recs[i].RecType) then
    begin
      Result := Recs[i];
      Exit;
    end;

    Result := nil;
  end else

  if (Direction = sdBack) then
  begin
    TimeFrom := TimeUTC - DeltaTime / MinPerDay;
    TimeTo := TimeUTC;
    Recs := FSource.FindPeriod(TimeFrom, TimeTo);
    for i := High(Recs) downto 0 do
    if (RecType = nil) or (RecType = Recs[i].RecType) then
    begin
      Result := Recs[i];
      Exit;
    end;

    Result := nil;
  end else

  begin
    TimeFrom := TimeUTC - DeltaTime / MinPerDay;
    TimeTo := TimeUTC + DeltaTime / MinPerDay;
    Recs := FSource.FindPeriod(TimeFrom, TimeTo);
    MinDist := 3 * DeltaTime / MinPerDay;
    Result := nil;

    for i := 0 to High(Recs) do
    if (RecType = nil) or (RecType = Recs[i].RecType) then
    begin
      CurDist := abs(Recs[i].Time - TimeUTC);
      if (CurDist < MinDist) then
      begin
        MinDist := CurDist;
        Result := Recs[i];
      end;
    end;
  end;

  { определяем границы }
 (* if (Direction = sdForward) then
  begin
    StartDate := ADate;
    StartTime := ATime;
  end else
  begin
    StartDate := ((ADate * MinPerDay + ATime) - DeltaTime) div MinPerDay;
    StartTime := ((ADate * MinPerDay + ATime) - DeltaTime) mod MinPerDay;
  end;

  if (Direction = sdBack) then
  begin
    FinishDate := ADate;
    FinishTime := ATime;
  end else
  begin
    FinishDate := ((ADate * MinPerDay + ATime) + DeltaTime) div MinPerDay;
    FinishTime := ((ADate * MinPerDay + ATime) + DeltaTime) mod MinPerDay;
  end;

  { ищем }
  MinDist := (FinishDate - StartDate + 1) * 2 * MinPerDay;
  Result := nil;

  for Date := StartDate to FinishDate do
  begin
    Page := GetPage(Date);
    for i := 0 to Page.Count - 1 do
    if ((Date > StartDate) or (Page[i].Time >= StartTime)) and
       ((Date < FinishDate) or (Page[i].Time <= FinishTime)) and
       ((RecType = nil) or (Page[i].RecType = RecType))
    then
    begin
      CurDist := abs((Date * MinPerDay + Page[i].Time) - (ADate * MinPerDay + ATime));
      if (CurDist < MinDist) then
      begin
        MinDist := CurDist;
        Result := Page[i];
      end;
    end;
  end;    *)
end;

{==============================================================================}
procedure TDiary.SetPostPrand(Index, Value: integer);
{==============================================================================}
begin
  case Index of
    1: // std
    if (Value <> FPostPrandStd) and
       (Value >= 0) and
       (Value <= 5*60) then
    begin
      FPostPrandStd := Value;
    end;

    2: // short
    if (Value <> FPostPrandShort) and
       (Value >= 0) and
       (Value <= 60) then
    begin
      FPostPrandShort := Value;
    end;

    3: // ins
    if (Value <> FPostPrandIns) and
       (Value >= 0) and
       (Value <= 5*60) then
    begin
      FPostPrandIns := Value;
    end;
  end;
end;

{==============================================================================}
function TDiary.GetLastBloodRecord: TBloodRecord;
{==============================================================================}
const
  INTERVAL = 7; // days
var
  TimeFrom, TimeTo: TDateTime;
  Recs: TRecordList;
  i: integer;      
begin
  TimeTo := GetTimeUTC;
  TimeFrom := TimeTo - INTERVAL;
  Recs := FSource.FindPeriod(TimeFrom, TimeTo);

  for i := High(Recs) downto 0 do
  begin
    if (Recs[i].RecType = TBloodRecord) then
    begin
      Result := TBloodRecord(Recs[i]);
      Exit;
    end;
  end;

  Result := nil;
end;

{==============================================================================}
procedure TDiary.SaveToXML(const FileName: string);
{==============================================================================}
{var
  XML: IXMLDocument;
  Page: TDiaryPage;
  Meal: TMealRecord;
  Root, PageNode, RecNode, ItemNode: IXMLNODE;
  i, j, k: integer;
  DS: char;  }
begin
 (* DS := SysUtils.DecimalSeparator;
  //SysUtils.DecimalSeparator := '.';

  try
    XML := NewXMLDocument();
    XML.Encoding := 'utf-8';
    XML.Version := '1.0';
    XML.NodeIndentStr := #9;
    XML.Options := [doNodeAutoIndent];

    Root := XML.AddChild('diary');

    //DiaryLocalSource.TDiaryLocalSource(FSource).

    // TODO: hardcoded date
    for i := Trunc(EncodeDate(2009, 12, 01)) to Trunc(GetTimeUTC()) do
    begin
      Page := GetPage(i);

      PageNode := Root.AddChild('page');
      PageNode.Attributes['date'] := DateToStr(Page.Date);
      PageNode.Attributes['mod'] := Page.TimeStamp;
      PageNode.Attributes['ver'] := Page.Version;

      for j := 0 to Page.Count - 1 do
      begin
        RecNode := PageNode.AddChild('rec');
        RecNode.Attributes['time']  := DateTimeToStr(i + Page[j].Time/MinPerDay);

        if (Page[j].RecType = TBloodRecord) then
        begin
          RecNode.Attributes['type']  := 'blood';
          RecNode.Attributes['value']  := TBloodRecord(Page[j]).Value;
          if (TBloodRecord(Page[j]).Finger > -1) then
            RecNode.Attributes['finger']  := TBloodRecord(Page[j]).Finger;
        end else

        if (Page[j].RecType = TInsRecord) then
        begin
          RecNode.Attributes['type']  := 'insulin';
          RecNode.Attributes['value']  := TInsRecord(Page[j]).Value;
        end else

        if (Page[j].RecType = TMealRecord) then
        begin
          Meal := TMealRecord(Page[j]);     

          RecNode.Attributes['type']  := 'meal';
          if (Meal.ShortMeal) then
            RecNode.Attributes['short']  := Meal.ShortMeal;

          for k := 0 to Meal.Count - 1 do
          begin
            ItemNode := RecNode.AddChild('item');
            ItemNode.Attributes['name']  := Meal[k].Name;
            ItemNode.Attributes['prots'] := Meal[k].RelProts;
            ItemNode.Attributes['fats']  := Meal[k].RelFats;
            ItemNode.Attributes['carbs'] := Meal[k].RelCarbs;
            ItemNode.Attributes['val']   := Meal[k].RelValue;
            ItemNode.Attributes['mass']  := Meal[k].Mass;
          end;
        end else

        if (Page[j].RecType = TMealRecord) then
        begin
          RecNode.Attributes['type']  := 'note';
          RecNode.Attributes['text']  := TNoteRecord(Page[j]).Text;
        end;
      end;
    end;

    XML.SaveToFile(FileName);
  finally
    SysUtils.DecimalSeparator := DS;
  end;   *)
end;

{==============================================================================}
procedure TDiary.SaveToJSON(const FileName: string);
{==============================================================================}
begin
  // see DiaryLocalSource.Save()
end;

{==============================================================================}
function TDiary.GetNextFinger: integer;
{==============================================================================}
var
  Blood: TBloodRecord;
begin
  Blood := GetLastBloodRecord();
  if (Blood <> nil) then
    Result := (Blood.Finger + 1) mod 10
  else
    Result := -1;
end;

initialization
  ActiveX.CoInitialize(nil)
finalization
  ActiveX.CoUninitialize;
end.

unit DiaryDatabase;

{$R+ O+}

interface

uses
  SysUtils, // Exception
  Classes, // TStrings
  Math, // Max
  DiaryRoutines, // TDate
  DiarySources,
  DiaryRecords,
  ActiveX,
  XMLDoc,
  XMLIntf,
  BusinessObjects,
  DiaryPage,
  DiaryPageSerializer;

type
  TSearchDirection = (sdBack, sdForward, sdAround);

  // {*} - устаревшее
  // {!} - трудности

  TPageCache = class (TDiaryPage)
  private
    FCalculatedPostprand: boolean;
  public
    constructor Create(PageData: TPageData);
    function GetNextDayFreeTime(StdMealPeriod, ShortMealPeriod, InsPeriod: integer): integer;
    procedure UpdatePostprand(FreeTime, StdMealPeriod, ShortMealPeriod, InsPeriod: integer);

    property CalculatedPostprand: boolean read FCalculatedPostprand;
  end;

  TDiary = class
  private
    FSource: IDiarySource;
    FCache: array of TPageCache;
    FModified: boolean;

    FPostPrandStd: integer;
    FPostPrandShort: integer;

    { событи€ }
    FOnChange: TEventPageChanged;

    { обработчики событий }
    procedure ProcessPageChanged(EventType: TPageEventType; Page: TDiaryPage; RecClass: TClassCustomRecord;
      RecInstance: TCustomRecord);

    function GetPage(Date: TDate): TDiaryPage;
    function GetPageIndex(Date: TDate; CalculatePostprand: boolean): integer;
    function TraceLastPage: integer;

    procedure UpdateCached_Postprand;
    procedure SetPostPrand(Index, Value: integer);
  public
    constructor Create(Source: IDiarySource);
    destructor Destroy; override;
    function FindRecord(RecType: TClassCustomRecord; ADate: TDate; ATime: integer;
      DeltaTime: integer; Direction: TSearchDirection): TCustomRecord;
    function GetLastBloodRecord: TBloodRecord;
    function GetNextFinger: integer;
    procedure Post;
    procedure PrepareCache(FromDate, ToDate: TDate);
    procedure ReloadCache;
    procedure ResetCache;
    procedure SaveToXML(const FileName: string);
    procedure SaveToJSON(const FileName: string);

    // свойства
    property Pages[Index: TDate]: TDiaryPage read GetPage; default;
    property Modified: boolean read FModified write FModified;
    property PostPrandStd: integer index 1 read FPostPrandStd write SetPostPrand default 210;
    property PostPrandShort: integer index 2 read FPostPrandShort write SetPostPrand default 30;
    property OnChange: TEventPageChanged read FOnChange write FOnChange;
 end;

implementation

{ TDiary }

{==============================================================================}
constructor TDiary.Create(Source: IDiarySource);
{==============================================================================}
begin
  if (Source = nil) then
    raise Exception.Create('Source can''t be nil')
  else
    FSource := Source;

  FModified := False;
  FPostPrandStd := 210;    // TODO: fix hardcode
  FPostPrandShort := 30;
end;

{==============================================================================}
function TDiary.GetPage(Date: TDate): TDiaryPage;
{==============================================================================}
begin
  Result := FCache[GetPageIndex(Date, True)];
end;

{==============================================================================}
function TDiary.GetPageIndex(Date: TDate; CalculatePostprand: boolean): integer;
{==============================================================================}

  function FindInCache(Date: TDate): integer;
  var
    L, R: integer;
  begin
    L := 0;
    R := High(FCache);
    while (L <= R) do
    begin
      Result := (L + R) div 2;
      if (FCache[Result].Date < Date) then L := Result + 1 else
      if (FCache[Result].Date > Date) then R := Result - 1 else
        Exit;
    end;
    Result := -1;
  end;

  function LoadFromSource(Date: TDate): integer;
  var
    PageData: TPageData;
  begin
    PageData := FSource.GetPage(Date);

    Result := Length(FCache);
    SetLength(FCache, Result + 1);
    FCache[Result] := TPageCache.Create(PageData);
    FCache[Result].AddChangeListener(ProcessPageChanged);
    Result := TraceLastPage();

    PageData.Free;
  end;

  function MakeSureExists(Date: TDate): integer;
  begin
    Result := FindInCache(Date);
    if (Result = -1) then
      Result := LoadFromSource(Date);
  end;

var
  Index: integer;
  FreeTime: integer;
begin
  Result := MakeSureExists(Date);

  if CalculatePostprand and (not FCache[Result].CalculatedPostprand) then
  begin
    Index := MakeSureExists(Date - 1);
    FreeTime := FCache[Index].GetNextDayFreeTime(PostPrandStd, PostPrandShort, PostPrandStd);
    Result := MakeSureExists(Date); // !!!
    FCache[Result].UpdatePostprand(FreeTime, PostPrandStd, PostPrandShort, PostPrandStd)
    // TODO: INS time, not meal
    // TODO: выделить эти параметры в рекорд/класс и сделать его отдельным полем
  end;
end;

{==============================================================================}
destructor TDiary.Destroy;
{==============================================================================}
begin
  ResetCache;
end;

function TDiary.FindRecord(RecType: TClassCustomRecord;
  ADate: TDate; ATime: integer; DeltaTime: integer;
  Direction: TSearchDirection): TCustomRecord;

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
  StartDate: TDate;
  StartTime: integer;
  FinishDate: TDate;
  FinishTime: integer;
  Date: TDate;

  Page: TDiaryPage;
  CurDist, MinDist: integer;
  i: integer;
begin
  { провер€ем аргументы }
  if not CorrectTime(ATime) then
    raise Exception.Create('FindRecord: некорректное значение Time (' + IntToStr(ATime) + ')');
  if (DeltaTime <= 0) then
    raise Exception.Create('FindRecord: некорректное значение DeltaTime (' + IntToStr(DeltaTime) + ')');

  { определ€ем границы }
  if (Direction = sdForward) then
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
  end;
end;

{==============================================================================}
procedure TDiary.ProcessPageChanged(EventType: TPageEventType; Page: TDiaryPage;
  RecClass: TClassCustomRecord; RecInstance: TCustomRecord);
{==============================================================================}
var
  PageData: TPageData;
begin
  //Log('TDiary.Changed()');

  FModified := True;

  // TODO: проверить, что не вызываетс€ слишком часто (например, при загрузке)
  // TODO: [Trunc(Now) + 1] Ч не лучшее решение
  if (Page <> nil) then
    UpdateCached_Postprand; // TODO: optimize
    //UpdatePostprand(Page.Date - 1, Trunc(Now) + 1);

  PageData := TPageData.Create;
  TDiaryPage.WriteTo(PageData, Page);

  //FSource.PostPage(PageData); // to save after every change

  if Assigned(FOnChange) then FOnChange(EventType, Page, RecClass, RecInstance);
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
  end;

  UpdateCached_Postprand;
end;

{==============================================================================}
function TDiary.TraceLastPage: integer;
{==============================================================================}
var
  temp: TPageCache;
begin
  Result := High(FCache);
  if (Result > -1) then
  begin
    Temp := FCache[Result];
    while (Result > 0) and (FCache[Result - 1].Date > Temp.Date) do
    begin
      FCache[Result] := FCache[Result-1];
      dec(Result);
    end;
    FCache[Result] := temp;
  end;
end;

{==============================================================================}
function TDiary.GetLastBloodRecord: TBloodRecord;
{==============================================================================}
const
  INTERVAL = 7;
var
  Date, Today: integer;
  Index, i: integer;
begin
  Today := Trunc(Now);
  for Date := Today downto (Today - INTERVAL + 1) do
  begin
    Index := GetPageIndex(Date, False);

    if (Index > -1) then
    for i := FCache[Index].Count - 1 downto 0 do
    if (FCache[Index][i] is TBloodRecord) then
    begin
      Result := TBloodRecord(FCache[Index][i]);
      Exit;
    end;
  end;

  Result := nil;
end;

procedure TDiary.PrepareCache(FromDate, ToDate: TDate);
var
  Date: TDate;
begin
  //Log('TDiary.PrepareCache()');

  // TODO: оптимизировать
  for Date := FromDate to ToDate do
    GetPageIndex(Date, True);
end;

procedure TDiary.ResetCache;
var
  i: integer;
begin
  for i := 0 to High(FCache) do
    FCache[i].Free;
  SetLength(FCache, 0);
end;

procedure TDiary.UpdateCached_Postprand;
var
  i: integer;
  FreeTime: integer;
begin
  //Log('TDiary.UpdateCachedPostprand()');

  FreeTime := 0; // just to avoid compiler's warning

  for i := 0 to High(FCache) do
  begin
    // если убрать проверку, то кэш будет разрастатьс€ при повторном вызове этой процедуры
    if FCache[i].CalculatedPostprand then
      FCache[i].UpdatePostprand(FreeTime - FCache[i].Date * MinPerDay, PostPrandStd, PostPrandShort, PostPrandStd);

    FreeTime := FCache[i].GetNextDayFreeTime(PostPrandStd, PostPrandShort, PostPrandStd);
    FreeTime := FreeTime + (FCache[i].Date + 1) * MinPerDay
  end;
end;

{==============================================================================}
procedure TDiary.Post;
{==============================================================================}
var
  Pages: TPageDataList;
  i: integer;
begin
  // выгружаем в список
  SetLength(Pages, Length(FCache));
  for i := 0 to High(Pages) do
  begin
    Pages[i] := TPageData.Create();
    TDiaryPage.WriteTo(Pages[i], FCache[i]);
  end;

  // отправл€ем источнику
  FSource.PostPages(Pages);

  // освобождаем пам€ть
  for i := 0 to High(Pages) do
    Pages[i].Free;

  FModified := False;
end;

{==============================================================================}
procedure TDiary.ReloadCache;
{==============================================================================}
var
  i: integer;
  PageData: TPageData;
begin
  for i := 0 to High(FCache) do
  begin
    PageData := FSource.GetPage(FCache[i].Date);
    TDiaryPage.ReadFrom(PageData, FCache[i]);
    PageData.Free;

    //FCache[i].FCalculatedPostprand := False; - нужно сохранить состо€ни€ дл€ UpdateCached_Postprand
  end;

  UpdateCached_Postprand;
end;

{==============================================================================}
procedure TDiary.SaveToXML(const FileName: string);
{==============================================================================}
var
  XML: IXMLDocument;
  Page: TDiaryPage;
  Meal: TMealRecord;
  Root, PageNode, RecNode, ItemNode: IXMLNODE;
  i, j, k: integer;
  DS: char;
begin
  DS := SysUtils.DecimalSeparator;
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
    for i := Trunc(EncodeDate(2009, 12, 01)) to Trunc(Now) do
    begin
      Page := GetPage(i);

      PageNode := Root.AddChild('page');
      PageNode.Attributes['date'] := DateToStr(Page.Date);
      PageNode.Attributes['mod'] := Page.TimeStamp;
      PageNode.Attributes['ver'] := Page.Version;

      for j := 0 to Page.Count - 1 do
      begin
        RecNode := PageNode.AddChild('rec');
        RecNode.Attributes['time']  := TimeToStr(Page[j].Time/MinPerDay);

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
  end;
end;

{==============================================================================}
procedure TDiary.SaveToJSON(const FileName: string);
{==============================================================================}

  function BlockBlood(R: TBloodRecord): string;
  begin
    Result := Format('{type: "blood", time: %d, value: %.1f, finger: %d}', [R.Time, R.Value, R.Finger]);
  end;

  function BlockIns(R: TInsRecord): string;
  begin
    Result := Format('{type: "insulin", time: %d, value: %.1f}', [R.Time, R.Value]);
  end;

  function BlockFood(R: TFoodMassed): string;
  begin
    Result := Format('{name: "%s", prots: %.1f, fats: %.1f, carbs: %.1f, value: %.1f, mass: %.1f}', [R.Name, R.RelProts, R.RelFats, R.RelCarbs, R.RelValue, R.Mass]);
  end;

  function BlockMeal(R: TMealRecord): string;
  var
    i: integer;
  begin
    Result := Format('{type: "meal", time: %d, content: [',
      [R.Time]);
    for i := 0 to R.Count - 1 do
    begin
      Result := Result + BlockFood(R[i]);
      if (i < R.Count - 1) then
        Result := Result + ', ';
    end;
    Result := Result + ']}';
  end;

  function BlockNote(R: TNoteRecord): string;
  begin
    Result := Format('{type: "note", time: %d, text: "%s"}', [R.Time, R.Text]);
  end;

  function BlockPage(P: TDiaryPage): string;
  var
    i: integer;
  begin
    Result := Format('{date: "%s", stamp: "%s", version: %d, content: [',
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
    Result := Result + ']}';
  end;

var
  JSON: string;
  Page: TDiaryPage;
  i: integer;
  //DS: char;
  S: TStrings;
begin
  //DS := SysUtils.DecimalSeparator;
  //SysUtils.DecimalSeparator := '.';
  JSON := '[';

  S := TStringList.Create;
  try
    // TODO: hardcoded date
    for i := Trunc(EncodeDate(2009, 12, 04)) to Trunc(Now) do
    begin
      Page := GetPage(i);

      JSON := JSON + BlockPage(Page);
      if (i < Page.Count - 1) then
        JSON := JSON + ', ';

      S.Add(json);
      json := '';
    end;

    JSON := JSON + ']';
    S.Add(json);
    json := '';

    S.SaveToFile(FileName);
  finally
    S.Free;
    //SysUtils.DecimalSeparator := DS;
  end;
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

{ TPageCache }

{==============================================================================}
constructor TPageCache.Create(PageData: TPageData);
{==============================================================================}
begin
  inherited Create();
  TDiaryPage.ReadFrom(PageData, Self);
  FCalculatedPostprand := False;
end;

{==============================================================================}
function TPageCache.GetNextDayFreeTime(StdMealPeriod, ShortMealPeriod,
  InsPeriod: integer): integer;
{==============================================================================}
var
  CurTime, i: integer;
begin
  //Log('TPageCache.GetNextDayFreeTime()');

  Result := 0;

  // TODO: дублирующийс€ код (UpdatePostprand)
  for i := 0 to Count - 1 do
  begin
    CurTime := Recs[i].Time;

    if (Recs[i].RecType = TInsRecord) then
      Result := Max(Result, CurTime + InsPeriod) else
    if (Recs[i].RecType = TMealRecord) then
      if TMealRecord(Recs[i]).Carbs > 0 then
         if TMealRecord(Recs[i]).ShortMeal then
           Result := Max(Result, CurTime + ShortMealPeriod)
         else
           Result := Max(Result, CurTime + StdMealPeriod);
  end;

  Result := Result - MinPerDay;
end;

{==============================================================================}
procedure TPageCache.UpdatePostprand(FreeTime, StdMealPeriod,
  ShortMealPeriod, InsPeriod: integer);
{==============================================================================}
var
  CurTime, i: integer;
begin
  //Log('TPageCache.UpdatePostPrand()');

  // TODO: дублирующийс€ код (GetNextDayFreeTime)
  for i := 0 to Count-1 do
  begin
    CurTime := Recs[i].Time;

    if (Recs[i].RecType = TInsRecord) then
    begin
      FreeTime := Max(FreeTime, CurTime + InsPeriod);
    end else

    if (Recs[i].RecType = TMealRecord) then
    begin
      if TMealRecord(Recs[i]).Carbs > 0 then
         if TMealRecord(Recs[i]).ShortMeal then
           FreeTime := Max(FreeTime, CurTime + ShortMealPeriod)
         else
           FreeTime := Max(FreeTime, CurTime + StdMealPeriod);
    end else

    if (Recs[i].RecType = TBloodRecord) then
    begin
      TBloodRecord(Recs[i]).PostPrand := (CurTime < FreeTime);
    end;
  end;
  FCalculatedPostprand := True;
end;

initialization
  ActiveX.CoInitialize(nil)
finalization
  ActiveX.CoUninitialize;
end.
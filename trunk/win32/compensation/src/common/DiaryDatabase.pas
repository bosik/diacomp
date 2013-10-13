unit DiaryDatabase;

{$R+ O+}

interface

uses
  SysUtils,
  Classes,
  Math,
  DiaryRoutines,
  DiarySources,
  DiaryRecords,
  AutoLog, // debug only
  ActiveX,
  XMLDoc,
  XMLIntf,
  BusinessObjects,
  Variants;

type
  { 2. ДНЕВНИК }

  TRecordsList = array of TCustomRecord; // слава полиморфизму!

  TDiaryPage = class;

  TEventPageChanged = procedure(EventType: TPageEventType; Page: TDiaryPage; RecClass: TClassCustomRecord; RecInstance: TCustomRecord) of object;

  TDiaryPage = class
  private
    FDate: TDate;
    {+}FRecs: TRecordsList;
    {+}FSilentChange: boolean;
    FTimeStamp: TDateTime;
    FVersion: integer;

    { события }
    FOnChange: array of TEventPageChanged;

    {+}procedure CheckIndex(Index: integer);
    {+}function GetRecord(Index: integer): TCustomRecord;
    {+}function GetStat(Index: integer): real;
    {#}procedure ProcessRecordChanged(RecInstance: TCustomRecord); overload;
    {#}procedure ProcessRecordChanged(EventType: TPageEventType; RecType: TClassCustomRecord; RecInstance: TCustomRecord = nil); overload;
    {+}function Trace(Index: integer): integer;
    {+}function TraceLast: integer;
  public
    {+}function Add(Rec: TCustomRecord): integer;
    {+}procedure Clear;
    {+}function Count: integer;
    constructor Create(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer); overload;
    constructor Create(PageData: TPageData); overload;
    destructor Destroy; override;
    {+}procedure Remove(Index: integer; AutoFree: boolean = True);

    // сахар
    {+}function FirstRec(RecType: TClassCustomRecord): TCustomRecord;
    {+}function LastRec(RecType: TClassCustomRecord): TCustomRecord;
    {+}function FindRecord(Rec: TCustomRecord): integer; overload;
    {+}function FirstBloodRec: TBloodRecord; deprecated;
    {+}function FirstInsRec: TInsRecord; deprecated;
    {+}function FirstMealRec: TMealRecord; deprecated;
    {+}function LastBloodRec: TBloodRecord; deprecated;
    {+}function LastInsRec: TInsRecord; deprecated;
    {+}function LastMealRec: TMealRecord; deprecated;

    // I/O

    procedure ReadFrom(S: TStringList); overload;
    procedure WriteTo(S: TStringList); overload;

    procedure ReadFrom(const S: string); overload;
    procedure WriteTo(out S: string); overload;

    procedure ReadFrom(S: TPageData); overload;
    procedure WriteTo(S: TPageData); overload;

    // Listeners

    procedure AddChangeListener(Listener: TEventPageChanged);

    // свойства

    property Date: TDate read FDate;
    property TimeStamp: TDateTime read FTimeStamp;
    property Version: integer read FVersion;

    {+}property Recs[Index: integer]: TCustomRecord read GetRecord; default;
    {+}property DayProts: real index 1 read GetStat;
    {+}property DayFats:  real index 2 read GetStat;
    {+}property DayCarbs: real index 3 read GetStat;
    {+}property DayValue: real index 4 read GetStat;
    {+}property DayMass:  real index 5 read GetStat;
    {+}property DayIns:   real index 6 read GetStat;
  end;

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

    { события }
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

{ TDiaryPage }

{==============================================================================}
function TDiaryPage.Add(Rec: TCustomRecord): integer;
{==============================================================================}
begin
  if (Rec = nil) then
    raise Exception.Create('Record can''t be nil');

  Result := Length(FRecs);
  SetLength(FRecs, Result + 1);
  FRecs[Result] := Rec;
  Rec.OnChange := ProcessRecordChanged;
  Result := TraceLast;

  //{#}Changed({FRecs[Result]}Rec);
  ProcessRecordChanged(etAdd, Rec.RecType, Rec);
end;

// TODO: эта процедура должна вызываться только мелкими изменениями пользователя
// избегать вызова её при загрузке данных
{==============================================================================}
procedure TDiaryPage.ProcessRecordChanged(EventType: TPageEventType; RecType: TClassCustomRecord;
  RecInstance: TCustomRecord = nil);
{==============================================================================}
var
  Index: integer;
  i: integer;
begin
  if FSilentChange then Exit;

  // обновляем печатьку
  FTimeStamp := Now;
  inc(FVersion);

  // принимаем коррекционные меры, если нужно
  if (RecInstance <> nil) then
  begin
    Index := FindRecord(RecInstance);
    if (Index <> -1) then
    begin
      Trace(Index);
      // TODO: etc.
    end;
  end;

  // информируем слушателей (ПОСЛЕ коррекций)

  for i := 0 to High(FOnChange) do
   FOnChange[i](EventType, Self, RecType, RecInstance);
end;

{==============================================================================}
procedure TDiaryPage.ProcessRecordChanged(RecInstance: TCustomRecord);
{==============================================================================}
begin
  ProcessRecordChanged(etModify, RecInstance.RecType, RecInstance);
end;

{==============================================================================}
procedure TDiaryPage.CheckIndex(Index: integer);
{==============================================================================}
begin
  if (Index < Low(FRecs)) or (Index > High(FRecs)) then
    raise ERangeError.CreateFmt('TDiaryPage: недопустимый индекс (%d)', [Index]);
end;

// используется при загрузке и при уничтожении
{==============================================================================}
procedure TDiaryPage.Clear;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FRecs) do
    FRecs[i].Free;
  SetLength(FRecs, 0);

  //{#}Changed(nil);
  //Changed(etRemove, nil);
  // TODO: нужен ли Changed? Когда вызывается Clear?
end;

{==============================================================================}
function TDiaryPage.Count: integer;
{==============================================================================}
begin
  Result := Length(FRecs);
end;

{==============================================================================}
constructor TDiaryPage.Create(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer);
{==============================================================================}
begin
  FOnChange := nil;

  FDate := ADate;
  FTimeStamp := ATimeStamp;
  FVersion := AVersion;

  FSilentChange := False;
end;

{==============================================================================}
constructor TDiaryPage.Create(PageData: TPageData);
{==============================================================================}
begin
  FOnChange := nil;

  //FSilentChange := True;
  ReadFrom(PageData);

  FSilentChange := False;
end;

{==============================================================================}
destructor TDiaryPage.Destroy;
{==============================================================================}
begin
  FOnChange := nil;
  FSilentChange := True;
  Clear;
end;

{==============================================================================}
function TDiaryPage.FindRecord(Rec: TCustomRecord): integer;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FRecs) do
  if (FRecs[i] = Rec) then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

{==============================================================================}
function TDiaryPage.FirstRec(RecType: TClassCustomRecord): TCustomRecord;
{==============================================================================}
var
  i: integer;
begin
  Result := nil;
  for i := 0 to High(FRecs) do
  if (FRecs[i].RecType = RecType) then
  begin
    Result := FRecs[i];
    Exit;
  end;
end;

{==============================================================================}
function TDiaryPage.FirstBloodRec: TBloodRecord;
{==============================================================================}
begin
  Result := TBloodRecord(FirstRec(TBloodRecord));
end;

{==============================================================================}
function TDiaryPage.FirstInsRec: TInsRecord;
{==============================================================================}
begin
  Result := TInsRecord(FirstRec(TInsRecord));
end;

{==============================================================================}
function TDiaryPage.FirstMealRec: TMealRecord;
{==============================================================================}
begin
  Result := TMealRecord(FirstRec(TMealRecord));
end;

{==============================================================================}
function TDiaryPage.GetRecord(Index: integer): TCustomRecord;
{==============================================================================}
begin
  // TODO: just debug
  if (Index < Low(FRecs)) or (Index > High(FRecs)) then
    raise ERangeError.Create('GetRecord: index "' + IntToStr(index) + '" is out of bounds [0; ' + IntToStr(High(FRecs)) + ']')
  else
    Result := FRecs[Index];
end;

{==============================================================================}
function TDiaryPage.GetStat(Index: integer): real;
{==============================================================================}
var
  i: integer;
begin
  {
    1 FDayProts: real;
    2 FDayFats: real;
    3 FDayCarbs: real;
    4 FDayValue: real;
    5 FDayMass: real;
    6 FDayIns: real;
  }
  Result := 0;
  case Index of
    1: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Prots;
    2: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Fats;
    3: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Carbs;
    4: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Value;
    5: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TMealRecord) then Result := Result + TMealRecord(FRecs[i]).Mass;
    6: for i := 0 to High(FRecs) do if (FRecs[i].RecType = TInsRecord)  then Result := Result + TInsRecord(FRecs[i]).Value;
  end;
end;

{==============================================================================}
function TDiaryPage.LastRec(RecType: TClassCustomRecord): TCustomRecord;
{==============================================================================}
var
  i: integer;
begin
  Result := nil;
  for i := High(FRecs) downto 0 do
  if (FRecs[i].RecType = RecType) then
  begin
    Result := FRecs[i];
    Exit;
  end;
end;

{==============================================================================}
function TDiaryPage.LastBloodRec: TBloodRecord;
{==============================================================================}
begin
  Result := TBloodRecord(LastRec(TBloodRecord));
end;

{==============================================================================}
function TDiaryPage.LastInsRec: TInsRecord;
{==============================================================================}
begin
  Result := TInsRecord(LastRec(TInsRecord));
end;

{==============================================================================}
function TDiaryPage.LastMealRec: TMealRecord;
{==============================================================================}
begin
  Result := TMealRecord(LastRec(TMealRecord));
end;

{==============================================================================}
procedure TDiaryPage.Remove(Index: integer; AutoFree: boolean = True);
{==============================================================================}
var
  i: integer;
  T: TClassCustomRecord;
begin
  CheckIndex(Index);
  T := TClassCustomRecord(FRecs[Index].ClassType);

  if AutoFree then
    FRecs[Index].Free;

  for i := Index to High(FRecs) - 1 do
    FRecs[i] := FRecs[i + 1];
  SetLength(FRecs, Length(FRecs) - 1);

  {#}ProcessRecordChanged(etRemove, T);
end;

{==============================================================================}
function TDiaryPage.Trace(Index: integer): integer;
{==============================================================================}
var
  Temp: TCustomRecord;
  Changed: boolean;
begin
  Result := Index;
  if (Index >= 0)and(Index <= High(FRecs)) then
  begin
    Temp := Recs[Result];
    Changed := False;

    { прогон вверх }
    while (Result > 0)and(FRecs[Result - 1].Time > Temp.Time) do
    begin
      FRecs[Result] := FRecs[Result - 1];
      dec(Result);
      Changed := True;
    end;

    { прогон вниз }
    while (Result < High(FRecs))and(FRecs[Result + 1].Time < Temp.Time) do
    begin
      FRecs[Result] := FRecs[Result + 1];
      inc(Result);
      Changed := True;
    end;

    { запись }
    if Changed then
      FRecs[Result] := Temp;
  end;
end;

{==============================================================================}
function TDiaryPage.TraceLast: integer;
{==============================================================================}
{var
  temp: TCustomRecord;
  Changed: boolean; }
begin
  (*
  Result := High(FRecs);
  if (Index >= 0)and(Index <= High(FRecs)) then
  begin
    Temp := Recs[Result];
    Changed := False;

    { прогон вверх }
    while (Result > 0)and(FRecs[Result-1].Time > Temp.Time) do
    begin
      FRecs[Result] := FRecs[Result-1];
      dec(Result);
      Changed := True;
    end;

    { запись }
    if Changed then
      FRecs[Result] := Temp;
  end;   *)

  Result := Trace(High(FRecs));
end;

{==============================================================================}
procedure TDiaryPage.ReadFrom(S: TStringList);
{==============================================================================}
var
  i,k: integer;
  CurStr: string;
  TempStr: string;
  TempTime: integer;
  TempValue: real;
  TempFinger: integer;
  TempShort: boolean;
  Meal: TMealRecord;
  TempFood: TFoodMassed;
begin
  // TODO: протестировать скорость загрузки дневника из XML
  //Log('TDiaryPage.ReadFrom() started');

  if S = nil then
  begin
    //Log('TDiaryPage.ReadFrom() error: S=nil');
    raise Exception.Create('TDiaryPage.ReadFrom(): поток для чтения не может быть nil');
  end;

  FSilentChange := True;

  try
    Clear;
    Meal := nil;

    for i := 0 to S.Count - 1 do
    if (S[i] <> '') then
    begin
      CurStr := S[i];
      case CurStr[1] of
        '*':
        begin
          TempTime := StrToTimeQuick(Copy(CurStr,2,5));

          k := pos('|', curStr);
          if k > 0 then
          begin
            TempValue := StrToFloat(CheckDot( Copy(CurStr, 8, k-8) ));
            TempFinger := StrToInt( Copy(CurStr, k+1, Length(CurStr)-k) );
          end else
          begin
            TempValue := StrToFloat(CheckDot(Copy(CurStr,8,Length(CurStr)-7)));
            TempFinger := -1;
          end;                                                            
          Add(TBloodRecord.Create(TempTime, TempValue, TempFinger));
        end;
        '-':
        begin
          TempTime := StrToTimeQuick(Copy(CurStr,2,5));
          TempValue := StrToFloat(CheckDot(Copy(CurStr,8,Length(CurStr)-7)));
          Add(TInsRecord.Create(TempTime, TempValue));
        end;
        ' ':
        begin
          TempTime := StrToTimeQuick(Copy(CurStr,2,5));
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
          TempTime := StrToTimeQuick(Copy(CurStr, 2, 5));
          TempStr := Copy(CurStr, 8, Length(CurStr) - 7);
          Add(TNoteRecord.Create(TempTime, TempStr));
        end;
        {else
          // и что, из-за одного символа вся база полетит?
          raise ELoadingError.Create('TDiaryPage.ReadFrom: Некорректные данные'#13+
            'Строка :'+#13+
            CurStr);   }
      end;
    end;
  finally
    //Log('TDiaryPage.ReadFrom() finished');
    //FUpdateStampOnChange := True;
  end;
  //Log('TDiaryPage.ReadFrom() done ok');

  FSilentChange := False;
end;

{==============================================================================}
procedure TDiaryPage.ReadFrom(const S: string);
{==============================================================================}
var
  Temp: TStringList;
begin
  Temp := TStringList.Create;
  try
    Temp.Text := S;
    ReadFrom(Temp);
  finally
    Temp.Free;
  end;
end;

{==============================================================================}
procedure TDiaryPage.ReadFrom(S: TPageData);
{==============================================================================}
begin
  FDate := S.Date;
  FTimeStamp := S.TimeStamp;
  FVersion := S.Version;
  ReadFrom(S.Page);


  {

  Switch: boolean;

  ------

  Switch := Switch and (not SilentMode);
  SilentMode := SilentMode or Switch;

  ...

  SilentMode := SilentMode and (not Switch);

  ------

  OldMode := SilentMode;
  if Switch then SilentMode := True;

  ...

  SilentMode := OldMode;

  }
end;

{==============================================================================}
procedure TDiaryPage.WriteTo(S: TStringList);
{==============================================================================}
var
  j, n: integer;
begin
  if (S = nil) then
    raise Exception.Create('TDiaryPage.WriteTo(): поток для записи не может быть nil');

  for j := 0 to Count - 1 do
  begin
    if (FRecs[j].RecType = TBloodRecord) then
    begin
      // TODO: use Format() instead
      s.Add(
        '*' + TimeToStr(FRecs[j].Time) +
        ' ' + FloatToStr(TBloodRecord(FRecs[j]).Value) +
        '|' + IntToStr(TBloodRecord(FRecs[j]).Finger)
      )
    end else

    if (FRecs[j].RecType = TInsRecord) then
    begin
      s.Add(
        '-' + TimeToStr(FRecs[j].Time) +
        ' ' + FloatToStr(TInsRecord(FRecs[j]).Value)
      );
    end else

    if (FRecs[j].RecType = TMealRecord) then
    begin
      if TMealRecord(FRecs[j]).ShortMeal then
        s.Add(' ' + TimeToStr(FRecs[j].Time) + 's')
      else
        s.Add(' ' + TimeToStr(FRecs[j].Time));

      for n := 0 to TMealRecord(FRecs[j]).Count - 1 do
        s.Add('#' + TMealRecord(FRecs[j])[n].Write);
    end else

    if (FRecs[j].RecType = TNoteRecord) then
    begin
      s.Add(
        '%' + TimeToStr(FRecs[j].Time) +
        ' ' + TNoteRecord(FRecs[j]).Text
      );
    end;
  end;
end;

{==============================================================================}
procedure TDiaryPage.WriteTo(out S: string);
{==============================================================================}
var
  Temp: TStringList;
begin
  Temp := TStringList.Create;
  try
    WriteTo(Temp);
    S := Temp.Text;

   { if (S <> '') and (S[Length(S)] = #13) then
      Delete(S, Length(S), 1);}
  finally
    Temp.Free;
  end;
end;

{==============================================================================}
procedure TDiaryPage.WriteTo(S: TPageData);
{==============================================================================}
begin
  S.Date := FDate;
  S.TimeStamp := FTimeStamp;
  S.Version := FVersion;
  WriteTo(S.Page);
end;

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
  { проверяем аргументы }
  if not CorrectTime(ATime) then
    raise Exception.Create('FindRecord: некорректное значение Time (' + IntToStr(ATime) + ')');
  if (DeltaTime <= 0) then
    raise Exception.Create('FindRecord: некорректное значение DeltaTime (' + IntToStr(DeltaTime) + ')');

  { определяем границы }
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

  // TODO: проверить, что не вызывается слишком часто (например, при загрузке)
  // TODO: [Trunc(Now) + 1] — не лучшее решение
  if (Page <> nil) then
    UpdateCached_Postprand; // TODO: optimize
    //UpdatePostprand(Page.Date - 1, Trunc(Now) + 1);

  PageData := TPageData.Create;
  Page.WriteTo(PageData);
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
    // если убрать проверку, то кэш будет разрастаться при повторном вызове этой процедуры
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
    FCache[i].WriteTo(Pages[i]);
  end;

  // отправляем источнику
  FSource.PostPages(Pages);

  // освобождаем память
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
    FCache[i].ReadFrom(PageData);
    PageData.Free;

    //FCache[i].FCalculatedPostprand := False; - нужно сохранить состояния для UpdateCached_Postprand
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
  inherited Create(PageData);
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

  // TODO: дублирующийся код (UpdatePostprand)
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

  // TODO: дублирующийся код (GetNextDayFreeTime)
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

procedure TDiaryPage.AddChangeListener(Listener: TEventPageChanged);

  function GetListenerIndex(L: TEventPageChanged): integer;
  var
    i: integer;
  begin
    for i := 0 to High(FOnChange) do
    if (@FOnChange[i] = @L) then
    begin
      Result := i;
      Exit;
    end;
    Result := -1;
  end;

begin
  if (Assigned(Listener)) then
  begin
    if (GetListenerIndex(Listener) = -1) then
    begin
      SetLength(FOnChange, Length(FOnChange) + 1);
      FOnChange[High(FOnChange)] := Listener;
    end;
  end;
end;

initialization
  ActiveX.CoInitialize(nil)
finalization
  ActiveX.CoUninitialize;
end.
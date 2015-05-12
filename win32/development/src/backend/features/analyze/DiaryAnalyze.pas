unit DiaryAnalyze;

interface

uses
  SysUtils,
  Windows,
  Classes,
  Math,
  AnalyzeInterface,
  DiaryRoutines,
  DiaryRecords,
  DiaryDatabase,
  DiaryDAO,

  // <DEBUG ONLY>
  JsonSerializer,
  DiaryPageSerializer,
  // </DEBUG ONLY>

  AutoLog;

type
  TAnalyzer = record
    Name: string;
    AnalyzeFunc: TAnalyzeFunction;
    InfoFunc: TInfoFunction;
  end;

  TAnalyzers = array of TAnalyzer;

  TAnalyzeResult = record
    KoofList: TKoofList;
    Error: real;
    Time: cardinal;
    {*} Weight: real;
    {*} AnList: TAnalyzeRecList;
  end;

  TAnalyzeResults = array of TAnalyzeResult;

  TValFunction = (vfLinearAbs, vfLinearAvg, vfDistance, vfQuadric);
  TRealArray = array of Real;

  { Initialization }
  function LoadAnalyzers(const FileName: string): TAnalyzers;
  function AddAnalyzers(const Source: TAnalyzers; var Target: TAnalyzers): boolean;

  { Analyzing }
  function Analyze(const Analyzer: TAnalyzer; const Items: TRecordList; const Par: TRealArray;
    CallBack: TCallbackProgressProc): TAnalyzeResult; overload;
  function Analyze(const Analyzers: TAnalyzers; const Items: TRecordList; const Par: TRealArray;
    CallBack: TCallbackProgressProc): TAnalyzeResults; overload;
  function BuildAvgAnalyzer(const AnalyzeResults: TAnalyzeResults): TAnalyzeResult;

  { Verifying }
  function GetRecError(const Rec: TAnalyzeRec; const KoofList: TKoofList; ValFunct: TValFunction): real;
  function GetRecListError(const List: TAnalyzeRecList; const KoofList: TKoofList; ValFunc: TValFunction): real;

  procedure AnalyzeBS(Base: TDiaryDAO; TimeFrom, TimeTo: TDateTime; out Mean, StdDev, Targeted, Less, More: Extended);

const
  PAR_ADAPTATION  = 0;
  //PAR_COMPRESSION = 1;

implementation

type
  // #dao
  TPrimeRec = record
    Date: TDateTime; // для взвешивания

    BloodInTime: integer;  // в минутах, возможно больше 1440
    BloodInValue: real;

    InsTime: integer;
    InsValue: real;

    FoodTime: integer;
    Prots,Fats,Carbs: real;

    BloodOutTime: integer;
    BloodOutValue: real;
  end;

  // #dao
  TPrimeRecList = array of TPrimeRec;

{======================================================================================================================}
function LoadAnalyzers(const FileName: string): TAnalyzers;
{======================================================================================================================}
var
  Lib: HModule;
  Analyzer: TAnalyzer;
  Index: integer;
  AnalFuncName: string;
  InfoFuncName: string;
  Found: boolean;
begin
  Log(DEBUG, 'Loading analyze unit ' + FileName);
  SetLength(Result, 0);

  if FileExists(FileName) then
  try
    Log(DEBUG, 'Analyze unit found, loading...');
    Lib := LoadLibrary(PChar(FileName));
    if (Lib <> 0) then
    begin
      Log(DEBUG, 'Analyze unit loaded ok');
      Index := 0;
      repeat
        AnalFuncName := AnalyzeFunctionName + IntToStr(Index);
        InfoFuncName:= InfoFunctionName + IntToStr(Index);

        @Analyzer.AnalyzeFunc := GetProcAddress(Lib, PAnsiChar(AnalFuncName));
        @Analyzer.InfoFunc := GetProcAddress(Lib, PAnsiChar(InfoFuncName));
        Found := (@Analyzer.AnalyzeFunc <> nil) and (@Analyzer.InfoFunc <> nil);

        if Found then
        begin
          Analyzer.Name := Analyzer.InfoFunc();
          Log(INFO, 'Analyze function found: ' + Analyzer.Name);

          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Analyzer;
        end;

        inc(Index);
      until not Found;

      Log(DEBUG, 'Analyze functions loading done, function found: ' + IntToStr(Index));
    end else
    begin
      Log(ERROR, 'Failed to load analyze unit, LoadLibrary returned 0, file name: ' + FileName);
    end;
  except
    on e: Exception do
    begin
      Log(ERROR, 'Failed to load analyze unit: ' + e.Message);
    end;
  end else
  begin
    Log(ERROR, 'Analyze unit not found: ' + FileName);
  end;
end;

{======================================================================================================================}
function AddAnalyzers(const Source: TAnalyzers; var Target: TAnalyzers): boolean;
{======================================================================================================================}
var
  i: integer;
begin
  Result := (Length(Source) > 0);

  if Result then
  begin
    SetLength(Target, Length(Target) + Length(Source));
    for i := 0 to High(Source) do
      Target[Length(Target) - Length(Source) + i] := Source[i];
  end;
end;

{ ПОДГОТОВКА МАТЕРИАЛА }

{======================================================================================================================}
function ExtractPrimeRecords(Items: TRecordList): TPrimeRecList;
{======================================================================================================================}
const
  MAX_BLOCK_TIME = 12 / 24; // 12 hours
var
  i, j: integer;

  PrevBloodTime: TDateTime;
  PrevBloodValue: real;

  Prots, Fats, Carbs, CurCarbs, MaxCarbs: real;
  Ins, CurIns, MaxIns: real;
  TimeF, TimeI: TDateTime;
  TimeShift: integer;

  // Debug: string;

  procedure InitCounters;
  begin
    Ins := 0;
    MaxIns := 0;
    Prots := 0;
    Fats := 0;
    Carbs := 0;
    MaxCarbs := -1;
    TimeF := -1;
    TimeI := -1;
  end;

  function GetAbsLocalMinutes(Time: TDateTime): integer;
  begin
    Result := Round(UTCToLocal(Time) * MinPerDay);
  end;

begin
  SetLength(Result, 0);

  PrevBloodTime := -1;
  PrevBloodValue := -1;
  InitCounters();

  { обработка }

  // TODO: hardcode
  UpdatePostprand(Items, 3.5 / HourPerDay, 3.5 / HourPerDay, 20 / MinPerDay);

  //Debug := JsonWrite(SerializeVersionedDiaryRecords(Items));
  //DiaryRoutines.WriteFile('temp\analyze_input.txt', Debug);

  // Debug := '';

  { 1. Создаём RecList, считая время в минутах от 01/01/1899 }
  for i := Low(Items) to High(Items) do
  begin
    // **** INSULIN RECORD ****
    if (Items[i].RecType = TInsRecord) then
    begin
      // {*}Debug := Debug + Items[i].ID + #9'ins'#9;

      CurIns := TInsRecord(Items[i]).Value;
      Ins := Ins + CurIns;
      if (CurIns > MaxIns) then
      begin
        MaxIns := CurIns;
        TimeI := Items[i].Time;
        // {*}Debug := Debug + 'max updated'#13;
      end else
      begin
        // {*}Debug := Debug + #13;
      end;
    end else

    // **** MEAL RECORD ****
    if (Items[i].RecType = TMealRecord) then
    begin
      // {*}Debug := Debug + Items[i].ID + #9'meal'#9;

      Prots := Prots + TMealRecord(Items[i]).Prots;
      Fats := Fats + TMealRecord(Items[i]).Fats;
      CurCarbs := TMealRecord(Items[i]).Carbs;
      Carbs := Carbs + CurCarbs;
      if (CurCarbs > MaxCarbs) then
      begin
        MaxCarbs := CurCarbs;
        TimeF := Items[i].Time;
        // {*}Debug := Debug + 'max updated'#13;
      end else
      begin
        // {*}Debug := Debug + #13;
      end;
    end else

    // **** BLOOD RECORD ****
    if (Items[i].RecType = TBloodRecord) then
    begin
      // {*}Debug := Debug + Items[i].ID + #9'blood'#9;
      if (not TBloodRecord(Items[i]).PostPrand) then
      begin
        if (PrevBloodValue > 0) and
           ((Carbs > 0) or (Prots > 0)) and
           (Ins > 0) and
           (Items[i].Time - PrevBloodTime < MAX_BLOCK_TIME) then
        begin
          { Add item }
          j := Length(Result);
          SetLength(Result, j + 1);
          Result[j].BloodInTime := GetAbsLocalMinutes(PrevBloodTime);
          Result[j].BloodInValue := PrevBloodValue;
          Result[j].InsTime :=  GetAbsLocalMinutes(TimeI);
          Result[j].InsValue := Ins;
          Result[j].FoodTime := GetAbsLocalMinutes(TimeF);
          Result[j].Prots := Prots;
          Result[j].Fats := Fats;
          Result[j].Carbs := Carbs;
          Result[j].BloodOutTime := GetAbsLocalMinutes(Items[i].Time);
          Result[j].BloodOutValue := TBloodRecord(Items[i]).Value;
          Result[j].Date := UTCToLocal(TimeF);
          // {*}Debug := Debug + 'OK - new item added'#9 +
          //   Format('%.1f'#9'%.1f'#9'%.1f'#9'%.1f'#13, [Result[j].BloodInValue, Result[j].InsValue, Result[j].Carbs,
          //     Result[j].BloodOutValue]);
        end else
        begin
          // {*}Debug := Debug + 'conditions failed - ignored'#13;
        end;

        { Reset }
        PrevBloodTime := Items[i].Time;
        PrevBloodValue := TBloodRecord(Items[i]).Value;
        InitCounters();
      end else
      begin
        // {*}Debug := Debug + 'postprandial - ignored'#13;
      end;
    end;
  end;

  //ShowMessage(Debug);

  { 2. Восстановление относительных времён }

  for i := 0 to High(Result) do
  begin
    if (Result[i].Prots > 0) or (Result[i].Carbs > 0) then
    begin
      {TimeShift := Result[i].FoodTime mod MinPerDay;
      TimeShift := Result[i].FoodTime-TimeShift;}
      TimeShift := (Result[i].FoodTime div MinPerDay);
    end else
    if (Result[i].InsValue > 0) then
    begin
      {TimeShift := Result[i].InsTime mod MinPerDay;
      TimeShift := Result[i].InsTime-TimeShift; }
      TimeShift := Result[i].InsTime div MinPerDay;
    end else
      TimeShift := ((Result[i].BloodOutTime + Result[i].BloodInTime) div 2) div MinPerDay;
      {ShowMessage(
        'AnalyzeUnit/ExtractRecords/Result[i]: '+
        'FoodTime = -1, InsTime=-1'); }

    TimeShift := TimeShift * MinPerDay;

    Result[i].BloodInTime := Result[i].BloodInTime - TimeShift;
    Result[i].BloodOutTime := Result[i].BloodOutTime - TimeShift;
    if (Result[i].InsTime >- 1) then
      Result[i].InsTime := Result[i].InsTime - TimeShift;
    if (Result[i].FoodTime >- 1) then
      Result[i].FoodTime := Result[i].FoodTime - TimeShift;
  end;

  FreeRecords(Items);
end;

{======================================================================================================================}
function FormatRecords(const PrimeList: TPrimeRecList; const Adaptation: real): TAnalyzeRecList;
{======================================================================================================================}
{ Копирует основные поля и вычисляет нормальные веса }

  function F(const X: real): real;
  { Adaptation in [0.0, 1.0]                 }
  {    0.0 is the slowest (but very stable), }
  {    1.0 is the quickest (but unstable)    }
  { X from [0, 1]                            }
  begin
    Result := 1 + 0.5 * Adaptation * ( sin(pi * (X - 0.5) ) - 1 );
    // Result := Adaptation * X + (1 - Adaptation);

    //Log(DEBUG, Format('F(%.2f, %.2f) = %.4f', [X, Adaptation, Result]));
  end;

var
  i: integer;
  MinTime: TDateTime;
  MaxTime: TDateTime;
  MinW, MaxW: real;
begin
  SetLength(Result, Length(PrimeList));

  if (Length(Result) > 0) then
  begin
    MinTime := PrimeList[0].Date;
    MaxTime := PrimeList[0].Date;

    for i := 1 to High(PrimeList) do
    begin
      MinTime := Math.Min(MinTime, PrimeList[i].Date);
      MaxTime := Math.Max(MaxTime, PrimeList[i].Date);
    end;

    for i := 0 to High(Result) do
    begin
      Result[i].Prots  := PrimeList[i].Prots;
      Result[i].Fats   := PrimeList[i].Fats;
      Result[i].Carbs  := PrimeList[i].Carbs;
      Result[i].Ins    := PrimeList[i].InsValue;
      Result[i].BSIn   := PrimeList[i].BloodInValue;
      Result[i].BSOut  := PrimeList[i].BloodOutValue;
      Result[i].Time   := (MinPerDay + PrimeList[i].FoodTime) mod MinPerDay;
      Result[i].Weight := F((PrimeList[i].Date - MinTime) / (MaxTime - MinTime)){ * PrimeList[i].Carbs};
    end;

    //Log(DEBUG, 'Saved', True);

    //SaveAnalyzeList(List, 'temp\denormalized_anlist.txt');

    { нормализация }
    MinW := Result[0].Weight;
    MaxW := Result[0].Weight;
    for i := 1 to High(Result) do
    begin
      MinW := Math.Min(MinW, Result[i].Weight);
      MaxW := Math.Max(MaxW, Result[i].Weight);
    end;

    if abs(MinW - MaxW) > 0.0001 then
      for i := 0 to High(Result) do
        Result[i].Weight := (Result[i].Weight - MinW) / (MaxW - MinW)
    else
      for i := 0 to High(Result) do
        Result[i].Weight := 1.0;
  end;
end;

// TODO: incapsulate as method "TAnalyzer.Analyze(Items, Par, CallBack)"
{======================================================================================================================}
function Analyze(const Analyzer: TAnalyzer; const Items: TRecordList; const Par: TRealArray;
  CallBack: TCallbackProgressProc): TAnalyzeResult;
{======================================================================================================================}
var
  PrimeList: TPrimeRecList;
  StartTime: cardinal;
begin
  PrimeList := ExtractPrimeRecords(Items);
  Result.AnList := FormatRecords(PrimeList, Par[PAR_ADAPTATION]);

  StartTime := GetTickCount();
  Analyzer.AnalyzeFunc(Result.AnList, Result.KoofList, CallBack);
  Result.Error := GetRecListError(Result.AnList, Result.KoofList, vfQuadric);
  Result.Time := GetTickCount() - StartTime;
end;

{======================================================================================================================}
function Analyze(const Analyzers: TAnalyzers; const Items: TRecordList; const Par: TRealArray;
  CallBack: TCallbackProgressProc): TAnalyzeResults;
{======================================================================================================================}
var
  i: integer;
begin
  if (Length(Analyzers) = 0) then
    raise Exception.Create('No analyzers loaded');

  SetLength(Result, Length(Analyzers));
  for i := 0 to High(Analyzers) do
    Result[i] := Analyze(Analyzers[i], Items, Par, Callback);
end;

{======================================================================================================================}
function BuildAvgAnalyzer(const AnalyzeResults: TAnalyzeResults): TAnalyzeResult;
{======================================================================================================================}
var
  SummWeight: Real;
  i, Time: integer;
begin
  if (Length(AnalyzeResults) = 0) then
    raise Exception.Create('No analyzers loaded');

  // weights: calculation & normalization

  SummWeight := 0;
  Result.Time := 0;
  for i := 0 to High(AnalyzeResults) do
  begin
    AnalyzeResults[i].Weight := Exp(-0.1 * Sqr(AnalyzeResults[i].Error));
    SummWeight := SummWeight + AnalyzeResults[i].Weight;
    Result.Time := Result.Time + AnalyzeResults[i].Time;
  end;

  if (SummWeight > 0) then
  for i := 0 to High(AnalyzeResults) do
    AnalyzeResults[i].Weight := AnalyzeResults[i].Weight / SummWeight;

  // calculation average curve

  for Time := 0 to MinPerDay - 1 do
  begin
    Result.KoofList[Time].k := 0;
    Result.KoofList[Time].q := 0;
    Result.KoofList[Time].p := 0;

    for i := 0 to High(AnalyzeResults) do
    begin
      Result.KoofList[Time].k := Result.KoofList[Time].k + AnalyzeResults[i].KoofList[Time].k * AnalyzeResults[i].Weight;
      Result.KoofList[Time].q := Result.KoofList[Time].q + AnalyzeResults[i].KoofList[Time].q * AnalyzeResults[i].Weight;
      Result.KoofList[Time].p := Result.KoofList[Time].p + AnalyzeResults[i].KoofList[Time].p * AnalyzeResults[i].Weight;
    end;
  end;

  Result.Error := GetRecListError(AnalyzeResults[0].AnList, Result.KoofList, vfQuadric);
  Result.Weight := 1.0;
end;

{======================================================================================================================}
function GetRecError(const Rec: TAnalyzeRec; const KoofList: TKoofList; ValFunct: TValFunction): real;
{======================================================================================================================}
var
  err: real;
  Koof: TKoof;
begin
  Koof := KoofList[Rec.Time];
  err :=
    (Rec.BSIn
    + Rec.Carbs * Koof.k
    + Rec.Prots * Koof.p
    - Rec.Ins   * Koof.q)
    - Rec.BSOut;

  case ValFunct of
    vfLinearAbs: Result := abs(err);
    vfLinearAvg: Result := err;
    vfDistance:  Result := abs(err) / Sqrt(Sqr(Rec.Carbs) + Sqr(Rec.Prots) + Sqr(Rec.Ins));
    vfQuadric:   Result := Sqr(err);
    else raise Exception.Create('GetRecError: недопустимый аргумент ValFunct');
  end;
end;

{======================================================================================================================}
function GetRecListError(const List: TAnalyzeRecList; const KoofList: TKoofList; ValFunc: TValFunction): real;
{======================================================================================================================}
var
  i: integer;
  SummWeight: Real;
begin
  Result := 0;
  SummWeight := 0;
  for i := 0 to High(List) do
  begin
    Result := Result + GetRecError(List[i], KoofList, ValFunc) * List[i].Weight;
    SummWeight := SummWeight + List[i].Weight;
  end;

  if (SummWeight > EPS) then
    Result := Result / SummWeight;

  if (ValFunc = vfQuadric) then
    Result := Sqrt(Result);
end;

{======================================================================================================================}
function GetKoofRandomation(const KoofList: TKoofList): real;
{======================================================================================================================}

  function Dist(n1,n2: integer): real;
  begin
    Result := Sqrt(
      Sqr(KoofList[n1].k - KoofList[n2].k)+
      Sqr(KoofList[n1].q - KoofList[n2].q)+
      Sqr(KoofList[n1].p - KoofList[n2].p));
  end;

{var
  i: integer; }
begin
  Result := 0;
  {for i := 0 to High(KoofList) do
    Result := Result + Dist(i,(i + 1) mod length(KoofList));  }

  { *** }
end;

{======================================================================================================================}
procedure AnalyzeBS(Base: TDiaryDAO; TimeFrom, TimeTo: TDateTime; out Mean, StdDev, Targeted, Less, More: Extended);
{======================================================================================================================}
var
  i: integer;
  List: array of double;
  Items: TRecordList;
begin
  Targeted := 0;
  Less := 0;
  More := 0;

  Items := Base.FindPeriod(TimeFrom, TimeTo);

  for i := Low(Items) to High(Items) do
  if (Items[i].RecType = TBloodRecord) and
     (not TBloodRecord(Items[i]).PostPrand) then
  begin
    SetLength(List, Length(List) + 1);
    List[High(List)] := TBloodRecord(Items[i]).Value;

    // TODO: брать данные из настроек и передавать как параметры
    // TODO: учитывать постпрандиальные замеры - для них тоже есть ТЦП
    if (List[High(List)] < 3.8) then
      Less := Less + 1 else
    if (List[High(List)] <= 6.2) then
      Targeted := Targeted + 1
    else
      More := More + 1;
  end;

  Math.MeanAndStdDev(List, Mean, StdDev);
  if (Length(List) > 0) then
  begin
    Targeted := Targeted / Length(List);
    Less := Less / Length(List);
    More := More / Length(List);
  end;
end;

end.


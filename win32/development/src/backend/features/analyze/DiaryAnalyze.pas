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
  // private
    AnalyzeFunc: TAnalyzeFunction;
    InfoFunc: TInfoFunction;
  // public
    Name: string;
    KoofList: TKoofList;   // коэффициенты
    Error: real;
    Weight: real;
    Time: cardinal;
  end;

  TValFunction = (vfLinearAbs, vfLinearAvg, vfDistance, vfQuadric);
  TRealArray = array of Real;

  { инициализация }
  function AddAnalyzer(const FileName: string): boolean;
  function AnalyzeDiary(Base: TDiaryDAO; TimeFrom, TimeTo: TDateTime; const Par: TRealArray;
    CallBack: TCallbackProgressProc): TAnalyzeRecList;

  { использование }
  function GetAnalyzersCount: integer;
  function GetAnalyzer(Index: integer): TAnalyzer;
  function GetKoof(Time: integer): TKoof; overload;
  function GetKoof(Time: integer; AnalyzerIndex: integer): TKoof; overload;

  { анализ результатов }
  function GetRecError(const Rec: TAnalyzeRec; const KoofList: TKoofList; ValFunct: TValFunction): real;
  function GetRecListError(const List: TAnalyzeRecList; const KoofList: TKoofList; ValFunc: TValFunction): real;

  procedure AnalyzeBS(Base: TDiaryDAO; TimeFrom, TimeTo: TDateTime; out Mean, StdDev, Targeted, Less, More: Extended);

  procedure SaveAnalyzeList(const List: TAnalyzeRecList; const FileName: string);

var
  // TODO: move to DiaryCore
  AvgAnalyzer: TAnalyzer;
  
const
  PAR_ADAPTATION  = 0;
  //PAR_COMPRESSION = 1;

implementation
  
var
  Analyzer: array of TAnalyzer;

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
  InitCounters;

  { обработка }

  // TODO: hardcode
  UpdatePostprand(Items, 3.5 / HourPerDay, 3.5 / HourPerDay, 20 / MinPerDay);

  //Debug := JsonWrite(SerializeVersionedDiaryRecords(Items));
  //DiaryRoutines.WriteFile('temp\analyze_input.txt', Debug);

  { 1. Создаём RecList, считая время в минутах от 01/01/1899 }
  for i := Low(Items) to High(Items) do
  begin
    // **** INSULIN RECORD ****
    if (Items[i].RecType = TInsRecord) and
       (PrevBloodTime > -1) then
    begin
      CurIns := TInsRecord(Items[i]).Value;
      Ins := Ins + CurIns;
      if (CurIns > MaxIns) then
      begin
        MaxIns := CurIns;
        TimeI := Items[i].Time;
      end;
    end else

    // **** MEAL RECORD ****
    if (Items[i].RecType = TMealRecord) and
       (PrevBloodTime > -1) then
    begin
      Prots := Prots + TMealRecord(Items[i]).Prots;
      Fats := Fats + TMealRecord(Items[i]).Fats;
        CurCarbs := TMealRecord(Items[i]).Carbs;
      Carbs := Carbs + CurCarbs;
      if (CurCarbs > MaxCarbs) then
      begin
        MaxCarbs := CurCarbs;
        TimeF := Items[i].Time;
      end;
    end else

    // **** BLOOD RECORD ****
    if (Items[i].RecType = TBloodRecord) and
       (not TBloodRecord(Items[i]).PostPrand) then
    begin
      if (PrevBloodValue = -1) then
      begin
        PrevBloodTime := Items[i].Time;
        PrevBloodValue := TBloodRecord(Items[i]).Value;
        InitCounters;
      end else
      if ((Carbs > 0) or (Prots > 0)) and (Ins > 0) and
         (Items[i].Time - PrevBloodTime < MAX_BLOCK_TIME)
         then
      //if (True) then
      begin
        { запись }

        {if (((TimeF + 1440) mod 1440) > 780)and
           (((TimeF + 1440) mod 1440) < 840)
        then  }
        begin
          j := length(Result);
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
        end;

        { подготовка к следующему циклу }
        PrevBloodTime := Items[i].Time;
        PrevBloodValue := TBloodRecord(Items[i]).Value;
        InitCounters;
      end else
      begin
        { замер нормальный, но перед ним ни еды, ни инсулина }
        PrevBloodTime := Items[i].Time;
        PrevBloodValue := TBloodRecord(Items[i]).Value;
        InitCounters;
      end;
    {=====================================================}
    end;
  end;

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
  CurTime: TDateTime;
  Min: TDateTime;
  MinW, MaxW: real;
begin
  SetLength(Result, Length(PrimeList));

  if (Length(Result) > 0) then
  begin
    CurTime := GetTimeUTC();
    Min := Trunc(CurTime);

    for i := 0 to High(PrimeList) do
    if (PrimeList[i].Date < Min) then
      Min := PrimeList[i].Date;

    for i := 0 to High(Result) do
    begin
      Result[i].Prots  := PrimeList[i].Prots;
      Result[i].Fats   := PrimeList[i].Fats;
      Result[i].Carbs  := PrimeList[i].Carbs;
      Result[i].Ins    := PrimeList[i].InsValue;
      Result[i].BSIn   := PrimeList[i].BloodInValue;
      Result[i].BSOut  := PrimeList[i].BloodOutValue;
      Result[i].Time   := (MinPerDay + PrimeList[i].FoodTime) mod MinPerDay;
      Result[i].Weight := F((PrimeList[i].Date - Min) / (CurTime - Min)){ * PrimeList[i].Carbs};
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

{======================================================================================================================}
function AddAnalyzer(const FileName: string): boolean;
{======================================================================================================================}
var
  Lib: HModule;
  Temp: TAnalyzer;
  Index: integer;
  AnalFuncName: string;
  InfoFuncName: string;
begin
  Log(DEBUG, 'Loading analyze unit ' + FileName);

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

        @Temp.AnalyzeFunc := GetProcAddress(Lib, PAnsiChar(AnalFuncName));
        @Temp.InfoFunc := GetProcAddress(Lib, PAnsiChar(InfoFuncName));
        Result :=
          (@Temp.AnalyzeFunc <> nil) and
          (@Temp.InfoFunc <> nil);

        if Result then
        begin
          Temp.Name := Temp.InfoFunc;
          Log(INFO, 'Analyze function found: ' + Temp.Name);

          SetLength(Analyzer, Length(Analyzer) + 1);
          Analyzer[High(Analyzer)] := Temp;
        end;

        inc(Index);
      until not Result;

      Log(DEBUG, 'Analyze functions loading done, function found: ' + IntToStr(Index));
    end else
    begin
      Log(ERROR, 'Failed to load analyze unit, LoadLibrary returned 0, file name: ' + FileName);
      Result := False;
    end;
  except
    on e: Exception do
    begin
      Log(ERROR, 'Failed to load analyze unit: ' + e.Message);
      Result := False;
    end;
  end else
  begin
    Log(ERROR, 'Analyze unit not found: ' + FileName);
    Result := False
  end;
end;

{======================================================================================================================}
function AnalyzeDiary(Base: TDiaryDAO; TimeFrom, TimeTo: TDateTime; const Par: TRealArray;
  CallBack: TCallbackProgressProc): TAnalyzeRecList;
{======================================================================================================================}
var
  Items: TRecordList;
  PrimeList: TPrimeRecList;
  StartTime: cardinal;
  SummWeight: Real;
  i, Time: integer;
begin
  if (Length(Analyzer) = 0) then
    raise Exception.Create('No analyzers loaded');

  // подготовка материала
  Items := Base.FindPeriod(TimeFrom, TimeTo);
  PrimeList := ExtractPrimeRecords(Items);
  Result := FormatRecords(PrimeList, Par[PAR_ADAPTATION]);

  // собственно анализ
  for i := 0 to High(Analyzer) do
  begin
    StartTime := GetTickCount;
    // TODO: incapsulate as method "TAnalyzer.Analyze(AnList)"
    Analyzer[i].AnalyzeFunc(Result, Analyzer[i].KoofList, CallBack);
    Analyzer[i].Error := GetRecListError(Result, Analyzer[i].KoofList, vfQuadric);
    Analyzer[i].Weight := Exp(-0.1 * Sqr(Analyzer[i].Error));
    Analyzer[i].Time := GetTickCount - StartTime;
  end;

  // нормализация весов
  SummWeight := 0;
  for i := 0 to High(Analyzer) do
    SummWeight := SummWeight + Analyzer[i].Weight;

  if (SummWeight > 0) then
  for i := 0 to High(Analyzer) do
    Analyzer[i].Weight := Analyzer[i].Weight / SummWeight;

  // определение средних коэффицентов
  for Time := 0 to MinPerDay - 1 do
  begin
    AvgAnalyzer.KoofList[Time].k := 0;
    AvgAnalyzer.KoofList[Time].q := 0;
    AvgAnalyzer.KoofList[Time].p := 0;
    AvgAnalyzer.Time := 0;

    for i := 0 to High(Analyzer) do
    begin
      AvgAnalyzer.KoofList[Time].k := AvgAnalyzer.KoofList[Time].k + Analyzer[i].KoofList[Time].k;
      AvgAnalyzer.KoofList[Time].q := AvgAnalyzer.KoofList[Time].q + Analyzer[i].KoofList[Time].q;
      AvgAnalyzer.KoofList[Time].p := AvgAnalyzer.KoofList[Time].p + Analyzer[i].KoofList[Time].p;
      AvgAnalyzer.Time := AvgAnalyzer.Time + Analyzer[i].Time;
    end;

    AvgAnalyzer.KoofList[Time].k := AvgAnalyzer.KoofList[Time].k / Length(Analyzer);
    AvgAnalyzer.KoofList[Time].q := AvgAnalyzer.KoofList[Time].q / Length(Analyzer);
    AvgAnalyzer.KoofList[Time].p := AvgAnalyzer.KoofList[Time].p / Length(Analyzer);
  end;

  AvgAnalyzer.Error := GetRecListError(Result, AvgAnalyzer.KoofList, vfQuadric);
  AvgAnalyzer.Weight := 1.0;
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
begin
  Result := 0;
  for i := 0 to High(List) do
    Result := Result + GetRecError(List[i], KoofList, ValFunc);

  if (Length(List) > 0) then
    Result := Result / Length(List);

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
  Targeted := Targeted / Length(List);
  Less := Less / Length(List);
  More := More / Length(List);
end;

{======================================================================================================================}
procedure SaveAnalyzeList(const List: TAnalyzeRecList; const FileName: string);
{======================================================================================================================}
var
  s: TStrings;
  i: integer;
begin
  s := TStringList.Create;
  try
    s := TStringList.Create;

    S.Add(Format('%s'#9'%s'#9'%s'#9'%s'#9'%s'#9'%s'#9'%s', [
      'Time',
      'Weight',
      'Prots',
      'Fats',
      'Carbs',
      'Ins',
      'DBS']));

    for i := 0 to High(List) do
    begin
      S.Add(Format('%d'#9'%f'#9'%f'#9'%f'#9'%f'#9'%f'#9'%f', [
        List[i].Time,
        List[i].Weight,
        List[i].Prots,
        List[i].Fats,
        List[i].Carbs,
        List[i].Ins,
        List[i].BSOut - List[i].BSIn]));
    end;
    S.SaveToFile(FileName);
  finally
    s.Free;
  end;
end;

{======================================================================================================================}
function GetAnalyzersCount: integer;
{======================================================================================================================}
begin
  Result := Length(Analyzer);
end;

{======================================================================================================================}
function GetAnalyzer(Index: integer): TAnalyzer;
{======================================================================================================================}
begin
  Result := Analyzer[Index]
end;

{======================================================================================================================}
function GetKoof(Time: integer): TKoof;
{======================================================================================================================}
begin
  Result := AvgAnalyzer.KoofList[Time];
end;

{======================================================================================================================}
function GetKoof(Time: integer; AnalyzerIndex: integer): TKoof;
{======================================================================================================================}
begin
  Result := GetAnalyzer(AnalyzerIndex).KoofList[Time];
end;

end.


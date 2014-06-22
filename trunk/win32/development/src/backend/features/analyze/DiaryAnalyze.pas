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
  procedure AnalyzeDiary(Base: TDiaryDAO; FromDate, ToDate: TDateTime; const Par: TRealArray;
    CallBack: TCallbackProgressProc);

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
  AnList: TAnalyzeRecList;
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

{==============================================================================}
procedure ExtractRecords(Base: TDiaryDAO; TimeFrom, TimeTo: TDateTime;
  out List: TPrimeRecList);
{==============================================================================}
var
  i, j: integer;
  Items: TRecordList;

  PrevBloodTime: TDateTime;
  PrevBloodValue: real;

  Prots,Fats,Carbs,CurCarbs,MaxCarbs: real;
  Ins,CurIns,MaxIns: real;
  TimeF,TimeI: TDateTime;
  TimeShift: integer;

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
  SetLength(List, 0);

  PrevBloodTime := -1;
  PrevBloodValue := -1;
  InitCounters;

  { обработка }

  Items := Base.FindPeriod(TimeFrom, TimeTo);
  // TODO: hardcode
  UpdatePostprand(Items, 3.5 / HourPerDay, 3.5 / HourPerDay, 20 / MinPerDay);

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
      if ((Carbs > 0) or (Prots > 0)) and (Ins > 0) then
      //if (True) then
      begin
        { запись }

        {if (((TimeF + 1440) mod 1440) > 780)and
           (((TimeF + 1440) mod 1440) < 840)
        then  }
        begin
          j := length(List);
          SetLength(List, j + 1);
          List[j].BloodInTime := GetAbsLocalMinutes(PrevBloodTime);
          List[j].BloodInValue := PrevBloodValue;
          List[j].InsTime :=  GetAbsLocalMinutes(TimeI);
          List[j].InsValue := Ins;
          List[j].FoodTime := GetAbsLocalMinutes(TimeF);
          List[j].Prots := Prots;
          List[j].Fats := Fats;
          List[j].Carbs := Carbs;
          List[j].BloodOutTime := GetAbsLocalMinutes(Items[i].Time);
          List[j].BloodOutValue := TBloodRecord(Items[i]).Value;
          List[j].Date := UTCToLocal(TimeF);
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

  for i := 0 to High(List) do
  begin
    if (List[i].Prots > 0) or (List[i].Carbs > 0) then
    begin
      {TimeShift := List[i].FoodTime mod MinPerDay;
      TimeShift := List[i].FoodTime-TimeShift;}
      TimeShift := (List[i].FoodTime div MinPerDay);
    end else
    if (List[i].InsValue > 0) then
    begin
      {TimeShift := List[i].InsTime mod MinPerDay;
      TimeShift := List[i].InsTime-TimeShift; }
      TimeShift := List[i].InsTime div MinPerDay;
    end else
      TimeShift := ((List[i].BloodOutTime + List[i].BloodInTime) div 2) div MinPerDay;
      {ShowMessage(
        'AnalyzeUnit/ExtractRecords/List[i]: '+
        'FoodTime = -1, InsTime=-1'); }

    TimeShift := TimeShift * MinPerDay;

    List[i].BloodInTime := List[i].BloodInTime - TimeShift;
    List[i].BloodOutTime := List[i].BloodOutTime - TimeShift;
    if (List[i].InsTime >- 1) then
      List[i].InsTime := List[i].InsTime - TimeShift;
    if (List[i].FoodTime >- 1) then
      List[i].FoodTime := List[i].FoodTime - TimeShift;
  end;
end;

{==============================================================================}
procedure FormatRecords(const PrimeList: TPrimeRecList; out List: TAnalyzeRecList;
  const Adaptation: real);
{==============================================================================}
{ Копирует основные поля и вычисляет нормальные веса }

  function F(const X: real): real;
  { Adaptation in [0.0, 1.0]                 }
  {    0.0 is the slowest (but very stable), }
  {    1.0 is the quickest (but unstable)    }
  { X from [0, 1]                            }
  begin
    Result := 1 + 0.5 * Adaptation * ( sin(pi * (X - 0.5) ) - 1 );
    // Result := Adaptation * X + (1 - Adaptation);

    Log(DEBUG, Format('F(%.2f, %.2f) = %.4f', [X, Adaptation, Result]));
  end;

var
  i: integer;
  CurTime: TDateTime;
  Min: TDateTime;
  MinW, MaxW: real;
begin
  SetLength(List, Length(PrimeList));

  if (Length(List) > 0) then
  begin
    CurTime := GetTimeUTC();
    Min := Trunc(CurTime);

    for i := 0 to High(PrimeList) do
    if (PrimeList[i].Date < Min) then
      Min := PrimeList[i].Date;

    for i := 0 to High(List) do
    begin
      List[i].Prots  := PrimeList[i].Prots;
      List[i].Fats   := PrimeList[i].Fats;
      List[i].Carbs  := PrimeList[i].Carbs;
      List[i].Ins    := PrimeList[i].InsValue;
      List[i].BSIn   := PrimeList[i].BloodInValue;
      List[i].BSOut  := PrimeList[i].BloodOutValue;
      List[i].Time   := (MinPerDay + PrimeList[i].FoodTime) mod MinPerDay;
      List[i].Weight := F((PrimeList[i].Date - Min) / (CurTime - Min)){ * PrimeList[i].Carbs};
    end;

    Log(DEBUG, 'Saved', True);

    SaveAnalyzeList(List, 'temp\denormalized_anlist.txt');

    { нормализация }
    MinW := List[0].Weight;
    MaxW := List[0].Weight;
    for i := 1 to High(List) do
    begin
      MinW := Math.Min(MinW, List[i].Weight);
      MaxW := Math.Max(MaxW, List[i].Weight);
    end;

    if abs(MinW - MaxW) > 0.0001 then
      for i := 0 to High(List) do
        List[i].Weight := (List[i].Weight - MinW) / (MaxW - MinW)
    else
      for i := 0 to High(List) do
        List[i].Weight := 1.0;
  end;
end;

{==============================================================================}
function AddAnalyzer(const FileName: string): boolean;
{==============================================================================}
var
  Lib: HModule;
  Temp: TAnalyzer;
  Index: integer;
  AnalFuncName: string;
  InfoFuncName: string;
begin
  if FileExists(FileName) then
  try
    Lib := LoadLibrary(PChar(FileName));
    if (Lib <> 0) then
    begin
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

          SetLength(Analyzer, Length(Analyzer) + 1);
          Analyzer[High(Analyzer)] := Temp;
        end;

        inc(Index);
      until not Result;
    end else
      Result := False;
  except
    Result := False;
  end else
    Result := False
end;

{==============================================================================}
procedure AnalyzeDiary(Base: TDiaryDAO; FromDate, ToDate: TDateTime; const Par: TRealArray; CallBack: TCallbackProgressProc);
{==============================================================================}
var
  PrimeList: TPrimeRecList;
  StartTime: cardinal;
  SummWeight: Real;
  i, Time: integer;
begin
  if (Length(Analyzer) = 0) then
    raise Exception.Create('No analyzers loaded');

  // подготовка материала
  ExtractRecords(Base, FromDate, ToDate, PrimeList);
  FormatRecords(PrimeList, AnList, Par[PAR_ADAPTATION]);

  // собственно анализ
  for i := 0 to High(Analyzer) do
  begin
    StartTime := GetTickCount;
    // TODO: incapsulate as method "TAnalyzer.Analyze(AnList)"
    Analyzer[i].AnalyzeFunc(AnList, Analyzer[i].KoofList, CallBack);
    Analyzer[i].Error := GetRecListError(AnList, Analyzer[i].KoofList, vfQuadric);
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

  AvgAnalyzer.Error := GetRecListError(AnList, AvgAnalyzer.KoofList, vfQuadric);
  AvgAnalyzer.Weight := 1.0;
end;

{==============================================================================}
function GetRecError(const Rec: TAnalyzeRec; const KoofList: TKoofList; ValFunct: TValFunction): real;
{==============================================================================}
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

{==============================================================================}
function GetRecListError(const List: TAnalyzeRecList; const KoofList: TKoofList; ValFunc: TValFunction): real;
{==============================================================================}
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

{==============================================================================}
function GetKoofRandomation(const KoofList: TKoofList): real;
{==============================================================================}

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

{==============================================================================}
procedure AnalyzeBS(Base: TDiaryDAO; TimeFrom, TimeTo: TDateTime; out Mean, StdDev, Targeted, Less, More: Extended);
{==============================================================================}
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

{==============================================================================}
procedure SaveAnalyzeList(const List: TAnalyzeRecList; const FileName: string);
{==============================================================================}
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

    for i := 0 to High(AnList) do
    begin
      S.Add(Format('%d'#9'%f'#9'%f'#9'%f'#9'%f'#9'%f'#9'%f', [
        AnList[i].Time,
        AnList[i].Weight,
        AnList[i].Prots,
        AnList[i].Fats,
        AnList[i].Carbs,
        AnList[i].Ins,
        AnList[i].BSOut - AnList[i].BSIn]));
    end;
    S.SaveToFile(FileName);
  finally
    s.Free;
  end;
end;

{==============================================================================}
function GetAnalyzersCount: integer;
{==============================================================================}
begin
  Result := Length(Analyzer);
end;

{==============================================================================}
function GetAnalyzer(Index: integer): TAnalyzer;
{==============================================================================}
begin
  Result := Analyzer[Index]
end;

{==============================================================================}
function GetKoof(Time: integer): TKoof;
{==============================================================================}
begin
  Result := AvgAnalyzer.KoofList[Time];
end;

{==============================================================================}
function GetKoof(Time: integer; AnalyzerIndex: integer): TKoof;
{==============================================================================}
begin
  Result := GetAnalyzer(AnalyzerIndex).KoofList[Time];
end;

end.


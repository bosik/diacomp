unit AnalyzeInterface;

interface

uses
  DiaryDatabase,
  DiaryRecords,
  SysUtils, Windows, Classes, Math, DiaryRoutines;

{

  ***** ПРОЦЕСС АНАЛИЗА *****
  1) выделение из дневника первичных записей (TPrimeRec)
  2) небольшое форматирование в тип TAnalyzeRec
  3) отправление их в модуль анализа и получение поминутного списка коэффициентов

}

const
  MinPerHour    = 60;       { *** }
  MinPerDay     = 24 * MinPerHour;
  HalfMinPerDay = MinPerDay div 2;

type
  { ===== ВХОДНЫЕ ДАННЫЕ ===== }
  TPrimeRec = record
    Date: TDate; // для взвешивания

    BloodInTime: integer;  // в минутах, возможно больше 1440
    BloodInValue: real;

    InsTime: integer;
    InsValue: real;

    FoodTime: integer;
    Prots,Fats,Carbs: real;

    BloodOutTime: integer;
    BloodOutValue: real;
  end;

  TPrimeRecList = array of TPrimeRec;

  TAnalyzeRec = record
    Prots,Fats,Carbs: real;
    Ins: real;
    BSIn, BSOut: real;
    Time: integer; // время привязки (в минутах)
    Weight: real; // для взвешивания
  end;  

  TAnalyzeRecList = array of TAnalyzeRec;

  { ===== ВЫХОДНЫЕ ДАННЫЕ ===== }
  TKoof = record
    k,q,p: real;
  end;
  TKoofList = array[0..MinPerDay - 1] of TKoof;

  { ===== ИМПОРТИРУЕМАЯ ФУНКЦИЯ ===== }

  TCallbackProgressProc = procedure(Progress: integer);

  TInfoFunction = function(): PChar;

  TAnalyzeFunction = function(
    const RecList: TAnalyzeRecList;
    var KoofList: TKoofList;
    CallBack: TCallbackProgressProc
  ): boolean; StdCall;

  TValFunction = (vfLinearAbs, vfLinearAvg, vfDistance, vfQuadric);
  TRealArray = array of Real;

{==============================================================================}

  procedure ExtractRecords(Base: TDiary; DaysCount: integer;
    out List: TPrimeRecList);

  // Adaptation in [0..0.5]:
  // 0.0 is the quickest (but unstable),
  // 0.5 is the slowest (but very stable)
  procedure FormatRecords(const PrimeList: TPrimeRecList; out List: TAnalyzeRecList;
    const Adaptation: real);

  { загрузка функции анализа }
  function LoadLib(const FileName: string;
    var AnFunc: TAnalyzeFunction;
    var InfoFunc: TInfoFunction
    ): boolean;

  { проведение анализа }
  function AnalyzeDiary(
    Base: TDiary;         // материал
    AnFunc: TAnalyzeFunction;     // ф-ция анализа
    DaysProcess: integer;   // параметры местные
    const Par: TRealArray;    // ...
    out KoofList: TKoofList;
    out AnList: TAnalyzeRecList;
    CallBack: TCallbackProgressProc
  ): boolean;

  { оценка }
  function GetRecError(const Rec: TAnalyzeRec;
    const KoofList: TKoofList;
    ValFunct: TValFunction): real;
  function GetRecListError(const List: TAnalyzeRecList;
    const KoofList: TKoofList; ValFunc: TValFunction): real;
  function GetKoofRandomation(const KoofList: TKoofList): real;
  procedure SaveRecords(const List: TAnalyzeRecList; const FileName: string);

const
  MinFinishTime = 7 * MinPerHour div 2; // 3.5 часа
  AnalyzeFunctionName = 'Analyze';
  InfoFunctionName    = 'Info';

  PAR_ADAPTATION  = 0;
  //PAR_COMPRESSION = 1;
implementation

  { ПОДГОТОВКА МАТЕРИАЛА }

{==============================================================================}
procedure ExtractRecords(Base: TDiary; DaysCount: integer;
  out List: TPrimeRecList);
{==============================================================================}
var
  i,j: integer;
  Date: TDate;
  PageBaseTime: integer;

  PrevBloodTime: integer;
  PrevBloodValue: real;

  Prots,Fats,Carbs,CurCarbs,MaxCarbs: real;
  Ins,CurIns,MaxIns: real;
  TimeF,TimeI: integer;
  TimeShift: integer;
  MealDate: TDate;

  FromDate, ToDate: TDate;

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

begin
  SetLength(List, 0);

  PrevBloodTime := -1;
  PrevBloodValue := -1;
  MealDate := 0;
  InitCounters;

  { обработка }

  { 1. Создаём RecList, считая время в минутах от 01/01/1899 }

  ToDate := Trunc(Now);
  FromDate := ToDate - DaysCount + 1;
  Base.PrepareCache(FromDate, ToDate);

  for Date := FromDate to ToDate do
  begin
    PageBaseTime := Date * MinPerDay;
    {=====================================================}
    for i := 0 to Base[Date].Count-1 do
    if (Base[Date][i].RecType = TInsRecord) then
    begin
      CurIns := TInsRecord(Base[Date][i]).Value;
      Ins := Ins + CurIns;
      if CurIns > MaxIns then
      begin
        MaxIns := CurIns;
        TimeI := PageBaseTime + Base[Date][i].Time;
      end;
    end else
    if (Base[Date][i].RecType = TMealRecord) then
    begin
      Prots := Prots + TMealRecord(Base[Date][i]).Prots;
      Fats := Fats + TMealRecord(Base[Date][i]).Fats;
        CurCarbs := TMealRecord(Base[Date][i]).Carbs;
      Carbs := Carbs + CurCarbs;
      if CurCarbs > MaxCarbs then
      begin
        MaxCarbs := CurCarbs;
        TimeF := PageBaseTime + Base[Date][i].Time;
        MealDate := Date;
      end;
    end else

    if (Base[Date][i].RecType = TBloodRecord) and
       (not TBloodRecord(Base[Date][i]).PostPrand) then
    begin
      if PrevBloodValue = -1 then
      begin
        PrevBloodTime := PageBaseTime + TBloodRecord(Base[Date][i]).Time;
        PrevBloodValue := TBloodRecord(Base[Date][i]).Value;
        InitCounters;
      end else
      if ((Carbs > 0) or (Prots > 0)) and (Ins > 0) then
      //if (True) then
      begin
        { запись }

        {if (((TimeF+1440) mod 1440) > 780)and
           (((TimeF+1440) mod 1440) < 840)
        then  }
        begin
          j := length(List);
          SetLength(List,j+1);
          List[j].BloodInTime := PrevBloodTime;
          List[j].BloodInValue := PrevBloodValue;
          List[j].InsTime := TimeI;  { абсолютное время }
          List[j].InsValue := Ins;
          List[j].FoodTime := TimeF; { абсолютное время }
          List[j].Prots := Prots;
          List[j].Fats := Fats;
          List[j].Carbs := Carbs;
          List[j].BloodOutTime := PageBaseTime + Base[Date][i].Time;
          List[j].BloodOutValue := TBloodRecord(Base[Date][i]).Value;
          List[j].Date := MealDate;
        end;

        { подготовка к следующему циклу }
        PrevBloodTime := PageBaseTime + Base[Date][i].Time;
        PrevBloodValue := TBloodRecord(Base[Date][i]).Value;
        InitCounters;
      end else
      begin
        { замер нормальный, но перед ним ни еды, ни инсулина }
        PrevBloodTime := PageBaseTime + TBloodRecord(Base[Date][i]).Time;
        PrevBloodValue := TBloodRecord(Base[Date][i]).Value;
        InitCounters;
      end;
    end;
    {=====================================================}
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
    if List[i].InsValue > 0 then
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
    if List[i].InsTime >- 1 then
      List[i].InsTime := List[i].InsTime - TimeShift;
    if List[i].FoodTime >- 1 then
      List[i].FoodTime := List[i].FoodTime - TimeShift;
  end;
end;



{==============================================================================}
procedure FormatRecords(const PrimeList: TPrimeRecList; out List: TAnalyzeRecList;
  const Adaptation: real);
{==============================================================================}
{ Копирует основные поля и вычисляет нормальные веса }

  function F(const X: real): real;
  { Adaptation in [0, 0.5] }
  { X from [0, 1] }
  begin
    result := (Adaptation-0.5)*sin(pi*(X-0.5))+0.5;
  end;

var
  i: integer;
  CurTime: TDateTime;
  Min: TDate;
  MinW, MaxW: real;
begin
  CurTime := now;
  Min := Trunc(CurTime);

  for i := 0 to high(PrimeList) do
  if PrimeList[i].Date < Min then
    Min := PrimeList[i].Date;

  Min := Min - 1; // muahahahaha  

  SetLength(List, Length(PrimeList));
  for i := 0 to high(List) do
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

  { нормализация }
  if Length(List) > 0 then
  begin
    MinW := List[0].Weight;
    MaxW := List[0].Weight;
    for i := 1 to high(List) do
    begin
      MinW := Math.Min(MinW, List[i].Weight);
      MaxW := Math.Max(MaxW, List[i].Weight);
    end;

    if abs(MinW - MaxW) > 0.0001 then
      for i := 0 to high(List) do
        List[i].Weight := (List[i].Weight - MinW) / (MaxW - MinW)
    else
      for i := 0 to high(List) do
        List[i].Weight := 1.0;
  end;
end;

(*
{==============================================================================}
procedure CalcZones(var List: TRecordList; ZonesCount: integer);
{==============================================================================}
var
  i: integer;
  TimeBand: integer;

  procedure GetRecZone(var R: TRec);
  { mod - на всякий случай ;) }
  begin
    if R.FoodTime>-1 then
    begin
      R.Zone := (R.FoodTime div TimeBand) mod ZonesCount;
      R.Pos := (R.FoodTime-R.Zone*TimeBand)/TimeBand;
    end else
    begin
      R.Zone := (R.InsTime div TimeBand) mod ZonesCount;
      R.Pos := (R.InsTime-R.Zone*TimeBand)/TimeBand;
    end;
  end;

begin
  if (ZonesCount>0)and(ZonesCount<=MinPerDay) then
  begin
    TimeBand := MinPerDay div ZonesCount;
    for i := 0 to high(List) do
      GetRecZone(List[i]);
  end;
end;

{==============================================================================}
function CreateZoneList(var List: TRecordList; ZonesCount,
  RecsPerZone: integer; CheckIns,CheckCarbs: boolean): TZoneList;
{==============================================================================}
var
  i: integer;
begin
  if (ZonesCount>0)and(ZonesCount<=MinPerDay)and
     (RecsPerZone>0) then
  begin
    CalcZones(List,ZonesCount);
    SetLength(result,ZonesCount);
    for i := high(List) downto 0 do
    begin
      if (length(result[List[i].Zone])<RecsPerZone) and
         ((not CheckIns) or (CheckIns and (List[i].InsValue>0)))and
         ((not CheckCarbs) or (CheckCarbs and (List[i].Carbs>0)))then
      begin
        SetLength(result[List[i].Zone],length(result[List[i].Zone])+1);
        result[List[i].Zone][high(result[List[i].Zone])] := List[i];
      end;
    end;
  end;
end;
*)

  { ЗАГРУЗКА БИБЛИОТЕКИ }
{==============================================================================}
function LoadLib(const FileName: string;
  var AnFunc: TAnalyzeFunction;
  var InfoFunc: TInfoFunction): boolean;
{==============================================================================}
var
  Lib: HModule;
begin
  if not FileExists(FileName) then
    Result := False else
  try
    Lib := LoadLibrary(PChar(FileName));
    if Lib = 0 then
      Result := False else
    begin
      @AnFunc := GetProcAddress(Lib, AnalyzeFunctionName);
      @InfoFunc := GetProcAddress(Lib, InfoFunctionName);
      Result :=
        (@AnFunc <> nil)and
        (@InfoFunc <> nil);
    end;
  except
    Result := False;
  end;
end;

{==============================================================================}
function AnalyzeDiary(
  Base: TDiary;       // материал
  AnFunc: TAnalyzeFunction;   // ф-ция анализа
    DaysProcess: integer;   // параметры местные
  //const Lim: TLimits;     // параметры для DLL
  const Par: TRealArray;
  out KoofList: TKoofList;
  out AnList: TAnalyzeRecList;
  CallBack: TCallbackProgressProc
): boolean;
{==============================================================================}
var
  PrimeList: TPrimeRecList;
begin
  if (@AnFunc <> nil) then
  begin
    ExtractRecords(Base, DaysProcess, PrimeList);
    FormatRecords(PrimeList, AnList, Par[PAR_ADAPTATION]);
    Result := AnFunc(AnList, KoofList, CallBack);
  end else
    Result := False;
end;

(*
{==============================================================================}
procedure CompleteKoofList(var KoofList: TKoofList);
{==============================================================================}
var
  Ni,i,Nj,j: integer;
  BasePot,BasePotN: integer;
  DeltaK,DeltaQ,DeltaP: real;
begin
  BasePot := -1;
  for i := 0 to high(KoofList) do
  if KoofList[i].Proved then
  begin
    BasePot := i;
    break;
  end;

  if BasePot = -1 then exit;

  for i := BasePot+1 to high(KoofList)+BasePot+1 do
  begin
    Ni := i mod length(KoofList);

    if KoofList[Ni].Proved then
    begin
      if not KoofList[(i-1) mod length(KoofList)].Proved then
      begin
        BasePotN := BasePot mod length(KoofList);

        if Ni<>BasePotN then
        begin
          DeltaK := (KoofList[Ni].K-KoofList[BasePotN].K)/(i-BasePot+KoofList[Ni].Pos-KoofList[BasePotN].Pos);
          DeltaQ := (KoofList[Ni].Q-KoofList[BasePotN].Q)/(i-BasePot+KoofList[Ni].Pos-KoofList[BasePotN].Pos);
          DeltaP := (KoofList[Ni].P-KoofList[BasePotN].P)/(i-BasePot+KoofList[Ni].Pos-KoofList[BasePotN].Pos);
        end else
        begin
          DeltaK := 0;
          DeltaQ := 0;
          DeltaP := 0;
        end;
        for j := BasePot+1 to i-1 do
        begin
          Nj := j mod length(KoofList);
          KoofList[Nj].K := KoofList[BasePotN].K + (j-BasePot)*DeltaK;
          KoofList[Nj].Q := KoofList[BasePotN].Q + (j-BasePot)*DeltaQ;
          KoofList[Nj].P := KoofList[BasePotN].P + (j-BasePot)*DeltaP;
          KoofList[Nj].Pos := 0.5;
        end;
      end;
      BasePot := i;
    end;
  end;
end;

{==============================================================================}
function GetKoofs(const K: TKoofList;
  const Time: integer; var R: TKoof): boolean;
{==============================================================================}
var
  TimeBand: integer;
  z1,z2: integer;
  w,pos: real;
begin
  if length(K) = 0 then
    result := false else
  begin
    TimeBand := MinPerDay div length(K);
    z1 := Time div TimeBand;
    if (z1 < 0) or (z1 > high(K)) then
    begin
      R.k := 0;
      R.q := 0;
      R.p := 0;
      result := false;
    end else
    begin
      pos := Time/TimeBand-z1;
      if pos>K[z1].Pos then
      begin
        z2 := z1+1;
        if z2 > high(K) then z2 := 0;
        w := (pos-K[z1].Pos)/(1-K[z1].Pos+K[z2].Pos);
      end else
      begin
        z2 := z1-1;
        if z2 < 0 then z2 := high(K);
        w := (K[z1].Pos-pos)/(1-K[z2].Pos+K[z1].Pos);
      end;
      R.k := (1-w)*K[z1].k + w*K[z2].k;
      R.q := (1-w)*K[z1].q + w*K[z2].q;
      R.p := (1-w)*K[z1].p + w*K[z2].p;
      result := (R.k<>0)or(R.q<>0)or(R.p<>0);
    end;
  end;
end;
*)

{==============================================================================}
function GetRecError(const Rec: TAnalyzeRec; const KoofList: TKoofList;
  ValFunct: TValFunction): real;
{==============================================================================}
var
  err: real;
  Koof: TKoof;
begin
  Koof := KoofList[Rec.Time];
  err :=
    Rec.Carbs * Koof.k +
    Rec.Prots * Koof.p -
    Rec.Ins * Koof.q
    - (Rec.BSOut - Rec.BSIn);

  case ValFunct of
    vfLinearAbs: result := abs(err);
    vfLinearAvg: result := err;
    vfDistance: result := abs(err) / Sqrt(Sqr(Rec.Carbs) + Sqr(Rec.Prots) + Sqr(Rec.Ins));
    vfQuadric: Result := Sqr(err);
    else raise Exception.Create('GetRecError: недопустимый аргумент ValFunct');
  end;
end;

{==============================================================================}
function GetRecListError(const List: TAnalyzeRecList; const KoofList: TKoofList;
  ValFunc: TValFunction): real;
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
    result := Sqrt(
      Sqr(KoofList[n1].k - KoofList[n2].k)+
      Sqr(KoofList[n1].q - KoofList[n2].q)+
      Sqr(KoofList[n1].p - KoofList[n2].p));
  end;

{var
  i: integer; }
begin
  result := 0;
  {for i := 0 to high(KoofList) do
    result := result + Dist(i,(i+1) mod length(KoofList));  }

  { *** }
end;

{==============================================================================}
procedure SaveRecords(const List: TAnalyzeRecList; const FileName: string);
{==============================================================================}
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  try
    for i := 0 to high(List) do
      s.Add(
        TimeToStr{Colon}(List[i].Time, ':')+#9+
        RealToStr(List[i].Carbs)+#9+
        RealToStr(List[i].Prots)+#9+
        RealToStr(List[i].Ins)+#9+
        RealToStr(List[i].BSOut - List[i].BSIn)
      );
    s.SaveToFile(FileName);
  finally
    s.Free;
  end;
end;

end.

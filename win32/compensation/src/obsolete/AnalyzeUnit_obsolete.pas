unit AnalyzeUnit;

interface

uses
  SysUtils,
  Windows,
  Dialogs,
  Classes,
  Graphics,
  DiaryBase,
  DiaryRoutines,
  SettingsINI,
  LineSystem;

type
  TRec = record
    BloodInTime: integer;
    BloodInValue: real;
    InsTime: integer;
    InsValue: real;
    FoodTime: integer;
    Prots,Fats,Carbs: real;
    BloodOutTime: integer;
    BloodOutValue: real;
    DayNumber: integer;
    Zone: integer;
    Pos: real;
  end;

  TTimeRec = record
    Time: integer;
    Value: real;
    Tag: real;
  end;

  TRecordList = array of TRec;

  TTimeLine = array of TTimeRec;
  TZoneList = array of TRecordList;
  TField = array of array of real;

  TKoof = record
    k,q,p: real;
    Pos: real;
    Proved: boolean;
  end;
  TKoofList = array of TKoof;

  TParamProp = record
    Min,Max: real;
  end;
  TParameters = array of TParamProp;

  TAnalyzeProc = procedure(const List: TRecordList;
    Par: TParameters; const Disc: real; var Koof: TKoof);

  {
    Давай разберемся, какие алгоритмы использованы
    в этом модуле. А то тут бред какой-то :(
  }

  // Итак, все начинается с извлечения записей
  function ExtractRecords(Base: TDiaryBase; Count: integer): TRecordList;
  function CreateZoneList(const List: TRecordList; ZonesCount,
    RecsPerZone: integer): TZoneList;


  { --------- ОБЩИЕ ПРОЦЕДУРЫ --------- }

  { Анализ дня }

  {======================================================}
  function AnalyzeZones(const ZoneList: TZoneList;
    Par: TParameters; const Disc: real; AnalyzeProc: TAnalyzeProc): TKoofList;
  {======================================================}
  function AnalyzeZonesAvg(const ZoneList: TZoneList;
    Par: TParameters; const Disc: real; AnalyzeProc: TAnalyzeProc): TKoofList;
  {======================================================}

  { Компрессоры }
  function Compress(const Koofs: TKoofList; Compression: real): TKoofList; overload;
  function CompressQuad(const Koofs: TKoofList; Compression: real): TKoofList;
  function CompressNeib(const Koofs: TKoofList; Compression: real): TKoofList;

  { Оценка }
  function GetAvgPos(const List: TRecordList): real;
  function GetSummDeviation(const List: TRecordList; const K,Q,P: real): real;
  function GetDeviation(const List: TRecordList; const K,Q,P: real): real;
  function GetZoneDeviation(const ZoneList: TZoneList;
    const Koofs: TKoofList): real;

  { --------- КОНКРЕТНЫЕ РЕАЛИЗАЦИИ --------- }

  { Вариант 1. Подбор (N-1) из N }

  procedure GetBest(const List: TRecordList; Par: TParameters; const Disc: real; var Koof: TKoof);
  function GetDay(const ZoneList: TZoneList; MinQ,MaxQ,DiscQ: real;
    MinP,MaxP,DiscP: real; AveragePriority: real): TKoofList;

  { Вариант 2. Подбор N из N }

  procedure Brute(const List: TRecordList;
    Par: TParameters; const Disc: real; var Koof: TKoof);

  { Вариант 3. Подбор троичным поиском }
 { procedure Trinity(const List: TRecordList;
    Par: TParameters; const Disc: real; var Koof: TKoof);
  function DirectAvgTrinity(const ZoneList: TZoneList;
    Par: TParameters; const Disc: real): TKoofList;    }

  { далее используем полученные данные }
{  procedure CompleteSimpK;
  procedure CompleteKoofList(var KoofList: TKoofList); }

  // рисуем график
  procedure DrawSimpK(Canvas: TCanvas; InRect: TRect);

  // извлекаем коэффициенты и дозы
  function SearchInjectedDose(const Page: TDiaryPage;
    Time: integer): integer;
  procedure ImproveMasses(var D: TMassedFoodList; DProts,DFats,DCarbs: real);

const
  MinPerDay     = 24*60;
  MinFinishTime = 120;
var
  Recs: TRecordList;
  Prepared: boolean;
  DataK: TTimeLine;
  SimpK: array of real;   

  TrinityList: ^TRecordList;
implementation

{=========================================================}
function ExtractRecords(Base: TDiaryBase; Count: integer): TRecordList;
{=========================================================}
var
  i,j,n: integer;
  PrevBlood: TBloodRecord;
  Prots,Fats,Carbs,CurCarbs,MaxCarbs: real;
  Ins,CurIns,MaxIns: real;
  TimeF,TimeI: integer;
  LastEventTime: integer;
  TimeShift: integer;

  procedure InitCounters;
  begin
    Ins:=0;
    MaxIns:=-1;
    Prots:=0;
    Fats:=0;
    Carbs:=0;
    MaxCarbs:=-1;
    TimeF:=-1;
    TimeI:=-1;
  end;

begin
  SetLength(result,0);

  PrevBlood:=nil;
  LastEventTime:=-1;
  InitCounters;

  { обработка }

  { сделаем в два шага }

  { 1. Образовываем RecList, считая время от начала ведения дневника }

  if Count>Base.Count then
    Count:=Base.Count;

  for n:=Base.Count-Count to Base.Count-1 do
  for i:=0 to Base[n].Count-1 do

  if Base[n][i].TagType=rtIns then
  begin
    CurIns:=TInsRecord(Base[n][i]).Value;
    Ins:=Ins+CurIns;
    if CurIns>MaxIns then
    begin
      MaxIns:=CurIns;
      TimeI:=n*MinPerDay + Base[n][i].Time;
    end;
    LastEventTime:=n*MinPerDay + Base[n][i].Time;
  end else
  if Base[n][i].TagType=rtMeal then
  begin
    Prots:=Prots+TMealRecord(Base[n][i]).Prots;
    Fats:=Fats+TMealRecord(Base[n][i]).Fats;
    CurCarbs:=TMealRecord(Base[n][i]).Carbs;
    Carbs:=Carbs+CurCarbs;
    if CurCarbs>MaxCarbs then
    begin
      MaxCarbs:=CurCarbs;
      TimeF:=n*MinPerDay + Base[n][i].Time;
    end;
    LastEventTime:=TimeF;
  end else

  if PrevBlood = nil then
  begin
    PrevBlood:=TBloodRecord.Create;
    PrevBlood.Time:=n*MinPerDay+TBloodRecord(Base[n][i]).Time;
    PrevBlood.Value:=TBloodRecord(Base[n][i]).Value;
    InitCounters;
  end else

  if (n*MinPerDay+Base[n][i].Time-LastEventTime>=MinFinishTime) then
  if (Carbs>0)or(Prots>0)or(Ins>0) then
  begin
    { запись }
    j:=length(result);
    SetLength(result,j+1);
    result[j].BloodInTime:=PrevBlood.Time;
    result[j].BloodInValue:=PrevBlood.Value;
    result[j].InsTime:=TimeI;  { уже абсолютное время }
    result[j].InsValue:=Ins;
    result[j].FoodTime:=TimeF; { уже абсолютное время }
    result[j].Prots:=Prots;
    result[j].Fats:=Fats;
    result[j].Carbs:=Carbs;
    result[j].BloodOutTime:=n*MinPerDay+Base[n][i].Time;
    result[j].BloodOutValue:=TBloodRecord(Base[n][i]).Value;
    result[j].DayNumber:=n;

    { подготовка к следующему циклу }
    PrevBlood.Time:=n*MinPerDay+TBloodRecord(Base[n][i]).Time;
    PrevBlood.Value:=TBloodRecord(Base[n][i]).Value;
    InitCounters;
  end else
  begin
    { т.е. замер нормальный, но перед ним ни еды, ни инсулина:= }
    PrevBlood.Time:=n*MinPerDay+TBloodRecord(Base[n][i]).Time;
    PrevBlood.Value:=TBloodRecord(Base[n][i]).Value;
    InitCounters;
  end;

  if PrevBlood<>nil then
    PrevBlood.Free;

  { 2. Восстановление относительных времён }

  for i:=0 to high(result) do
  begin
    if result[i].FoodTime>-1 then
    begin
      TimeShift:=result[i].FoodTime mod MinPerDay;
      TimeShift:=result[i].FoodTime-TimeShift;
    end else
    if result[i].InsTime>-1 then
    begin
      TimeShift:=result[i].InsTime mod MinPerDay;
      TimeShift:=result[i].InsTime-TimeShift;
    end else
      ShowMessage(
        'AnalyzeUnit/ExtractRecords/result[i]: '+
        'FoodTime = -1, InsTime=-1');

    result[i].BloodInTime:=result[i].BloodInTime-TimeShift;
    if result[i].InsTime>-1 then
      result[i].InsTime:=result[i].InsTime-TimeShift;
    if result[i].FoodTime>-1 then
      result[i].FoodTime:=result[i].FoodTime-TimeShift;
    result[i].BloodOutTime:=result[i].BloodOutTime-TimeShift;
  end;
end;

{=========================================================}
function CreateZoneList(const List: TRecordList; ZonesCount,
  RecsPerZone: integer): TZoneList;
{=========================================================}
var
  i: integer;
  TimeBand: integer;

  procedure GetRecZone(var R: TRec);
  { mod - на всякий случай ;) }
  begin
    if R.FoodTime>-1 then
    begin
      R.Zone:=(R.FoodTime div TimeBand) mod ZonesCount;
      R.Pos:=(R.FoodTime-R.Zone*TimeBand)/TimeBand;
    end else
    //if R.InsTime>-1 then
    begin
      R.Zone:=(R.InsTime div TimeBand) mod ZonesCount;
      R.Pos:=(R.InsTime-R.Zone*TimeBand)/TimeBand;
    end;
  end;

begin
  if (ZonesCount>0)and(ZonesCount<=MinPerDay)and
     (RecsPerZone>0) then
  begin
    TimeBand:=MinPerDay div ZonesCount;
    SetLength(result,ZonesCount);
    for i:=high(List) downto 0 do
    begin
      GetRecZone(List[i]);

      if length(result[List[i].Zone])<RecsPerZone then
      //if (List[i].Carbs>0)and(List[i].InsValue>0) then
      begin
        SetLength(result[List[i].Zone],length(result[List[i].Zone])+1);
        result[List[i].Zone][high(result[List[i].Zone])]:=List[i];
      end;
    end;
  end;
end;

{=========================================================}
function AnalyzeZones(const ZoneList: TZoneList;
  Par: TParameters; const Disc: real; AnalyzeProc: TAnalyzeProc): TKoofList;
{=========================================================}
var
  i: integer;
begin
  SetLength(result,length(ZoneList));
  for i:=0 to high(ZoneList) do
  if length(ZoneList[i])>0 then
    AnalyzeProc(ZoneList[i],Par,Disc,
    result[i])
  else
  begin
    result[i].k:=0;
    result[i].q:=0;
    result[i].p:=0;
    result[i].Pos:=0.5;
  end;
end;

{=========================================================}
function AnalyzeZonesAvg(const ZoneList: TZoneList;
  Par: TParameters; const Disc: real; AnalyzeProc: TAnalyzeProc): TKoofList;
{=========================================================}
var
  List: TRecordList;
  i,j: integer;
  Temp: TKoof;
begin
  SetLength(List,1);
  SetLength(result,length(ZoneList));

  for i:=0 to high(ZoneList) do
  if length(ZoneList[i])>0 then
  begin
    result[i].k:=0;
    result[i].q:=0;
    result[i].p:=0;
    for j:=0 to high(ZoneList[i]) do
    begin
      List[0]:=ZoneList[i][j];
      AnalyzeProc(List,Par,Disc,Temp);
      result[i].k:=result[i].k+Temp.K;
      result[i].q:=result[i].q+Temp.Q;
      result[i].p:=result[i].p+Temp.P;
    end;
    result[i].k:=result[i].k/length(ZoneList[i]);
    result[i].q:=result[i].q/length(ZoneList[i]);
    result[i].p:=result[i].p/length(ZoneList[i]);
  end;
end;

{=========================================================}
function Compress(const Koofs: TKoofList; Compression: real): TKoofList;
{=========================================================}
var
  i: integer;
  AvK,AvQ,AvP: real;
  Count: integer;
begin
  result:=Koofs;
  if length(Koofs) = 0 then Exit;

  { 1. Находим средние значения }
  AvK:=0;
  AvQ:=0;
  AvP:=0;
  Count:=0;

  for i:=0 to high(Koofs) do
  if Koofs[i].k>0 then
  begin
    AvK:=AvK + Koofs[i].k;
    AvQ:=AvQ + Koofs[i].q;
    AvP:=AvP + Koofs[i].p;
    inc(Count);
  end;

  if Count=0 then exit;

  AvK:=AvK / Count;
  AvQ:=AvQ / Count;
  AvP:=AvP / Count;

  { 2. Пересчитываем коэффициенты в соответствии с Compression }
  SetLength(result,length(Koofs));
  for i:=0 to high(result) do
  if Koofs[i].k>0 then
  begin
    result[i].k:=AvK*Compression + Koofs[i].k*(1-Compression);
    result[i].q:=AvQ*Compression + Koofs[i].q*(1-Compression);
    result[i].p:=AvP*Compression + Koofs[i].p*(1-Compression);
  end;
end;

{=========================================================}
function CompressQuad(const Koofs: TKoofList; Compression: real): TKoofList;
{=========================================================}

  function F(const X: real): real;
  var
    R,s1,s2: real;
  begin
    if Compression<>0 then
      R:=x/Compression/2
    else
      R:=0;

    s1:=( (1-R)*x + (R)*Compression*x );

    if Compression<>1 then
      R:=0.5+(x-Compression)/(1-Compression)/2
    else
      R:=1;
    s2:=(R)*Compression + (1-R)*Compression*x;

    if x<Compression then
      result:=s1
    else
      result:=s2;
  end;

var
  i: integer;
  AvK,AvQ,AvP: real;
  MdK,MdQ,MdP: real;
  CdK,CdQ,CdP: real;
  Cmp: real;
  Count: integer;
begin
  result:=Koofs;
  if length(Koofs) = 0 then Exit;

  { 1. Находим средние значения }
  AvK:=0;
  AvQ:=0;
  AvP:=0;
  Count:=0;

  for i:=0 to high(Koofs) do
  if Koofs[i].k>0 then
  begin
    AvK:=AvK + Koofs[i].k;
    AvQ:=AvQ + Koofs[i].q;
    AvP:=AvP + Koofs[i].p;
    inc(Count);
  end;

  if Count=0 then exit;

  AvK:=AvK / Count;
  AvQ:=AvQ / Count;
  AvP:=AvP / Count;

  { 2. Находим максимальные отклонения }
  MdK:=0;
  MdQ:=0;
  MdP:=0;
  for i:=0 to high(Koofs) do
  if Koofs[i].k>0 then
  begin
    CdK:=abs(AvK-Koofs[i].k);
    CdQ:=abs(AvQ-Koofs[i].q);
    CdP:=abs(AvP-Koofs[i].p);
    if CdK > MdK then MdK:=CdK;
    if CdQ > MdQ then MdQ:=CdQ;
    if CdP > MdP then MdP:=CdP;
  end;

  { 3. Пересчитываем коэффициенты }
  SetLength(result,length(Koofs));
  for i:=0 to high(result) do
  if Koofs[i].k>0 then
  begin
    if MdK>0 then
    begin
      Cmp:=F(abs(result[i].k-AvK)/MdK);
      result[i].k:=AvK*Cmp + Koofs[i].k*(1-Cmp);
    end;
    if MdQ>0 then
    begin
      Cmp:=F(abs(result[i].q-AvQ)/MdQ);
      result[i].q:=AvQ*Cmp + Koofs[i].q*(1-Cmp);
    end;
    if MdP>0 then
    begin
      Cmp:=F(abs(result[i].p-AvP)/MdP);
      result[i].p:=AvP*Cmp + Koofs[i].p*(1-Cmp);
    end;
  end;
end;

{=========================================================}
function CompressNeib(const Koofs: TKoofList; Compression: real): TKoofList;
{=========================================================}
var
  i: integer;
begin
  result:=Koofs;
  if length(Koofs) = 0 then Exit;

  for i:=1 to high(result)-1 do
  if result[i].k>0 then
  begin
    if (Koofs[i-1].k>0)and(Koofs[i+1].k>0) then
      result[i].k:=Compression*(Koofs[i-1].k+Koofs[i+1].k)/2 + (1-Compression)*result[i].k else
    if (Koofs[i-1].k>0)and(Koofs[i+1].k=0) then
      result[i].k:=Compression*(Koofs[i-1].k) + (1-Compression)*result[i].k else
    if (Koofs[i-1].k=0)and(Koofs[i+1].k>0) then
      result[i].k:=Compression*(Koofs[i+1].k) + (1-Compression)*result[i].k;

    result[i].q:=Compression*(Koofs[i-1].q+Koofs[i+1].q)/2 + (1-Compression)*result[i].q;
    result[i].p:=Compression*(Koofs[i-1].p+Koofs[i+1].p)/2 + (1-Compression)*result[i].p;
  end;
end;

{=========================================================}
function GetAvgPos(const List: TRecordList): real;
{=========================================================}
var
  i: integer;
begin
  if length(List) = 0 then
    result:=0.5 else
  begin
    result:=0;
    for i:=0 to high(List) do
      result:=result+List[i].Pos;
    result:=result/length(List);
  end;
end;

{=========================================================}
function GetSummDeviation(const List: TRecordList; const K,Q,P: real): real;
{=========================================================}
var
  i: integer;
begin
  result:=0;
  for i:=0 to high(list) do
    result:=result+abs(
    (List[i].Carbs*K
    +List[i].Prots*P
    -List[i].InsValue*Q
    +List[i].BloodInValue-List[i].BloodOutValue){/
    ( Sqrt(Sqr(List[i].Carbs)+Sqr(List[i].Prots)+Sqr(List[i].InsValue)) )}
    );
end;

{=========================================================}
function GetDeviation(const List: TRecordList; const K,Q,P: real): real;
{=========================================================}
begin
  if length(list)>0 then
    result:=GetSummDeviation(List,K,Q,P)/length(List)
  else
    result:=0;
end;

{=========================================================}
function GetZoneDeviation(const ZoneList: TZoneList;
  const Koofs: TKoofList): real;
{=========================================================}
var
  i,count: integer;
begin
  result:=0;
  if length(ZoneList)>0 then
  begin
    count:=0;
    for i:=0 to high(ZoneList) do
    if length(ZoneList[i])>0 then
    begin
      result:=result + GetSummDeviation(
        ZoneList[i],Koofs[i].k,koofs[i].q,koofs[i].p);
      count:=count+length(ZoneList[i]);
    end;
    if count>0 then result:=Result/count;
  end;
end;

{=========================================================}
procedure GetBest(const List: TRecordList;
  Par: TParameters; const Disc: real; var Koof: TKoof);
{=========================================================}
var
  i,Qj,Pj: integer;
  QCount,PCount: integer;
  Vals: array of real;
  Delta, BestDelta: real;
  CurQ,CurP: real;
  Average: real;
begin
  if length(List)=0 then exit;
  QCount:=Round((par[0].Max-par[0].Min)/Disc)+1;
  PCount:=Round((par[1].Max-par[1].Min)/Disc)+1;
  if QCount<=0 then exit;
  if PCount<=0 then exit;

  SetLength(vals,length(list));
  BestDelta:=-1;

  for Qj:=0 to QCount-1 do
  begin
    CurQ:=Qj*Disc+par[0].Min;
    if CurQ=0 then continue;

    for Pj:=0 to PCount-1 do
    begin
      CurP:=Pj*Disc+par[1].Min;
      if CurP=0 then continue;

      { 1. Calc Average }
      Average:=0;
      for i:=0 to high(list) do
      if List[i].Carbs>0 then
      begin
        Vals[i]:=(List[i].BloodOutValue-List[i].BloodInValue+
          CurQ*List[i].InsValue - CurP*List[i].Prots)/List[i].Carbs;
        Average:=Average+Vals[i];
      end;
      Average:=Average/length(list);

      { 2. Calc Delta }
      Delta:=0;
      for i:=0 to high(list) do
      if List[i].Carbs>0 then
        Delta:=Delta+abs(Average-Vals[i]);

      //Delta:=Delta/CurQ;

      { 3. Compare with BestDelta }
      if (BestDelta=-1)or(Delta<BestDelta) then
      begin
        Koof.Q:=CurQ;
        Koof.P:=CurP;
        Koof.K:=Average;
        BestDelta:=Delta;
      end;
    end;
  end;
end;

{=========================================================}
function GetDay(const ZoneList: TZoneList; MinQ,MaxQ,DiscQ: real;
  MinP,MaxP,DiscP: real; AveragePriority: real): TKoofList;
{=========================================================}
var
  i,j: integer;
  AverageQ: real;
  AverageP: real;
  Count: integer;
begin
  if length(ZoneList) = 0 then Exit;

  { готовим выходной список }
  SetLength(result,length(ZoneList));

  { вычисляем лучшие (минимальные) значения }
  { попутно ищем среднюю ЦЕИ и белка}
  AverageQ:=0;
  AverageP:=0;
  Count:=0;
  for i:=0 to high(ZoneList) do
  if length(ZoneList[i])>0 then
  begin
    {GetBest(
      ZoneList[i],
      MinQ,MaxQ,DiscQ,
      MinP,MaxP,DiscP,
      result[i].k,result[i].q,result[i].p); }
    AverageQ:=AverageQ + result[i].q;
    AverageP:=AverageP + result[i].p;
    inc(Count);
  end;
  AverageQ:=AverageQ / Count;
  AverageP:=AverageP / Count;

  { теперь пересчитываем q и p в соответствии с AveragePriority }
  { и пересчитываем k }

  for i:=0 to high(result) do
  if length(ZoneList[i])>0 then
  begin
    result[i].q:=
      AverageQ*AveragePriority + result[i].q*(1-AveragePriority);
    result[i].p:=
      AverageP*AveragePriority + result[i].p*(1-AveragePriority);

    { "а k вычисляем как среднее по всем записям зоны! :) " }

    { вот здесь и слабость... :( }

    result[i].K:=0;
    for j:=0 to high(ZoneList[i]) do
    if ZoneList[i][j].Carbs>0 then
    begin
      result[i].K:=result[i].K+
        (ZoneList[i][j].BloodOutValue-ZoneList[i][j].BloodInValue
        +result[i].q*ZoneList[i][j].InsValue
        -result[i].p*ZoneList[i][j].Prots)/ZoneList[i][j].Carbs;
    end;
    result[i].K:=result[i].K/length(ZoneList[i]);
  end;
end;

{=========================================================}
procedure Brute(const List: TRecordList; Par: TParameters;
  const Disc: real; var Koof: TKoof);
{=========================================================}
type
  TCalcStruct = record
    Val: real;
    Buf: array of real;
  end;

var
  Kj,Qj,Pj,i: integer;
  CountK,CountQ,CountP: integer;
  BestDelta,CurDelta: real;
  ValK,ValQ,ValP: array of TCalcStruct;
  DBS: array of real;
begin
  if length(List)=0 then
  begin
    Koof.K:=0;
    Koof.Q:=0;
    Koof.P:=0;
    exit;
  end;

  CountK:=Round((par[0].Max-par[0].Min)/Disc)+1;
  CountQ:=Round((par[1].Max-par[1].Min)/Disc)+1;
  CountP:=Round((par[2].Max-par[2].Min)/Disc)+1;
  BestDelta:=1000000000;

  { инициализация }
  SetLength(ValK,CountK);
  for Kj:=0 to CountK-1 do
  begin
    ValK[Kj].Val:=Kj*Disc+par[0].Min;
    SetLength(ValK[Kj].Buf,length(List));
    for i:=0 to high(List) do
      ValK[Kj].Buf[i]:=ValK[Kj].Val*List[i].Carbs;
  end;

  SetLength(ValQ,CountQ);
  for Qj:=0 to CountQ-1 do
  begin
    ValQ[Qj].Val:=Qj*Disc+par[1].Min;
    SetLength(ValQ[Qj].Buf,length(List));
    for i:=0 to high(List) do
      ValQ[Qj].Buf[i]:=ValQ[Qj].Val*List[i].InsValue;
  end;

  SetLength(ValP,CountP);
  for Pj:=0 to CountP-1 do
  begin
    ValP[Pj].Val:=Pj*Disc+par[2].Min;
    SetLength(ValP[Pj].Buf,length(List));
    for i:=0 to high(List) do
      ValP[Pj].Buf[i]:=ValP[Pj].Val*List[i].Prots;
  end;

  SetLength(DBS,length(List));
  for i:=0 to high(List) do
    DBS[i]:=List[i].BloodInValue-List[i].BloodOutValue;

  { перебор }
  for Kj:=0 to CountK-1 do
  for Qj:=0 to CountQ-1 do
  for Pj:=0 to CountP-1 do
  begin
    CurDelta:=0;
    for i:=0 to high(list) do
      CurDelta:=CurDelta+abs(
      DBS[i]  //List[i].BloodInValue-List[i].BloodOutValue
      +ValK[Kj].Buf[i] //+List[i].Carbs*ValK[Kj]
      +ValP[Pj].Buf[i] //+List[i].Prots*ValP[Pj]
      -ValQ[Qj].Buf[i] //-List[i].InsValue*ValQ[Qj]
      );

    if CurDelta < BestDelta then
    begin
      BestDelta:=CurDelta;
      Koof.K:=ValK[Kj].Val;
      Koof.Q:=ValQ[Qj].Val;
      Koof.P:=ValP[Pj].Val;
    end;
  end;
end;

(*
function DevFunction(const Values: array_of_real): real;
begin
  result:=GetSummDeviation(TrinityList^,Values[0],Values[1],Values[2]);
end;


{=========================================================}
procedure Trinity(const List: TRecordList;
  Par: TParameters; const Disc: real; var Koof: TKoof);
{=========================================================}
var
  Limits: TLimitArray;
  X: array_of_real;
begin
  SetLength(Limits,3);
  Limits[0].Min:=Par[0].Min;
  Limits[0].Max:=Par[0].Max;
  Limits[1].Min:=Par[1].Min;
  Limits[1].Max:=Par[1].Max;
  Limits[2].Min:=Par[2].Min;
  Limits[2].Max:=Par[2].Max;

  TrinityList:=@List;

  SolveSystem(
    3,Limits,DevFunction,Disc,X);

  Koof.K:=X[0];
  Koof.Q:=X[1];
  Koof.P:=X[2];
  Koof.Pos:=GetAvgPos(List);
end;

{=========================================================}
function DirectAvgTrinity(const ZoneList: TZoneList;
  Par: TParameters; const Disc: real): TKoofList;
{=========================================================}
var
  List: TRecordList;
  i,j: integer;
  K,Q,P: real;
  Limits: TLimitArray;
  X: array_of_real;
begin
  SetLength(Limits,3);
  Limits[0].Min:=Par[0].Min;
  Limits[0].Max:=Par[0].Max;
  Limits[1].Min:=Par[1].Min;
  Limits[1].Max:=Par[1].Max;
  Limits[2].Min:=Par[2].Min;
  Limits[2].Max:=Par[2].Max;

  SetLength(List,1);
  SetLength(result,length(ZoneList));

  for i:=0 to high(ZoneList) do
  if length(ZoneList[i])>0 then
  begin
    result[i].k:=0;
    result[i].q:=0;
    result[i].p:=0;
    for j:=0 to high(ZoneList[i]) do
    begin
      List[0]:=ZoneList[i][j];

      {=============================================}
      //Trinity(List,Par,K,Q,P);
      TrinityList:=@List;
      SolveSystem(3,Limits,DevFunction,Disc,X);
      {=============================================}

      result[i].k:=result[i].k+X[0];
      result[i].q:=result[i].q+X[1];
      result[i].p:=result[i].p+X[2];
    end;
    result[i].k:=result[i].k/length(ZoneList[i]);
    result[i].q:=result[i].q/length(ZoneList[i]);
    result[i].p:=result[i].p/length(ZoneList[i]);
  end;
end;

{=========================================================}
procedure CompleteSimpK;
{=========================================================}
var
  i,j,c,N: integer;
  PrevZero: boolean;
  Last,Delta: real;
begin
  { init }
  N:=length(SimpK);

  {!!!} if N=0 then exit; {!!!}

  if SimpK[0]=0 then
  begin
    c:=-1;
    for i:=N-1 downto 0 do
    if SimpK[i]<>0 then
    begin
      last:=SimpK[i];
      c:=N-1-i;
      break;
    end;
    if c=-1 then exit;
  end else
    PrevZero:=false;

  { circle }
  for i:=0 to N-1 do
  if SimpK[i]=0 then
  begin
    if not PrevZero then
      c:=1
    else
      inc(c);
    PrevZero:=true;
  end else
  begin
    if PrevZero then
    begin
      Delta:=(SimpK[i]-Last)/(c+1);
      for j:=i-c to i-1 do
        SimpK[(j+N) mod N]:=last+Delta*(j-i+c+1)
    end;
    Last:=SimpK[i];
    PrevZero:=false;
    c:=0;
  end;
end;

{=========================================================}
procedure CompleteKoofList(var KoofList: TKoofList);
{=========================================================}
var
  Ni,i,Nj,j: integer;
  BasePot: integer;
  DeltaK,DeltaQ,DeltaP: real;
begin
  BasePot:=-1;
  for i:=0 to high(KoofList) do
  if KoofList[i].Proved then
  begin
    BasePot:=i;
    break;
  end;

  if BasePot=-1 then exit;

  for Ni:=BasePot+1 to high(KoofList)+BasePot+1 do
  begin
    i:=Ni mod length(KoofList);

    if KoofList[i].Proved then
    if KoofList[i-1].Proved then
      BasePot:=i else
    begin
      DeltaK:=(KoofList[i].K-KoofList[BasePot].K)/(i-BasePot);
      DeltaQ:=(KoofList[i].Q-KoofList[BasePot].Q)/(i-BasePot);
      DeltaP:=(KoofList[i].P-KoofList[BasePot].P)/(i-BasePot);
      for Nj:=BasePot+1 to Ni-1 do
      begin
        j:=Nj mod length(KoofList);
        KoofList[j].K:=KoofList[BasePot].K + j*DeltaK;
        KoofList[j].Q:=KoofList[BasePot].Q + j*DeltaQ;
        KoofList[j].P:=KoofList[BasePot].P + j*DeltaP;
      end;
    end;
  end;
end;
 *)

{=========================================================}
procedure DrawSimpK(Canvas: TCanvas; InRect: TRect);
{=========================================================}
const
  BRD = 30;
var
  w,h: integer;
  kx,ky: real;
  max: real;
  i: integer;
  TimeBand: integer;
  MinTime,MaxTime,Time: integer;
  b: integer;
begin
  if length(SimpK)=0 then exit;
  TimeBand:=MinPerDay div length(SimpK);

  max:=0.15;

  for i:=0 to high(DataK) do
    if DataK[i].Value>max then max:=DataK[i].Value;

  w:=InRect.Right-InRect.Left;
  h:=InRect.Bottom-InRect.Top;
  ky:=(h-2*BRD)/max;
  kx:=(w-2*BRD)/24;

  with Canvas do
  begin
    Brush.Color:=clWhite;
    Font.Color:=clBlack;
    FillRect(Rect(0,0,w,h));

    Pen.Color:=clBlack;
    Pen.Width:=1;

    { оси }
    Pen.Style:=psSolid;
    Pen.Color:=clBlack;
    MoveTo(BRD,BRD);
    LineTo(BRD,h-BRD);
    LineTo(w-BRD,h-BRD);

    { риски Y }
    Pen.Style:=psDot;
    Pen.Color:=clSilver;
    for i:=1 to Round(max*100) do
    begin
      MoveTo(BRD-3,Round(h-BRD-ky*i/100)-2);
      LineTo(w-BRD,Round(h-BRD-ky*i/100)-2);
      TextOut(
        BRD-28,
        h-BRD-Round(ky*i/100)-(TextHeight('0') div 2),
        FloatToStr(i/100)
      );
    end;

    { риски X }
    for i:=1 to 24 do
    begin
      MoveTo(BRD+Round(i*kx),BRD);
      LineTo(BRD+Round(i*kx),h-BRD+2);
      TextOut(
        BRD+Round(i*kx)-(TextWidth('0') div 2),
        h-BRD+4,
        IntToStr(i)
      );
    end;

    { кривая }
    Pen.Style:=psSolid;
    Pen.Width:=2;
    Pen.Color:=clBlack;

    MoveTo(
      Round(BRD+0/60*kx),
      Round(h-BRD-(SimpK[0]+SimpK[high(SimpK)])/2*ky)
    );

    for i:=0 to high(SimpK) do
    begin
      LineTo(
        Round(BRD+(i+0.5)*TimeBand/60*kx),
        Round(h-BRD-SimpK[i]*ky)
      ); 

      {LineTo(
        Round(BRD+(i+1)*TimeBand/60*kx),
        Round(h-BRD-SimpK[i]*ky)
      );  }

      {TextOut(
        Round(BRD+i*TimeBand/60*kx)+5,
        Round(h-BRD-SimpK[i]*ky)-20,
        FloatToStr(Round(SimpK[i]*1000)/1000)
      );

      MoveTo(
        Round(BRD+(i+1)*TimeBand/60*kx),
        Round(h-BRD-SimpK[i]*ky)
      );  }
    end;

    LineTo(
      Round(BRD+(high(SimpK)+1)*TimeBand/60*kx),
      Round(h-BRD-(SimpK[0]+SimpK[high(SimpK)])/2*ky)
    );

    if length(DataK)>0 then
    begin
      MaxTime:=DataK[0].Time div MinPerDay;
      MinTime:=DataK[high(DataK)].Time div MinPerDay;
      { точки }
      for i:=high(DataK) downto 0 do
      begin
        Time:=DataK[i].Time div MinPerDay;
        if MaxTime<>MinTime then
          b:=Round(
            Sqr(Sqr((Time-MinTime)/(MaxTime-MinTime)))
            *255)
        else
          b:=255;
        Pen.Color:=RGB(b,0,0);

        Rectangle(
          1-2+Round(BRD+(DataK[i].Time mod MinPerDay)/60*kx),
          1-2+Round(h-BRD-DataK[i].Value*ky),
          1+2+Round(BRD+(DataK[i].Time mod MinPerDay)/60*kx),
          1+2+Round(h-BRD-DataK[i].Value*ky)
        );
      end;
    end;
  end;
end;

{=========================================================}
function SearchInjectedDose(const Page: TDiaryPage; Time: integer): integer;
{=========================================================}
   {
      1) ...[b][i]...<time>...      - result:=Ival, выход
      2) ...<time>...               - result:=SInjDose(Day-1)
      3) ...[b]...<time>...         - поиск от Bn вперед
        3.1) ...<time>...[b][i]...  - result:=0;
        3.2) ...<time>...[i][b]...  - result:=Ival, выход
        3.3) ...<time>...           - result:=SInjDose(Day+1)
        <----------->
   }

var
  i: integer;
  StartN: integer;
begin
  result:=0;
(*
  {if (DayNumber<low(diary))or
     (DayNumber>high(Diary))
  then
    ErrorMessage('Ошибка SearchInjectedDose: неверное '+
      'значение DayNumber ('+IntToStr(DayNumber)+')')
  else }
  begin
    StartN:=-1;
    for i:=high(Page.Chron) downto 0 do
    if (Page.Chron[i].RecType=rtIns)and
       (Page.Ins[
       Page.Chron[i].RecNumber].Time<Time)
    then
    begin
      result:=Page.Ins[
        Page.Chron[i].RecNumber].Value;
      exit;
    end else
    if (Page.Chron[i].RecType=rtBlood)and
       (Page.Blood[
       Page.Chron[i].RecNumber].Time<Time)
    then
    begin
      StartN:=i;
      break;
    end;

    if StartN>-1 then
    begin
      for i:=StartN+1 to high(Page.Chron) do

      if (Page.Chron[i].RecType=rtBlood)and
         (Page.Blood[
       Page.Chron[i].RecNumber].Time>Time)
      then break else

      if (Page.Chron[i].RecType=rtIns)then
      begin
        result:=Page.Ins[
          Page.Chron[i].RecNumber].Value;
        exit;
      end;
    end else
    begin
      (*if DayNumber>0 then
        { это рекурсияяяя! =) }
        result:=SearchInjectedDose(
          DayNumber-1,
          Time+MinPerDay);
    end;

  end;   *)
end;

{=========================================================}
procedure ImproveMasses(var D: TMassedFoodList; DProts,DFats,DCarbs: real);
{=========================================================}


const
  ChangeMass       = 1;
var
  BestMass,TempMass: array of real;
  Size: integer;
  CurMass: real;
  OldMark,BestMark,CurMark: real;
  i: integer;

  {
  function Value(p,f,c: real): real;
  begin
    result:=
      abs(p-DProts)+
      abs(f-DFats)+
      abs(c-DCarbs);
  end;

  function ValueMenu(const Menu: TDishList): real;
  var
    ResP,ResF,ResC: real;
    i: integer;
  begin
    ResP:=0;
    ResF:=0;
    ResC:=0;
    for i:=0 to high(Menu) do
    begin
      ResP:=ResP+Menu[i].RelProts*Menu[i].Mass/100;
      ResF:=ResF+Menu[i].RelFats*Menu[i].Mass/100;
      ResC:=ResC+Menu[i].RelCarbs*Menu[i].Mass/100;
    end;
    result:=Value(ResP,ResF,ResC);
  end;

  procedure Search(From: integer; AccumP,AccumF,AccumC: real);
  var
    i: shortint;
    j: integer;
  begin
    if From>=Size then
    begin
      // обработка
      CurMark:=Value(AccumP,AccumF,AccumC);
      if CurMark<BestMark then
      begin
        BestMark:=CurMark;
        for j:=0 to Size-1 do
          BestMass[j]:=TempMass[j];
      end;
    end else
    begin
      for i:=-1 to 1 do
      begin
        CurMass:=D[From].Mass+i*ChangeMass;
        if CurMass<0 then CurMass:=0;
        TempMass[From]:=CurMass;

        Search(
          From+1,
          AccumP + D[From].RelProts * CurMass/100,
          AccumF + D[From].RelFats  * CurMass/100,
          AccumC + D[From].RelCarbs * CurMass/100
        );
      end;
    end;
  end;   }

begin
(*

  if length(D)=0 then exit;

  { оценим, что было до вмешательства }
  OldMark:=ValueMenu(D);

  { произведем одну иттерацию улучшения }
  Size:=length(D);
  SetLength(TempMass,Size);
  SetLength(BestMass,Size);
  for i:=0 to Size-1 do
    BestMass[i]:=D[i].Mass;

  BestMark:=OldMark;
  Search(0,0,0,0);

  for i:=0 to Size-1 do
    D[i].Mass:=BestMass[i];

  for i:=0 to Size-1 do
    CalculateDishCompact(D[i]);

  {ShowMessage(
    'Old Mark: '+RealToStr(OldMark)+#13+
    'New Mark: '+RealToStr(BestMark));  } *)
end;

end.

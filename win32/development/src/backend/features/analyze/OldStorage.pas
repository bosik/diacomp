unit OldStorage;

interface

implementation

(*
{=========================================================}
function GetAvgPos(const List: TRecordList): real;
{=========================================================}
var
  i: integer;
  Summ: real;
begin
  if length(List) = 0 then
    result := 0.5 else
  begin
    Summ := 0;
    result := 0;
    for i := 0 to high(List) do
    begin
      result := result + List[i].Pos*List[i].Weight;
      Summ := Summ + List[i].Weight;
    end;
    if Summ>0 then
      result := result/Summ;
  end;
end;


{=========================================================}
function GetSummDeviation(const List: TRecordList; const K,Q,P: real): real;
{=========================================================}
var
  i: integer;
begin
  result := 0;
  for i := 0 to high(list) do
    result := result+Sqr(
      List[i].BloodInValue
      +List[i].Carbs*K
      +List[i].Prots*P
      -List[i].InsValue*Q
      -List[i].BloodOutValue
    );
end;

var
  TrinityList: ^TRecordList;
  TrinityZone: ^TZoneList;
  ParamList: ^TParamList;

var
  AvgK,AvgQ,AvgP: real;

{================================================================}
function GetKRand(const List: TRecordList; const Q,P: real;
  var Deviation: real): real; {summ randomation}
{================================================================}
var
  i: integer;
  CurTerm: real;
  Ks: array of real;
  Summ: real;
begin
  result := 0;
  Summ := 0;
  Deviation := 0;
  
  for i := 0 to high(List) do
  if List[i].Carbs>0 then
  begin
    CurTerm :=
      (List[i].BloodOutValue
      -List[i].BloodInValue
      +Q*List[i].InsValue
      -P*List[i].Prots)
      {/Q};

    if CurTerm < 0 then
      result := result + 100500;  

    SetLength(Ks,length(ks)+1);
    Ks[high(Ks)] := CurTerm / List[i].Carbs;

    result := result + CurTerm;
    Summ := Summ+List[i].Carbs;
  end;

  if Summ>0 then
  begin
    result := result/Summ;

    { [100%] абсолютное вычисление - плохо, т.к. приводит к занижению K }
    {for i := 0 to high(Ks) do
      Deviation := Deviation + abs(Ks[i]-result);}

    { [ ? ] относительное вычисление - плохо, т.к. приводит к завышению K }
    if result <>0 then
    for i := 0 to high(Ks) do
      Deviation := Deviation + abs(1-Ks[i]/result);
  end;
end;

{ В Values передаются коэффициенты Q и P }

{================================================================}
function DevFunction_XRand(const Values: array_of_real): real;
{================================================================}
var
  CurRand: real;
  i: integer;
begin
  result := 0;
  for i := 0 to high(TrinityZone^) do
  begin
    GetKRand(TrinityZone^[i], Values[0], Values[1], CurRand);
    result := result + CurRand;
  end;
end;

{================================================================}
function DevFunction_DoubleXRand(const Values: array_of_real): real;
{================================================================}
var
  CurRand,CurAvgDev: real;
  Compression: real;
begin
  Compression := ParamList^[PAR_COMPRESSION];

  GetKRand(TrinityList^,Values[0],Values[1],CurRand);
  CurAvgDev := Sqr(6*(AvgQ-Values[0])) + Sqr(30*(AvgP-Values[1]));

  result := 
    (1-Compression)*15*CurRand+
    Compression*CurAvgDev;
end;

{================================================================}
function Analyze_GoldAverage(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  const Par: TParamList;
  var KoofList: TKoofList): boolean; StdCall;
{================================================================}

  function F(const X: real): real;
  { Par[PAR_ADAPTATION] in [0, 0.5] }
  begin
    result := (Par[PAR_ADAPTATION]-0.5)*sin(pi*(X-0.5))+0.5;
  end;
  
  function fMin(a,b: integer): integer; overload;
    begin  if a<b then result := a else result := b; end;
  function fMin(a,b: real): real; overload;
    begin  if a<b then result := a else result := b; end;
  function fMax(a,b: integer): integer; overload;
    begin  if a>b then result := a else result := b; end;
  function fMax(a,b: real): real; overload;
    begin  if a>b then result := a else result := b; end;

  function GetK(const Rec: TRec; Q,P: real): real;
  begin
    // BsIn + result*Carbs + P*Prots - Q*Ins = BsOut
    // result = (BsOut - BsIn - P*Prots + Q*Ins) / Carbs
    result := (
      +Rec.BloodOutValue-Rec.BloodInValue
      -P*Rec.Prots+Q*Rec.InsValue
      )/Rec.Carbs;
  end;

{

  Параметры:
    - коэффициент пропорциональности CurRand/CurAvgDev
    - коэффициент компрессии
    - коэффициент адаптации
}

var
  Limits: TLimitArray;
  X: array_of_real;
  i,j,k: integer;
  CurTerm,Weight,Summ, SummWeight: real;
  CurK: real;
  Min,Max: real;
  MinDate,MaxDate: integer;
begin
  SetLength(KoofList,length(ZoneList));
  result := false;
  if length(KoofList)=0 then exit;

  //{!!} r := TStringList.Create;

  { 1. Нахождение средних Q и P }
  SetLength(Limits,2);
  Limits[0].Min := Lim[2].Min;  {q}
  Limits[0].Max := Lim[2].Max;  {q}
  Limits[1].Min := Lim[3].Min;  {p}
  Limits[1].Max := Lim[3].Max;  {p}

  TrinityZone := @ZoneList;
  ParamList := @Par;
  SolveSystem(2, Limits, DevFunction_XRand, 0.001, X);

  AvgQ := X[0];
  AvgP := X[1];

  Min := high(Integer);
  Max := 0;

  for i := 0 to high(KoofList) do
  begin
    TrinityList := @ZoneList[i];
    SolveSystem(2,Limits,DevFunction_DoubleXRand,0.001,X);

    {== определение границ ==}
    MinDate := high(Integer);
    MaxDate := 0;
    for j := 0 to high(ZoneList[i]) do
    begin
      MinDate := fMin(MinDate, ZoneList[i][j].DayNumber);
      MaxDate := fMax(MaxDate, ZoneList[i][j].DayNumber);
    end;

    if MinDate = MaxDate then dec(MinDate);

    {== нахождение средних значений ==}
    Summ := 0;
    SummWeight := 0;

    SetLength(KoofList[i].Points,0);
    for j := 0 to high(ZoneList[i]) do
    if ZoneList[i][j].Carbs>0 then
    begin
      CurK := GetK(ZoneList[i][j], {X[0]}AvgQ, {X[1]}AvgP);
      Weight := F((ZoneList[i][j].DayNumber-MinDate)/(MaxDate-MinDate)) * ZoneList[i][j].Carbs;
      Summ := Summ + CurK*Weight;
      SummWeight := SummWeight + Weight;

      k := length(KoofList[i].Points);
      SetLength(KoofList[i].Points,k+1);
      KoofList[i].Points[k].Pos := ZoneList[i][j].Pos;
      KoofList[i].Points[k].Value := CurK;
      KoofList[i].Points[k].Tag := ZoneList[i][j].DayNumber;
      ZoneList[i][j].Weight := Weight;

      Min := fMin(Min, KoofList[i].Points[k].Tag);
      Max := fMax(Max, KoofList[i].Points[k].Tag);
    end;

    if SummWeight>0 then
    begin
      KoofList[i].k := Summ/SummWeight;
      KoofList[i].q := {X[0]}AvgQ;
      KoofList[i].p := {X[1]}AvgP;
      KoofList[i].Pos := GetAvgPos(ZoneList[i]);
      KoofList[i].Proved := true;
    end else
      KoofList[i].Proved := false;
  end;

  { Масштабирование тегов }
  if Min < Max then
  for i := 0 to high(KoofList) do
  for j := 0 to high(KoofList[i].Points) do
    KoofList[i].Points[j].Tag := 
    (KoofList[i].Points[j].Tag-Min)/(Max-Min);

  //{!!} r.SaveToFile('outfile.txt');
  //{!!} r.Free;
end;   *)


{==============================================================================}

function DevFunction_FullRand(const Values: array_of_real): real;
var
  i,n: integer;
  A: array of
  record
    K,Q,P: real;
  end;
  AvgK,AvgQ,AvgP: real;
  DerK,DerQ,DerP: real;
  Count: integer;
begin
  result := 0;
  Count := 0;

  for n := 0 to high(TrinityZone^) do
  begin
    { 1.Вычисление K,Q,P }

    SetLength(A,0);
    for i := 0 to high(TrinityZone^[n]) do
    if (TrinityZone^[n][i].Carbs>0)and
       (TrinityZone^[n][i].InsValue>0)and
       (TrinityZone^[n][i].Prots>0) then
    begin
      SetLength(A,length(A)+1);

      A[high(A)].K := 
        (TrinityZone^[n][i].BloodOutValue
        -TrinityZone^[n][i].BloodInValue
        +Values[1]*TrinityZone^[n][i].InsValue
        -Values[2]*TrinityZone^[n][i].Prots)/
        TrinityZone^[n][i].Carbs; {*}

      A[high(A)].Q := 
        -(TrinityZone^[n][i].BloodOutValue
        -TrinityZone^[n][i].BloodInValue
        -Values[0]*TrinityZone^[n][i].Carbs
        -Values[2]*TrinityZone^[n][i].Prots)/
        TrinityZone^[n][i].InsValue; {*}

      A[high(A)].P := 
        (TrinityZone^[n][i].BloodOutValue
        -TrinityZone^[n][i].BloodInValue
        +Values[1]*TrinityZone^[n][i].InsValue
        -Values[0]*TrinityZone^[n][i].Carbs)/
        TrinityZone^[n][i].Prots; {*}
    end;

    { 2.Нахождение средних значений }
    if length(A)>0 then
    begin
      AvgK := 0;
      AvgQ := 0;
      AvgP := 0;
      for i := 0 to high(A) do
      begin
        AvgK := AvgK + A[i].K;
        AvgQ := AvgQ + A[i].Q;
        AvgP := AvgP + A[i].P;
      end;
      AvgK := AvgK / length(A);
      AvgQ := AvgQ / length(A);
      AvgP := AvgP / length(A);

      DerK := 0;
      DerQ := 0;
      DerP := 0;
      for i := 0 to high(A) do
      begin
        DerK := DerK + Sqr(A[i].K-AvgK);
        DerQ := DerQ + Sqr(A[i].Q-AvgQ);
        DerP := DerP + Sqr(A[i].P-AvgP);
      end;

      DerK := DerK / length(A);
      DerQ := DerQ / length(A);
      DerP := DerP / length(A);

      result := result + DerK + DerQ + DerP;
      inc(Count,length(A));
    end;
  end;

  if Count>0 then
    result := result/Count; 
end;

function DevFunction_Double(const Values: array_of_real): real;
const
  Accuracy = 0.10;
var
  Dev,Ran: real;
begin
  Dev := GetSummDeviation(TrinityList^,Values[0],Values[1],Values[2]);

  Ran := {Sqrt}(
    Sqr(AvgK - Values[0])+
    Sqr(AvgQ - Values[1])+
    Sqr(AvgP - Values[2]));
  {Ran := 0;
  if AvgK<>0 then Ran := Ran+Sqr(1-Values[0]/AvgK);
  if AvgQ<>0 then Ran := Ran+Sqr(1-Values[1]/AvgQ);
  if AvgP<>0 then Ran := Ran+Sqr(1-Values[2]/AvgP); }

  result := 
    Accuracy * Dev +
    (1-Accuracy) * Ran;
end;

function DevFunction_K(const Values: array_of_real): real;
var
  i: integer;
begin
  result := 0;
  for i := 0 to high(TrinityZone^) do
    result := result+
    GetSummDeviation(TrinityZone^[i],Values[0],Values[1],Values[2]);
end;

function DevFunction(const Values: array_of_real): real;
begin
  result := GetSummDeviation(TrinityList^,Values[0],Values[1],Values[2]);
end;

{=========================================================}
function Analyze_0(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  var KoofList: TKoofList): boolean; StdCall;
{=========================================================}
var
  i: integer;
begin
  SetLength(KoofList,length(ZoneList));
  result := true;

  for i := 0 to high(KoofList) do
  begin
    KoofList[i].k := 0;
    KoofList[i].q := 0;
    KoofList[i].p := 0;
    KoofList[i].Pos := GetAvgPos(ZoneList[i]);
    KoofList[i].Proved := true;
  end;
end;

{=========================================================}
function Analyze_Brute(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  var KoofList: TKoofList): boolean; StdCall;
{=========================================================}
type
  PRealNode = ^TRealNode;
  TRealNode = record
    Value: currency;
    Next: PRealNode;
  end;

  PCalcStruct = ^TCalcStruct;
  TCalcStruct = record
    Koof: currency;
    Data: PRealNode;
    Next: PCalcStruct
  end;

  procedure Destroy(var Head: PRealNode);
  var
    temp: PRealNode;
  begin
    while Head<>nil do
    begin
      Temp := Head;
      Head := Head^.Next;
      Dispose(Temp);
    end;
  end;

  procedure Init(var Head: PRealNode);
  begin
    if Head<>nil then
    begin
      Destroy(Head^.Next);
      Head^.Next := nil;
    end else
    begin
      New(Head);
      Head^.Next := nil;
    end;
  end;

  procedure Add(var Tail: PRealNode; const Value: real);
  begin
    if Tail<>nil then
    begin
      New(Tail^.Next);
      Tail := Tail^.Next;
      Tail^.Next := nil;
      Tail^.Value := Value;
    end;
  end;

const
  Disc = 0.01;
label
  Down;
var
  Kj,Qj,Pj,i,n: integer;
  CountK,CountQ,CountP: integer;
  BestDelta,CurDelta: real;

  HeadK,HeadQ,HeadP: PCalcStruct;
  CurK,CurQ,CurP: PCalcStruct;
  BufK,BufQ,BufP: PRealNode;
  Cur: PRealNode;

  DBS: PRealNode;
begin
  SetLength(KoofList,length(ZoneList));
  if length(KoofList)=0 then exit;

  CountK := Round((lim[1].Max-lim[1].Min)/Disc)+1;
  CountQ := Round((lim[2].Max-lim[2].Min)/Disc)+1;
  CountP := Round((lim[3].Max-lim[3].Min)/Disc)+1;

  { инициализация значений }
  New(HeadK);
  HeadK^.Next := nil;
  HeadK^.Data := nil;
  CurK := HeadK;
  for Kj := 0 to CountK-1 do
  begin
    New(CurK^.Next);
    CurK := CurK^.Next;
    CurK^.Next := nil;
    CurK^.Data := nil;
    CurK^.Koof := Kj*Disc+lim[1].Min;
  end;

  New(HeadQ);
  HeadQ^.Next := nil;
  HeadQ^.Data := nil;
  CurQ := HeadQ;
  for Qj := 0 to CountQ-1 do
  begin
    New(CurQ^.Next);
    CurQ := CurQ^.Next;
    CurQ^.Next := nil;
    CurQ^.Data := nil;
    CurQ^.Koof := Qj*Disc+lim[2].Min;
  end;

  New(HeadP);
  HeadP^.Next := nil;
  HeadP^.Data := nil;
  CurP := HeadP;
  for Pj := 0 to CountP-1 do
  begin
    New(CurP^.Next);
    CurP := CurP^.Next;
    CurP^.Next := nil;
    CurP^.Data := nil;
    CurP^.Koof := Pj*Disc+lim[3].Min;
  end;

  DBS := nil;

  { поехали... }
  for n := 0 to high(ZoneList) do
  begin
    KoofList[n].Pos := GetAvgPos(ZoneList[n]);
    KoofList[n].Proved := false;

    if length(ZoneList[n])=0 then
    begin
      KoofList[n].K := 0;
      KoofList[n].Q := 0;
      KoofList[n].P := 0;
      continue;
    end;

    KoofList[n].Proved := true;
    BestDelta := 1000000000;

    { инициализация просчётов }
    CurK := HeadK;
    for Kj := 0 to CountK-1 do
    begin
      CurK := CurK^.Next;
      Init(CurK^.Data);

      Cur := CurK^.Data;
      for i := 0 to high(ZoneList[n]) do
        Add(Cur,CurK^.Koof*ZoneList[n][i].Carbs);
    end;

    CurQ := HeadQ;
    for Qj := 0 to CountQ-1 do
    begin
      CurQ := CurQ^.Next;
      Init(CurQ^.Data);
      Cur := CurQ^.Data;
      for i := 0 to high(ZoneList[n]) do
        Add(Cur,CurQ^.Koof*ZoneList[n][i].InsValue);
    end;

    CurP := HeadP;
    for Pj := 0 to CountP-1 do
    begin
      CurP := CurP^.Next;
      Init(CurP^.Data);
      Cur := CurP^.Data;
      for i := 0 to high(ZoneList[n]) do
        Add(Cur,CurP^.Koof*ZoneList[n][i].Prots);
    end;

    Init(DBS);
    Cur := DBS;

    for i := 0 to high(ZoneList[n]) do
      Add(Cur,ZoneList[n][i].BloodInValue-
        ZoneList[n][i].BloodOutValue);

    { перебор }

    CurK := HeadK;
    for Kj := 0 to CountK-1 do
    begin
      CurK := CurK^.Next;

      CurQ := HeadQ;
      for Qj := 0 to CountQ-1 do
      begin
        CurQ := CurQ^.Next;

        CurP := HeadP;
        for Pj := 0 to CountP-1 do
        begin
          CurP := CurP^.Next;

          CurDelta := 0;

          Cur := DBS;
          BufK := CurK^.Data;
          BufQ := CurQ^.Data;
          BufP := CurP^.Data;
          for i := 0 to high(ZoneList[n]) do
          begin
            Cur := Cur^.Next;
            BufK := BufK^.Next;
            BufQ := BufQ^.Next;
            BufP := BufP^.Next;

            CurDelta := CurDelta+abs(
              Cur^.Value
              +BufK^.Value
              -BufQ^.Value
              +BufP^.Value
              );
            if CurDelta>BestDelta then goto down;
          end;

          if CurDelta < BestDelta then
          begin
            BestDelta := CurDelta;
            KoofList[n].K := CurK^.Koof;
            KoofList[n].Q := CurQ^.Koof;
            KoofList[n].P := CurP^.Koof;
          end;

          down:
        end;
      end;
    end;
  end;

  { освобождение памяти }
  while HeadK<>nil do
  begin
    Destroy(HeadK^.Data);
    CurK := HeadK;
    HeadK := CurK^.Next;
    Dispose(CurK);
  end;

  while HeadQ<>nil do
  begin
    Destroy(HeadQ^.Data);
    CurQ := HeadQ;
    HeadQ := CurQ^.Next;
    Dispose(CurQ);
  end;

  while HeadP<>nil do
  begin
    Destroy(HeadP^.Data);
    CurP := HeadP;
    HeadP := CurP^.Next;
    Dispose(CurP);
  end;

  Destroy(DBS);
end;

{=========================================================}
function Analyze_OldAvg(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  var KoofList: TKoofList): boolean; StdCall;
{=========================================================}
var
  i,j: integer;
  q,p: real;
  SummCarbs: real;
begin
  SetLength(KoofList,length(ZoneList));
  q := 3.35;//(Lim[2].Min+Lim[2].Max)/2;
  p := 0;

  for i := 0 to high(KoofList) do
  begin
    KoofList[i].k := 0;
    KoofList[i].q := q;
    KoofList[i].p := p;
    KoofList[i].Pos := GetAvgPos(ZoneList[i]);

    SummCarbs := 0;
    for j := 0 to high(ZoneList[i]) do
    if ZoneList[i][j].Carbs>0 then
    begin
      KoofList[i].k := KoofList[i].k+
        (ZoneList[i][j].BloodOutValue-
        ZoneList[i][j].BloodInValue-
        p*ZoneList[i][j].Prots+
        q*ZoneList[i][j].InsValue){/
        ZoneList[i][j].Carbs};

      SummCarbs := SummCarbs+ZoneList[i][j].Carbs;
    end;

    if SummCarbs>0 then
    begin
      KoofList[i].k := KoofList[i].k/SummCarbs;
      KoofList[i].Proved := KoofList[i].k>0;
    end else
      KoofList[i].Proved := false;
  end;
end;

{=========================================================}
function Analyze_OldAvgQ(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  var KoofList: TKoofList): boolean; StdCall;
{=========================================================}
var
  i,j: integer;
  k,p: real;
  Summ: real;
begin
  SetLength(KoofList,length(ZoneList));
  k := 0.28;//(Lim[2].Min+Lim[2].Max)/2;
  p := 0;

  for i := 0 to high(KoofList) do
  begin
    KoofList[i].k := k;
    KoofList[i].q := 0;
    KoofList[i].p := p;
    KoofList[i].Pos := GetAvgPos(ZoneList[i]);

    Summ := 0;
    for j := 0 to high(ZoneList[i]) do
    if ZoneList[i][j].InsValue>0 then
    begin
      KoofList[i].q := KoofList[i].q+
        (k*ZoneList[i][j].Carbs
        +p*ZoneList[i][j].Prots
        -ZoneList[i][j].BloodOutValue
        +ZoneList[i][j].BloodInValue);

      Summ := Summ+ZoneList[i][j].InsValue;
    end;

    if Summ>0 then
    begin
      KoofList[i].q := KoofList[i].q/Summ;
      KoofList[i].Proved := KoofList[i].q>0;
    end else
      KoofList[i].Proved := false;
  end;
end;

{=========================================================}
function Analyze_Trinity(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  var KoofList: TKoofList): boolean; StdCall;
{=========================================================}
var
  n: integer;
  Limits: TLimitArray;
  X: array_of_real;

  AvgK,AvgQ,AvgP: real;
  Count: integer;
begin
  SetLength(KoofList,length(ZoneList));
  if length(KoofList)=0 then exit;

  SetLength(Limits,3);
  Limits[0].Min := Lim[1].Min;
  Limits[0].Max := Lim[1].Max;
  Limits[1].Min := Lim[2].Min;
  Limits[1].Max := Lim[2].Max;
  Limits[2].Min := Lim[3].Min;
  Limits[2].Max := Lim[3].Max;

  { 1. Переопределение границ }

  AvgK := 0;
  AvgQ := 0;
  AvgP := 0;
  Count := 0;

  for n := 0 to high(ZoneList) do
  if length(ZoneList[n])>0 then
  begin
    TrinityList := @ZoneList[n];
    SolveSystem(
      3,Limits,DevFunction,0.01,X);

    AvgK := AvgK + X[0];
    AvgQ := AvgQ + X[1];
    AvgP := AvgP + X[2];
    inc(Count);
  end;

  AvgK := AvgK / Count;
  AvgQ := AvgQ / Count;
  AvgP := AvgP / Count;

  Limits[0].Min := AvgK*(1-0.4);
  Limits[0].Max := AvgK*(1+0.4);
  Limits[1].Min := AvgQ*(1-0.2);
  Limits[1].Max := AvgQ*(1+0.2);
  Limits[2].Min := AvgP*(1-0.2);
  Limits[2].Max := AvgP*(1+0.2);   

  { 2. Расчёт }

  for n := 0 to high(ZoneList) do
  if length(ZoneList[n])>0 then
  begin
    TrinityList := @ZoneList[n];
    SolveSystem(
      3,Limits,DevFunction,0.01,X);
    KoofList[n].K := X[0];
    KoofList[n].Q := X[1];
    KoofList[n].P := X[2];
    KoofList[n].Pos := GetAvgPos(ZoneList[n]);
    KoofList[n].Proved := true;
  end else
    KoofList[n].Proved := false;
end;

{=========================================================}
function Analyze_TrinityCombo(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  var KoofList: TKoofList): boolean; StdCall;
{=========================================================}
const
  Compression = 0.5;
var
  i,j: integer;
  Limits: TLimitArray;
  X: array_of_real;
  AvgQ,AvgP: real;
  Count: integer;
begin
  SetLength(KoofList,length(ZoneList));
  if length(KoofList)=0 then exit;

  { 1. Определим значения Q и P для каждой зоны такие, }
  { чтобы разброс соответствующих K был минимален      }
  { 2. Попутно находим средние Q и P }

  SetLength(Limits,3);
  Limits[0].Min := Lim[1].Min;  {k}
  Limits[0].Max := Lim[1].Max;  {k}
  Limits[1].Min := Lim[2].Min;  {q}
  Limits[1].Max := Lim[2].Max;  {q}
  Limits[2].Min := Lim[3].Min;  {p}
  Limits[2].Max := Lim[3].Max;  {p}

  AvgQ := 0;
  AvgP := 0;
  Count := 0;

  for i := 0 to high(ZoneList) do
  begin
    if length(ZoneList[i])>0 then
    begin
      TrinityList := @ZoneList[i];
      SolveSystem(
        2,Limits,DevFunction_K,0.01,X);
      KoofList[i].Q := X[0];
      KoofList[i].P := X[1];
      AvgQ := AvgQ+KoofList[i].q;
      AvgP := AvgP+KoofList[i].p;
      inc(Count);
    end else
    begin
      KoofList[i].Q := 0;
      KoofList[i].P := 0;
    end;
    KoofList[i].Pos := GetAvgPos(ZoneList[i]);
  end;

  if Count>0 then
  begin
    AvgQ := AvgQ/Count;
    AvgP := AvgP/Count;
  end;

  { 2. Компрессируем Q и P }
  for i := 0 to high(KoofList) do
  if KoofList[i].q>0 then
  begin
    KoofList[i].q := Compression*AvgQ + (1-Compression)*KoofList[i].q;
    KoofList[i].p := Compression*AvgP + (1-Compression)*KoofList[i].p;
    KoofList[i].Proved := true;
  end else
  begin
    KoofList[i].q := AvgQ;
    KoofList[i].p := AvgP;
    KoofList[i].Proved := false;
  end;  

  { 3. Вычисляем средние значения K }
  for i := 0 to high(KoofList) do
  begin
    KoofList[i].k := 0;
    Count := 0;
    for j := 0 to high(ZoneList[i]) do
    if ZoneList[i][j].Carbs>0 then
    begin
      KoofList[i].k := KoofList[i].k+
        (ZoneList[i][j].BloodOutValue-
        ZoneList[i][j].BloodInValue-
        KoofList[i].p*ZoneList[i][j].Prots+
        KoofList[i].q*ZoneList[i][j].InsValue)/
        ZoneList[i][j].Carbs;
      inc(count);
    end;
    if Count>0 then
      KoofList[i].k := KoofList[i].k/Count;
  end;
end;

{=========================================================}
function Analyze_TrinityDouble(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  var KoofList: TKoofList): boolean; StdCall;
{=========================================================}
var
  i: integer;
  Limits: TLimitArray;
  X: array_of_real;
begin
  SetLength(KoofList,length(ZoneList));
  if length(KoofList)=0 then exit;

  { 1. Определим средние значения Q и P }

  SetLength(Limits,3);
  Limits[0].Min := Lim[1].Min;  {k}
  Limits[0].Max := Lim[1].Max;  {k}
  Limits[1].Min := Lim[2].Min;  {q}
  Limits[1].Max := Lim[2].Max;  {q}
  Limits[2].Min := Lim[3].Min;  {p}
  Limits[2].Max := Lim[3].Max;  {p}

  TrinityZone := @ZoneList;
  SolveSystem(
    3,Limits,DevFunction_K,0.001,X);

  AvgK := X[0]*4;
  AvgQ := X[1]*4;
  AvgP := X[2]*4;
  {AvgK := 0.28;
  AvgQ := 4.4;
  AvgP := 0.01;  }

  { 2.Найдём лучшие значения для каждой зоны }
  { Качество определяется сразу по двум критериям }

  for i := 0 to high(ZoneList) do
  begin
    KoofList[i].Pos := GetAvgPos(ZoneList[i]);

    if length(ZoneList[i])>0 then
    begin
      TrinityList := @ZoneList[i];
      SolveSystem(
        3,Limits,DevFunction_Double,0.001,X);
      KoofList[i].K := X[0];
      KoofList[i].Q := X[1];
      KoofList[i].P := X[2];
      KoofList[i].Proved := true;
    end else
      KoofList[i].Proved := false; 
  end;
end;

{=========================================================}
function Analyze_Father(
  const ZoneList: TZoneList;
  const Lim: TLimits;
  var KoofList: TKoofList): boolean; StdCall;
{=========================================================}

  function RecDelta(i1,i2: integer): real;
  begin
    result := ZoneList[i1][i2].BloodOutValue-
            ZoneList[i1][i2].BloodInValue;
  end;

const
  MAX_DEV = 0.5;
var
  i,n: integer;
  Summ,CurWeight,SummWeight: real;
  X,Delta,CurTerm: real;
begin
  SetLength(KoofList,length(ZoneList));
  if length(KoofList)=0 then exit;

  for n := 0 to high(ZoneList) do
  if length(ZoneList[n])>0 then
  begin
    { 1. Нахождение X }
    Summ := 0;
    SummWeight := 0;
    for i := 0 to high(ZoneList[n]) do
    if (abs(RecDelta(n,i))<MAX_DEV)and
       (ZoneList[n][i].Carbs>0) then
    begin
      CurWeight := 1-2*abs(RecDelta(n,i));
      Summ := Summ+
        (ZoneList[n][i].InsValue/ZoneList[n][i].Carbs)*
        CurWeight;
      SummWeight := SummWeight+CurWeight;
    end;

    if (Summ=0)or(CurWeight=SummWeight) then
    begin
      KoofList[n].Proved := false;
      continue;
    end;

    if SummWeight>0 then
      Summ := Summ/SummWeight;

    X := Summ;

    { 2. Нахождение Q и K }
    Summ := 0;
    SummWeight := 0;
    for i := 0 to high(ZoneList[n]) do
    if abs(X*ZoneList[n][i].Carbs-ZoneList[n][i].InsValue)>0.001 then
    begin
      Delta := RecDelta(n,i);
      CurWeight := abs(Delta);
      CurTerm := Delta/
        (X*ZoneList[n][i].Carbs-ZoneList[n][i].InsValue)
        *CurWeight;
      Summ := Summ+CurTerm;
      SummWeight := SummWeight+CurWeight;
    end;

    if SummWeight>0 then
      Summ := Summ/SummWeight;

    KoofList[n].q := Summ;
    KoofList[n].k := X*Summ;
    KoofList[n].p := 0;
    KoofList[n].Proved := (KoofList[n].k>0)and(KoofList[n].q>0);
    KoofList[n].Pos := GetAvgPos(ZoneList[n]);
  end else
    KoofList[n].Proved := false;


  (*
  { 1. Определим средние значения Q и P }

  SetLength(Limits,3);
  Limits[0].Min := Lim[1].Min;  {k}
  Limits[0].Max := Lim[1].Max;  {k}
  Limits[1].Min := Lim[2].Min;  {q}
  Limits[1].Max := Lim[2].Max;  {q}
  Limits[2].Min := Lim[3].Min;  {p}
  Limits[2].Max := Lim[3].Max;  {p}

  TrinityZone := @ZoneList;
  SolveSystem(3,Limits,DevFunction_FullRand,0.01,X);

  AvgK := X[0];
  AvgQ := X[1];
  AvgP := X[2];

  for i := 0 to high(KoofList) do
  begin
    KoofList[i].k := AvgK;
    KoofList[i].q := AvgQ;
    KoofList[i].p := AvgP;
    KoofList[i].Pos := GetAvgPos(ZoneList[i]);
    KoofList[i].Proved := true;
  end;   }

  {for n := 0 to high(ZoneList) do
  if length(ZoneList[n])>0 then
  begin
    TrinityList := @ZoneList[n];
    SolveSystem(
      3,Limits,DevFunction_FullRand,0.01,X);
    KoofList[n].K := X[0];
    KoofList[n].Q := X[1];
    KoofList[n].P := X[2];
    KoofList[n].Pos := GetAvgPos(ZoneList[n]);
    KoofList[n].Proved := true;
  end else
    KoofList[n].Proved := false;    *)
end;

end.

library MathAn;

{$R+}

uses
  ShareMem,
  Matrixes,
  Math,
  AnalyzeInterface in 'src\analyze\AnalyzeInterface.pas',
  DiaryRecords in 'src\bo\DiaryRecords.pas',
  BusinessObjects in 'src\bo\BusinessObjects.pas',
  DiaryRoutines in 'src\common\DiaryRoutines.pas';

  // TODO 1: move code to some unit into src/analyze

type
  TTimePoint = record
    Time: integer;
    Value: real;
  end;

  TWeightedTimePoint = record
    Time: integer;
    Value: real;
    Weight: real;
  end;

  TWeightedTimePointArray = array of TWeightedTimePoint;

  TDayArray = array[0..MinPerDay - 1] of real;

var
  TIME_WEIGHTS: array[0..HalfMinPerDay + 1] of Real;

{==============================================================================}
function TimeDistance(T1, T2: integer): integer;
{==============================================================================}
begin
  { получаем расстояние }
  Result := abs(T1-T2);

  { получаем кратчайшее расстояние }
  if Result > HalfMinPerDay then
    Result := MinPerDay - Result;
end;

{==============================================================================}
function Sine(const X: real): real;
{==============================================================================}
const
  P = 0.0; // P in [0, 0.5]
begin
  { точки: (0;1), (1;0), между ними красивая синусоида }
  Result := (P-0.5)*sin(pi*(X-0.5))+0.5;
end;

{==============================================================================}
procedure PrepareTimeWeights(K: Real);
{==============================================================================}
var
  Time: integer;
begin
  for Time := 0 to High(TIME_WEIGHTS) do
  begin
    TIME_WEIGHTS[Time] := Exp(-K * sqr(Time / HalfMinPerDay));
  end;
end;

{==============================================================================}
function TimeWeight(T1, T2: integer; K: real): real;
{==============================================================================}
const
  TIME_ACT = 60;
  //MIN = Exp(-0.5);
var
  Time: integer;
begin
  { получаем расстояние }
  Time := TimeDistance(T1, T2);

  { вычисляем коэффициент }

  {if Time < 60 then
    Result := 1
  else
    Result := 3600*3600 / Sqr(Sqr(Time));  }

  { экспоненциально-линейный вариант }
  //Result := Exp(-K*(Time/HalfMinPerDay));

  { экспоненциально-радикальный вариант }
  //Result := Exp(-K*sqrt(Time/HalfMinPerDay));

  { экспоненциально-квадратичный вариант }
  //Result := Exp(-K*sqr(Time/HalfMinPerDay));
  Result := TIME_WEIGHTS[Time];

  { экспоненциально-квадратичный вариант с корректировкой}
  //Result := Exp(- K * sqr(Time/HalfMinPerDay/Sqrt(2*K)) );       <=>
  //Result := Exp( -sqr(Time/HalfMinPerDay)/2 );

  { экспоненциально-квадратичный вариант с корректировкой и Power }
  //Result := Power(16*Exp( -Power(Time/HalfMinPerDay, 32) / 2 ), 128);

  { экспоненциально-степенной вариант }
  //Result := Exp(-K*Power(Time/HalfMinPerDay, 4) );

  { экспоненциально-квадратичный с ограничением вариант }
  {if Time < TIME_ACT then
    Result := Exp(-K*sqr(Time/HalfMinPerDay))
  else
    Result := 0.01*Exp(-K*sqr(Time/HalfMinPerDay)); }

  { синусоидальный вариант }
  {if Time < TIME_ACT then
    Result := 1*Sine(Time/TIME_ACT) + 0.1
  else
    Result := 0.1*(1-Sqrt((Time-TIME_ACT)/(HalfMinPerDay-TIME_ACT))); }

  //Result := 1/Sqr((Time/HalfMinPerDay+0.15));
  //Result := Sqrt(1 - Time/HalfMinPerDay);
  //Result := 1;

  { линейный вариант }
  //Result := Power(1-Time/HalfMinPerDay, 16);

  { кусочно-линейный вариант }
  {if Time < TIME_ACT then
    Result := 10
  else
    Result := 0.1*Power(1-(Time-TIME_ACT)/(HalfMinPerDay-TIME_ACT), 16);  }
end;

{==============================================================================}
function ApproximatePoint(const Data: TWeightedTimePointArray; Factor: real; Time: integer): real;
{==============================================================================}
//const
//  APP_POWER = 8;
var
  i: integer;
  Summ, SummWeight: real;
  CurWeight: real;

  //Pnt: TPointArray;
  //P: TPolynomial;
begin
  { расставляем веса, считаем }
  Summ := 0;
  SummWeight := 0;
  for i := Low(Data) to High(Data) do
  if not IsNAN(Data[i].Value) then
  begin
    CurWeight := Data[i].Weight * TimeWeight(Time, Data[i].Time, Factor); {!!! оптимизировать}
    SummWeight := SummWeight + CurWeight;
    Summ := Summ + Data[i].Value * CurWeight;
  end;

  if (SummWeight = 0) then
    Result := NaN // если (все Data[i].Weight = 0) or (Length(Data) = 0)
  else
    Result := Summ / SummWeight;

  // готовим массив точек
  (*SetLength(Pnt, Length(Data));
  for i := 0 to High(Data) do
  begin
    Pnt[i].X := Data[i].Time;
    Pnt[i].Y := Data[i].Value;
  end;

  // находим аппроксимирующий полином
  P := MinSqrApproximation(APP_POWER, Pnt);

  // вычисляем его значение
  Result := CalcPolynomial(P, Time);    *)
end;

{==============================================================================}
procedure Approximate(const Data: TWeightedTimePointArray; Factor: real;
  out Result: TDayArray);
{==============================================================================}
var
  Time: integer;
begin
  { оптимизационная заглушка }
  if (Length(Data) = 0) then
  begin
    for Time := 0 to MinPerDay - 1 do
      Result[Time] := NaN;
    Exit;
  end;

  { расставляем веса, считаем }
  for Time := 0 to MinPerDay - 1 do
    Result[Time] := ApproximatePoint(Data, Factor, Time);
end;

(*
{==============================================================================}
procedure ApproximateList(const Data: TWeightedTimePointArray; Factor: real;
  out Result: TDayArray);
{==============================================================================}
var
  Time: integer;
begin
  { оптимизационная заглушка }
  if Length(Data) = 0 then
  begin
    for Time := 0 to MinPerDay-1 do
      Result[Time] := NaN;
    Exit;
  end;

  { расставляем веса, считаем }
  for Time := 0 to MinPerDay-1 do
    Result[Time] := ApproximatePoint(Data, Factor, Time);
end;
 *)
(*
{==============================================================================}
function Analyze_Math(const RecList: TAnalyzeRecList; out KoofList: TKoofList): boolean; StdCall;
{==============================================================================}
const
  N = 3; // количество измерений

var
  L: array of record
    A: array of real;
    B,K: real;
  end;
  t: integer;

  function DeltaTime(Time: integer): real;
  begin
    Result := 1;//TimeWeight(Time, T);
  end;

  function A_div(j: integer): real;
  var
    i: integer;
  begin
    Result := 0;
    for i := 0 to N-1 do // [1,N]
      Result := Result + Sqr(L[j].A[i]);
  end;

  function B(k: integer): real;
  var
    j: integer;
  begin
    Result := 0;
    for j := 0 to High(L) do  // [1,m]
      Result := Result + 2 * (L[j].K * DeltaTime(RecList[j].Time)) * L[j].A[k] * L[j].B / A_div(j);
  end;

  function A(k,i: integer): real;
  var
    j: integer;
  begin
    Result := 0;
    for j := 0 to High(L) do  // [1,m]
      Result := Result + 2 * (L[j].K * DeltaTime(RecList[j].Time)) * L[j].A[k] *  L[j].A[i] / A_div(j);
  end;

var
  i,k: integer;
  Sys: TLineSystem;
  Eq, Res: TExtendedArray;
begin
  { подготовка массива L }
  SetLength(L ,Length(RecList));
  for i := 0 to High(L) do
  begin
    SetLength(L[i].A, N);
    L[i].A[0] := RecList[i].Carbs;
    L[i].A[1] := -RecList[i].Ins;
    L[i].A[2] := RecList[i].Prots;
    L[i].B := RecList[i].BSOut-RecList[i].BSIn;
    L[i].K := 1;//RecList[i].Weight; {***}
  end;

  { обработка }
  Sys := TLineSystem.Create(N);
  SetLength(Eq, N+1);

  for t := 0 to MinPerDay-1 do
  begin      
    Sys.InitSystem(N);
    for k := 0 to N-1 do
    begin
      { формируем строку Eq }
      Eq[0] := B(k);
      for i := 1 to N do
        Eq[i] := A(k, i-1);
      { добавляем в систему }
      Sys.AddEquation(Eq);
    end;

    { решаем систему }
    Sys.Solve(Res);

    KoofList[t].k := Res[0];
    KoofList[t].q := Res[1];
    KoofList[t].p := Res[2];
  end;

  Sys.Free;
  Result := True;
end;
*)

function FAbs(const X,Y: real): real;
begin
  Result := abs(X - Y);
end;

function FSqr(const X,Y: real): real;
begin
  Result := sqr(X - Y);
end;

function FRel(const X,Y: real): real;
begin
  Result := abs(1 - X/Y);
end;

type
  TDifFunction = function(const X,Y: real): real;
  //TWeightedTimePointArray = array of TWeightedTimePoint;

{==============================================================================}
function GetDev(const RecList: TAnalyzeRecList; const KoofList: TKoofList; F: TDifFunction): real;
{==============================================================================}
var
  i,n: integer;
begin
  Result := 0;
  N := 0;
  for i := 0 to High(RecList) do
  begin
    Result := Result + F(
      RecList[i].BSIn
      + KoofList[RecList[i].Time].k * RecList[i].Carbs
      - KoofList[RecList[i].Time].q * RecList[i].Ins
      + KoofList[RecList[i].Time].p * RecList[i].Prots,
      RecList[i].BSOut);
    inc(N);
  end;

  if N > 0 then
    Result := Result / N;
end;

{==============================================================================}
function GetRand(const Z: TDayArray; const K: TWeightedTimePointArray; F: TDifFunction): real;
{==============================================================================}
var
  i,n: integer;
begin
  Result := 0;
  N := 0;
  for i := 0 to High(K) do
  if (not IsNAN(K[i].Value)) then
  begin
    Result := Result + F(Z[K[i].Time], K[i].Value);
    inc(N);
  end;

  if N > 0 then
    Result := Result / N;
end;

{==============================================================================}
function CalculateK(const Rec: TAnalyzeRec; const Q,P: Real): Real; overload;
{==============================================================================}
begin
  if Rec.Carbs > 0 then
    Result := (Rec.BSOut - Rec.BSIn + Rec.Ins * Q - Rec.Prots * P) / Rec.Carbs
  else
    Result := NaN;
end;

{==============================================================================}
procedure CalculateK(const Rec: TAnalyzeRec; const Q,P: Real; out K: TWeightedTimePoint); overload;
{==============================================================================}
begin
  K.Time := Rec.Time;
  K.Value := CalculateK(Rec, Q, P);
  K.Weight := {exp(-0.1*sqr(RecList[i].BSOut - 5.0)) *} Rec.Weight;
end;

{==============================================================================}
procedure CalculateK(const RecList: TAnalyzeRecList; const Q,P: real; out Ks: TWeightedTimePointArray); overload;
{==============================================================================}
var
  i: integer;
begin
  // расчёт K в точках
  SetLength(Ks, Length(RecList));
  for i := 0 to High(Ks) do
    CalculateK(RecList[i], Q, P, Ks[i]);
end;

{==============================================================================}
procedure CopyKQP(const Ks: TDayArray; const Q,P: real; out KoofList: TKoofList);
{==============================================================================}
var
  i: integer;
begin
  // считывание
  for i := 0 to MinPerDay - 1 do
  begin
    KoofList[i].k := Ks[i];
    KoofList[i].q := Q;
    KoofList[i].p := P;
  end;
end;

{==============================================================================}
function Info_BruteforceQP: PChar; StdCall;
{==============================================================================}
begin
  Result := 'Brute Force QP 1.0';
end;

{==============================================================================}
function Analyze_BruteforceQP(const RecList: TAnalyzeRecList;
  out KoofList: TKoofList; CallBack: TCallbackProgressProc = nil): boolean; StdCall;
{==============================================================================}
const
  MinQ   = 1.50;
  MaxQ   = 5.00;
  DiscQ  = 0.0125;
  CountQ = Round((MaxQ - MinQ) / DiscQ) + 1;

  MinP   = 0.00;
  MaxP   = 0.00;
  DiscP  = 0.05;
  CountP = Round((MaxP - MinP) / DiscP) + 1;

var
  V: array[0..CountQ - 1, 0..CountP - 1] of
  record
    Q, P: real;
    G: array[1..3] of Real;
  end;

  (*function GetAverage(Time: integer): real;
  var
    i: integer;
    Summ, SummWeight: real;
  begin
    { расставляем веса }
    for i := 0 to High(K) do
    if IsNAN(K[i].Value) then
      K[i].Weight := 0
    else
      K[i].Weight :=
        exp(-0.1*sqr(RecList[i].BSOut - 5.0)) *
        RecList[i].Weight * TimeWeight(Time, RecList[i].Time, 80);

    { считаем }
    Summ := 0;
    SummWeight := 0;
    for i := 0 to High(K) do
    begin
      SummWeight := SummWeight + K[i].Weight;
      Summ := Summ + K[i]*W[i];
    end;

    if SummWeight = 0 then
      Result := NAN
    else
      Result := Summ / SummWeight;
  end;    *)

  procedure NormalizeV;
  var
    i,j,k: integer;
    Min, Max: real;
  begin
    for k := 1 to 3 do
    begin
      Min := High(Integer);
      Max := Low(Integer);
      for i := 0 to CountQ - 1 do
      for j := 0 to CountP - 1 do
      if (not IsNAN(V[i,j].G[k])) then
      begin
        if (V[i,j].G[k] < Min) then Min := V[i,j].G[k];
        if (V[i,j].G[k] > Max) then Max := V[i,j].G[k];
      end;

      if (abs(Min - Max) > 0.0001) then
      begin
        for i := 0 to CountQ - 1 do
        for j := 0 to CountP - 1 do
        if (not IsNAN(V[i,j].G[k])) then
          V[i,j].G[k] := (V[i,j].G[k] - Min) / (Max - Min);
      end;
    end;
  end;

var
  Ks: TWeightedTimePointArray;
  Z: TDayArray;

  i, j: integer;

  CurDev, BestDev: real;
  BestQ: real;
  BestP: real;
begin
  { *** требования ***}
  { 1) Length(RecList)  > 0          }
  { 2) RecList[i].Carbs > 0          }
  { 3) Summ[ RecList[i].Weight ] > 0 }

  // Коэффициенты Q и P принимаются неизменными в течение суток
  // Коэффициент K - меняющимся

  if (Length(RecList) = 0) then
  begin
    for i := 0 to MinPerDay - 1 do
    begin
      KoofList[i].k := 0;
      KoofList[i].q := 0;
      KoofList[i].p := 0;
    end;
    Result := False;
    Exit;
  end;

  // инициализация
  SetLength(Ks, Length(RecList));

  //i := Length(RecList);
  //BestQ := i;

  // заполнение
  for i := 0 to CountQ - 1 do
  begin
    for j := 0 to CountP - 1 do
    begin
      { выставляем значения }

      V[i,j].Q := MinQ + i * DiscQ;
      V[i,j].P := MinP + j * DiscP;

      { оцениваем }

      // находим K в точках, считаем среднюю кривую
      CalculateK(RecList, V[i,j].Q, V[i,j].P, Ks);
      Approximate(Ks, 90, Z);
      CopyKQP(Z, V[i,j].Q, V[i,j].P, KoofList); // for GetDev only

      // находим оценки
      V[i,j].G[1] := GetRand(Z, Ks, FRel);//GetDev(FAbs);
      V[i,j].G[2] := 0;//GetRand(Z, Ks, FAbs);
      V[i,j].G[3] := GetDev(RecList, KoofList, FSqr);
    end;
    if Assigned(CallBack) then
      CallBack((i+1) * 100 div CountQ);
  end;

  NormalizeV;
  BestDev := High(Integer);

  // to avoid compiler's warning
  BestQ := 0;
  BestP := 0;

  for i := 0 to CountQ - 1 do
  for j := 0 to CountP - 1 do
  begin
    CurDev := Sqr(V[i,j].G[1]) + Sqr(V[i,j].G[2]) + Sqr(V[i,j].G[3]);
    if (CurDev < BestDev) then
    begin
      BestDev := CurDev;
      BestQ := V[i,j].Q;
      BestP := V[i,j].P;
    end;
  end;

  // находим K в точках, считаем среднюю кривую
  CalculateK(RecList, BestQ, BestP, Ks);
  Approximate(Ks, 90, Z);

  // копируем в массив коэффициентов
  CopyKQP(Z, BestQ, BestP, KoofList);

 { if Assigned(CallBack) then
    CallBack(100);   }

  Result := True;
end;

{==============================================================================}
function Analyze_AutoQ(const RecList: TAnalyzeRecList;
  out KoofList: TKoofList; CallBack: TCallbackProgressProc = nil): boolean; StdCall;
{==============================================================================}
var
  Data: TWeightedTimePointArray;
  Forces: TWeightedTimePointArray;
  ForceField: TDayArray;

  procedure InitData;
  var
    i: integer;
  begin
    SetLength(Forces, Length(RecList));
    SetLength(Data, Length(RecList));
    for i := 0 to High(Data) do
    begin
      Data[i].Time := Reclist[i].Time;
      //Data[i].Weight := RecList[i].Weight;
      Forces[i].Time := RecList[i].Time;
      Forces[i].Weight := 1;
    end;
  end;

  { полностью пересчитывает K }
  procedure RecalcK;
  var
    i: integer;
    Z: TDayArray;
  begin
    // расчёт K в точках
    for i := 0 to High(Data) do
    if RecList[i].Carbs > 0 then
    begin
      //Data[i].Time := RecList[i].Time;
      Data[i].Value := (
        RecList[i].BSOut - RecList[i].BSIn +
        RecList[i].Ins * KoofList[RecList[i].Time].Q -
        RecList[i].Prots * KoofList[RecList[i].Time].P) / RecList[i].Carbs;
      Data[i].Weight := {exp(-0.1*sqr(RecList[i].BSOut - 5.0)) *} RecList[i].Weight;
    end else
      Data[i].Value := NAN;

    // расчёт средних
    Approximate(Data, 80, Z);

    // считывание
    for i := 0 to MinPerDay-1 do
      KoofList[i].k := Z[i];
  end;

  { нормализация по модулю, возвращает среднее значение }
  function GetAverageForce: real;
  var
    i: integer;
  begin
    Result := 0;
    for i := 0 to MinPerDay-1 do
      Result := Result + abs(ForceField[i]);

    Result := Result / MinPerDay;
  end;

  procedure NormalizeForceField;
  var
    i: integer;
    Max: real;
  begin
    Max := 0;
    for i := 0 to MinPerDay-1 do
    begin
      if abs(ForceField[i]) > Max then
        Max := abs(ForceField[i]);
    end;

    if Max > 0 then
    for i := 0 to MinPerDay-1 do
      ForceField[i] := ForceField[i] / Max;
  end;

  function ExpectedBS(n: integer): real;
  {var
    Ins: real;  }
  begin
    Result := RecList[n].BSIn
      + RecList[n].Carbs * KoofList[RecList[n].Time].k
      - RecList[n].Ins   * KoofList[RecList[n].Time].q
      + RecList[n].Prots * KoofList[RecList[n].Time].p
    ;
    {Ins := (RecList[n].BSIn - 5.0 + KoofList[RecList[n].Time].k*RecList[n].Carbs)/KoofList[RecList[n].Time].q;
    Result := RecList[n].BSIn
      + RecList[n].Carbs * KoofList[RecList[n].Time].k
      - Ins              * KoofList[RecList[n].Time].q
      + RecList[n].Prots * KoofList[RecList[n].Time].p
    ;  }
    //Result := 5.0;
  end;

const
  EPS         = 0.100;
  STEP        = 0.050;
  INIT_Q      = 4;
  MAX_COUNTER = 5;
var
  i,counter: integer;
  BSTrg, AvgForce, V: real;
begin
  { *** требования ***}
  { 1) Length(RecList)  > 0 }
  { 2) RecList[i].Carbs > 0 }

  if Length(RecList) = 0 then
  begin
    Result := False;
    Exit;
  end;

  { инициализация }
  for i := 0 to MinPerDay-1 do
  begin
    KoofList[i].q := INIT_Q;
    KoofList[i].p := 0;
  end;
  InitData;
  Counter := 0;

  repeat
    { вычисляем K }
    RecalcK;

    { вычисляем силы }
    for i := 0 to High(Forces) do
    begin
      BSTrg := ExpectedBS(i);
      if (RecList[i].BSOut - RecList[i].BSIn) * (BSTrg - RecList[i].BSIn) >= 0 then
      begin
        V := Sqr(RecList[i].BSOut - BSTrg) {* Sqr(RecList[i].BSOut - RecList[i].BSIn)};

        if ((RecList[i].BSIn >= RecList[i].BSOut) and (RecList[i].BSOut > BSTrg)) or     // недолёт сверху
           ((RecList[i].BSIn <= RecList[i].BSOut) and (RecList[i].BSOut < BSTrg)) then   // недолёт снизу
          Forces[i].Value := - V else

        if ((RecList[i].BSIn >= BSTrg) and (BSTrg > RecList[i].BSOut)) or     // перелёт сверху
           ((RecList[i].BSIn <= BSTrg) and (BSTrg < RecList[i].BSOut)) then   // перелёт снизу
          Forces[i].Value := + V

        else
          Forces[i].Value := 0; // на всякий случай

        {if RecList[i].BSOut < BSTrg then
          Forces[i].Value := +V
        else
          Forces[i].Value := -V; }
      end else
        Forces[i].Value := 0;
    end;

    { аппроксимируем }
    Approximate(Forces, 10, ForceField);

    { определяем AvgForce }
    AvgForce := GetAverageForce;

    { нормализуем силы }
    NormalizeForceField;

    { применяем силы }
    for i := 0 to MinPerDay-1 do
      KoofList[i].q := KoofList[i].q + ForceField[i] * STEP;

    inc(Counter);

    if Assigned(CallBack) then
      CallBack(Counter * 100 div MAX_COUNTER);
  until (AvgForce < Eps) or (Counter >= MAX_COUNTER);

  if Assigned(CallBack) then
    CallBack(100);
  Result := True;
end;

{==============================================================================}
function Info_HardMath: PChar; StdCall;
{==============================================================================}
begin
  Result := 'HardMath 1.0';
end;

{==============================================================================}
function Analyze_HardMath(const RecList: TAnalyzeRecList;
  out KoofList: TKoofList; CallBack: TCallbackProgressProc = nil): boolean; StdCall;
{==============================================================================}
var
  A, F, At, X, W, WA, WF: TMatrix;
  i,n: integer;
  //tick: cardinal;
begin
  if Length(RecList) = 0 then
  begin
    Result := False;
    Exit;
  end;

  { prots(p), /fats(f)/, carbs(k), ins(q) }

  //tick := GetTickCount;

  A := TMatrix.Create(Length(RecList), 3);
  F := TMatrix.Create(Length(RecList), 1);
  W := TMatrix.Create(Length(RecList), Length(RecList));

  try
    // init matrixes
    for i := 0 to A.Rows - 1 do
    begin
      A[i, 0] := RecList[i].Prots;
      //A[i, 1] := RecList[i].Fats;
      A[i, 1] := RecList[i].Carbs;
      A[i, 2] := -RecList[i].Ins;
      F[i, 0] := RecList[i].BSOut - RecList[i].BSIn;
    end;

    // solve fow every 1440 point
    for n := 0 to MinPerDay - 1 do
    begin
      for i := 0 to A.Rows - 1 do
      begin
        W[i, i] := Sqr(RecList[i].BSOut - RecList[i].BSIn) * TimeWeight(RecList[i].Time, n, 0) * Sqrt(RecList[i].Carbs) * 10;
      end;

      try
        WA := W.Mul(A);
        WF := W.Mul(F);
        At := WA.Transpose();
        try
          X := TMatrix.SolveCramer(At.Mul(WA), At.Mul(WF));
          KoofList[n].p := X[0, 0];
          KoofList[n].k := X[1, 0];
          KoofList[n].q := X[2, 0];
          X.Free;
        finally
          WA.Free;
          WF.Free;
          At.Free;
        end;
      except
        on E: EMatrixException do
        begin
          Result := False;

          KoofList[n].p := 0;
          KoofList[n].k := 0;
          KoofList[n].q := 0;
        end;
      end;
    end;

    Result := True;
  finally
    A.Free;
    F.Free;
    W.Free;
  end;
  //ShowMessage(IntToStr(GetTIckCount - tick));
end;

exports
  Analyze_BruteforceQP name AnalyzeFunctionName + '0',
  Info_BruteforceQP    name InfoFunctionName    + '0'

  //Analyze_HardMath     name AnalyzeFunctionName + '1',
  //Info_HardMath        name InfoFunctionName    + '1',

  ;

  //Analyze_Math name AnalyzeFunctionName;
  //Analyze_AutoQ name AnalyzeFunctionName;
  //Analyze_BruteforceQP name AnalyzeFunctionName;

begin
  PrepareTimeWeights(40);

  { ИДЕИ
    1. Вычислять коэффициенты не с нуля, а последовательными приближениями
    2. Пересмотреть линейную модель
  }
end.

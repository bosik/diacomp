unit AnalyzeUtils;

interface

uses
  Math,
  AnalyzeInterface,
  DiaryRoutines;

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

  TDifFunction = function(const X,Y: real): real;
  //TWeightedTimePointArray = array of TWeightedTimePoint;

  function TimeWeight(T1, T2: integer; K: real): real;
  procedure Approximate(const Data: TWeightedTimePointArray; Factor: real; out Result: TDayArray);
  function CalculateK(const Rec: TAnalyzeRec; const Q,P: Real): Real; overload;
  procedure CalculateK(const Rec: TAnalyzeRec; const Q,P: Real; out K: TWeightedTimePoint); overload;
  procedure CalculateK(const RecList: TAnalyzeRecList; const Q,P: real; out Ks: TWeightedTimePointArray); overload;
  procedure CopyKQP(const Ks: TDayArray; const Q,P: real; out KoofList: TKoofList);
  // function Remove(const K: TWeightedTimePointArray; index: integer): TWeightedTimePointArray;
  function Filter(const Data: TWeightedTimePointArray; const Factor, Limit: Real): TWeightedTimePointArray;

  function GetDev(const RecList: TAnalyzeRecList; const KoofList: TKoofList; F: TDifFunction): real;
  function GetRand(const Z: TDayArray; const K: TWeightedTimePointArray; F: TDifFunction): real;

  function FAbs(const X,Y: real): real;
  function FSqr(const X,Y: real): real;
  function FRel(const X,Y: real): real;

var
  TIME_WEIGHTS: array[0..HalfMinPerDay + 1] of Real;

implementation

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
procedure Approximate(const Data: TWeightedTimePointArray; Factor: real; out Result: TDayArray);
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
function Remove(const K: TWeightedTimePointArray; index: integer): TWeightedTimePointArray;
{==============================================================================}
begin
  Result := Copy(K, Low(K), Length(K));
  Result[Index] := Result[High(Result)];
  SetLength(Result, Length(Result) - 1);
end;

{==============================================================================}
function Filter(const Data: TWeightedTimePointArray; const Factor, Limit: Real): TWeightedTimePointArray;
{==============================================================================}

// Limit: 0.05 (very strict) - 0.60 (very moderate)

var
  i, count: integer;
  Average: Real;
begin
  SetLength(Result, Length(Data));
  count := 0;

  for i := Low(Data) to High(Data) do
  begin
    Average := ApproximatePoint(Remove(Data, i), Factor, Data[i].Time);

    if (abs(Data[i].Value - Average) / Average < Limit) then
    begin
      Result[count] := Data[i];
      inc(count);
    end;
  end;

  SetLength(Result, count);
end;

initialization
  PrepareTimeWeights(40);

end.

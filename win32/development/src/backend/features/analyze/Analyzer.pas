unit Analyzer;

{$R+}

interface

uses
  Matrixes,
  Math,
  SysUtils,
  AnalyzeInterface,
  DiaryRecords,
  BusinessObjects,
  DiaryRoutines,
  Bases,
  AutoLog,
  AnalyzeUtils;

type
  TAnalyzerBruteforceQP = class(TAnalyzer)
  public
    function Analyze(const RecList: TAnalyzeRecList; out KoofList: TKoofList; CallBack: TCallbackProgressProc = nil): boolean; override;
    function GetName: PChar; override;
  end;

implementation

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

{==============================================================================}
function TAnalyzerBruteforceQP.GetName(): PChar;
{==============================================================================}
begin
  Result := 'Brute Force QP 1.0';
end;

{==============================================================================}
function TAnalyzerBruteforceQP.Analyze(const RecList: TAnalyzeRecList;
  out KoofList: TKoofList; CallBack: TCallbackProgressProc = nil): boolean;
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

const
  APPROX_FACTOR = 90;
  FILTER_LIMIT  = 0.5;
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
      Ks := Filter(Ks, APPROX_FACTOR, FILTER_LIMIT);
      Approximate(Ks, APPROX_FACTOR, Z);
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
  Ks := Filter(Ks, APPROX_FACTOR, FILTER_LIMIT);
  Approximate(Ks, APPROX_FACTOR, Z);

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
  for i := 0 to MinPerDay - 1 do
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
    for i := 0 to MinPerDay - 1 do
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
          FreeAndNil(X);
        finally
          FreeAndNil(WA);
          FreeAndNil(WF);
          FreeAndNil(At);
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
    FreeAndNil(A);
    FreeAndNil(F);
    FreeAndNil(W);
  end;
  //ShowMessage(IntToStr(GetTIckCount - tick));
end;

  { ИДЕИ
    1. Вычислять коэффициенты не с нуля, а последовательными приближениями
    2. Пересмотреть линейную модель
  }
end.

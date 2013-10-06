unit DiaryAnalyze;

interface

uses
  SysUtils,
  Windows,
  Math,
  AnalyzeInterface,
  DiaryRoutines,
  DiaryRecords,
  DiaryDatabase;

type
  TAnalyzer = record
  // private
    AnalyzeFunc: TAnalyzeFunction;
    InfoFunc: TInfoFunction;
  // public
    Name: string;
    KoofList: TKoofList;   // ������������
  end;

  TValFunction = (vfLinearAbs, vfLinearAvg, vfDistance, vfQuadric);
  TRealArray = array of Real;

  { ������������� }
  function AddAnalyzer(const FileName: string): boolean;
  procedure AnalyzeDiary(Base: TDiary; FromDate, ToDate: TDate; const Par: TRealArray; CallBack: TCallbackProgressProc);

  { ������������� }
  function GetAnalyzersCount: integer;
  function GetAnalyzer(Index: integer): TAnalyzer;
  function GetKoof(Time: integer): TKoof; overload;
  function GetKoof(Time: integer; AnalyzerIndex: integer): TKoof; overload;

  { ������ ����������� }
  function GetRecError(const Rec: TAnalyzeRec; const KoofList: TKoofList; ValFunct: TValFunction): real;
  function GetRecListError(const List: TAnalyzeRecList; const KoofList: TKoofList; ValFunc: TValFunction): real;


var
  AnList: TAnalyzeRecList;
  AvgKoofList: TKoofList;
  
implementation

var
  Analyzer: array of TAnalyzer; // TODO: hide it

type
  // #dao
  TPrimeRec = record
    Date: TDate; // ��� �����������

    BloodInTime: integer;  // � �������, �������� ������ 1440
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

{ ���������� ��������� }

{==============================================================================}
procedure ExtractRecords(Base: TDiary; FromDate, ToDate: TDate;
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

  { ��������� }

  { 1. ������ RecList, ������ ����� � ������� �� 01/01/1899 }
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
        { ������ }

        {if (((TimeF + 1440) mod 1440) > 780)and
           (((TimeF + 1440) mod 1440) < 840)
        then  }
        begin
          j := length(List);
          SetLength(List,j+1);
          List[j].BloodInTime := PrevBloodTime;
          List[j].BloodInValue := PrevBloodValue;
          List[j].InsTime := TimeI;  { ���������� ����� }
          List[j].InsValue := Ins;
          List[j].FoodTime := TimeF; { ���������� ����� }
          List[j].Prots := Prots;
          List[j].Fats := Fats;
          List[j].Carbs := Carbs;
          List[j].BloodOutTime := PageBaseTime + Base[Date][i].Time;
          List[j].BloodOutValue := TBloodRecord(Base[Date][i]).Value;
          List[j].Date := MealDate;
        end;

        { ���������� � ���������� ����� }
        PrevBloodTime := PageBaseTime + Base[Date][i].Time;
        PrevBloodValue := TBloodRecord(Base[Date][i]).Value;
        InitCounters;
      end else
      begin
        { ����� ����������, �� ����� ��� �� ���, �� �������� }
        PrevBloodTime := PageBaseTime + TBloodRecord(Base[Date][i]).Time;
        PrevBloodValue := TBloodRecord(Base[Date][i]).Value;
        InitCounters;
      end;
    end;
    {=====================================================}
  end;

  { 2. �������������� ������������� ����� }

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
{ �������� �������� ���� � ��������� ���������� ���� }

  // Adaptation in [0..0.5]:
  // 0.0 is the quickest (but unstable),
  // 0.5 is the slowest (but very stable)

  function F(const X: real): real;
  { Adaptation in [0, 0.5] }
  { X from [0, 1] }
  begin
    Result := (Adaptation - 0.5) * sin(pi * (X - 0.5)) + 0.5;
  end;

var
  i: integer;
  CurTime: TDateTime;
  Min: TDate;
  MinW, MaxW: real;
begin
  CurTime := now;
  Min := Trunc(CurTime);

  for i := 0 to High(PrimeList) do
  if PrimeList[i].Date < Min then
    Min := PrimeList[i].Date;

  Min := Min - 1; // muahahahaha  

  SetLength(List, Length(PrimeList));
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

  { ������������ }
  if Length(List) > 0 then
  begin
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
begin
  if FileExists(FileName) then
  try
    Lib := LoadLibrary(PChar(FileName));
    if (Lib <> 0) then
    begin    
      @Temp.AnalyzeFunc := GetProcAddress(Lib, AnalyzeFunctionName);
      @Temp.InfoFunc := GetProcAddress(Lib, InfoFunctionName);
      Result :=
        (@Temp.AnalyzeFunc <> nil) and
        (@Temp.InfoFunc <> nil);

      if Result then
      begin
        Temp.Name := Temp.InfoFunc;

        SetLength(Analyzer, Length(Analyzer) + 1);
        Analyzer[High(Analyzer)] := Temp;
      end;
    end else
      Result := False;
  except
    Result := False;
  end else
    Result := False
end;

{==============================================================================}
procedure AnalyzeDiary(Base: TDiary; FromDate, ToDate: TDate; const Par: TRealArray; CallBack: TCallbackProgressProc);
{==============================================================================}
var
  PrimeList: TPrimeRecList;
  i, Time: integer;
begin
  if (Length(Analyzer) = 0) then
    raise Exception.Create('No analyzers loaded');

  // ���������� ���������
  ExtractRecords(Base, FromDate, ToDate, PrimeList);
  FormatRecords(PrimeList, AnList, Par[PAR_ADAPTATION]);

  // ���������� ������
  for i := 0 to High(Analyzer) do
  begin
    // TODO: incapsulate as method "TAnalyzer.Analyze(AnList)"
    Analyzer[i].AnalyzeFunc(AnList, Analyzer[i].KoofList, CallBack);
  end;

  // ����������� ������� ������������
  for Time := 0 to MinPerDay - 1 do
  begin
    AvgKoofList[Time].k := 0;
    AvgKoofList[Time].q := 0;
    AvgKoofList[Time].p := 0;

    for i := 0 to High(Analyzer) do
    begin
      AvgKoofList[Time].k := AvgKoofList[Time].k + Analyzer[i].KoofList[Time].k;
      AvgKoofList[Time].q := AvgKoofList[Time].q + Analyzer[i].KoofList[Time].q;
      AvgKoofList[Time].p := AvgKoofList[Time].p + Analyzer[i].KoofList[Time].p;
    end;

    AvgKoofList[Time].k := AvgKoofList[Time].k / Length(Analyzer);
    AvgKoofList[Time].q := AvgKoofList[Time].q / Length(Analyzer);
    AvgKoofList[Time].p := AvgKoofList[Time].p / Length(Analyzer);
  end;
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
    Rec.Carbs * Koof.k +
    Rec.Prots * Koof.p -
    Rec.Ins * Koof.q
    - (Rec.BSOut - Rec.BSIn);

  case ValFunct of
    vfLinearAbs: result := abs(err);
    vfLinearAvg: result := err;
    vfDistance: result := abs(err) / Sqrt(Sqr(Rec.Carbs) + Sqr(Rec.Prots) + Sqr(Rec.Ins));
    vfQuadric: Result := Sqr(err);
    else raise Exception.Create('GetRecError: ������������ �������� ValFunct');
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
  Result := AvgKoofList[Time];
end;

{==============================================================================}
function GetKoof(Time: integer; AnalyzerIndex: integer): TKoof;
{==============================================================================}
begin
  Result := GetAnalyzer(AnalyzerIndex).KoofList[Time];
end;

end.


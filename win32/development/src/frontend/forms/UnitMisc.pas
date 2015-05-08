unit UnitMisc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DiaryCore, BusinessObjects, DiaryRecords,
  DiaryAnalyze {TODO: remove}, AnalyzeInterface, DiaryRoutines, FoodBaseDAO,
  SettingsINI, Math, Statistics, ExtCtrls, JsonSerializer, DiaryPageSerializer, uLkJSON;

type
  TFormMisc = class(TForm)
    ButtonExportXml: TButton;
    ButtonExportJson: TButton;
    ButtonBruteforce: TButton;
    Label1: TLabel;
    Button5: TButton;
    Button4: TButton;
    Button3: TButton;
    Button6: TButton;
    Button8: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    Timer1: TTimer;
    ButtonCovariance: TButton;
    ButtonExportRaw: TButton;
    Button1: TButton;
    ButtonFoodBSCorrelation: TButton;
    ButtonVerifyLinear: TButton;
    ButtonExportFoodBase: TButton;
    ButtonFoodCompare: TButton;
    procedure ButtonExportXmlClick(Sender: TObject);
    procedure ButtonExportJsonClick(Sender: TObject);
    procedure ButtonBruteforceClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ButtonCovarianceClick(Sender: TObject);
    procedure ButtonExportRawClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonFoodBSCorrelationClick(Sender: TObject);
    procedure ButtonVerifyLinearClick(Sender: TObject);
    procedure ButtonExportFoodBaseClick(Sender: TObject);
    procedure ButtonFoodCompareClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMisc: TFormMisc;

implementation

uses MainUnit;

{$R *.dfm}

procedure TFormMisc.ButtonExportXmlClick(Sender: TObject);
var
  tick: cardinal;
begin
  tick := GetTickCount;
  //FoodBase.SaveToXML('FoodBase.xml');
  //DishBase.SaveToXML('DishBase.xml');
  Diary.SaveToXML('temp\Diary.xml');
  ShowMessage(Format('Time: %d', [GetTickCount - tick]));
end;

procedure TFormMisc.ButtonExportJsonClick(Sender: TObject);
var
  tick: cardinal;
begin
  tick := GetTickCount;
  Diary.SaveToJSON('temp\Diary.json');
  ShowMessage(Format('Time: %d', [GetTickCount - tick]));
end;

procedure TFormMisc.ButtonBruteforceClick(Sender: TObject);
{var
  CurrentTime: integer;
  W: array of Real;

  procedure CalcWeights;
  var
    i: integer;
    TimeDist: integer;
  begin
    SetLength(W, Length(AnalyzeResult[0].AnList));
    for i := 0 to High(W) do
    begin
      TimeDist := abs(AnalyzeResult.AnList[i].Time - CurrentTime);
      TimeDist := Min(TimeDist, 1440 - TimeDist);
      W[i] := AnalyzeResult.AnList[i].Weight * Exp(-0.000001 * TimeDist * TimeDist);
    end;
  end;

  function Value(const K, Q, P: Real): Real;
  var
    i: integer;
    Expected: Real;
  begin
    Result := 0;
    for i := 0 to High(AnalyzeResult.AnList) do
    begin
      Expected :=
        AnalyzeResult.AnList[i].BSIn
        + K * AnalyzeResult.AnList[i].Carbs
        - Q * AnalyzeResult.AnList[i].Ins
        + P * AnalyzeResult.AnList[i].Prots;
      Result := Result + W[i] * Sqr(AnalyzeResult.AnList[i].BSOut - Expected);
    end;
  end;

const
  MIN_K = 0.0;  MAX_K = 1.0;  DISC_K = 0.001;
  MIN_Q = 0.0;  MAX_Q = 6.0;  DISC_Q = 0.001;
  MIN_P = 0.0;  MAX_P = 1.0;  DISC_P = 0.001;
var
  K, Q, P: real;
  Cur, Best: Real;
  BestK, BestQ, BestP: Real;      }
begin
 { CurrentTime := GetCurrentMinutes();
  CalcWeights;
  Best := High(Integer);

  BestK := 0;
  BestQ := 0;
  BestP := 0;

  K := MIN_K;
  while (K < MAX_K) do
  begin
    Q := MIN_Q;
    while (Q < MAX_Q) do
    begin
      P := MIN_P;
      while (P < MAX_P) do
      begin
        Cur := Value(K, Q, P);
        if (Cur < Best) then
        begin
          Best := Cur;
          BestK := K;
          BestQ := Q;
          BestP := P;
          Label1.Caption := Format('K = %.3f; Q = %.3f; P = %.3f', [BestK, BestQ, BestP]);
        end;
        P := P + DISC_P;
      end;
      Q := Q + DISC_Q;
    end;
    K := K + DISC_K;
    Label1.Caption := IntToStr(Round((K - MIN_K) / (MAX_K - MIN_K) * 100)) + '%';
    Application.ProcessMessages;
  end;

  ShowMessage(Format('K = %.3f'#13'Q = %.3f'#13'P = %.3f', [BestK, BestQ, BestP]));  }
end;

procedure TFormMisc.Button5Click(Sender: TObject);
(*var
  CurrentTime: integer;
  W: array of Real;

  procedure CalcWeights;
  var
    i: integer;
    TimeDist: integer;
  begin
    SetLength(W, Length(AnList));
    for i := 0 to High(W) do
    begin
      TimeDist := abs(AnList[i].Time - CurrentTime);
      TimeDist := Min(TimeDist, 1440 - TimeDist);
      W[i] := {Sqr(AnList[i].BSOut - AnList[i].BSIn) *} AnList[i].Weight * Exp(-0.0001 * TimeDist * TimeDist);
    end;
  end;

  function Value(const K, Q, P: Real): Real;
  var
    i: integer;
    Expected: Real;
  begin
    Result := 0;
    for i := 0 to High(AnList) do
    begin
      Expected :=
        AnList[i].BSIn
        + K * AnList[i].Carbs
        - Q * AnList[i].Ins
        + P * AnList[i].Prots;
      Result := Result + W[i] * Sqr(AnList[i].BSOut - Expected);


      {Expected := Sqr(AnList[i].BSIn + K * AnList[i].Carbs - Q * AnList[i].Ins + P * AnList[i].Prots - AnList[i].BSOut);
      Expected := Expected / (Sqr(AnList[i].Carbs) + Sqr(AnList[i].Ins) + Sqr(AnList[i].Prots));
      Result := Result + W[i] * Expected;      }
    end;
  end;

const
  MIN_K = 0.0;  MAX_K = 1.0;  DISC_K = 0.001;
  MIN_Q = 0.0;  MAX_Q = 6.0;  DISC_Q = 0.001;
  MIN_P = 0.0;  MAX_P = 0.5;  DISC_P = 0.001;
  DISC = 0.000001;
var
  K, Q, P: real;
  DK, DQ, DP: Real;
  L: Real;
  SpeedK: Real;
  SpeedQ: Real;
  SpeedP: Real;
  OldDK, OldDQ, OldDP: Real;
  Val, OldVal: Real;
  n: integer;    *)
begin  (*
  for CurrentTime := 0 to MinPerDay - 1 do
  begin
    CalcWeights;

    K := (MIN_K + MAX_K) / 2;
    Q := (MIN_Q + MAX_Q) / 2;
    P := (MIN_P + MAX_P) / 2;

    SpeedK := 0.1;
    SpeedQ := 0.1;
    SpeedP := 0.1;

    {DK := 0;
    DQ := 0;
    DP := 0; }
    Val := High(Integer);

    repeat
      // определяем скорость
      OldVal := Val;
      Val := Value(K, Q, P);
      if (Val > OldVal) then
      begin
        SpeedK := SpeedK * 0.5;
        SpeedQ := SpeedQ * 0.5;
        SpeedP := SpeedP * 0.5;
      end;

      // определяем направление
      DK := (Value(K + DISC, Q, P) - Value(K - DISC, Q, P)) / 2;
      DQ := (Value(K, Q + DISC, P) - Value(K, Q - DISC, P)) / 2;
      DP := (Value(K, Q, P + DISC) - Value(K, Q, P - DISC)) / 2;

      L := Sqrt(Sqr(DK) + Sqr(DQ) + Sqr(DP));
      DK := DK / L;
      DQ := DQ / L;
      DP := DP / L;

      // идём
      K := K - DK * SpeedK * (MAX_K - MIN_K);
      Q := Q - DQ * SpeedQ * (MAX_Q - MIN_Q);
      P := P - DP * SpeedP * (MAX_P - MIN_P);

      StatusBar.Panels[1].Text := IntToStr((CurrentTime + 1) * 100 div 1440) + '%';
      Application.ProcessMessages;
    //until (L < 0.0001);
    until (Sqr(SpeedK) + Sqr(SpeedQ) + Sqr(SpeedP) < 0.000000001);

    KoofList[CurrentTime].k := k;
    KoofList[CurrentTime].q := q;
    KoofList[CurrentTime].p := p;
  end;
  //ShowMessage(IntToStr(n));

  StatusBar.Panels[0].Text := 'Вычислено';
  Application.ProcessMessages;     *)
end;

procedure TFormMisc.Button4Click(Sender: TObject);

{  function GetTime(w: integer): integer;
  begin
    if w>=0 then
      Result := w else
      Result := w+MinPerDay;
  end;    }

{var
  Cloud: TPointsCloud;
  temp: TRecordList;
  i: integer; }
begin
(*
  SetLength(recs,0);
  for i := 0 to DiaryBase.Count-1 do
  begin
    if i=DiaryBase.Count-1 then
      ExtractRecords(DiaryBase[i],DiaryBase[i],i,True,temp)
    else
      ExtractRecords(DiaryBase[i],DiaryBase[i + 1], i, False, temp);
    CompareRecords(recs,temp);
  end;
  for i := 0 to High(recs) do
  if (recs[i].Carbs>0){and
     (recs[i].Fats>0)} then
    AddPoint(
      Cloud,
      {-------------------------------------------------}
      //(recs[i].Carbs)/(recs[i].Prots+recs[i].Fats+recs[i].Carbs),
      //recs[i].Prots,
      //recs[i].Fats,
      //recs[i].InsValue,
      //recs[i].Carbs,
      //i,
      //(recs[i].Carbs-recs[i].BloodOutValue-5)/0.25,
      GetTime(recs[i].BloodOutTime-recs[i].BloodInTime)/60,

      {-------------------------------------------------}
      //recs[i].InsValue,
      //recs[i].InsValue+(recs[i].BloodOutValue-5)/10,
      (recs[i].BloodOutValue-recs[i].BloodInValue+
       InsulinCost*recs[i].InsValue )/(recs[i].Carbs)/InsulinCost,

      {-------------------------------------------------}
      //recs[i].BloodOutTime-recs[i].BloodInTime
      //recs[i].Fats
      //(recs[i].BloodOutValue-recs[i].BloodInValue+
      // 5.2*recs[i].InsValue )/(recs[i].Carbs)/4.5
      //recs[i].InsValue
      //recs[i].BloodInTime
      i
      (*
      recs[i].BloodOutTime-recs[i].BloodInTime,

      recs[i].InsValue,
      //{recs[i].BloodOutValue-}recs[i].BloodInValue,

      (recs[i].BloodOutValue-recs[i].BloodInValue+
       4.5*recs[i].InsValue )/(recs[i].Carbs)/4.5,
       
      //recs[i].BloodOutTime-recs[i].BloodInTime
    );

//  K = (Out-In  + Q*Ins )/Carbs

  DrawCloud(Cloud,ImageLarge,1,0.1);   *)
end;

procedure TFormMisc.Button3Click(Sender: TObject);
var
  DaySumm: integer;
  PercentProts: real;
  PercentFats: real;
  PercentCarbs: real;   

  function F(Food: TFoodItem): real; overload;
  var
    summ: real;
  begin
    summ := Food.RelProts + Food.RelFats + Food.RelCarbs;
    if summ = 0 then
      Result := 100500
    else
      Result :=
        sqr(Food.RelProts/summ - PercentProts)+
        sqr(Food.RelFats/summ  - PercentFats)+
        sqr(Food.RelCarbs/summ - PercentCarbs)
  end;

  function F(Food: TDishItem): real; overload;
  var
    summ: real;
  begin
    summ := Food.RelProts + Food.RelFats + Food.RelCarbs;
    if summ = 0 then
      Result := 100500
    else
      Result :=
        sqr(Food.RelProts/summ - PercentProts)+
        sqr(Food.RelFats/summ  - PercentFats)+
        sqr(Food.RelCarbs/summ - PercentCarbs)
  end;

var
  i: integer;
  s: TStrings;
  FoodList: TFoodItemList;
begin
  DaySumm := Value['NormProts'] + Value['NormFats'] + Value['NormCarbs'];
  PercentProts := Value['NormProts'] / DaySumm ;
  PercentFats  := Value['NormFats']  / DaySumm ;
  PercentCarbs := Value['NormCarbs'] / DaySumm ;

  s := TStringList.Create;
  try
    FoodList := FoodBaseLocal.FindAll(false);
    for i := 0 to High(FoodList) do
      s.Add(FoodList[i].Name + #9 + FloatToStr(F(FoodList[i])));
    s.SaveToFile('FoodList.txt');
  finally
    s.Free;
  end;

 { s := TStringList.Create;
  try
    for i := 0 to DishBaseLocal.Count - 1 do
      s.Add(DishBase[i].Name + #9 + FloatToStr(F(DishBase[i])));
    s.SaveToFile('temp\DishList.txt');
  finally
    s.Free;
  end; }
  // TODO: restore
end;

procedure TFormMisc.Button6Click(Sender: TObject);
{var
  M: array of
  record
    Name: string;
    Mass: real;
    Count: integer;
  end;

  procedure Add(const Name: string; Mass: Real);
  var
    i: integer;
  begin
    for i := 0 to High(M) do
    if (M[i].Name = Name) then
    begin
      M[i].Mass := M[i].Mass + Mass;
      M[i].Count := M[i].Count + 1;
      Exit;
    end;
    SetLength(M, Length(M) + 1);
    M[High(M)].Name := Name;
    M[High(M)].Mass := Mass;
    M[High(M)].Count := 1;
  end;

var
  Meal: TMealRecord;
  s: TStringList;
  d, i, k: integer; }
begin
  // TODO: restore
  {for d := Trunc(GetTimeUTC() - 30) to Trunc(GetTimeUTC()) do
  for i := 0 to Diary[d].Count - 1 do
  if (Diary[d][i].RecType = TMealRecord) then
  begin
    Meal := TMealRecord(Diary[d][i]);
    for k := 0 to Meal.Count - 1 do
      Add(Meal[k].Name, Meal[k].Mass);
  end;

  s := TStringList.Create;
  for i := 0 to High(M) do
    S.Add(M[i].Name + #9 + FloatToStr(M[i].Mass / M[i].Count));
  S.SaveToFile('temp\average_mass.txt');
  S.Free; }
end;

procedure TFormMisc.Button8Click(Sender: TObject);

  // TODO: в отправленном логе об ошибке в качестве отправителя e-mail указывать логин пользователя
  // TODO: реализовать на сервере API для выгрузки некоторых настроек (целевой СК, нормы и т.п.)

 { procedure TestVersion;
  var
    Version: integer;
  begin
    Version := WebClient.GetFoodBaseVersion();
    ShowMessage(Format('Food base verson: %d', [Version]))
  end;

  procedure TestDownload;
  var
    Data: string;
  begin
    Data := WebClient.DownloadFoodBase();
    ShowMessage(Data);
  end;

  procedure TestUpload;
  var
    S: TStrings;
  begin
    S := TStringList.Create;
    try
      S.LoadFromFile(WORK_FOLDER + FoodBase_FileName);

      if WebClient.UploadFoodBase(S.Text, FoodBaseLocal.Version) then
        ShowMessage('Done')
      else
        ShowMessage('Can''t upload foodbase');
    finally
      S.Free;
    end;
  end;

  procedure TestReport(const S: string);
  begin
    if WebClient.Report(S) then
      ShowMessage('Reported OK')
    else
      ShowMessage('Can''t report');
  end;  }

begin
  // TestVersion;
  //TestDownload;
  //TestUpload;
  //SyncFoodbase;
  //TestReport('Hi! У меня тут ошибка, он что-то пишет :(');
end;

procedure TFormMisc.Timer1Timer(Sender: TObject);
{var
  tick: cardinal;    }
begin
{  if (Timer1.Tag = 1) then
  begin
    Timer1.Tag := 0;
    tick := GetTickCount();
    Memo1.Lines.Text := WebSource.Search(AnsiLowerCase(Trim(Edit1.Text)));
    sleep(1000);
    Label1.Caption := 'Time: ' + IntToStr(GetTickCount() - tick);
    //Windows.Beep(1000, 50);
  end; }
end;

procedure TFormMisc.Edit1Change(Sender: TObject);
begin
  Timer1.Tag := 1;
end;

procedure TFormMisc.ButtonCovarianceClick(Sender: TObject);
{type
  TRow = record
    Name: string;
    Data: array of Double;
  end;

  function ExpectedBS(i: integer): Double;
  var
    Koof: TKoof;
  begin
    Koof := GetKoof(AnalyzeResult.AnList[i].Time);
    Result := AnalyzeResult.AnList[i].BSIn + AnalyzeResult.AnList[i].Carbs * Koof.k + AnalyzeResult.AnList[i].Prots * Koof.p - AnalyzeResult.AnList[i].Ins * Koof.q;
  end;

var
  S: TStrings;
  Rows: array of TRow;
  i, j: integer;
  temp: string;  }
begin
 (* S := TStringList.Create;

  try
    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'BSIn';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].BSIn;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'Prots';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].Prots;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'Fats';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].Fats;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'Carbs';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].Carbs;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'Value';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].Prots * ENERGY_PROTS + AnalyzeResult.AnList[i].Fats * ENERGY_FATS + AnalyzeResult.AnList[i].Carbs * ENERGY_CARBS;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'Ins';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].Ins;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'Carbs/Ins';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].Carbs / AnalyzeResult.AnList[i].Ins;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'BSOut';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].BSOut;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'BSDelta';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].BSOut - AnalyzeResult.AnList[i].BSIn;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'BSExp';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := ExpectedBS(i);

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'BSErr';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := ExpectedBS(i) - AnalyzeResult.AnList[i].BSOut;

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'BSErr2';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := Sqr(ExpectedBS(i) - AnalyzeResult.AnList[i].BSOut);

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'BSTargetErr';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := AnalyzeResult.AnList[i].BSOut - Value['TargetBS'];

    SetLength(Rows, Length(Rows) + 1);
    Rows[High(Rows)].Name := 'BSTargetErr2';
    SetLength(Rows[High(Rows)].Data, Length(AnalyzeResult.AnList));
    for i := 0 to High(AnalyzeResult.AnList) do
      Rows[High(Rows)].Data[i] := Sqr(AnalyzeResult.AnList[i].BSOut - Value['TargetBS']);

    // ===============================================================

    S.Add('Means:');
    for i := 0 to High(Rows) do
      S.Add(Format('%s'#9'%.4f', [Rows[i].Name, Mean(Rows[i].Data)]));

    S.Add('');
    S.Add('STD:');
    for i := 0 to High(Rows) do
      S.Add(Format('%s'#9'%.4f', [Rows[i].Name, Sqrt(PopnVariance(Rows[i].Data))]));

    S.Add('');
    S.Add('Correlations:');
    {for i := 0 to High(Rows) - 1 do
    for j := i + 1 to High(Rows) do
      S.Add(Format('%s'#9'%s'#9'%.4f', [Rows[i].Name, Rows[j].Name, LinearCorrelation(Rows[i].Data, Rows[j].Data)])); }

    temp := '';
    for j := 0 to High(Rows) do
      temp := temp + #9 + Rows[j].Name;
    S.Add(temp);

    for i := 0 to High(Rows) do
    begin
      temp := Rows[i].Name;
      for j := 0 to High(Rows) do
      begin
        temp := temp + #9 + Format('%.4f', [LinearCorrelation(Rows[i].Data, Rows[j].Data)]);
      end;
      S.Add(temp);
    end;

    S.SaveToFile('temp\covariance.txt');
  finally
    S.Free;
  end;  *)
end;

procedure TFormMisc.ButtonExportRawClick(Sender: TObject);
begin
  //SaveAnalyzeList(AnalyzeResult.AnList, 'temp\raw.txt');
end;

procedure TFormMisc.Button1Click(Sender: TObject);
{var
  W: array[0..MinPerDay - 1] of
  record
    Value: Real;
    Count: integer;
  end;

  procedure Process(PrevDate: TDate; PrevBlood: TBloodRecord; CurDate: TDate; CurBlood: TBloodRecord);
  var
    i, k: integer;
    pt: integer;
    v: real;
  begin
    if (PrevDate < CurDate) then
      pt := PrevBlood.Time - MinPerDay
    else
      pt := PrevBlood.Time;

    for i := pt to CurBlood.Time - 1 do
    begin
      k := (i + MinPerDay) mod MinPerDay;
      v := PrevBlood.Value + (CurBlood.Value - PrevBlood.Value) * (i - pt) / (CurBlood.Time - pt);
      W[k].Value := W[k].Value + v;
      W[k].Count := W[k].Count + 1;
    end;
  end;

var
  d, i: integer;

  PrevBlood: TBloodRecord;
  PrevDate: TDate;  }
begin
 { for i := 0 to MinPerDay - 1 do
  begin
    W[i].Value := 0.0;
    W[i].Count := 0;
  end;

  PrevDate := Trunc(GetTimeUTC() - 30 - 1);
  PrevBlood := Diary[PrevDate].FindRecordLast(TBloodRecord) as TBloodRecord;

  for d := Trunc(GetTimeUTC() - 30) to Trunc(GetTimeUTC()) - 1 do
  for i := 0 to Diary[d].Count - 1 do
  if (Diary[d][i].RecType = TBloodRecord) then
  begin
    Process(PrevDate, PrevBlood, d, TBloodRecord(Diary[d][i]));

    PrevDate := d;
    PrevBlood := TBloodRecord(Diary[d][i]);
  end;

  Memo1.Clear;
  for i := 0 to MinPerDay - 1 do
  if W[i].Count > 0 then
    Memo1.Lines.Add(FloatToStr(W[i].Value / W[i].Count))
  else
    Memo1.Lines.Add('');  }

    // TODO: restore

end;

{======================================================================================================================}
procedure TFormMisc.ButtonFoodBSCorrelationClick(Sender: TObject);
{======================================================================================================================}
const
  MIN_INTERVAL = 4 / HourPerDay;
  MAX_INTERVAL = 9 / HourPerDay;
type
  TNode = record
    Items: TStringArray;
    BS: real;
  end;

  TFoodInfo = record
    Name: string;
    PresentedCount: integer;
    PresentedSumm: real;
    NonPresentedCount: integer;
    NonPresentedSumm: real;
  end;

  function ExtractNames(Meal: TMealRecord): TStringArray;
  var
    i: integer;
  begin
    SetLength(Result, Meal.Count);
    for i := 0 to Meal.Count - 1 do
      Result[i] := Meal[i].Name;
  end;

var
  Data: array of TFoodInfo;

  procedure ProcessFood(const FoodName: string; BS: Real);
  var
    i: integer;
  begin
    for i := 0 to High(Data) do
    if (Data[i].Name = FoodName) then
    begin
      Data[i].PresentedCount := Data[i].PresentedCount + 1;
      Data[i].PresentedSumm := Data[i].PresentedSumm + BS;
      Exit;
    end;

    SetLength(Data, Length(Data) + 1);
    with Data[High(Data)] do
    begin
      Name := FoodName;
      PresentedCount := 1;
      PresentedSumm := BS;
    end;
  end;

  procedure ProcessNode(const Node: TNode);
  var
    i: integer;
  begin
    for i := 0 to High(Node.Items) do
      ProcessFood(Node.Items[i], Node.BS);
  end;

var
  Recs: TRecordList;
  buf: TStringArray;
  Nodes: array of TNode;
  MealTime: TDateTime;
  i: integer;
  TotalBS: real;
  AvgBS: real;
begin
  // STEP 1: extracting data for analysis

  Recs := LocalSource.FindPeriod(EncodeDate(2009, 11, 1), EncodeDate(2015, 01, 01));
  try
    for i := Low(Recs) to High(Recs) do
    begin
      if (Recs[i].RecType = TMealRecord) then
      begin
        buf := ExtractNames(TMealRecord(Recs[i]));
        MealTime := Recs[i].Time;
      end else
      if (Recs[i].RecType = TBloodRecord) and
         (Recs[i].Time - MealTime > MIN_INTERVAL) and
         (Recs[i].Time - MealTime < MAX_INTERVAL) then
      begin
        SetLength(Nodes, Length(Nodes) + 1);
        with Nodes[High(Nodes)] do
        begin
          Items := buf;
          BS := TBloodRecord(Recs[i]).Value;
        end;
      end;
    end;
  finally
    FreeRecords(Recs);
  end;

  // STEP 2: analysis

  TotalBS := 0.0;
  for i := 0 to High(Nodes) do
  begin
    ProcessNode(Nodes[i]);
    TotalBS := TotalBS + Nodes[i].BS;
  end;

  AvgBS := TotalBS / Length(Nodes);

  for i := 0 to High(Data) do
  begin
    Data[i].NonPresentedCount := Length(Nodes) - Data[i].PresentedCount;
    Data[i].NonPresentedSumm := TotalBS - Data[i].PresentedSumm; 
  end;

  // STEP 3: output
  with TStringList.Create do
  try
    Add(Format('Meals analyzed: %d', [Length(Nodes)]));
    Add(Format('Foods analyzed: %d', [Length(Data)]));
    Add(Format('Average BS: %.3f', [AvgBS]));
    Add('');

    for i := 0 to High(Data) do
      Add(Format(
        '%s'#9'%.4f'#9'%.3f'#9'%.3f'#9'%.3f',
        [
          Data[i].Name,
          Data[i].PresentedCount / Length(Nodes) * 100,
          Data[i].NonPresentedSumm / Data[i].NonPresentedCount,
          Data[i].PresentedSumm / Data[i].PresentedCount,
          Data[i].PresentedSumm / Data[i].PresentedCount - Data[i].NonPresentedSumm / Data[i].NonPresentedCount
        ]
      ));
    SaveToFile('temp\Food_affect.txt');
  finally
    Free;
  end;
end;

procedure TFormMisc.ButtonVerifyLinearClick(Sender: TObject);
{var
  s: TStrings;
  i: integer;  }
begin
 { s := TStringList.Create;
  try
    s := TStringList.Create;

    for i := 0 to High(AnalyzeResult.AnList) do
    begin
      if (AnalyzeResult.AnList[i].Ins > 1) then
      S.Add(Format('%.4f'#9'%.1f', [
        AnalyzeResult.AnList[i].Carbs / AnalyzeResult.AnList[i].Ins,
        AnalyzeResult.AnList[i].BSOut - AnalyzeResult.AnList[i].BSIn]));
    end;
    S.SaveToFile('temp\linearity.txt');
  finally
    s.Free;
  end;  }
end;

procedure TFormMisc.ButtonExportFoodBaseClick(Sender: TObject);
var
  Foods: TFoodItemList;
  Filtered: TFoodItemList;
  s: string;
  i: integer;
begin
  Foods := FoodBaseLocal.FindAll(True);
  for i := 0 to High(Foods) do
  //if ({not Foods[i].FromTable and }(pos('Теремок', Foods[i].Name) > 0)) then
  begin
    Foods[i].Version := 0;
    Foods[i].TimeStamp := Now();
    Foods[i].Tag := 0;

    SetLength(Filtered, Length(Filtered) + 1);
    Filtered[High(Filtered)] := Foods[i];
  end;

  s := JsonWrite(SerializeVersionedFoodItems(Filtered));
  with (TStringList.Create) do
  begin
    //Add(IntToStr(Length(Filtered)));
    Add(s);
    SaveToFile('temp\food.json');
    Free;
  end;
end;

procedure TFormMisc.ButtonFoodCompareClick(Sender: TObject);
type
  TFoodItemPair = record
    Food1: TFoodItem;
    Food2: TFoodItem;
  end;

  TFoodItemPairList = array of TFoodItemPair;

  function Find(const ID: string; List: TFoodItemList): TFoodItem; overload;
  var
    i: integer;
  begin
    for i := Low(List) to High(List) do
    if (List[i].ID = ID) then
    begin
      Result := List[i];
      Exit;
    end;
    Result := nil;
  end;

  function Find(const Item: TFoodItem; List: TFoodItemList): TFoodItem; overload;
  begin
    Result := Find(Item.ID, List);
  end;

  function Sub(A, B: TFoodItemList): TFoodItemList;
  var
    i: integer;
  begin
    SetLength(Result, 0);
    for i := Low(A) to High(A) do
    if (Find(A[i], B) = nil) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := A[i];
    end;
  end;

  function Intersect(A, B: TFoodItemList): TFoodItemPairList;
  var
    i: integer;
    Bi: TFoodItem;
  begin
    SetLength(Result, 0);
    for i := Low(A) to High(A) do
    begin
      Bi := Find(A[i], B);
      if (Bi <> nil) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)].Food1 := A[i];
        Result[High(Result)].Food2 := Bi;
      end;
    end;
  end;

var
  Json: TlkJSONlist;
  Food1: TFoodItemList;
  Food2: TFoodItemList;

  Only1: TFoodItemList;
  Only2: TFoodItemList;
  Inter: TFoodItemPairList;

  i: integer;
begin
  try
    Json := TlkJSON.ParseText(MakeSureJsonList(ReadFile('temp\food1.txt'))) as TlkJSONlist;
    Food1 := ParseVersionedFoodItems(Json);
  finally
    Json.Free;
  end;

   try
    Json := TlkJSON.ParseText(MakeSureJsonList(ReadFile('temp\food2.txt'))) as TlkJSONlist;
    Food2 := ParseVersionedFoodItems(Json);
  finally
    Json.Free;
  end;

  Only1 := Sub(Food1, Food2);
  Only2 := Sub(Food2, Food1);
  Inter := Intersect(Food1, Food2);

  with (TStringList.Create) do
  begin
    for i := 0 to High(Only1) do
      Add(Format('ONLY1'#9'%s'#9'%s', [Only1[i].ID, Only1[i].Name]));

    for i := 0 to High(Only2) do
      Add(Format('ONLY2'#9'%s'#9'%s', [Only2[i].ID, Only2[i].Name]));

    for i := 0 to High(Inter) do
    begin
      if (Inter[i].Food1.Version = Inter[i].Food2.Version) then
      begin
        if (Inter[i].Food1.Hash = Inter[i].Food2.Hash) then
          Add(Format('INTER_OK'#9'%s'#9'%s', [Inter[i].Food1.ID, Inter[i].Food1.Name]))
        else
          Add(Format('INTER_FAILHASH'#9'%s'#9'%s', [Inter[i].Food1.ID, Inter[i].Food1.Name]));
      end else
        Add(Format('INTER_FAILVER'#9'%s'#9'%s', [Inter[i].Food1.ID, Inter[i].Food1.Name, Inter[i].Food1.Version, Inter[i].Food2.Name, Inter[i].Food2.Version]));
    end;

    SaveToFile('temp\compare.txt');
    Free;
  end;

  BusinessObjects.Free(Food1);
  BusinessObjects.Free(Food2);
end;

end.

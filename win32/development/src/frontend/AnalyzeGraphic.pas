unit AnalyzeGraphic;

interface

uses
  AnalyzeInterface,
  DiaryRoutines,
  Windows,
  ExtCtrls,
  Classes,
  SysUtils,
  Graphics,
  Math,
  AutoLog,
  DiaryPage,
  DiaryDatabase,
  DiaryRecords,
  BusinessObjects;

type
  // TODO: remove
  TKoofType = (kfK, kfQ, kfP, kfX);

  TDayCurve = array[0..MinPerDay - 1] of Real;
  TWeightedDayPoint = record
    Time: 0..MinPerDay - 1;
    Value: Real;
    Weight: Real;
  end;

  TBSPoint = record
    Time, Value: Real;
  end;

  TBSPointList = array of TBSPoint;

  TColoredPoint = record
    Time, Value: Real;
    Color: TColor;
  end;

  TChartData = record
    // inputs

    Curve: TBSPointList;
    CurveColor: TColor;
    Points: array of TColoredPoint;

    MarginTop: integer;
    MarginRight: integer;
    TextMarginX, TextMarginY: integer;
    Width, Height: integer;

    // devirates
    MarginBottom: integer;
    MarginLeft: integer;
    MaxValue: real;
  end;

  procedure DrawBS(Recs: TRecordList; BaseDate: TDateTime; Image: TImage; Mini: boolean);
  procedure DrawBS_Int(const Base: TDiary; FromDay, ToDay: integer; Image: TImage);

  procedure DrawKoof(Image: TImage; const KoofList: TKoofList;
    const RecList: TAnalyzeRecList; KoofType: TKoofType; DrawPoints: boolean);
  procedure DrawDayCurve(Image: TImage; const Curve: TDayCurve;
    const Points: array of TWeightedDayPoint; ColorCurve, ColorPointWeight0,
    ColorPointWeight1: TColor);

var
  LeftBord: integer;
  
const
  Brd      = 20; { BS    }
  eSize    = 3;  { ...   }
  TopBord  = 20; { Koofs }

  COLOR_BS_HIGH   = $E6E6FF; // $000060;
  COLOR_BS_NORMAL = $E6FFE6; // $006000;
  COLOR_BS_CURVE  = clMaroon; // $FFFFFF;
  COLOR_K         = $EEEEFF;
  COLOR_Q         = $FFEEEE;
  COLOR_P         = $80FFFF;
  COLOR_X         = $EEEEEE;
  COLOR_BACK      = clWhite; // clBlack
  COLOR_TIMEPOS   = clSilver;
  COLOR_AXIS_MAIN = clBlack; // clYellow;
  COLOR_AXIS_SUB  = $DCDCDC;
  COLOR_TITLES    = clBlack; // clLime

implementation

uses SettingsINI;

  function CalcX(Chart: TChartData; x: Real): integer;
  begin
    Result := Chart.MarginLeft + Round(x / HourPerDay * (Chart.Width - Chart.MarginLeft - Chart.MarginRight));
  end;

  function CalcY(Chart: TChartData; y: Real): integer;
  begin
    Result := Chart.MarginTop + Round((1 - y / Chart.MaxValue) * (Chart.Height - Chart.MarginTop - Chart.MarginBottom));
  end;

  function InterpolateQubic(Data: TBSPointList; DeltaX: Real): TBSPointList;

    procedure Add(var List: TBSPointList; Time, Value: Real);
    begin
      SetLength(List, Length(List) + 1);
      List[High(List)].Time := Time;
      List[High(List)].Value := Value;
    end;

  var
    i: integer;
    x1, y1, x2, y2, dy1, dy2: Real;
    a, b, c, d: Real;
  begin
    if (Length(Data) < 2) then
    begin
      Result := Data;
      Exit;
    end;

    SetLength(Result, 0);

    for i := 0 to High(Data) - 1 do
    begin
      x1 := Data[i].Time;
      y1 := Data[i].Value;
      x2 := Data[i + 1].Time;
      y2 := Data[i + 1].Value;

      if (i > 0) then
        dy1 := (Data[i + 1].Value - Data[i - 1].Value)  / (Data[i + 1].Time - Data[i - 1].Time)
      else
        dy1 := 0;

      if (i < High(Data) - 1) then
        dy2 := (Data[i + 2].Value - Data[i].Value)  / (Data[i + 2].Time - Data[i].Time)
      else
        dy2 := 0;

      a := (dy1 + dy2) / (x2 - x1) / (x2 - x1) - 2 * (y2 - y1) / (x2 - x1) / (x2 - x1) / (x2 - x1);
		  b := (y2 - y1) / (x2 - x1) / (x2 - x1) - (dy1 + a * (x2 * x2 + x1 * x2 - 2 * x1 * x1)) / (x2 - x1);
		  c := dy1 - 3 * x1 * x1 * a - 2 * x1 * b;
		  d := y1 - x1 * x1 * x1 * a - x1 * x1 * b - x1 * c;

      Add(Result, Data[i].Time, Data[i].Value);

      while (x1 < x2) do
      begin
        y1 := a * x1 * x1 * x1 + b * x1 * x1 + c * x1 + d;
        Add(Result, x1, y1);
        x1 := x1 + 0.2;
      end;

      Add(Result, Data[i + 1].Time, Data[i + 1].Value);
    end;
  end;

  function FindMax(Data: TBSPointList; InitMax: Real = 0.0): real;
  var
    i: integer;
  begin
    Result := InitMax;

    for i := 0 to High(Data) do
    if (Data[i].Value > Result) then
      Result := Data[i].Value;
  end;

procedure DrawChart(Image: TImage; Chart: TChartData; Mini: boolean);
const
  TX: array[0..6] of integer = (1, 2, 3, 4, 6, 12, 24);
  MAX_Y_LABELS = 20;
var
  LabelWidth, LabelHeight: integer;
  ExtendedLabelWidth, ExtendedLabelHeight: Real;
  TrottleX: integer;
  TrottleY: integer;

  BS_PREPRAND_LOW: Real;
  BS_PREPRAND_HIGH: Real;
  BS_POSTPRAND_HIGH: Real;

  cx, cy: integer;
  i: integer;
begin
  Chart.MaxValue := FindMax(Chart.Curve, 8.0) * 1.2;
  
  BS_PREPRAND_LOW   := Value['BS1'];
  BS_PREPRAND_HIGH  := Value['BS2'];
  BS_POSTPRAND_HIGH := Value['BS3'];

  with Image.Picture.Bitmap.Canvas do
  begin
    Image.Picture.Bitmap.Width := Chart.Width;
    Image.Picture.Bitmap.Height := Chart.Height;

    { очистка }
    Brush.Color := COLOR_BACK;
    FillRect(Image.ClientRect);

    Chart.MarginLeft := 2 * Chart.TextMarginX + TextWidth(IntToStr(Round(Chart.MaxValue)));
    Chart.MarginBottom := 2 * Chart.TextMarginY + TextHeight('123');

    { зелёная зона }
    Brush.Color := COLOR_BS_NORMAL;
    FillRect(Rect(CalcX(Chart, 0), CalcY(Chart, BS_PREPRAND_LOW), CalcX(Chart, HourPerDay), CalcY(Chart, BS_PREPRAND_HIGH)));

    { красная зона }
    Brush.Color := COLOR_BS_HIGH;
    FillRect(Rect(CalcX(Chart, 0), CalcY(Chart, BS_PREPRAND_HIGH), CalcX(Chart, HourPerDay), CalcY(Chart, BS_POSTPRAND_HIGH)));

    { Scale }

    Brush.Color := COLOR_BACK;
    Pen.Color := COLOR_AXIS_SUB;
    Pen.Width := 1;
    Font.Style := [];
    Font.Color := COLOR_TITLES;

    if Mini then
    begin
      Pen.Style := psSolid;
      Font.Size := 5;
    end else
    begin
      Pen.Style := psDot;
      Font.Size := 8;
    end;

    LabelWidth := TextWidth('24');
    ExtendedLabelWidth := 1.5 * LabelWidth;
    LabelHeight := TextHeight('123');
    ExtendedLabelHeight := LabelHeight;

    for i := 0 to High(TX) do
    begin
      TrottleX := TX[i];

      if (CalcX(Chart, TrottleX) - CalcX(Chart, 0) >= ExtendedLabelWidth) then
        break;
    end;

    TrottleY := 1;
    while ((abs(CalcY(Chart, 0) - CalcY(Chart, TrottleY)) < ExtendedLabelHeight) or (Chart.MaxValue / TrottleY > MAX_Y_LABELS)) do
    begin
      if ((abs(CalcY(Chart, 0) - CalcY(Chart, TrottleY * 2))  >= ExtendedLabelHeight) and (Chart.MaxValue / (TrottleY * 2) <= MAX_Y_LABELS)) then
      begin
        TrottleY := TrottleY * 2;
        break;
      end;

      if ((abs(CalcY(Chart, 0) - CalcY(Chart, TrottleY * 5)) >= ExtendedLabelHeight) and (Chart.MaxValue / (TrottleY * 5) <= MAX_Y_LABELS)) then
      begin
        TrottleY := TrottleY * 5;
        break;
      end;

      TrottleY := TrottleY * 10;
    end;

    { Шкала Y }
    for i := 1 to Round(Chart.MaxValue) do
    begin
      if ((i mod TrottleY) = 0) then
      begin
        MoveTo(CalcX(Chart, 0), CalcY(Chart, i));
        LineTo(CalcX(Chart, HourPerDay), CalcY(Chart, i));

        TextOut(
          Chart.MarginLeft - Chart.TextMarginX - TextWidth(IntToStr(i)),
          CalcY(Chart, i) - (LabelHeight div 2),
          IntToStr(i)
        );
      end;  
    end;

    { Шкала X }
    for i := 0 to HourPerDay do
    if ((i mod TrottleX) = 0) then
    begin
      MoveTo(CalcX(Chart, i), CalcY(Chart, 0));
      LineTo(CalcX(Chart, i), CalcY(Chart, Chart.MaxValue));

      TextOut(
        CalcX(Chart, i) - (TextHeight(IntToStr(i)) div 2),
        Chart.MarginTop + (Chart.Height - Chart.MarginTop - Chart.MarginBottom) + Chart.TextMarginY,
        IntToStr(i)
      );
    end;

    { оси }
    Pen.Style := psSolid;
    Pen.Color := COLOR_AXIS_MAIN;
    MoveTo(CalcX(Chart, 0), CalcY(Chart, Chart.MaxValue));
    LineTo(CalcX(Chart, 0), CalcY(Chart, 0));
    LineTo(CalcX(Chart, HourPerDay), CalcY(Chart, 0));

    if (Length(Chart.Curve) > 0) then
    begin
      { кривая }
      Pen.Width := 1;
      if Mini then
        Pen.Style := psSolid
      else
        Pen.Style := psDot;

      Pen.Color := COLOR_BS_CURVE;

      { основная линия }

      MoveTo(CalcX(Chart, Chart.Curve[0].Time), CalcY(Chart, Chart.Curve[0].Value));
      for i := 0 to High(Chart.Curve) do
      begin
        LineTo(CalcX(Chart, Chart.Curve[i].Time), CalcY(Chart, Chart.Curve[i].Value));
      end;
    end;

    { точки }
    if (not mini) then
    begin
      Pen.Style := psSolid;

      for i := 0 to High(Chart.Points) do
      begin
        Brush.Color := Chart.Points[i].Color;
        Pen.Color := Chart.Points[i].Color;

        cx := CalcX(Chart, Chart.Points[i].Time);
        cy := CalcY(Chart, Chart.Points[i].Value);
        Ellipse(cx - eSize, cy - eSize, cx + eSize, cy + eSize);
      end;
    end;
  end;
end;

{======================================================================================================================}
procedure DrawBS(Recs: TRecordList; BaseDate: TDateTime; Image: TImage; Mini: boolean);
{======================================================================================================================}

  function FetchBloodRecords(Recs: TRecordList; BaseDate: TDateTime): TBSPointList;
  var
    i: integer;
  begin
    SetLength(Result, 0);

    for i := 0 to High(Recs) do
    if (recs[i].RecType = TBloodRecord) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].Time := (Recs[i].Time - BaseDate) * HoursPerDay;
      Result[High(Result)].Value := (Recs[i] as TBloodRecord).Value;
    end;
  end;

var
  Chart: TChartData;
  Data: TBSPointList;

  i: integer;
begin
  Data := FetchBloodRecords(Recs, BaseDate);

  Chart.Curve := InterpolateQubic(Data, 0.2);
  Chart.Width := Image.Width;
  Chart.Height := Image.Height;

  SetLength(Chart.Points, Length(Data));
  for i := 0 to High(Data) do
  begin
    Chart.Points[i].Time := Data[i].Time;
    Chart.Points[i].Value := Data[i].Value;
    Chart.Points[i].Color := COLOR_BS_CURVE;
  end;

  if (Mini) then
  begin
    Chart.MarginTop := 5;
    Chart.MarginRight := 10;
    Chart.TextMarginX := 5;
    Chart.TextMarginY := 5;
  end else
  begin
    Chart.MarginTop := 20;
    Chart.MarginRight := 20;
    Chart.TextMarginX := 10;
    Chart.TextMarginY := 10;
  end;

  DrawChart(Image, Chart, Mini);
end;

{======================================================================================================================}
procedure DrawBS_Int(const Base: TDiary; FromDay, ToDay: integer; Image: TImage);
{======================================================================================================================}
(*const
  BRD   = 20;
  eSize = 3;
  InitMax = 8;
var
  h: integer;
  kx,ky: real;
  max: real;
  //x1,y1,x2,y2: integer;
  cx,cy: integer;
  //TempBlood: TBloodRecord;

  points: array of
  record
    R: array of
    record
      Pos: real;
      Value: real;
    end;
    AvqPos: real;
    AvqValue: real;
  end;
  n,i: integer;
  Zone: integer;
  Border: integer;

  procedure CalcXY(Time,Value: real; var x,y: integer); overload;
  begin
    x := Round(Border + kx/60*Time);
    y := Round(h-Border - ky*Value);
  end;   *)

begin
 (* { проверка корректности FromDay и ToDay }
  if (FromDay>=0)and(FromDay<Base.Count)and
     (ToDay>=0)and(ToDay<Base.Count)
  then
  begin
    { проверка порядка следования }
    if FromDay>ToDay then
    begin
      FromDay := FromDay xor ToDay;
      ToDay   := FromDay xor ToDay;
      FromDay := FromDay xor ToDay;
    end;

    { вычисление точек }
    SetLength(points,24);
    for n := FromDay to ToDay do
    for i := 0 to Base[n].Count-1 do
    if Base[n][i].TagType=rtBlood then
    begin
      Zone := (Base[n][i].Time div 60) mod 24;
      SetLength(Points[Zone].R,length(Points[Zone].R) + 1);
      Points[Zone].R[High(Points[Zone].R)].Value := TBloodRecord(Base[n][i]).Value;
      Points[Zone].R[High(Points[Zone].R)].Pos := Frac(Base[n][i].Time/60);
    end;

    { вычисление средних и максимума }
    Max := InitMax;
    for i := 0 to 23 do
    begin
      Points[i].AvqPos := 0;
      Points[i].AvqValue := 0;
      if length(Points[i].R)>0 then
      begin
        for n := 0 to High(Points[i].R) do
        begin
          Points[i].AvqPos := Points[i].AvqPos + Points[i].R[n].Pos;
          Points[i].AvqValue := Points[i].AvqValue + Points[i].R[n].Value;
        end;
        Points[i].AvqPos := Points[i].AvqPos / length(Points[i].R);
        Points[i].AvqValue := Points[i].AvqValue / length(Points[i].R);
      end;

      if Points[i].AvqValue > Max then
        Max := Points[i].AvqValue;
    end;

    { очистка }
    PrepareBS(Image,Max,false,kx,ky,Border);

    { рисование }
    {w := Image.Width; }
    h := Image.Height;
    with Image.Canvas do
    begin
      CalcXY(
        Points[0].AvqPos*60,
        Points[0].AvqValue,
        cx,cy);
      MoveTo(cx,cy);

      for i := 0 to 23 do
      if Points[i].AvqValue>0 then
      begin
        CalcXY(
          (i+Points[i].AvqPos)*60,
          Points[i].AvqValue,
          cx,cy);
        LineTo(cx,cy);
      end;
    end;
  end else
  begin
    { очистка }
    PrepareBS(Image,InitMax,false,kx,ky,Border);
  end;
         *)



(*  max := FindMax;
  w := Image.Width;
  h := Image.Height;
  ky := (h-2*Border)/max;
  kx := (w-2*Border)/24;

  with Image.Canvas do
  begin
    { очистка }
    Brush.Color := clWhite;
    FillRect(Image.ClientRect);

    Pen.Color := clBlack;
    Pen.Width := 1;
    Font.Style := [];
    Font.Color := clBlack;

    if Mini then
    begin
      Font.Size := 5;
      Pen.Style := psSolid;
    end else
    begin
      Font.Size := 8;
      Pen.Style := psDot;
    end;

    { зеленая зона }
    Brush.Color := RGB(220,255,220);
    CalcXY(0,3.7,x1,y1);
    CalcXY(MinPerDay,6.2,x2,y2);
    FillRect(Rect(x1,y1,x2,y2));

    { красная зона }
    Brush.Color := RGB(255,220,220);
    CalcXY(0,6.2,x1,y1);
    CalcXY(MinPerDay,7.8,x2,y2);
    FillRect(Rect(x1,y1,x2,y2));

    Brush.Color := clWhite;

    { риски Y }
    Pen.Color := RGB(220,220,220);
    for i := 1 to Round(max) do
    begin
      MoveTo(Border-3,Round(h-Border-ky*i));
      LineTo(w-Border,Round(h-Border-ky*i));
      if (not mini)or
         ((i mod 2)=0) then
      TextOut(
        Border-15,
        h-Border-Round(ky*i)-(TextHeight('0') div 2),
        IntToStr(i)
      );
    end;

    { риски X }
    for i := 0 to 24 do
    if (not mini)or
       ((i mod 3)=0) then
    begin
      MoveTo(Border+Round(i*kx),Border);
      LineTo(Border+Round(i*kx),h-Border+2);
      TextOut(
        Border+Round(i*kx)-(TextWidth('0') div 2),
        h-Border+4,
        IntToStr(i)
      );
    end;

    { оси }
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    MoveTo(Border,Border);
    LineTo(Border,h-Border);
    LineTo(w-Border,h-Border);

    {--------------------------}
    if (Day<0)or(Day>=Base.Count) then Exit;
    {--------------------------}

    { кривая }
    Pen.Width := 1;
    if Mini then
      Pen.Style := psSolid
    else
      Pen.Style := psDot;

    Pen.Color := clMaroon;

    { последняя точка предыдущего дня }
    if (Day>0) then
    begin
      TempBlood := Base[day-1].LastBloodRec;
      if TempBlood<>nil then
      begin
        CalcXY(
          TempBlood.Time-MinPerDay,
          TempBlood.Value,
          cx,cy);
        MoveTo(cx,cy);
      end;
    end else
    begin
      TempBlood := Base[day].FirstBloodRec;
      if TempBlood<>nil then
      begin
        CalcXY(
          TempBlood.Time,
          TempBlood.Value,
          cx,cy);
        MoveTo(cx,cy);
      end;
    end;

    { основная линия }
    for i := 0 to Base[Day].Count-1 do
    if Base[Day][i].TagType=rtBlood then
    begin
      CalcXY(
        TBloodRecord(Base[Day][i]).Time,
        TBloodRecord(Base[Day][i]).Value,
        cx,cy);
      LineTo(cx,cy);
    end;

    { продолжение на следующий день }

    if (Day<Base.Count-1)then
    begin
      TempBlood := Base[Day + 1].FirstBloodRec;
      if TempBlood<>nil then
      begin
        CalcXY(
          TempBlood.Time+MinPerDay,
          TempBlood.Value,
         cx,cy);
        LineTo(cx,cy);
      end;
    end;

    { точки }
    if not mini then
    begin
      Brush.Color := clMaroon;
      Pen.Color := clMaroon;
      Pen.Style := psSolid;
      for i := 0 to Base[Day].Count-1 do
      if Base[Day][i].TagType=rtBlood then
      begin
        CalcXY(
          TBloodRecord(Base[Day][i]).Time,
          TBloodRecord(Base[Day][i]).Value,
          cx,cy);
        Ellipse(cx-eSize,cy-eSize,cx+eSize,cy+eSize);
      end;
    end;
  end;   *)
end;

{======================================================================================================================}
procedure DrawKoof(Image: TImage; const KoofList: TKoofList;
  const RecList: TAnalyzeRecList; KoofType: TKoofType; DrawPoints: boolean);
{======================================================================================================================}

  function GetK(const Rec: TAnalyzeRec): real;
  begin
    if Rec.Carbs > 0 then
      Result := (
        - KoofList[Rec.Time].p * Rec.Prots
        + KoofList[Rec.Time].q * Rec.Ins
        + (Rec.BSOut - Rec.BSIn)
      ) / Rec.Carbs
    else
      Result := -10;
  end;

  function GetX(k, q, p: real): real; overload;
  const
    PROTS = 0.25;
  begin
    if abs(q) > 0.0001 then
      Result := (k + PROTS * p) / q
    else
      Result := 0.0;
  end;

  function GetX(n: integer): real; overload;
  begin
    Result := GetX (KoofList[n].k, KoofList[n].q, KoofList[n].p);
  end;

  function GetX(const Rec: TAnalyzeRec): real; overload;
  begin
    Result := GetX(Rec.Time);
  end;

  function GetQ(const Rec: TAnalyzeRec): real;
  begin
    if Rec.Ins > 0 then
      Result := (
        + KoofList[Rec.Time].p * Rec.Prots
        + KoofList[Rec.Time].k * Rec.Carbs
        - (Rec.BSOut-Rec.BSIn)
      ) / (Rec.Ins)
    else
      Result := -10;
  end;

  function GetP(const Rec: TAnalyzeRec): real;
  begin
    if Rec.Prots > 0 then
      Result := (
        - KoofList[Rec.Time].k * Rec.Carbs
        + KoofList[Rec.Time].q * Rec.Ins
        + (Rec.BSOut-Rec.BSIn)
      ) / (Rec.Prots)
    else
      Result := -10;
  end;

var
  Curve: TDayCurve;
  Points: array of TWeightedDayPoint;
  ColorCurve: TColor;
  ColorPointWeight0: TColor;
  ColorPointWeight1: TColor;
  i: integer;
begin
  StartProc('DrawKoof');

  // Создаём набор данных
  case KoofType of
    kfK:
      begin
        for i := 0 to MinPerDay - 1 do Curve[i] := KoofList[i].k;

        if (DrawPoints) then
        begin
          SetLength(Points, Length(RecList));
          for i := 0 to High(Points) do
          begin
            Points[i].Time := RecList[i].Time;
            Points[i].Value := GetK(RecList[i]);
            Points[i].Weight := RecList[i].Weight;
          end;
        end;

        ColorCurve := COLOR_K;
        ColorPointWeight0 := COLOR_BACK;
        ColorPointWeight1 := clRed;
      end;

    kfQ:
      begin
        for i := 0 to MinPerDay - 1 do Curve[i] := KoofList[i].q;

        if (DrawPoints) then
        begin
          SetLength(Points, Length(RecList));
          for i := 0 to High(Points) do
          begin
            Points[i].Time := RecList[i].Time;
            Points[i].Value := GetQ(RecList[i]);
            Points[i].Weight := RecList[i].Weight;
          end;
        end;

        ColorCurve := COLOR_Q;
        ColorPointWeight0 := COLOR_BACK;
        ColorPointWeight1 := clBlue;
      end;

    kfP:
      begin
        for i := 0 to MinPerDay - 1 do Curve[i] := KoofList[i].p;

        if (DrawPoints) then
        begin
          SetLength(Points, Length(RecList));
          for i := 0 to High(Points) do
          begin
            Points[i].Time := RecList[i].Time;
            Points[i].Value := GetP(RecList[i]);
            Points[i].Weight := RecList[i].Weight;
          end;
        end;

        ColorCurve := COLOR_P;
        ColorPointWeight0 := COLOR_BACK;
        ColorPointWeight1 := clOlive;
      end;

    kfX:
      begin
        for i := 0 to MinPerDay - 1 do Curve[i] := GetX(i);

        if (DrawPoints) then
        begin
          SetLength(Points, Length(RecList));
          for i := 0 to High(Points) do
          begin
            Points[i].Time := RecList[i].Time;
            Points[i].Value := GetX(RecList[i]);
            Points[i].Weight := RecList[i].Weight;
          end;
        end;

        ColorCurve := COLOR_X;
        ColorPointWeight0 := COLOR_BACK;
        ColorPointWeight1 := clBlack;
      end;

    else
      raise Exception.Create('Неизвестный тип коэффициента');
  end;

  // Рисуем
  DrawDayCurve(Image, Curve, Points, ColorCurve, ColorPointWeight0, ColorPointWeight1);

  FinishProc;
end;

{======================================================================================================================}
procedure DrawDayCurve(Image: TImage; const Curve: TDayCurve; const Points: array of TWeightedDayPoint; ColorCurve,
  ColorPointWeight0, ColorPointWeight1: TColor);
{======================================================================================================================}
var
  Acc: real;
  Max: real;
  Wd: integer;

  procedure CalcAcc;
  const
    FAcc: array[1..13] of real = (1000, 100, 10, 5, 1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001);
    FWd: array[1..13] of integer =  (0,   0,  0, 0, 0,   1,   1,    2,    2,     3,     3,      4,      4);
  var
    n, LabelHeight: integer;
  begin
    LabelHeight := Image.Canvas.TextHeight('123');
    Acc := 1;
    Wd := 0;
    for n := High(FAcc) downto 1 do
    if Trunc(Max / FAcc[n]) * LabelHeight * 2 <= Image.Height - 2 * TopBord then
    begin
      Acc := FAcc[n];
      Wd := FWd[n];
      Exit;
    end;
  end;

var
  i: integer;
  kx, ky: real;
  NewPoint: TPoint;
  Hour, Min, Sec, MSec: word;
  TimePos: integer;
begin
  StartProc('DrawDayCurve');

  Image.Picture.Bitmap.Width := Image.Width;
  Image.Picture.Bitmap.Height := Image.Height;

  { 1. Находим максимум }
  Max := 0;
  for i := 0 to MinPerDay - 1 do
    Max := Math.Max(Max, Curve[i]);
  for i := 0 to High(Points) do
    Max := Math.Max(Max, Points[i].Value);

  if (Max > 1000) then
    Max := 1000 else
  if Max < EPS then
    Max := 1;

  Max := Max * 1.2;

  CalcAcc;
  Max := Round(Max / Acc) * Acc;
  LeftBord :=
    Math.Max(
      Image.Canvas.TextWidth(FloatToStr(Max)),
      Image.Canvas.TextWidth(FloatToStr(Max + Acc))
    ) + 8;
  kx := (Image.Width - 2 * LeftBord) / MinPerDay;
  ky := (Image.Height - 2 * TopBord) / Max;

  with Image.Canvas do
  begin
    { очистка }
    Brush.Color := COLOR_BACK;
    FillRect(Rect(0, 0, Image.Width, Image.Height));

    Pen.Width := 1;
    Pen.Style := psDot;
    Pen.Color := COLOR_AXIS_SUB;
    Font.Color := COLOR_TITLES;
    Font.Size := 9;

    { Шкала X }
    for i := 0 to HourPerDay do
    begin
      // TODO: optimize
      MoveTo(LeftBord + Round(i * MinPerHour * kx), TopBord);
      LineTo(LeftBord + Round(i * MinPerHour * kx), Image.Height - TopBord);
      TextOut(
        LeftBord + Round(i * MinPerHour * kx) - (TextWidth(IntToStr(i)) div 2),
        Image.Height - TopBord + 4,
        IntToStr(i)
      );
    end;

    { Шкала Y }
    for i := 1 to Round(Max / Acc) do
    begin
      // TODO: optimize
      MoveTo(LeftBord, Image.Height - TopBord - Round(i * Acc * ky));
      LineTo(Image.Width - LeftBord, Image.Height - TopBord - Round(i * Acc * ky));
      TextOut(
        4,
        Image.Height - TopBord - Round(i * Acc * ky) - (TextHeight('123') div 2),
        Format('%.' + IntToStr(Wd) + 'f', [i * Acc])
      );
    end;

    { Оси }
    Pen.Style := psSolid;
    Pen.Color := COLOR_AXIS_MAIN;
    MoveTo(LeftBord, TopBord);
    LineTo(LeftBord, Image.Height - TopBord);
    LineTo(Image.Width - LeftBord, Image.Height - TopBord);

    { Кривая }
    Pen.Width := 4;
    Pen.Style := psSolid;
    Pen.Color := ColorCurve;

    MoveTo(LeftBord, Image.Height - TopBord - Round(Curve[0] * ky));
    for i := 0 to MinPerDay - 1 do
    begin
      LineTo(LeftBord + Round(i * kx), Image.Height - TopBord - Round(Curve[i] * ky));
    end;

    { Точки }
    Pen.Width := 1;
    for i := 0 to High(Points) do
    begin
      NewPoint := Point(
        LeftBord + Round(Points[i].Time * kx),
        Image.Height - TopBord - Round(Points[i].Value  * ky)
      );

      Brush.Color := RGB(
        Round(GetRValue(ColorPointWeight0) * (1 - Points[i].Weight) + GetRValue(ColorPointWeight1) * Points[i].Weight),
        Round(GetGValue(ColorPointWeight0) * (1 - Points[i].Weight) + GetGValue(ColorPointWeight1) * Points[i].Weight),
        Round(GetBValue(ColorPointWeight0) * (1 - Points[i].Weight) + GetBValue(ColorPointWeight1) * Points[i].Weight)
      );
      Pen.Color := COLOR_BACK;
      Ellipse(
        NewPoint.X - 2 - 1,
        NewPoint.Y - 2 - 1,
        NewPoint.X + 3 + 1,
        NewPoint.Y + 3 + 1
      );
    end;

    {========================================}
    DecodeTime(now, Hour, Min, Sec, MSec);
    Min := Hour * MinPerHour + Min;
    TimePos := Round((Min / MinPerDay) * (Image.Width - 2 * LeftBord));

    Pen.Color := COLOR_TIMEPOS;
    Pen.Style := psSolid;
    MoveTo(LeftBord + TimePos, Image.Height - TopBord);
    LineTo(LeftBord + TimePos, TopBord);
    {========================================}
  end;

  FinishProc;
end;

end.

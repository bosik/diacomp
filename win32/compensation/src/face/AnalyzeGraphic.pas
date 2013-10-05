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
  DiaryDatabase,
  DiaryRecords,
  BusinessObjects;

type
  TKoofType = (kfK, kfQ, kfP, kfX);

  procedure PrepareBS(Image: TImage; Max: real; Mini: boolean;
    var kx,ky: real; var Border: integer);
  procedure DrawBS(PagePrev, PageCur, PageNext: TDiaryPage; Image: TImage; Mini: boolean);
  procedure DrawBS_Int(const Base: TDiary; FromDay,ToDay: integer; Image: TImage);

  procedure DrawKoof(Image: TImage; const KoofList: TKoofList;
    const RecList: TAnalyzeRecList; KoofType: TKoofType; DrawPoints: boolean);
  procedure CheckTimePos(Image: TImage); deprecated;

var
  LeftBord: integer;
  TimePos: integer = -1;
  
const
  Brd      = 20; { BS    }
  eSize    = 3;  { ...   }
  TopBord  = 20; { Koofs }

  COLOR_K       = clRed;
  COLOR_Q       = clBlue;
  COLOR_P       = clOlive;
  COLOR_X       = clBlack;
  COLOR_BACK    = clWhite;
  COLOR_TIMEPOS = clSilver;

implementation

uses SettingsINI;

{==============================================================================}
procedure PrepareBS(Image: TImage; Max: real; Mini: boolean;
  var kx,ky: real; var Border: integer);
{==============================================================================}
var
  w,h: integer;
  i: integer;
  x1,y1,x2,y2: integer;

  procedure CalcXY(Time: integer; Value: real; var x,y: integer); overload;
  begin
    x := Round(Border + kx/60*Time);
    y := Round(h-Border - ky*Value);
  end;

begin
  with Image.Canvas do
  begin
    w := Image.Width;
    h := Image.Height;
    Image.Picture.Bitmap.Width := w;
    Image.Picture.Bitmap.Height := h;

    { очистка }
    Brush.Color := COLOR_BACK;
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

    {if Mini then
      Border := Round(1.00*BRD)
    else
      Border := BRD; }
    Border := 3*TextHeight('24') div 2;

    ky := (h-2*Border)/max;
    kx := (w-2*Border)/24;

    { зеленая зона }
    Brush.Color := RGB(230,255,230);
    CalcXY(0,Value['BS1'], x1, y1);
    CalcXY(MinPerDay,Value['BS2'], x2, y2);
    FillRect(Rect(x1,y1,x2,y2));

    { красная зона }
    Brush.Color := $E6E6FF;
    //RGB(255,230,230);
    CalcXY(0,Value['BS2'], x1, y1);
    CalcXY(MinPerDay,Value['BS3'], x2, y2);
    FillRect(Rect(x1, y1, x2, y2));

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
        Border+Round(i*kx)-(TextWidth(IntToStr(i)) div 2),
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
  end;
end;

{==============================================================================}
procedure DrawBS(PagePrev, PageCur, PageNext: TDiaryPage; Image: TImage;
  Mini: boolean);
{==============================================================================}

  function FindMax: real;
  const
    InitMax = 8;
  var
    i: integer;
  begin
    result := InitMax;

    for i := 0 to PageCur.Count-1 do
    if (PageCur[i].RecType = TBloodRecord) and
       (TBloodRecord(PageCur[i]).Value > Result)then
      result := TBloodRecord(PageCur[i]).Value;
  end;

var
  h: integer;
  kx,ky: real;
  max: real;
  i: integer;
  cx,cy: integer;
  TempBlood: TBloodRecord;
  Border: integer;

  procedure CalcXY(Time: integer; Value: real; var x,y: integer);
  begin
    x := Round(Border + kx/60*Time);
    y := Round(h-Border - ky*Value);
  end;

  procedure GetPrevBlood(var Rec: TBloodRecord; var Delta: integer);
  begin
    if PagePrev.Count > 0 then
    begin
      TempBlood := PagePrev.LastBloodRec;
      if TempBlood = nil then
      begin
        TempBlood := PageCur.FirstBloodRec;
        Delta := 0;
      end else
        Delta := -MinPerday;
    end else
    begin
      TempBlood := PageCur.FirstBloodRec;
      Delta := 0;
    end;
  end;

  procedure GetNextBlood(var Rec: TBloodRecord; var Delta: integer);
  begin
    if PageNext.Count > 0 then
    begin
      TempBlood := PageNext.FirstBloodRec;
      if TempBlood = nil then
      begin
        TempBlood := PageCur.LastBloodRec;
        Delta := 0;
      end else
        Delta := +MinPerday;
    end else
    begin
      TempBlood := PageCur.LastBloodRec;
      Delta := 0;
    end;
  end;

begin
  max := FindMax;
  PrepareBS(Image,Max,Mini,kx,ky,Border);
  Image.Picture.Bitmap.Width := Image.Width;
  Image.Picture.Bitmap.Height := Image.Height;

  if (PageCur = nil) or (PageCur.Count = 0) then exit;

  h := Image.Height;

  with Image.Canvas do
  begin
    { кривая }
    Pen.Width := 1;
    if Mini then
      Pen.Style := psSolid
    else
      Pen.Style := psDot;

    Pen.Color := clMaroon;

    { последняя точка предыдущего дня }
    GetPrevBlood(TempBlood, i);
    if TempBlood <> nil then
    begin
      CalcXY(
        TempBlood.Time+i,
        TempBlood.Value,
        cx,cy);
      MoveTo(cx,cy);
    end;

    { основная линия }
    for i := 0 to PageCur.Count-1 do
    if (PageCur[i].RecType = TBloodRecord) then
    begin
      CalcXY(
        TBloodRecord(PageCur[i]).Time,
        TBloodRecord(PageCur[i]).Value,
        cx,cy);
      LineTo(cx,cy);
    end;

    { продолжение на следующий день }
    GetNextBlood(TempBlood, i);
    if TempBlood <> nil then
    begin
      CalcXY(
        TempBlood.Time+i,
        TempBlood.Value,
       cx,cy);
      LineTo(cx,cy);
    end;

    { точки }
    if not mini then
    begin   
      Pen.Color := clMaroon;
      Pen.Style := psSolid;
      for i := 0 to PageCur.Count - 1 do
      if (PageCur[i].RecType = TBloodRecord) then
      begin
        CalcXY(
          TBloodRecord(PageCur[i]).Time,
          TBloodRecord(PageCur[i]).Value,
          cx,cy);

        if TBloodRecord(PageCur[i]).PostPrand then
          Brush.Color := COLOR_BACK
        else
          Brush.Color := clMaroon;

        Ellipse(cx-eSize,cy-eSize,cx+eSize,cy+eSize);
      end;
    end;
  end;
end;

{==============================================================================}
procedure DrawBS_Int(const Base: TDiary; FromDay,ToDay: integer; Image: TImage);
{==============================================================================}
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
      SetLength(Points[Zone].R,length(Points[Zone].R)+1);
      Points[Zone].R[high(Points[Zone].R)].Value := TBloodRecord(Base[n][i]).Value;
      Points[Zone].R[high(Points[Zone].R)].Pos := Frac(Base[n][i].Time/60);
    end;

    { вычисление средних и максимума }
    Max := InitMax;
    for i := 0 to 23 do
    begin
      Points[i].AvqPos := 0;
      Points[i].AvqValue := 0;
      if length(Points[i].R)>0 then
      begin
        for n := 0 to high(Points[i].R) do
        begin
          Points[i].AvqPos := Points[i].AvqPos+Points[i].R[n].Pos;
          Points[i].AvqValue := Points[i].AvqValue+Points[i].R[n].Value;
        end;
        Points[i].AvqPos := Points[i].AvqPos/length(Points[i].R);
        Points[i].AvqValue := Points[i].AvqValue/length(Points[i].R);
      end;

      if Points[i].AvqValue>Max then
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
      TempBlood := Base[Day+1].FirstBloodRec;
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

{==============================================================================}
procedure DrawKoof(Image: TImage; const KoofList: TKoofList;
  const RecList: TAnalyzeRecList; KoofType: TKoofType; DrawPoints: boolean);
{==============================================================================}
var
  Acc: real;
  Max: real;
  Wd: integer;

  procedure CalcAcc;
  const
    FAcc: array[1..13] of real = (1000, 100, 10, 5, 1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001);
    FWd: array[1..13] of integer =  (0,   0,  0, 0, 0,   1,   1,    2,    2,     3,     3,      4,      4);
  var
    n,LabelHeight: integer;
  begin
    LabelHeight := Image.Canvas.TextHeight('123');
    Acc := 1;
    Wd := 0;
    for n := high(FAcc) downto 1 do
    if Trunc(Max/FAcc[n])*LabelHeight <= Image.Height-2*TopBord then
    begin
      Acc := FAcc[n];
      Wd := FWd[n];
      Exit;
    end;
  end;

const
  COLOR_K = $EEEEFF;
  COLOR_Q = $FFEEEE;
  COLOR_P = $80FFFF;
  COLOR_X = $EEEEEE;
  TEXT_NO_DATA = 'Нет данных';

var
  i: integer;
  kx,ky: real;
  v: real;
  PrevPoint,NewPoint: TPoint;
  Hour, Min, Sec, MSec: word;

  function GetK(const Rec: TAnalyzeRec): real;
  begin
    if Rec.Carbs > 0 then
      result := (
        -KoofList[Rec.Time].p * Rec.Prots
        +KoofList[Rec.Time].q * Rec.Ins
        +(Rec.BSOut-Rec.BSIn)
      ) / Rec.Carbs
    else
      result := -10;
  end;

  function GetX(k,q,p: real): real; overload;
  const
    PROTS = 0.25;
  begin
    if q <> 0 then
      result := (k+PROTS*p)/q
    else
      result := 0;
  end;

  function GetX(n: integer): real; overload;
  begin
    result := GetX (KoofList[n].k, KoofList[n].q, KoofList[n].p);
  end;

  function GetX(const Rec: TAnalyzeRec): real; overload;
  begin
    result := GetX(Rec.Time);
  end;

  function GetQ(const Rec: TAnalyzeRec): real;
  begin
    if Rec.Ins > 0 then
      Result := (
        +KoofList[Rec.Time].p * Rec.Prots
        +KoofList[Rec.Time].k * Rec.Carbs
        -(Rec.BSOut-Rec.BSIn)
      ) / (Rec.Ins)
    else
      Result := -10;
  end;

  function GetP(const Rec: TAnalyzeRec): real;
  begin
    if Rec.Prots > 0 then
      Result := (
        -KoofList[Rec.Time].k * Rec.Carbs
        +KoofList[Rec.Time].q * Rec.Ins
        +(Rec.BSOut-Rec.BSIn)
      ) / (Rec.Prots)
    else
      Result := -10;
  end;

  procedure Line;
  begin
    with Image.Canvas do
    begin
      MoveTo(PrevPoint.X,PrevPoint.Y);
      LineTo(NewPoint.X,NewPoint.Y);
      MoveTo(PrevPoint.X,PrevPoint.Y+1);
      LineTo(NewPoint.X,NewPoint.Y+1);
    end;
  end;

 { function GetMax(a,b: real): real; overload;
  begin
    if a > b then
      result := a
    else
      result := b;
  end;

  function GetMax(a,b: integer): integer; overload;
  begin
    if a > b then
      result := a
    else
      result := b;
  end;   }

begin
  StartProc('DrawKoof');

  Image.Picture.Bitmap.Width := Image.Width;
  Image.Picture.Bitmap.Height := Image.Height;

  { 1. Если нет данных }
  {if length(KoofList) = 0 then
  with Image.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect(0,0,Image.Width,Image.Height));
    Font.Color := clSilver;
    Font.Size := 20;
    TextOut(
      (Image.Width-TextWidth(TEXT_NO_DATA)) div 2,
      50,
      TEXT_NO_DATA
    );
    exit;
  end;  }

  { 2.Находим максимум }
  Max := 0;
  case KoofType of
    kfK: for i := 0 to MinPerDay-1 do Max := Math.Max(Max, KoofList[i].k);
    kfQ: for i := 0 to MinPerDay-1 do Max := Math.Max(Max, KoofList[i].q);
    kfP: for i := 0 to MinPerDay-1 do Max := Math.Max(Max, KoofList[i].p);
    kfX: for i := 0 to MinPerDay-1 do Max := Math.Max(Max, GetX(i));
  end;

  if (Max > 1000) then
      Max := 1000;

  if Max <= 0 then
    Max := 1
  else
    Max := Max * 1.2;

  CalcAcc;
  {*}Max := Round(Max / Acc) * Acc;
  LeftBord :=
    Math.Max(
      Image.Canvas.TextWidth(FloatToStr(Max)),
      Image.Canvas.TextWidth(FloatToStr(Max + Acc))
    )+8;
  kx := (Image.Width-2*LeftBord)/MinPerDay;
  ky := (Image.Height-2*TopBord)/Max;

  with Image.Canvas do
  begin
    { очистка }
    Brush.Color := clWhite;
    FillRect(Rect(0,0,Image.Width,Image.Height));

    { риски X }
    Pen.Width := 1;
    Pen.Style := psDot;
    Pen.Color := clSilver;
    Font.Color := clBlack;
    Font.Size := 9;
    for i := 0 to HourPerDay do
    begin
      MoveTo(LeftBord+Round(i*60*kx),TopBord);
      LineTo(LeftBord+Round(i*60*kx),Image.Height-TopBord);
      TextOut(
        LeftBord + Round(i*60*kx) - (TextWidth(IntToStr(i)) div 2),
        Image.Height-TopBord+4,
        IntToStr(i)
      );
    end;

    { риски Y }

    for i := 1 to Round(Max/Acc) do
    begin
      MoveTo(
        LeftBord,
        Image.Height-TopBord-Round(i*Acc*ky));
      LineTo(
        Image.Width-LeftBord,
        Image.Height-TopBord-Round(i*Acc*ky));

      TextOut(
        4,
        Image.Height-TopBord-Round(i*Acc*ky)-
        (TextHeight('123') div 2),
        Format('%.' + IntToStr(Wd) + 'f', [i * Acc])
      );
    end;

    { ОСИ }
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    MoveTo(LeftBord,TopBord);
    LineTo(LeftBord,Image.Height-TopBord);
    LineTo(Image.Width-LeftBord,Image.Height-TopBord);

    { КОЭФФИЦИЕНТ }
    Pen.Width := 1;
    Pen.Style := psSolid;

    case KoofType of
      kfK: Pen.Color := COLOR_K;
      kfQ: Pen.Color := COLOR_Q;
      kfP: Pen.Color := COLOR_P;
      kfX: Pen.Color := COLOR_X;
    end;

    {if 1-KoofList[high(KoofList)].Pos+KoofList[0].Pos<>0 then
      w := KoofList[0].Pos/(1-KoofList[high(KoofList)].Pos+KoofList[0].Pos)
    else
      w := 0;

    case KoofType of
      kfK: v := (1-w)*KoofList[0].K + w*KoofList[high(KoofList)].K;
      kfQ: v := (1-w)*KoofList[0].Q + w*KoofList[high(KoofList)].Q;
      kfP: v := (1-w)*KoofList[0].P + w*KoofList[high(KoofList)].P;
      kfX: v := (1-w)*GetX(0)       + w*GetX(high(KoofList));
    end;

    PrevPoint := Point(
      LeftBord,
      Image.Height-TopBord-Round(v*ky)
    );   }

    case KoofType of
      kfK: MoveTo(LeftBord, Image.Height-TopBord-Round(KoofList[0].k*ky));
      kfQ: MoveTo(LeftBord, Image.Height-TopBord-Round(KoofList[0].q*ky));
      kfP: MoveTo(LeftBord, Image.Height-TopBord-Round(KoofList[0].p*ky));
      kfX: MoveTo(LeftBord, Image.Height-TopBord-Round(GetX(0)*ky));
    end;

    Pen.Width := 4;

    for i := 0 to MinPerDay - 1 do
    begin
      {if (KoofList[i].Proved)and
         (KoofList[(i-1+length(KoofList)) mod length(KoofList)].Proved)then
      Pen.Style := psSolid else
        Pen.Style := psDot; }
      case KoofType of
        kfK: LineTo(LeftBord+Round(i*kx), Image.Height-TopBord-Round(KoofList[i].k*ky));
        kfQ: LineTo(LeftBord+Round(i*kx), Image.Height-TopBord-Round(KoofList[i].q*ky));
        kfP: LineTo(LeftBord+Round(i*kx), Image.Height-TopBord-Round(KoofList[i].p*ky));
        kfX: LineTo(LeftBord+Round(i*kx), Image.Height-TopBord-Round(GetX(i)*ky));
      end;

     (* case KoofType of
        kfK: NewPoint := Point(
               LeftBord+Round((i+KoofList[i].Pos)*kx),
               Image.Height-TopBord-Round(KoofList[i].k*ky)
             );
        kfQ: NewPoint := Point(
               LeftBord+Round((i+KoofList[i].Pos)*kx),
               Image.Height-TopBord-Round(KoofList[i].q*ky)
             );
        kfP: NewPoint := Point(
               LeftBord+Round((i+KoofList[i].Pos)*kx),
               Image.Height-TopBord-Round(KoofList[i].p*ky)
             );
        kfX: NewPoint := Point(
               LeftBord+Round((i+KoofList[i].Pos)*kx),
               Image.Height-TopBord-Round(GetX(i)*ky)
             );
      end;

      Line;

      if (KoofList[i].Proved) then
      begin
        Pen.Style := psSolid;
        Brush.Color := Pen.Color;
        {Rectangle(
          NewPoint.X-2,
          NewPoint.Y-2,
          NewPoint.X+4,
          NewPoint.Y+4
        );}
      end;
      Brush.Color := clWhite;
      PrevPoint := NewPoint;  *)


    end;

    Pen.Width := 1;

   { NewPoint := Point(
      Image.Width-LeftBord,
      Image.Height-TopBord-Round(v*ky)
    );

    Line;   }

    if DrawPoints then

    { Точки для ОКК }
   { if KoofType = kfX then
    begin
      for i := 0 to high(KoofList) do
      for j := high(KoofList[i].Points) downto 0 do
      begin
        NewPoint := Point(
          LeftBord+Round((i+KoofList[i].Points[j].Pos)*kx),
          Image.Height-TopBord-Round(KoofList[i].Points[j].Value/KoofList[i].Q*ky)
        );
        Brush.Color := RGB(
          Round(KoofList[i].Points[j].Tag*255),
          0,0);
        Pen.Color := Brush.Color;
        Rectangle(
          NewPoint.X-1,
          NewPoint.Y-1,
          NewPoint.X+2,
          NewPoint.Y+2
        );
      end;
    end else   }
    { Точки для К * }
    if KoofType = kfK then
    begin
      for i := 0 to high(RecList) do
      begin
        {v := (RecList[i].DeltaBS + KoofList[Reclist[i].Time].q*RecList[i].Ins)/
          RecList[i].Carbs;  }
        v := GetK(RecList[i]);

        NewPoint := Point(
          LeftBord+Round(RecList[i].Time*kx),
          Image.Height-TopBord-Round(v  * ky)
        );

        Brush.Color := RGB(Round(RecList[i].Weight*255),0,0);
        Pen.Color := clWhite;//RGB(255-Round(RecList[i].Weight*255),255,255);
        Ellipse(
          NewPoint.X-2-1,
          NewPoint.Y-2-1,
          NewPoint.X+3+1,
          NewPoint.Y+3+1
        );
      end;
    end else
    if KoofType = kfQ then
    begin
      for i := 0 to high(RecList) do
      begin
        v := GetQ(RecList[i]);

        NewPoint := Point(
          LeftBord+Round(RecList[i].Time*kx),
          Image.Height-TopBord-Round(v  * ky)
        );

        Brush.Color := RGB(0, 0, Round(RecList[i].Weight*255));
        Pen.Color := RGB(255, 255, 255-Round(RecList[i].Weight*255));
        Ellipse(
          NewPoint.X-2,
          NewPoint.Y-2,
          NewPoint.X+3,
          NewPoint.Y+3
        );
      end;
    end else
    if KoofType = kfP then
    begin
      for i := 0 to high(RecList) do
      begin
        v := GetP(RecList[i]);

        NewPoint := Point(
          LeftBord+Round(RecList[i].Time*kx),
          Image.Height-TopBord-Round(v  * ky)
        );

        Brush.Color := RGB(Round(RecList[i].Weight*255), Round(RecList[i].Weight*255), 0);
        Pen.Color := Brush.Color;
        Rectangle(
          NewPoint.X-1,
          NewPoint.Y-1,
          NewPoint.X+2,
          NewPoint.Y+2
        );
      end;
    end else
    if KoofType = kfX then
    begin
      for i := 0 to high(RecList) do
      begin
        {v := (RecList[i].DeltaBS + KoofList[Reclist[i].Time].q*RecList[i].Ins)/
          RecList[i].Carbs;  }
        v := GetX(RecList[i]);

        NewPoint := Point(
          LeftBord+Round(RecList[i].Time*kx),
          Image.Height-TopBord-Round(v  * ky)
        ); 

        Brush.Color :=
        RGB(
          Round(RecList[i].Weight*255),
          0,0);
        Pen.Color := Brush.Color;
        Rectangle(
          NewPoint.X-1,
          NewPoint.Y-1,
          NewPoint.X+2,
          NewPoint.Y+2
        );
      end;
    end;

    {========================================}
    DecodeTime(now, Hour, Min, Sec, MSec);
    Min := Hour*MinPerHour + Min;
    TimePos := Round({(Sec+MSec/1000)/60}  (Min/MinPerDay)*(Image.Width-2*LeftBord));

    Pen.Color := COLOR_TIMEPOS;
    Pen.Style := psSolid;
    //Pen.Mode := pmNotXor;
    MoveTo(LeftBord+TimePos,Image.Height-TopBord);
    LineTo(LeftBord+TimePos,TopBord);
    //Pen.Mode := pmCopy;
    {========================================}
  end;

  FinishProc;
end;

procedure CheckTimePos(Image: TImage);
var
  NewTimePos: integer;
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(now, Hour, Min, Sec, MSec);
  Min := Hour*MinPerHour + Min;
  NewTimePos := Round(Min/MinPerDay*(Image.Width-2*LeftBord));
  if NewTimePos<>TimePos then
  begin    
    with Image.Canvas do
    begin
      Pen.Color := COLOR_TIMEPOS;
      Pen.Mode := pmNotXor;
      Pen.Style := psSolid;
      MoveTo(LeftBord+TimePos,Image.Height-TopBord);
      LineTo(LeftBord+TimePos,TopBord);
      MoveTo(LeftBord+NewTimePos,Image.Height-TopBord);
      LineTo(LeftBord+NewTimePos,TopBord);
      Pen.Mode := pmCopy;
    end;
    TimePos := NewTimePos;
  end;
end;

(*
{==============================================================================}
procedure DrawKoofs(const Koofs: TKoofList; Image: TImage);
{==============================================================================}
const
  COLOR_K = clRed;
  COLOR_Q = clBlue;
  COLOR_P = clOlive;
  COLOR_X = clBlack;
var
  i: integer;
  MaxK,MaxQ: real;
  kx,kyk,kyq: real;
  s: string;
  v,w: real;
begin
  if length(Koofs) = 0 then
  begin
    Image.Canvas.Brush.Color := clWhite;
    Image.Canvas.FillRect(Rect(0,0,Image.Width,Image.Height));
    exit;
  end;

  MaxQ := 0;//10;
  MaxK := 0;//.8;
  for i := 0 to high(Koofs) do
  begin
    if Koofs[i].k>MaxK then MaxK := Koofs[i].k;
    if Koofs[i].p>MaxK then MaxK := Koofs[i].p;
    if Koofs[i].q>MaxQ then MaxQ := Koofs[i].q;
  end;

  if (MaxK=0)or(MaxQ=0) then exit;

  kx := (Image.Width-2*LeftBord)/length(Koofs);
  kyk := (Image.Height-2*TopBord)/MaxK*0.8;
  kyq := (Image.Height-2*TopBord)/MaxQ*0.8;

  with Image.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect(0,0,Image.Width,Image.Height));

    { СЕТКА }
    Pen.Width := 1;
    Pen.Style := psDot;
    Pen.Color := clSilver;
    for i := 0 to high(Koofs) do
    begin
      MoveTo(LeftBord+Round(i*kx),TopBord);
      LineTo(LeftBord+Round(i*kx),Image.Height-TopBord);
    end;

    { ОСИ }
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    MoveTo(LeftBord,TopBord);
    LineTo(LeftBord,Image.Height-TopBord);
    LineTo(Image.Width-LeftBord,Image.Height-TopBord);

    { КОЭФФИЦИЕНТЫ }
    Pen.Width := 1;
    Pen.Style := psSolid;

    { K }
    Brush.Color := clWhite;
    Pen.Color := COLOR_K;

    w := Koofs[0].Pos/(1-Koofs[high(Koofs)].Pos+Koofs[0].Pos);
    v := (1-w)*Koofs[0].K + w*Koofs[high(Koofs)].K;
    MoveTo(
      LeftBord,
      TopBord+Image.Height-Round(v*kyk)
    );
    for i := 0 to high(Koofs) do
    begin
      if Koofs[i].Proved then
        Pen.Style := psSolid else
        Pen.Style := psDot;
      LineTo(
        LeftBord+Round((i+Koofs[i].Pos)*kx),
        Image.Height-TopBord-Round(Koofs[i].k*kyk)
      );
    end;

    { Q }
    Pen.Color := COLOR_Q;
    w := Koofs[0].Pos/(1-Koofs[high(Koofs)].Pos+Koofs[0].Pos);
    v := (1-w)*Koofs[0].Q + w*Koofs[high(Koofs)].Q;
    MoveTo(
      LeftBord,
      Image.Height-TopBord-Round(v*kyq)
    );
    for i := 0 to high(Koofs) do
    begin
      if Koofs[i].Proved then
        Pen.Style := psSolid else
        Pen.Style := psDot;
      LineTo(
        LeftBord+Round((i+Koofs[i].Pos)*kx),
        Image.Height-TopBord-Round(Koofs[i].q*kyq)
      );
    end;

    { P }
    Pen.Color := COLOR_P;
    w := Koofs[0].Pos/(1-Koofs[high(Koofs)].Pos+Koofs[0].Pos);
    v := (1-w)*Koofs[0].P + w*Koofs[high(Koofs)].P;
    MoveTo(
      LeftBord,
      Image.Height-TopBord-Round(v*kyk)
    );
    for i := 0 to high(Koofs) do
    begin
      if Koofs[i].Proved then
        Pen.Style := psSolid else
        Pen.Style := psDot;
      LineTo(
        LeftBord+Round((i+Koofs[i].Pos)*kx),
        Image.Height-TopBord-Round(Koofs[i].p*kyk)
      );
    end;

    { X }
    Pen.Color := COLOR_X;
    if Koofs[high(Koofs)].q<>0 then
      MoveTo(
        LeftBord+Round((-1+Koofs[high(Koofs)].Pos)*kx),
        Image.Height-TopBord-Round((Koofs[high(Koofs)].k+0.25*Koofs[high(Koofs)].p)/Koofs[high(Koofs)].q *kyk)
      )
    else
      MoveTo(LeftBord+Round((-1+Koofs[high(Koofs)].Pos)*kx),500);

    for i := 0 to high(Koofs) do
    if (Koofs[i].q<>0)then
    begin
      if (Koofs[i].Proved)then
        Pen.Style := psSolid else
        Pen.Style := psDot;
      LineTo(
        LeftBord+Round((i+Koofs[i].Pos)*kx),
        Image.Height-TopBord-Round((Koofs[i].k+0.25*Koofs[i].p)/Koofs[i].q *kyk)
      );
    end;

    { zones }
    Brush.Color := clWhite;

    { Number }
    Font.Color := clBlack;
    for i := 0 to length(Koofs) do
    if (24*i)mod(length(Koofs)) = 0 then
      TextOut(
        LeftBord+Round(i*kx)-(TextWidth(IntToStr(i)) div 2),
        Image.Height-TopBord+4,
        IntToStr(24*i div length(Koofs))
      );

    for i := 0 to high(Koofs) do
    begin
      { K Value }
      if Koofs[i].Proved then
        Font.Color := COLOR_K
      else
        Font.Color := clSilver;

      s := FloatToStr(Round(Koofs[i].K*1000)/1000);
      TextOut(
        LeftBord+Round((i+0.5)*kx)-(TextWidth(s) div 2),
        TopBord+20,
        s
      );

      { Q Value }
      if Koofs[i].Proved then
        Font.Color := COLOR_Q
      else
        Font.Color := clSilver;

      s := FloatToStr(Round(Koofs[i].Q*1000)/1000);
      TextOut(
        LeftBord+Round((i+0.5)*kx)-(TextWidth(s) div 2),
        TopBord+50,
        s
      );

      { P Value }
      if Koofs[i].Proved then
        Font.Color := COLOR_P
      else
        Font.Color := clSilver;

      s := FloatToStr(Round(Koofs[i].P*1000)/1000);
      TextOut(
        LeftBord+Round((i+0.5)*kx)-(TextWidth(s) div 2),
        TopBord+80,
        s
      );

      { K/Q Value }
      {
      if Koofs[i].Proved then
        Font.Color := COLOR_X
      else
        Font.Color := clSilver;

      s := FloatToStr(Round(Koofs[i].K/Koofs[i].Q*1000)/1000);
      TextOut(
        LeftBord+Round((i+0.5)*kx)-(TextWidth(s) div 2),
        TopBord+130,
        s
      );
      }
    end;
  end;
end; *)

end.



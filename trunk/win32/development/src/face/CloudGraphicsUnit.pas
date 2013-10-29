unit CloudGraphicsUnit;

{ Модуль для работы с графиками }

interface

uses
  Graphics, ExtCtrls, Classes, SysUtils, Windows;

type
  TPointsCloud = array of
  record
    x,y,z: real;
  end;

  procedure AddPoint(var Cloud: TPointsCloud; x,y,z: real);
  { общий случай графика }
  procedure DrawCloud(const Cloud: TPointsCloud; Image: TImage;
    DiscX,DiscY: real);
  { график, для которого ось абсцисс расчитана на сутки }
  procedure DrawCloudDay(const Cloud: TPointsCloud; Image: TImage;
    InitMaxY,DiscX,DiscY: real);

const
  BackColor   = $FFFFFF-clWhite;
  PointsColor = $FFFFFF-clRed;
  OxesColor   = $FFFFFF-clBlack;
  LinesColor  = $FFFFFF-clSilver;
  FontColor   = $FFFFFF-clBlack;
implementation

{=========================================================}
function RealToStr(x: real): string;
{=========================================================}
begin
  Result := FloatToStr(Round(x*100)/100);
end;

{=========================================================}
procedure AddPoint(var Cloud: TPointsCloud; x,y,z: real);
{=========================================================}
begin
  SetLength(Cloud,length(Cloud)+1);
  Cloud[high(Cloud)].x := x;
  Cloud[high(Cloud)].y := y;
  Cloud[high(Cloud)].z := z;
end;

{=========================================================}
procedure DrawCloud(const Cloud: TPointsCloud; Image: TImage;
  DiscX,DiscY: real);
{=========================================================}
{
  Cloud - облако точек
  Image - компонент для рисования
  DiscX,DiscY - дискретность расстановки подписей
}   
  procedure TruncUp(var x: real; D: real);
  begin
    if Frac(x/d)<>0 then
      if x>0 then
        x := Trunc(x/d)*d+d
      else
        x := Trunc(x/d)*d;
  end;

  procedure TruncDown(var x: real; D: real);
  begin
    if Frac(x/d)<>0 then
      if x<0 then
        x := Trunc(x/d)*d-d
      else
        x := Trunc(x/d)*d;
  end;

const
  BRD = 30;
var
  w,h: integer;
  kx,ky: real;
  MCX,MCY: integer;  // Marks Count
  MaxX,MaxY,MaxZ,MinX,MinY,MinZ: real;
  DX,DY: real;
  i: integer;
  b: integer;
begin
  if length(Cloud)=0 then exit;

  MinX := Cloud[0].x;
  MinY := Cloud[0].y;
  MinZ := Cloud[0].z;
  MaxX := Cloud[0].x;
  MaxY := Cloud[0].y;
  MaxZ := Cloud[0].z;

  for i := 0 to high(Cloud) do
  begin
    if Cloud[i].x<MinX then MinX := Cloud[i].x;
    if Cloud[i].y<MinY then MinY := Cloud[i].y;
    if Cloud[i].z<MinZ then MinZ := Cloud[i].z;

    if Cloud[i].x>MaxX then MaxX := Cloud[i].x;
    if Cloud[i].y>MaxY then MaxY := Cloud[i].y;

    if Cloud[i].z>MaxZ then MaxZ := Cloud[i].z;
  end;

  //MaxX := 3;

  TruncUp(MaxY,DiscY);
  TruncDown(MinY,DiscY);
  TruncUp(MaxX,DiscX);
  TruncDown(MinX,DiscX);

  DX := MaxX-MinX;
  DY := MaxY-MinY;

  if DX=0 then DX := 1;
  if DY=0 then DY := 1;

  MCX := Round(DX/DiscX);
  MCY := Round(DY/DiscY);
  w := Image.Width;
  h := Image.Height;
  kx := (w-2*BRD)/DX;
  ky := (h-2*BRD)/DY;

  with Image.Canvas do
  begin
    Brush.Color := BackColor;
    FillRect(Rect(0,0,w,h));
    Pen.Color := OxesColor;
    Pen.Width := 1;
    Pen.Style := psDot;
    Pen.Color := LinesColor;
    Font.Color := FontColor;

    { риски X }
    for i := 0 to MCX do
    begin
      MoveTo(BRD+Round(i/MCX*DX*kx),BRD);
      LineTo(BRD+Round(i/MCX*DX*kx),h-BRD+2);
      TextOut(
        BRD+Round(i/MCX*(MaxX-MinX)*kx)-(TextWidth('0') div 2),
        h-BRD+4,
        RealToStr(i/MCX*DX+MinX)
      );
    end;

    { риски Y }
    for i := 0 to MCY do
    begin
      MoveTo(BRD-3,Round(h-BRD-ky*i/MCY*DY));
      LineTo(w-BRD,Round(h-BRD-ky*i/MCY*DY));
      TextOut(
        BRD-28,
        h-BRD-Round(i/MCY*(MaxY-MinY)*ky)-(TextHeight('0') div 2),
        RealToStr(i/MCY*DY+MinY)
      );
    end;

    { оси }
    Pen.Style := psSolid;
    //Pen.Color := clWhite;
    MoveTo(BRD-Round(MinX*kx),BRD);
    LineTo(BRD-Round(MinX*kx),h-BRD);
    MoveTo(BRD,Round(h-BRD-ky*(0-MinY)));
    LineTo(w-BRD,Round(h-BRD-ky*(0-MinY)));

    { точки }
    Pen.Style := psSolid;
    Pen.Width := 2;

    for i := 0 to high(Cloud) do
    begin
      if MaxZ<>MinZ then
        b := Round((Cloud[i].z-MinZ)/(MaxZ-MinZ)*255)
      else
        b := 255;
      Pen.Color := {PointsColor;//}RGB(255,b,255);

      Rectangle(
        -0+Round(BRD+(Cloud[i].x-MinX)*kx),
        -0+Round(h-BRD-(Cloud[i].y-MinY)*ky),
        +2+Round(BRD+(Cloud[i].x-MinX)*kx),
        +2+Round(h-BRD-(Cloud[i].y-MinY)*ky)
      );
    end;
  end;
end;

{=========================================================}
procedure DrawCloudDay(const Cloud: TPointsCloud; Image: TImage;
  InitMaxY,DiscX,DiscY: real);
{=========================================================}
{
  Cloud - облако точек
  Image - компонент для рисования
  InitMaxX, InitMaxY - начальные границы
    (предполагается, что координаты точек неотрицательны)
  DiscX,DiscY - дискретность расстановки подписей

}

  procedure TestMin(var Min: real; R: real);
  begin
    if R < Min then Min := R;
  end;

  procedure TestMax(var Max: real; R: real);
  begin
    if R > Max then Max := R;
  end;

const
  BRD = 30;
  MarksCount = 20;
var
  w,h: integer;
  kx,ky: real;
  MCX,MCY: integer;
  MinX,MinY,MinZ: real;
  MaxX,MaxY,MaxZ: real;
  i: integer;
  b: integer;
begin
  if length(Cloud)=0 then exit;

  MinX := 0;
  MaxX := 1440;
  MinY := 0;
  MaxY := InitMaxY;
  MinZ := Cloud[0].z;
  MaxZ := Cloud[0].z;
  for i := 0 to high(Cloud) do
  begin
    TestMin(MinX, Cloud[i].X);
    TestMin(MinY, Cloud[i].Y);
    TestMin(MinZ, Cloud[i].Z);

    TestMax(MaxX, Cloud[i].X);
    TestMax(MaxY, Cloud[i].Y);
    TestMax(MaxZ, Cloud[i].Z);
  end;

  MCX := Round(MaxX/DiscX);
  MCY := Round(MaxY/DiscY);
  w := Image.Width;
  h := Image.Height;
  ky := (h-2*BRD)/MaxY;
  kx := (w-2*BRD)/MaxX;

  with Image.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect(0,0,w,h));
    Pen.Color := clBlack;
    Pen.Width := 1;

    { оси }
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    MoveTo(BRD,BRD);
    LineTo(BRD,h-BRD);
    LineTo(w-BRD,h-BRD);

    { риски Y }
    Pen.Style := psDot;
    Pen.Color := clSilver;
    for i := 1 to MCX do
    begin
      MoveTo(BRD-3,Round(h-BRD-ky*i/MCX*MaxY));
      LineTo(w-BRD,Round(h-BRD-ky*i/MCX*MaxY));
      TextOut(
        BRD-28,
        h-BRD-Round(ky*i/100)-(TextHeight('0') div 2),
        FloatToStr(i/MCX*MaxX/100)
      );
    end;

    { риски X }
    for i := 1 to MCY do
    begin
      MoveTo(BRD+Round(i/MCY*MaxX*kx),BRD);
      LineTo(BRD+Round(i/MCY*MaxX*kx),h-BRD+2);
      TextOut(
        BRD+Round(i/MCY*MaxX*kx)-(TextWidth('0') div 2),
        h-BRD+4,
        FloatToStr(i/MCY*MaxX)
      );
    end;

    { точки }
    Pen.Style := psSolid;
    Pen.Width := 2;

    for i := 0 to high(Cloud) do
    begin
      if MaxZ<>MinZ then
        b := Round(Cloud[i].z/(MaxZ-MinZ)*255)
      else
        b := 255;
      Pen.Color := RGB(b,0,0);

      Rectangle(
        -2+Round(BRD+Cloud[i].x*kx),
        -2+Round(h-BRD-Cloud[i].y*ky),
        +2+Round(BRD+Cloud[i].x*kx),
        +2+Round(h-BRD-Cloud[i].y*ky)
      );
    end;
  end;
end;

end.

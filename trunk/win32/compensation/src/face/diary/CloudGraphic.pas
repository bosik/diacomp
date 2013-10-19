unit CloudGraphic;

interface

uses
  Graphics,
  Classes,
  Controls,
  SysUtils,
  DiaryRoutines;

type
  T3DPoint = record
    X,Y,Z: real;
  end;

  TPointsCloud = array of T3DPoint;

  TCloud = class
  private
    FData: TPointsCloud;
    function GetPoint(Index: integer): T3DPoint;
  protected
    function AddPoint(const X,Y,Z: real): integer; overload;
    procedure Clear;
    function Count: integer;

    property Data[Index: integer]: T3DPoint read GetPoint; default ;
  end;

  TCloudGraphic = class(TGraphicControl)
  private
    FBitMap: TBitMap;
    FCloud: TCloud;
  protected
    procedure Paint; override;
    procedure DrawIntoBuffer;
  public
    function AddPoint(const X,Y,Z: real): integer; overload;
    function AddPoint(const Point: T3DPoint): integer; overload;
    procedure Clear;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redraw;
  published
    property Align;
    property Anchors;
    property Cloud: TCloud read FCloud;
    property ParentShowHint;
    property ShowHint;
    property Visible;
  end;

  procedure Register;

const
  BackColor   = $FFFFFF-clWhite;
  PointsColor = $FFFFFF-clRed;
  OxesColor   = $FFFFFF-clBlack;
  LinesColor  = $FFFFFF-clSilver;
  FontColor   = $FFFFFF-clBlack;

implementation

{=========================================================}
procedure Register;
{=========================================================}
begin
  RegisterComponents('Компенсация', [TCloudGraphic]);
end;

function RGB(R,G,B: byte): TColor;
begin
  result:=R + (G shl 8) + (B shl 16);
end;

{ TCloudGraphic }

{=========================================================}
constructor TCloudGraphic.Create(AOwner: TComponent);
{=========================================================}
begin
  inherited Create(AOwner);

  Width:=100;
  Height:=75;

  FBitMap:=TBitMap.Create;
  FBitMap.Width:=Width;
  FBitMap.Height:=Height;

  FCloud:=TCloud.Create;
end;

{=========================================================}
destructor TCloudGraphic.Destroy;
{=========================================================}
begin
  FBitMap.Free;
  FCloud.Free;
  inherited;
end;

{=========================================================}
procedure TCloudGraphic.Paint;
{=========================================================}
begin
  if (csDesigning in ComponentState) then
	with inherited Canvas do
	begin
	  Pen.Style:=psDash;
	  Brush.Style:=bsClear;
	  Rectangle(0,0,Width,Height);
	end;
end;

{=========================================================}
function TCloudGraphic.AddPoint(const X, Y, Z: real): integer;
{=========================================================}
begin
  result:=FCloud.AddPoint(X,Y,Z);
end;

{=========================================================}
function TCloudGraphic.AddPoint(const Point: T3DPoint): integer;
{=========================================================}
begin
  result:=FCloud.AddPoint(Point.X,Point.Y,Point.Z);
end;

{=========================================================}
procedure TCloudGraphic.Clear;
{=========================================================}
begin
  FCloud.Clear;
end;

{ TCloud }

{=========================================================}
function TCloud.AddPoint(const X, Y, Z: real): integer;
{=========================================================}
begin
  result:=length(FData);
  SetLength(FData,result+1);
  FData[result].X:=X;
  FData[result].Y:=Y;
  FData[result].Z:=Z;
end;

{=========================================================}
procedure TCloud.Clear;
{=========================================================}
begin
  SetLength(FData,0);
end;

{=========================================================}
function TCloud.Count: integer;
{=========================================================}
begin
 result:=length(FData);
end;

{=========================================================}
function TCloud.GetPoint(Index: integer): T3DPoint;
{=========================================================}
begin
  result:=FData[Index];
end;

{=========================================================}
procedure TCloudGraphic.DrawIntoBuffer;
{=========================================================}
const
  BRD = 30;

  procedure TruncUp(var x: real; D: real);
  begin
    if Frac(x/d)<>0 then
      if x>0 then
        x:=Trunc(x/d)*d+d
      else
        x:=Trunc(x/d)*d;
  end;

  procedure TruncDown(var x: real; D: real);
  begin
    if Frac(x/d)<>0 then
      if x<0 then
        x:=Trunc(x/d)*d-d
      else
        x:=Trunc(x/d)*d;
  end;

var
  w,h: integer;
  kx,ky: real;
  MCX,MCY: integer;  // Marks Count
  MaxX,MaxY,MaxZ,MinX,MinY,MinZ: real;
  DX,DY: real;
  i: integer;
  b: integer;
  DiscX,DiscY: real;
  Str: string;

  function GetAccVer(const Min,Max: real): real;
  const
    Acc: array[1..15] of real = (1000, 500, 100, 50, 10, 5, 1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001);
  var
    n,LabelHeight: integer;
  begin
    LabelHeight:=FBitMap.Canvas.TextHeight('123');
    result:=1;

    for n:=high(Acc) downto 1 do
    if Trunc((Max-Min)/Acc[n])*LabelHeight<=Height-2*Brd then
    begin
      result:=Acc[n];
      exit;
    end;
  end;

  function GetAccGor(const Min,Max: real): real;
  const
    Acc: array[1..15] of real = (1000, 500, 100, 50, 10, 5, 1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001);
  var
    n,i: integer;
    r: real;
  begin
    result:=1;
    for n:=high(Acc) downto 1 do
    begin
      r:=0;
      for i:=0 to FCloud.Count-1 do
        r:=r+FBitMap.Canvas.TextWidth(
          FloatToStr(Round((i*Acc[n]+MinX)/Acc[n])*Acc[n]));
      if r<=Width-2*Brd then
      begin
        result:=Acc[n];
        exit;
      end;
    end;
  end;

begin
  if FCloud.Count = 0 then exit;

  MinX:=FCloud[0].x;
  MinY:=FCloud[0].y;
  MinZ:=FCloud[0].z;
  MaxX:=FCloud[0].x;
  MaxY:=FCloud[0].y;
  MaxZ:=FCloud[0].z;

  for i:=0 to FCloud.Count-1 do
  begin
    if FCloud[i].x<MinX then MinX:=FCloud[i].x;
    if FCloud[i].y<MinY then MinY:=FCloud[i].y;
    if FCloud[i].z<MinZ then MinZ:=FCloud[i].z;

    if FCloud[i].x>MaxX then MaxX:=FCloud[i].x;
    if FCloud[i].y>MaxY then MaxY:=FCloud[i].y;

    if FCloud[i].z>MaxZ then MaxZ:=FCloud[i].z;
  end;

  DiscX:=GetAccGor(MinX,MaxX);
  DiscY:=GetAccVer(MinY,MaxY);

  //MaxX:=3;

  TruncUp(MaxY,DiscY);
  TruncDown(MinY,DiscY);
  TruncUp(MaxX,DiscX);
  TruncDown(MinX,DiscX);

  DX:=MaxX-MinX;
  DY:=MaxY-MinY;

  if DX=0 then DX:=1;
  if DY=0 then DY:=1;

  MCX:=Round(DX/DiscX);
  MCY:=Round(DY/DiscY);
  w:=Width;
  h:=Height;
  kx:=(w-2*BRD)/DX;
  ky:=(h-2*BRD)/DY;

  FBitMap.Width:=w;
  FBitMap.Height:=h;

  with FBitMap.Canvas do
  begin
    Brush.Color:=BackColor;
    FillRect(Rect(0,0,w,h));
    Pen.Color:=OxesColor;
    Pen.Width:=1;
    Pen.Style:=psDot;
    Pen.Color:=LinesColor;
    Font.Color:=FontColor;

    { риски X }
    for i:=0 to MCX do
    begin
      MoveTo(BRD+Round(i/MCX*DX*kx),BRD);
      LineTo(BRD+Round(i/MCX*DX*kx),h-BRD+2);
      Str:=FloatToStr(Round((i/MCX*DX+MinX)/DiscX)*DiscX);
      TextOut(
        BRD+Round(i/MCX*(MaxX-MinX)*kx)-(TextWidth(Str) div 2),
        h-BRD+4,
        str
      );
    end;

    { риски Y }
    for i:=0 to MCY do
    begin
      MoveTo(BRD-3,Round(h-BRD-ky*i/MCY*DY));
      LineTo(w-BRD,Round(h-BRD-ky*i/MCY*DY));
      TextOut(
        BRD-28,
        h-BRD-Round(i/MCY*(MaxY-MinY)*ky)-(TextHeight('0') div 2),
        FloatToStr(Round((i/MCY*DY+MinY)/DiscY)*DiscY)
      );
    end;

    { оси }
    Pen.Style:=psSolid;
    Pen.Width:=2;
    MoveTo(BRD-Round(MinX*kx),BRD);
    LineTo(BRD-Round(MinX*kx),h-BRD);
    MoveTo(BRD,Round(h-BRD-ky*(0-MinY)));
    LineTo(w-BRD,Round(h-BRD-ky*(0-MinY)));
    Pen.Width:=1;

    { точки }
    Pen.Style:=psSolid;
    Pen.Width:=2;

    for i:=0 to FCloud.Count-1 do
    begin
      if MaxZ<>MinZ then
        b:=255-Round((FCloud[i].z-MinZ)/(MaxZ-MinZ)*255)
      else
        b:=0;
      Pen.Color:=RGB(255,b,255);

      Rectangle(
        -0+Round(BRD+(FCloud[i].x-MinX)*kx),
        -0+Round(h-BRD-(FCloud[i].y-MinY)*ky),
        +2+Round(BRD+(FCloud[i].x-MinX)*kx),
        +2+Round(h-BRD-(FCloud[i].y-MinY)*ky)
      );
    end;
  end;
end;

procedure TCloudGraphic.Redraw;
begin
  DrawIntoBuffer;
  Canvas.Draw(0,0,FBitMap);
end;

end.

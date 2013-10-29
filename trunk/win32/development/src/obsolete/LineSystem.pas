unit LineSystem;
{ Решение системы линейных уравнений }

interface

uses
  Windows, SysUtils,
  Matrixes;

type
  TRealArray = array of real;
  { нулевой столбец - для свободных членов }
  TSystemCol = record
    N: integer;
    Rows: TRealArray;
  end;

  TLineSystem = class
  private
    Data: array of TSystemCol;
    CountX,CountE: integer;
    procedure SwapCols(n: integer);
    procedure PrepareSystem;
    function GetDeterminant: real;
  public
    constructor Create(ACountX: integer);
    function InitSystem(NewCountX: integer): boolean;
    function AddEquation(const E: TRealArray): boolean;
    function RemoveEquation(N: integer): boolean;
    function Solve(out Res: TRealArray): integer;
  end;

const
  SOLVE_SOLVED               = 0;
  SOLVE_NOT_ENOUGH_EQUATIONS = 1;
  SOLVE_TOO_MUCH_EQUATIONS   = 2;
  SOLVE_NOSOLUTIONS          = 3;
  SOLVE_INFINITY             = 4;
implementation

{ TLineSystem }

{=========================================================}
function TLineSystem.InitSystem(NewCountX: integer): boolean;
{=========================================================}
var
  i: integer;
begin
  if NewCountX>0 then
  begin
    SetLength(Data,0);
    SetLength(Data,NewCountX+1);
    CountX := NewCountX;
    CountE := 0;
    for i := 0 to CountX do
      Data[i].N := i;
    result := true;
  end else
    result := false;
end;

{=========================================================}
function TLineSystem.AddEquation(const E: TRealArray): boolean;
{=========================================================}
var
  i: integer;
begin
  if length(E)=CountX+1 then
  begin
    inc(CountE);
    for i := 0 to CountX do
    begin
      SetLength(data[i].Rows,CountE);
      data[i].Rows[CountE-1] := E[i];
    end;
    result := true;
  end else
    result := false;
end;

{=========================================================}
function TLineSystem.RemoveEquation(N: integer): boolean;
{=========================================================}
var
  i,j: integer;
begin
  if (N<0)or(N>CountE-1) then
    result := false
  else
  begin
    for i := 0 to CountX do
    begin
      for j := n to CountE-2 do
        Data[i].Rows[j] := Data[i].Rows[j+1];
      dec(CountE);
      SetLength(Data[i].Rows,CountE);
    end;
    result := true;
  end;
end;

{=========================================================}
procedure TLineSystem.SwapCols(n: integer);
{=========================================================}
var
  i: integer;
  r: real;
begin
  for i := 0 to CountE-1 do
  begin
    r := Data[n].Rows[i];
    Data[n].Rows[i] := Data[0].Rows[i];
    Data[0].Rows[i] := r;
  end;
  Data[0].N := Data[0].N xor Data[n].N;
  Data[n].N := Data[0].N xor Data[n].N;
  Data[0].N := Data[0].N xor Data[n].N;
end;

{=========================================================}
procedure TLineSystem.PrepareSystem;
{=========================================================}
var
  i,j,w: integer;
  f: array of boolean;
begin
  SetLength(f,CountX);

  for i := 1 to CountX do
  begin
    f[i-1] := false;
    for j := 0 to CountE-1 do
    if Data[i].Rows[j]<>0 then
    begin
      f[i-1] := true;
      break;
    end;
  end;

  w := 1;
  for i := 1 to CountX do
  if f[i-1] then
  begin
    { записать столбец i на место w }
    if i<>w then Data[w] := Data[i];
    inc(w);
  end;

  if w-1<>CountX then
  begin
    i := CountX;
    Data[w] := Data[i];
    SetLength(Data,w+1);
  end;
end;

{=========================================================}
function TLineSystem.GetDeterminant: real;
{=========================================================}
var
  z: array of integer;
  i,N: integer;
  f: boolean;
  q: real;
begin
  result := 0;
  N := CountX;
  SetLength(z,N);
  for i := 0 to N-1 do z[i] := i+1;
  repeat
    q := SignIt(GetDisordersCount(Z));
    //if i=1 then q := 1 else q := -1;
    for i := 0 to N-1 do
      q := q*Data[i+1].Rows[z[i]-1];
    result := result+q;
    f := GenerateNext(z);
  until not f;
end;

{=========================================================}
function TLineSystem.Solve(out Res: TRealArray): integer;
{=========================================================}
  { Метод Крамера }
var
  //XCount: integer;
  j: integer;
  D,Di: real;

  {
  SOLVE_SOLVED               = 0;
  SOLVE_NOT_ENOUGH_EQUATIONS = 1;
  SOLVE_TOO_MUCH_EQUATIONS   = 2;
  SOLVE_NOSOLUTIONS          = 3;
  SOLVE_INFINITY             = 4;
  }

begin
  PrepareSystem;

  if (CountX=0)or(CountE<CountX) then
    result := SOLVE_INFINITY else
  if (CountE>CountX) then
    result := SOLVE_NOSOLUTIONS else
  begin
    SetLength(Res,CountX);
    D := GetDeterminant;
    if D = 0 then
      result := SOLVE_INFINITY
    else
    begin
      for j := 0 to CountX-1 do
      begin
        SwapCols(j+1);
        Di := GetDeterminant;
        SwapCols(j+1);
        res[j] := Di/D;
      end;
      Result := SOLVE_SOLVED;
    end;
  end;
end;

constructor TLineSystem.Create(ACountX: integer);
begin
  InitSystem(ACountX);
end;

end.

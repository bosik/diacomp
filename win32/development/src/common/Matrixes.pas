unit Matrixes;

{$R+}

interface

uses
  Math,
  SysUtils{,
  Polynomials};

type
  { data }

  TIntArray = array of integer;

  TExtendedArray = array of Extended;

  {  exceptions  }

  EMatrixException = class (Exception);

  ENumericFailure = class (EMatrixException);

  ESizeRestrictionFailure = class (EMatrixException);

  ENonQuadraticMatrixException = class(ESizeRestrictionFailure)
    constructor Create(const Msg: string; Rows, Cols: integer);
  end;

  TMatrix = class
  private
    FData: array of array of Extended;
    FRows, FCols: integer;
  protected
    function GetValue(i,j: integer): Extended;
    procedure SetValue(i,j: integer; const Value: Extended);
  public
    procedure AddRow(const Row: TExtendedArray); deprecated;

    procedure CopyFrom(Source: TMatrix);
    constructor Create(Rows, Cols: integer); overload;
    constructor Create(Source: TMatrix); overload;

    // вычисляет определитель по определению =)
    function GetDeterminant(N: integer): Extended; overload;
    function GetDeterminant(): Extended; overload;
    // без учёта последнего столбца
    function GetDeterminantSystem(): Extended; deprecated;
    procedure InsertRow(const Row: TExtendedArray);
    function IsQuadratic(): boolean;
    function Mul(A: TMatrix): TMatrix;
    procedure MulRow(Row: integer; const Factor: Extended);
    procedure Randomize(Min, Max: integer);
    procedure SetSize(Rows, Cols: integer);
    procedure SolveCramerOld(out X: TExtendedArray); overload; deprecated; // use class function instead
    class function SolveCramer(A, F: TMatrix): TMatrix; overload;
    procedure SolveGauss(out X: TExtendedArray);
    procedure SummRow(SourceRow, TargetRow: integer; const Factor: Extended);
    procedure SwapCols(Col1, Col2: integer);
    procedure SwapRows(Row1, Row2: integer);
    function Transpose(): TMatrix;

    property Cols: integer read FCols;
    property Element[i, j: integer]: Extended read GetValue write SetValue; default;
    property Rows: integer read FRows;
  end;

implementation

const
  EPS = 0.0000001;

{==============================================================================}
procedure Swap(var X1, X2: Extended);
{==============================================================================}
var
  Temp: Extended;
begin
  Temp := X1;
  X1 := X2;
  X2 := Temp;
end;

{ ENonQuadraticMatrixException }

constructor ENonQuadraticMatrixException.Create(const Msg: string; Rows,
  Cols: integer);
begin
  inherited CreateFmt('%s (rows: %d, cols: %d)', [Msg, Rows, Cols]);
end;

{ TMatrix }

{==============================================================================}
procedure TMatrix.CopyFrom(Source: TMatrix);
{==============================================================================}
var
  i, j: integer;
begin
  SetSize(Source.Rows, Source.Cols);
  for i := 0 to FRows - 1 do
  for j := 0 to FCols - 1 do
    FData[i, j] := Source[i, j];
end;

{==============================================================================}
constructor TMatrix.Create(Rows, Cols: integer);
{==============================================================================}
begin
  SetSize(Rows, Cols);
end;

{==============================================================================}
function TMatrix.GetDeterminant(N: integer): Extended;
{==============================================================================}

  // создаёт массив размера Size и заполняет его числами 1..Size
  procedure Init(var X: TIntArray; Size: integer);
  var
    i: integer;
  begin
    SetLength(X, Size);
    for i := 0 to Size - 1 do
      X[i] := i + 1;
  end;

  function GetDisordersFactor(const X: TIntArray): integer;
  var
    f: boolean;
    i,j,n: integer;
  begin
    f := True;
    n := High(x);
    for i := 0 to n-1 do
    for j := i + 1 to n do
    if x[i] > x[j] then
      f := not f;

    if f then
      Result := 1
    else
      Result := -1;
  end;

  function GenerateNext(var X: TIntArray): boolean;
  var
    i: integer; 
    t: integer; { первый элемент, идущий не в порядке возр }
    r: integer; { минимальный элемент, больший конечного }
    y: integer; { для обмена }
    Size: integer;
  begin
    Size := length(x);
    t := Size-1;
    while (t > 0) and (X[t-1] > X[t]) do dec(t);
    dec(t);

    if t = -1 then
      result := False else
    begin
      result := True;
      r := Size-1;
      while X[r] < X[t] do dec(r);

      y := X[r];
      X[r] := X[t];
      X[t] := y;

      for i := 0 to (size-t-2) div 2 do
      begin
        y := X[t + i + 1];
        X[t + i + 1] := X[size-i-1];
        X[size-i-1] := y;
      end;
    end;
  end;

var
  z: TIntArray;
  i: integer;
  f: boolean;
  q: Extended;
begin
  Result := 0;
  Init(z, N);

  repeat
    q := GetDisordersFactor(z);
    for i := 0 to N - 1 do
      q := q * FData[i, z[i] - 1];

    Result := Result + q;
    f := GenerateNext(z);
  until not f;
end;

{==============================================================================}
constructor TMatrix.Create(Source: TMatrix);
{==============================================================================}
begin
  CopyFrom(Source);
end;

{==============================================================================}
function TMatrix.GetDeterminant(): Extended;
{==============================================================================}
begin
  if (not IsQuadratic()) then
    raise ENonQuadraticMatrixException.Create('Can''t get determinant of non-quadratic matrix', Rows, Cols) else
  {if (Rows = 0) then
    raise ESizeRestrictionFailure.Create('Can''t get determinant: matrix has no rows') else
  if (Cols = 0) then
    raise ESizeRestrictionFailure.Create('Can''t get determinant: matrix has no cols')
  else     }
    Result := GetDeterminant(Rows);
end;

{==============================================================================}
function TMatrix.GetDeterminantSystem(): Extended;
{==============================================================================}
begin
  if (Rows <> Cols - 1) then
    raise ENonQuadraticMatrixException.Create('Can''t get determinant: system matrix is not quadratic', Rows, Cols-1) else
  if (Rows = 0) then
    raise ESizeRestrictionFailure.Create('Can''t get determinant: system matrix has no rows') else
  if (Cols - 1 = 0) then
    raise ESizeRestrictionFailure.Create('Can''t get determinant: system matrix has no cols')
  else
    Result := GetDeterminant(Rows);
end;

{==============================================================================}
function TMatrix.GetValue(i, j: integer): Extended;
{==============================================================================}
begin
  Result := FData[i, j];
end;

{==============================================================================}
procedure TMatrix.InsertRow(const Row: TExtendedArray);
{==============================================================================}
{  OLD: AddRow }
var
  i: integer;
begin
  if (Length(Row) <> Cols) then
    raise ESizeRestrictionFailure.CreateFmt('Can''t insert new row: it''s size (%d) is different from matrix''s one (%d)', [Length(Row), Cols]);

  SetSize(Rows + 1, Cols);
  for i := 0 to Cols - 1 do
    FData[Rows - 1, i] := Row[i];
end;

{==============================================================================}
function TMatrix.Mul(A: TMatrix): TMatrix;
{==============================================================================}
var
  i, j, k: integer;
begin
  if (Self.Cols <> A.Rows) then
    raise ESizeRestrictionFailure.CreateFmt('Can''t multiplicate matrixes: it''s cols count (%d) is different from multiplier''s rows count (%d)', [Self.Cols, A.Rows]);

  Result := TMatrix.Create(Self.Rows, A.Cols);
  for i := 0 to Result.Rows - 1 do
  for j := 0 to Result.Cols - 1 do
  begin
    Result[i, j] := 0;
    for k := 0 to Self.Cols - 1 do
      Result[i, j] := Result[i, j] +
        Self[i, k] * A[k, j];
  end;
end;

{==============================================================================}
procedure TMatrix.MulRow(Row: integer; const Factor: Extended);
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to Cols - 1 do
    FData[Row, i] := FData[Row, i] * Factor;
end;

{==============================================================================}
procedure TMatrix.Randomize(Min, Max: integer);
{==============================================================================}
var
  i, j: integer;
begin
  for i := 0 to FRows - 1 do
  for j := 0 to FCols - 1 do
    FData[i, j] := RandomRange(Min, Max);
end;

{==============================================================================}
procedure TMatrix.SetSize(Rows, Cols: integer);
{==============================================================================}
begin
  //if (NewM > 0) and (NewN > 0) then
  begin
  SetLength(FData, Rows, Cols);
  FRows := Rows;
  FCols := Cols;
  end;
end;

{==============================================================================}
procedure TMatrix.SetValue(i, j: integer; const Value: Extended);
{==============================================================================}
begin
  FData[i, j] := Value;
end;

{==============================================================================}
procedure TMatrix.SolveCramerOld(out X: TExtendedArray);
{==============================================================================}
var
  j: integer;
  D, Dj: Extended;
begin
  // Требование: A.Cols = A.Rows + 1
  // Особенности: сохраняет исходную матрицу

  D := GetDeterminantSystem();
  if (abs(D) < EPS) then
    raise ENumericFailure.Create('Line system has infinite solutions count')
  else
  begin
    SetLength(X, Cols - 1);
    for j := 0 to Cols - 2 do
    begin
      SwapCols(j, Cols - 1);
      Dj := GetDeterminantSystem();
      SwapCols(j, Cols - 1);
      X[j] := Dj / D;
    end;
  end;
end;


{==============================================================================}
class function TMatrix.SolveCramer(A, F: TMatrix): TMatrix;
{==============================================================================}
var
  T: TMatrix;
  D: Extended;
  j: integer;

  procedure ReplaceCol(n: integer);
  var
    i: integer;
  begin
    for i := 0 to F.Rows - 1 do
      T[i, n] := F[i, 0];
  end;

  procedure RestoreCol(n: integer);
  var
    i: integer;
  begin
    for i := 0 to F.Rows - 1 do
      T[i, n] := A[i, n];
  end;

begin
  // Требования:
  //   A.Cols = A.Rows = F.Rows
  //   F.Cols > 0
  // Особенности: сохраняет исходную матрицу

  if (not A.IsQuadratic()) then
    raise ENonQuadraticMatrixException.Create('Can''t solve non-quadratic matrix', A.Rows, A.Cols) else
  if (A.Rows <> F.Rows) then
    raise ESizeRestrictionFailure.CreateFmt('Matrixes A (%d) and F (%d) has different rows count', [A.Rows, F.Rows]);
  if (F.Cols = 0) then
    raise ESizeRestrictionFailure.Create('Matrixes F has no cols');

  D := A.GetDeterminant();
  if (abs(D) < EPS) then
    raise ENumericFailure.Create('Line system has infinite solutions count or matrix is ill-conditioned');

  T := TMatrix.Create(A);
  Result := TMatrix.Create(F.Rows, 1);

  for j := 0 to A.Cols - 1 do
  begin
    ReplaceCol(j);
    Result[j, 0] := T.GetDeterminant() / D;
    RestoreCol(j);
  end;
end;

{==============================================================================}
procedure TMatrix.SolveGauss(out X: TExtendedArray);
{==============================================================================}

  function FindNonzeroRow(StartRow, Col: integer): integer;
  var
    i: integer;
  begin
    for i := StartRow to Rows - 1 do
    if FData[i, Col] <> 0 then
    begin
      Result := i;
      Exit;
    end;
    Result := -1;
  end;

var
  i, j: integer;
  s: Extended;
begin
  // Требование: A.Cols = A.Rows + 1
  // Особенности: изменяет исходную матрицу

  // прямой ход - приводим к треугольному виду
  for i := 0 to Rows - 1 do
  begin
    if (FData[i, i] = 0) then
      SwapRows(i, FindNonzeroRow(i + 1, i));

    MulRow(i, 1/FData[i, i]);

    for j := i + 1 to Rows - 1 do
      SummRow(i, j, -FData[j, i]);
  end;

  // обратный ход
  SetLength(X, Cols - 1);
  for i := Rows - 1 downto 0 do
  begin
    s := FData[i, Cols - 1];
    for j := i + 1 to Cols - 2 do
      S := S - FData[i, j] * X[j];
    X[i] := S / FData[i, i];
  end;
end;

{==============================================================================}
procedure TMatrix.SummRow(SourceRow, TargetRow: integer; const Factor: Extended);
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to FCols - 1 do
    FData[TargetRow, i] := FData[TargetRow, i] + FData[SourceRow, i] * Factor;
end;

{==============================================================================}
procedure TMatrix.SwapCols(Col1, Col2: integer);
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to Rows - 1 do
  begin
    Swap(FData[i, Col1], FData[i, Col2]);
  end;
end;

{==============================================================================}
procedure TMatrix.SwapRows(Row1, Row2: integer);
{==============================================================================}
var
  i: integer;
  //y: Extended;
begin
  for i := 0 to Cols - 1 do
  begin
    Swap(FData[Row1, i], FData[Row2, i]);
    {y := FData[Row1, i];
    FData[Row1, i] := FData[Row2, i];
    FData[Row2, i] := y; }
  end;
end;

{==============================================================================}
function TMatrix.Transpose(): TMatrix;
{==============================================================================}
var
  i, j: integer;
begin
  Result := TMatrix.Create(Cols, Rows);

  for i := 0 to Cols - 1 do
  for j := 0 to Rows - 1 do
    Result[i, j] := Self[j, i];
end;

{==============================================================================}
function TMatrix.IsQuadratic: boolean;
{==============================================================================}
begin
  Result := (FRows = FCols);
end;

{==============================================================================}
procedure TMatrix.AddRow(const Row: TExtendedArray);
{==============================================================================}
begin
  // deprecated, use InsertRow directly instead
  InsertRow(Row);
end;

end.

unit Statistics;

interface

uses
  Math, SysUtils;

  // Calculates Cov(X,Y) = E[ X_ * Y_] = E[ (X - E[x]) * (Y - E[Y]) ], where
  //    E[] — the expected (i.e. mean) value
  //    X_ — the centered X
  //    Y_ — the centered Y
  function Covariance(const X,Y: array of Double): Double;

  // Calculates Cov(X,Y) / (q(X) * q(Y)), where
  //    q() — standart deviation
  function LinearCorrelation(const X,Y: array of Double): Double;


implementation

{==============================================================================}
function Covariance(const X,Y: array of Double): Double;
{==============================================================================}
var
  MeanX, MeanY: Double;
  Summ: Double;
  i: integer;
begin
  if (Length(X) <> Length(Y)) then raise Exception.CreateFmt('X (%d) and Y (%d) contains different count of elements', [Length(X), Length(Y)]);

  MeanX := Mean(X);
  MeanY := Mean(Y);

  Summ := 0;
  for i := Low(X) to High(X) do
    Summ := Summ + (X[i] - MeanX) * (Y[i] - MeanY);

  Result := Summ / Length(X);
end;

{==============================================================================}
function LinearCorrelation(const X,Y: array of Double): Double;
{==============================================================================}
begin
  //Result := Covariance(X, Y) / (StdDev(X) * StdDev(Y));
  Result := Covariance(X, Y) / (Sqrt(PopnVariance(X)) * Sqrt(PopnVariance(Y)));
end;

end.


unit LineSystem;

interface

type
  array_of_real = array of real;

  TDeviationFunction = function(const Values: array_of_real): real;

  TLimitArray = array of
  record
    Min,Max: real;
  end;

function SolveSystem(const Limits: TLimitArray;
  DeviationFunction: TDeviationFunction; const Epsilon: real;
  var Answer: array_of_real): real;

implementation

{=================================================================}
function SolveSystem(const Limits: TLimitArray;
  DeviationFunction: TDeviationFunction; const Epsilon: real;
  var Answer: array_of_real): real;
{=================================================================}
var
  UnknownsCount: integer;

  function Find(k: integer; a,b: real): real; { deviation }
  var
    r1,r2,d1,d2: real;
  begin
    if abs(a-b) < 2*Epsilon then
    begin
      Answer[k] := (a+b)/2;
      if k < UnknownsCount-1 then
        result := Find(k+1, Limits[k+1].Min, Limits[k+1].Max)
      else
        result := DeviationFunction(Answer);
    end else
    begin
      r1 := (a+a+b)/3;
      r2 := (a+b+b)/3;
      if k < UnknownsCount-1 then
      begin
        Answer[k] := r1;
        d1 := Find(k+1, Limits[k+1].Min, Limits[k+1].Max);
        Answer[k] := r2;
        d2 := Find(k+1, Limits[k+1].Min, Limits[k+1].Max);
      end else
      begin
        Answer[k] := r1;
        d1 := DeviationFunction(Answer);
        Answer[k] := r2;
        d2 := DeviationFunction(Answer);
      end;
      if d1 < d2 then
        result := Find(k,a,r2)
      else
        result := Find(k,r1,b);
    end;
  end;

begin
  UnknownsCount := Length(Limits);
  SetLength(Answer, UnknownsCount);

  if UnknownsCount>0 then
    result := Find(0,Limits[0].Min,Limits[0].Max)
  else
    result := 0;
end;


end.

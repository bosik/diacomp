unit JsonSerializer;

interface

uses
  SysUtils,
  uLkJSON,
  DiaryRoutines;

  function JsonRead(const s: string): TlkJSONbase;
  function JsonWrite(json: TlkJSONbase): string;

implementation

{==============================================================================}
function JsonRead(const s: string): TlkJSONbase;
{==============================================================================}
begin
  Result := TlkJSON.ParseText(S);
  if not Assigned(Result) then
    raise Exception.Create('Invalid JSON: ' + S);
end;

{==============================================================================}
function JsonWrite(json: TlkJSONbase): string;
{==============================================================================}
var
  i: integer;
begin
  Result := GenerateReadableText(json, i);
  RemoveAll(Result, #13);
  RemoveAll(Result, #10);
end;

end.

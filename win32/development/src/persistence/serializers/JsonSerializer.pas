unit JsonSerializer;

interface

uses
  SysUtils,
  uLkJSON;

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
begin
  Result := TlkJSON.GenerateText(json);
end;

end.

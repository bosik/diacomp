unit HashService;

interface

uses
  DiaryRoutines,
  BusinessObjects,
  SysUtils;

type
  // TODO: rename to TSortedStringMap

  THashService = class(TStringMap)
  protected
    function GetValue(Key: string): string; override;
  public
    procedure UpdateDiff(Prefix, Diff: TCompactGUID);
  end;

  function Sum(a, b: TCompactGUID): TCompactGUID;
  function Sub(a, b: TCompactGUID): TCompactGUID;
  function CalculateHash(Hashes: TGUIDList): TCompactGUID; overload;
  function CalculateHash(Hashes: TStringArray): string; overload;

const
  // TODO: rename
  LETTERS: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
  MAX_PREFIX_SIZE = 4;
  EMPTY_HASH = '';

implementation

var
  SUMMS: array[char, char] of char;
  SUBS: array[char, char] of char;

{======================================================================================================================}
procedure Init();
{======================================================================================================================}
var
  i, j: integer;
begin
  for i := 0 to High(LETTERS) do
  for j := 0 to High(LETTERS) do
  begin
    SUMMS[LETTERS[i], LETTERS[j]] := LETTERS[(i + j) mod 16];
    SUBS[LETTERS[i], LETTERS[j]] := LETTERS[(i - j + 16) mod 16];
  end;
end;

{======================================================================================================================}
function Sum(a, b: TCompactGUID): TCompactGUID;
{======================================================================================================================}
var
  i: integer;
begin
  if (a = EMPTY_HASH) then
    Result := b else
  if (b = EMPTY_HASH) then
    Result := a else
  begin
    SetLength(Result, COMPACT_GUID_SIZE);
    for i := 1 to COMPACT_GUID_SIZE do
      Result[i] := SUMMS[a[i], b[i]];
  end;
end;

{======================================================================================================================}
function Sub(a, b: TCompactGUID): TCompactGUID;
{======================================================================================================================}
var
  i: integer;
begin
  if (b = EMPTY_HASH) then
    Result := a else
  begin
    SetLength(Result, COMPACT_GUID_SIZE);
    if (a = EMPTY_HASH) then
    begin
      for i := 1 to COMPACT_GUID_SIZE do
        Result[i] := SUBS['0', b[i]];
    end else
    begin
      for i := 1 to COMPACT_GUID_SIZE do
        Result[i] := SUBS[a[i], b[i]];
    end;
  end;
end;

{======================================================================================================================}
function CalculateHash(Hashes: TGUIDList): TCompactGUID;
{======================================================================================================================}
var
  i: integer;
begin
  Result := EMPTY_HASH;

  for i := 0 to High(Hashes) do
    Result := Sum(Result, Hashes[i]);
end;

{======================================================================================================================}
function CalculateHash(Hashes: TStringArray): string;
{======================================================================================================================}
var
  i: integer;
begin
  Result := EMPTY_HASH;

  for i := 0 to High(Hashes) do
    Result := Sum(Result, Hashes[i]);
end;

{ THashService }

{======================================================================================================================}
function THashService.GetValue(Key: string): string;
{======================================================================================================================}
var
  k: integer;
begin
  k := IndexOf(Key);
  if (k > -1) then
    Result := GetItem(k).Value
  else
    Result := EMPTY_HASH;
end;

{======================================================================================================================}
procedure THashService.UpdateDiff(Prefix, Diff: TCompactGUID);
{======================================================================================================================}
var
  Key: TCompactGUID;
  i: integer;
begin
  Prefix := AnsiLowerCase(Prefix);

  for i := 0 to MAX_PREFIX_SIZE do
  begin
    Key := Copy(Prefix, 1, i);
    Add(Key, Sum(GetValue(Key), Diff));
  end;
end;

initialization
  Init();
end.

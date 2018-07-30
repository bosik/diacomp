unit MerkleTree;

interface

uses
  SysUtils,
  DiaryRoutines,
  HashService;

type
  THashTree = class
    function GetHash(const Prefix: string): string; virtual; abstract;
    function GetHashChildren(const Prefix: string): TStringMap; virtual; abstract;
  end;

  TMerkleTree = class (THashTree)
  private
    FHash: string;
    FChildren: array[0..15] of TMerkleTree;
    FData: TStringMap;
  protected
    procedure PutLong(const Key, Value: string; PrefixSize: integer);
    procedure PutShort(const Key, Value: string);
  public
    function Child(const Key: string): TMerkleTree;
    procedure Clear();
    constructor Create;
    destructor Destroy; override;
    function GetHash(const Prefix: string): string; override;
    function GetHashChildren(const Prefix: string): TStringMap; override;
    procedure Put(const Key, Value: string; PrefixSize: integer; UpdateHash: boolean = True);
    procedure PutAll(Map: TStringMap; PrefixSize: integer; UpdateHash: boolean = True);
    function UpdateHash(): string;
  end;

implementation

var
  CHAR_TO_INT: array[char] of integer;
  INT_TO_CHAR: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

{ TMerkleTree }

{======================================================================================================================}
function TMerkleTree.Child(const Key: string): TMerkleTree;
{======================================================================================================================}
var
  i: integer;
begin
  Result := Self;
  for i := 1 to Length(Key) do
  begin
    Result := Result.FChildren[CHAR_TO_INT[Key[i]]];
    if (Result = nil) then
      Exit;
  end;     
end;

{======================================================================================================================}
procedure TMerkleTree.Clear;
{======================================================================================================================}
var
  i: integer;
begin
  FData.Clear();
  for i := 0 to 15 do
    FreeAndNil(FChildren[i]);
end;

{======================================================================================================================}
constructor TMerkleTree.Create;
{======================================================================================================================}
begin
  FData := TStringMap.Create;
end;

{======================================================================================================================}
destructor TMerkleTree.Destroy;
{======================================================================================================================}
begin
  Clear();
  FreeAndNil(FData);
end;

{======================================================================================================================}
function TMerkleTree.GetHash(const Prefix: string): string;
{======================================================================================================================}
var
  Child: TMerkleTree;
begin
  Child := Self.Child(Prefix);
  if (Child <> nil) then
    Result := Child.FHash
  else
    Result := EMPTY_HASH;
end;

{======================================================================================================================}
function TMerkleTree.GetHashChildren(const Prefix: string): TStringMap;
{======================================================================================================================}
var
  Child: TMerkleTree;
  i: integer;
begin
  Result := TStringMap.Create();

  Child := Self.Child(Prefix);
  if (Child <> nil) then
  begin
    if (Child.FData.Count > 0) then
    begin
      Result.AddAll(Child.FData);
    end else
    begin
      for i := 0 to 15 do
      if (Child.FChildren[i] <> nil) then
        Result.Add(Prefix + INT_TO_CHAR[i], Child.FChildren[i].FHash);
    end;
  end;
end;

{======================================================================================================================}
procedure TMerkleTree.Put(const Key, Value: string; PrefixSize: integer; UpdateHash: boolean = True);
{======================================================================================================================}
begin
  if (Length(Key) > PrefixSize) then
    PutLong(Key, Value, PrefixSize)
  else
    PutShort(Key, Value);

  if (UpdateHash) then
    Self.UpdateHash();
end;

{======================================================================================================================}
procedure TMerkleTree.PutAll(Map: TStringMap; PrefixSize: integer; UpdateHash: boolean);
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to Map.Count - 1 do
    Put(Map.Item[i].Key, Map.Item[i].Value, PrefixSize, False);

  if (UpdateHash) then
    Self.UpdateHash();
end;

{======================================================================================================================}
procedure TMerkleTree.PutLong(const Key, Value: string; PrefixSize: integer);
{======================================================================================================================}
var
  Index: integer;
begin
  if (PrefixSize > 0) then
  begin
    Index := CHAR_TO_INT[Key[1]];
    if (FChildren[Index] = nil) then
    begin
      FChildren[Index] := TMerkleTree.Create();
    end;
    FChildren[Index].PutLong(Copy(Key, 2, Length(Key) - 1), Value, PrefixSize - 1);
  end else
  begin
    FData.Add(Key, Value, True);
  end;
end;

{======================================================================================================================}
procedure TMerkleTree.PutShort(const Key, Value: string);
{======================================================================================================================}
var
  Index: integer;
begin
  if (Length(Key) > 0) then
  begin
    Index := CHAR_TO_INT[Key[1]];
    if (FChildren[Index] = nil) then
    begin
      FChildren[Index] := TMerkleTree.Create();
    end;
    FChildren[Index].PutShort(Copy(Key, 2, Length(Key) - 1), Value);
  end else
  begin
    FHash := Value;
  end;
end;

{======================================================================================================================}
function TMerkleTree.UpdateHash(): string;
{======================================================================================================================}
var
  Hashes: TStringArray;
  i: integer;
begin
  if (FData.Count > 0) then
  begin
    Hashes := FData.Values;
  end else
  begin
    for i := 0 to High(FChildren) do
    if (FChildren[i] <> nil) then
    begin
      SetLength(Hashes, Length(Hashes) + 1);
      Hashes[High(Hashes)] := FChildren[i].UpdateHash();
    end;
  end;

  FHash := CalculateHash(Hashes);
  Result := FHash;
end;

initialization
  CHAR_TO_INT['0'] := 0;
  CHAR_TO_INT['1'] := 1;
  CHAR_TO_INT['2'] := 2;
  CHAR_TO_INT['3'] := 3;
  CHAR_TO_INT['4'] := 4;
  CHAR_TO_INT['5'] := 5;
  CHAR_TO_INT['6'] := 6;
  CHAR_TO_INT['7'] := 7;
  CHAR_TO_INT['8'] := 8;
  CHAR_TO_INT['9'] := 9;
  CHAR_TO_INT['a'] := 10;
  CHAR_TO_INT['b'] := 11;
  CHAR_TO_INT['c'] := 12;
  CHAR_TO_INT['d'] := 13;
  CHAR_TO_INT['e'] := 14;
  CHAR_TO_INT['f'] := 15;

  CHAR_TO_INT['A'] := 10;
  CHAR_TO_INT['B'] := 11;
  CHAR_TO_INT['C'] := 12;
  CHAR_TO_INT['D'] := 13;
  CHAR_TO_INT['E'] := 14;
  CHAR_TO_INT['F'] := 15;
end.

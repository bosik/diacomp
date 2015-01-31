unit ObjectService;

interface

uses
  SysUtils,
  BusinessObjects,
  HashService;

type
  EItemNotFoundException = class(Exception)
    constructor Create(ID: TCompactGUID);
  end;

  EDuplicateException = class(Exception)
    constructor Create(Item: TVersioned);
  end;

  TObjectService = class
  public
    procedure Delete(ID: TCompactGUID); virtual; abstract;
    function FindChanged(Since: TDateTime): TVersionedList; virtual; abstract;
    function FindById(ID: TCompactGUID): TVersioned; virtual; abstract;
    function FindByIdPrefix(Prefix: TCompactGUID): TVersionedList;  virtual; abstract;
    function GetHash(Prefix: TCompactGUID): TCompactGUID; virtual; abstract;
    function GetHashChildren(Prefix: TCompactGUID): THashService; virtual; abstract;
    procedure Save(const Items: TVersionedList); overload; virtual; abstract;
    procedure SetHash(Prefix, Hash: TCompactGUID); virtual; abstract;

    // sugar
    procedure Save(Item: TVersioned); overload; virtual;
    function UpdateHashTree(const Prefix: TCompactGUID = ''): TCompactGUID;
    procedure UpdateHashBranch(const Prefix: TCompactGUID);
  end;

implementation

{ EItemNotFoundException }

{======================================================================================================================}
constructor EItemNotFoundException.Create(ID: TCompactGUID);
{======================================================================================================================}
begin
  inherited CreateFmt('Item {%s} not found', [ID]);
end;

{ EDuplicateException }

{======================================================================================================================}
constructor EDuplicateException.Create(Item: TVersioned);
{======================================================================================================================}
begin
  inherited CreateFmt('Item {%s} already exists', [Item.ID]);
end;

{ TDAO }

{======================================================================================================================}
procedure TObjectService.Save(Item: TVersioned);
{======================================================================================================================}
var
  List: TVersionedList;
begin
  SetLength(List, 1);
  List[0] := Item;
  Save(List);
end;

{======================================================================================================================}
procedure TObjectService.UpdateHashBranch(const Prefix: TCompactGUID);
{======================================================================================================================}
var
  Children: THashService;
  Hash: TCompactGUID;
begin
  Children := GetHashChildren(Prefix);
  Hash := CalculateHash(Children.Values);
  Children.Free;

  if (Hash <> '') then
    SetHash(Prefix, Hash);

  if (Length(Prefix) > 0) then
    UpdateHashBranch(Copy(Prefix, 1, Length(Prefix) - 1));
end;

{======================================================================================================================}
function TObjectService.UpdateHashTree(const Prefix: TCompactGUID): TCompactGUID;
{======================================================================================================================}
var
  Key, Value: TCompactGUID;
  i: integer;
  ChildHashes: TGUIDList;
  Children: TVersionedList;
begin
  if (Length(Prefix) < MAX_PREFIX_SIZE) then
  begin
    for i := 1 to Length(LETTERS) do
    begin
      Key := Prefix + LETTERS[i];
      Value := UpdateHashTree(Key);

      if (Value <> '') then
      begin
        SetLength(ChildHashes, Length(ChildHashes) + 1);
        ChildHashes[High(ChildHashes)] := Value;
      end;
    end;
  end else
  begin
    Children := FindByIdPrefix(Prefix);
    SetLength(ChildHashes, Length(Children));
    for i := 0 to High(Children) do
      ChildHashes[i] := Children[i].Hash;
  end;

  Result := CalculateHash(ChildHashes);
  if (Result <> '') then
    SetHash(Prefix, Result);
end;

end.

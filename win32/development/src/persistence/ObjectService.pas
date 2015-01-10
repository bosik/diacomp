unit ObjectService;

interface

uses
  SysUtils,
  BusinessObjects;

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
    procedure Save(const Items: TVersionedList); overload; virtual; abstract;

    // sugar
    procedure Save(Item: TVersioned); overload; virtual;
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

end.

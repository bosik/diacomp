unit DAO;

interface

uses
  SysUtils,
  DiaryRoutines,
  BusinessObjects;

type
  EItemNotFoundException = class(Exception)
    constructor Create(ID: TCompactGUID);
  end;

  EDuplicateException = class(Exception)
    constructor Create(Item: TVersioned);
  end;

  TDAO = class
  public
    function FindChanged(Since: TDateTime): TVersionedList; virtual; abstract;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TVersionedList; virtual; abstract;
    function FindById(ID: TCompactGUID): TVersioned; virtual; abstract;
    procedure Post(const Recs: TVersionedList); virtual; abstract;
  end;

implementation

{ EItemNotFoundException }

constructor EItemNotFoundException.Create(ID: TCompactGUID);
begin
  inherited CreateFmt('Item {%s} not found', [ID]);
end;

{ EDuplicateException }

constructor EDuplicateException.Create(Item: TVersioned);
begin
  inherited CreateFmt('Item {%s} already exists', [Item.ID]);
end;

end.

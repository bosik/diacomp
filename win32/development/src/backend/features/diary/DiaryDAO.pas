unit DiaryDAO;

interface

uses
  DiaryRecords,
  ObjectService,
  BusinessObjects;

type
  TDiaryDAO = class (TObjectService)
  public
    procedure Add(const R: TVersioned); virtual;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TVersionedList; virtual; abstract;
  end;

implementation

{ TDiaryDAO }

{======================================================================================================================}
procedure TDiaryDAO.Add(const R: TVersioned);
{======================================================================================================================}
var
  Recs: TVersionedList;
begin
  SetLength(Recs, 1);
  Recs[0] := R;
  Save(Recs);
end;

end.


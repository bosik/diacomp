unit DiaryDAO;

interface

uses
  DiaryRecords,
  ObjectService,
  BusinessObjects;

type
  TDiaryDAO = class (TObjectService)
  public
    procedure Add(const R: TCustomRecord); virtual;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; virtual; abstract;
  end;

implementation

{ TDiaryDAO }

{==============================================================================}
procedure TDiaryDAO.Add(const R: TCustomRecord);
{==============================================================================}
var
  Recs: TVersionedList;
begin
  SetLength(Recs, 1);
  Recs[0] := R;
  Save(Recs);
end;

end.


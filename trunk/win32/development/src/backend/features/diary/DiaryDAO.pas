unit DiaryDAO;

interface

uses
  DiaryRecords,
  ObjectService;

type
  TDiaryDAO = class (TObjectService)
  public
    procedure Add(const R: TCustomRecord); virtual; abstract;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; virtual; abstract;
  end;

implementation

end.


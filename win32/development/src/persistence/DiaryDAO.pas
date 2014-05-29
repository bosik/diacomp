unit DiaryDAO;

interface

uses
  DiaryRecords,
  DAO;

type
  TDiaryDAO = class (TDAO)
  public
    procedure Add(const R: TCustomRecord); virtual; abstract;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; virtual; abstract;
  end;

implementation

end.


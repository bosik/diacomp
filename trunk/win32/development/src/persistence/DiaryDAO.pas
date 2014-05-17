unit DiaryDAO;

interface

uses
  Bases,
  DiaryRecords,
  DiaryRoutines,
  BusinessObjects,
  DAO;

type
  TDiaryDAO = class (TDAO)
  public
    procedure Add(const R: TCustomRecord); virtual; abstract;
    procedure Delete(ID: TCompactGUID); virtual; abstract;
    function FindChanged(Since: TDateTime): TRecordList; virtual; abstract;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; virtual; abstract;
    function FindById(ID: TCompactGUID): TCustomRecord; virtual; abstract;
    procedure Post(const Recs: TRecordList); virtual; abstract;
  end;

implementation

end.

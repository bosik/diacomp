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
    function FindChanged(Since: TDateTime): TRecordList; reintroduce; virtual; abstract;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; reintroduce;virtual; abstract;
    function FindById(ID: TCompactGUID): TCustomRecord; reintroduce; virtual; abstract;
    procedure Post(const Recs: TRecordList); overload; virtual; abstract;

    // sugar
    procedure Post(const Rec: TCustomRecord); overload;
  end;

implementation

{ TDiaryDAO }

procedure TDiaryDAO.Post(const Rec: TCustomRecord);
var
  List: TRecordList;
begin
  SetLength(List, 1);
  List[0] := Rec;
  Post(List);
end;

end.

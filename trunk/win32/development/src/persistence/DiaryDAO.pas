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
    //function FindChanged(Since: TDateTime): TVersionedList;
    function FindPeriod(TimeFrom, TimeTo: TDateTime): TRecordList; reintroduce;virtual; abstract;
    //function FindById(ID: TCompactGUID): TCustomRecord; virtual; abstract;
    //procedure Save(const Recs: TRecordList); overload; virtual; abstract;

    procedure Save(const Recs: TVersionedList); overload; override;
    procedure Save(const Recs: TRecordList); overload; virtual; abstract;

    // sugar
    //procedure Save(const Rec: TCustomRecord); overload;
  end;

implementation

{ TDiaryDAO }

{procedure TDiaryDAO.Post(const Rec: TCustomRecord);
var
  List: TRecordList;
begin
  SetLength(List, 1);
  List[0] := Rec;
  Save(List);
end;
        }
{ TDiaryDAO }

procedure TDiaryDAO.Save(const Recs: TVersionedList);
begin
  Save(VersionedToRecord(Recs));
end;

end.

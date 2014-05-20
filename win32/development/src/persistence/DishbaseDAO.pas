unit DishbaseDAO;

interface

uses
  BusinessObjects,
  DiaryRoutines,
  DAO;

type
  TDishbaseDAO = class (TDAO)
    procedure Delete(ID: TCompactGUID); virtual; abstract;
    function FindAll(ShowRemoved: boolean): TDishItemList; virtual; abstract;
    function FindAny(const Filter: string): TDishItemList; virtual; abstract;
    function FindOne(const Name: string): TDish; virtual; abstract;
    function FindChanged(Since: TDateTime): TDishItemList; virtual; abstract;
    function FindById(ID: TCompactGUID): TDish; virtual; abstract;
    procedure Save(Item: TDish); overload; virtual; abstract;
    procedure Save(const Items: TDishItemList); overload; virtual; abstract;
  end;

implementation

end.

unit FoodbaseDAO;

interface

uses
  BusinessObjects,
  DiaryRoutines,
  DAO;

type
  TFoodbaseDAO = class (TDAO)
    procedure Delete(ID: TCompactGUID); virtual; abstract;
    function FindAll(ShowRemoved: boolean): TFoodItemList; virtual; abstract;
    function FindAny(const Filter: string): TFoodItemList; virtual; abstract;
    function FindOne(const Name: string): TFood; virtual; abstract;
    function FindChanged(Since: TDateTime): TFoodItemList; reintroduce; virtual; abstract;
    function FindById(ID: TCompactGUID): TFood; virtual; abstract;
    procedure Save(Item: TFood); overload; virtual; abstract;
    procedure Save(const Items: TFoodItemList); overload; virtual; abstract;
  end;

implementation

end.

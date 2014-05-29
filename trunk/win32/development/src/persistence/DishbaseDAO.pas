unit DishbaseDAO;

interface

uses
  BusinessObjects,
  DAO;

type
  TDishbaseDAO = class (TDAO)
    function FindAll(ShowRemoved: boolean): TDishItemList; virtual; abstract;
    function FindAny(const Filter: string): TDishItemList; virtual; abstract;
    function FindOne(const Name: string): TDishItem; virtual; abstract;
  end;

implementation

end.

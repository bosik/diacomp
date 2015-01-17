unit DishbaseDAO;

interface

uses
  BusinessObjects,
  ObjectService;

type
  TDishbaseDAO = class (TObjectService)
    function FindAll(ShowRemoved: boolean): TDishItemList; virtual; abstract;
    function FindAny(const Filter: string): TDishItemList; virtual; abstract;
    function FindOne(const Name: string): TDishItem; virtual; abstract;
  end;

implementation

end.

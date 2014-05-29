unit FoodbaseDAO;

interface

uses
  BusinessObjects,
  ObjectService;

type
  TFoodbaseDAO = class (TObjectService)
    function FindAll(ShowRemoved: boolean): TFoodItemList; virtual; abstract;
    function FindAny(const Filter: string): TFoodItemList; virtual; abstract;
    function FindOne(const Name: string): TFoodItem; virtual; abstract;
  end;

implementation

end.

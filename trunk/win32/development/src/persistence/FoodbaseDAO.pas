unit FoodbaseDAO;

interface

uses
  SysUtils,
  BusinessObjects;

type
  TFoodList = array of TFood;

  EItemNotFoundException = class(Exception);

  TFoodbaseDAO = class
    // ”дал€ет указанный продукт из базы; в случае отсутстви€ выбрасывает исключение EItemNotFoundException
    procedure Delete(Food: TFood); virtual; abstract;

    // ¬озвращает все имеющиес€ в базе продукты
    function FindAll(): TFoodList; virtual; abstract;

    // »щЄт все продукты, содержащие указанную строку в названии (без учЄта регистра)
    function FindAny(const Filter: string): TFoodList; virtual; abstract;

    // »щет продукт с точным названием; в случае отсутстви€ возвращает nil
    function FindOne(const Name: string): TFood; virtual; abstract;

    // «амен€ет всю базу указанным списком, номер версии мен€етс€ на указанный
    {*}procedure ReplaceAll(const NewList: TFoodList; NewVersion: integer); virtual; abstract;

    // ƒобавл€ет (create) или обновл€ет (update) продукт в базе
    procedure Save(Food: TFood); virtual; abstract;

    // ѕолучает номер версии базы
    {*}function Version(): integer; virtual; abstract;
  end;

implementation

end.

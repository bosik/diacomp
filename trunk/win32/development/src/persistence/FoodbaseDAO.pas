unit FoodbaseDAO;

interface

uses
  SysUtils,
  BusinessObjects,
  DiaryRoutines;

type
  TFoodList = array of TFood;

  EItemNotFoundException = class(Exception)
    constructor Create(ID: TCompactGUID);
  end;

  EDuplicateException = class(Exception)
    constructor Create(Food: TFood);
  end;

  TFoodbaseDAO = class
    // ƒобавл€ет продукт в базу и возвращает его идентификатор (без изменений);
    // в случае существовани€ продукта с тем же ID выбрасывает исключение EDuplicateException
    function Add(Food: TFood): TCompactGUID; virtual; abstract;

    // ”дал€ет указанный продукт из базы; в случае отсутстви€ выбрасывает исключение EItemNotFoundException
    procedure Delete(ID: TCompactGUID); virtual; abstract;

    // ¬озвращает все имеющиес€ в базе продукты
    function FindAll(): TFoodList; virtual; abstract;

    // »щЄт все продукты, содержащие указанную строку в названии (без учЄта регистра)
    function FindAny(const Filter: string): TFoodList; virtual; abstract;

    // »щет продукт с точным названием; в случае отсутстви€ возвращает nil
    function FindOne(const Name: string): TFood; virtual; abstract;

    // «амен€ет всю базу указанным списком, номер версии мен€етс€ на указанный
    {*}procedure ReplaceAll(const NewList: TFoodList; NewVersion: integer); virtual; abstract;

    // ќбновл€ет продукт в базе; в случае отсутстви€ выбрасывает исключение EItemNotFoundException
    procedure Update(Food: TFood); virtual; abstract;

    // ѕолучает номер версии базы
    {*}function Version(): integer; virtual; abstract;
  end;

implementation

{ EItemNotFoundException }

constructor EItemNotFoundException.Create(ID: TCompactGUID);
begin
  inherited CreateFmt('ѕродукт {%s} не найден', [ID]);
end;

{ EDuplicateException }

constructor EDuplicateException.Create(Food: TFood);
begin
  inherited CreateFmt('ѕродукт "%s" (#%s) уже существует', [Food.Name, Food.ID]);
end;

end.

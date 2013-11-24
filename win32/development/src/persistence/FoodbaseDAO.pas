unit FoodbaseDAO;

interface

uses
  SysUtils,
  BusinessObjects;

type
  TFoodList = array of TFood;

  EItemNotFoundException = class(Exception)
    constructor Create(Food: TFood);
  end;

  EDuplicateException = class(Exception)
    constructor Create(Food: TFood);
  end;

  TFoodbaseDAO = class
    // Добавляет продукт в базу; в случае существования выбрасывает исключение EDuplicateException
    procedure Add(Food: TFood); virtual; abstract;

    // Удаляет указанный продукт из базы; в случае отсутствия выбрасывает исключение EItemNotFoundException
    procedure Delete(Food: TFood); virtual; abstract;

    // Возвращает все имеющиеся в базе продукты
    function FindAll(): TFoodList; virtual; abstract;

    // Ищёт все продукты, содержащие указанную строку в названии (без учёта регистра)
    function FindAny(const Filter: string): TFoodList; virtual; abstract;

    // Ищет продукт с точным названием; в случае отсутствия возвращает nil
    function FindOne(const Name: string): TFood; virtual; abstract;

    // Заменяет всю базу указанным списком, номер версии меняется на указанный
    {*}procedure ReplaceAll(const NewList: TFoodList; NewVersion: integer); virtual; abstract;

    // Обновляет продукт в базе; в случае отсутствия выбрасывает исключение EItemNotFoundException
    procedure Update(Food: TFood); virtual; abstract;

    // Получает номер версии базы
    {*}function Version(): integer; virtual; abstract;
  end;

implementation

{ EItemNotFoundException }

constructor EItemNotFoundException.Create(Food: TFood);
begin
  inherited CreateFmt('Продукт #%d (%s) не найден', [Food.ID, Food.Name]);
end;

{ EDuplicateException }

constructor EDuplicateException.Create(Food: TFood);
begin
  inherited CreateFmt('Продукт #%d (%s) уже существует', [Food.ID, Food.Name]);
end;

end.

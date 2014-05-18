unit TextInterface;

interface

uses
  Classes, SysUtils, DiaryRoutines;

var
  APPLICATION_TITLE                   : string = 'Компенсация';

  BALLOON_INFO_NEW_VERSION_AVAILABLE  : string = 'Доступна новая версия программы. Для просмотра более подробной информации нажмите на это сообщение.';
  BALLOON_ERROR_ANALYZER_NOT_FOUNDED  : string = 'Ошибка: модуль анализа не загружен';
  MAIN_BASES                               : string;
  MAIN_BASES_FOOD_TITLE                    : string;
  MAIN_BASES_DISH_TITLE                    : string;
  MAIN_DIARY                               : string;
  MAIN_DIARY_PANEL_TIME_FINGER_NOT_FOUND   : string = 'Предыдущий замер СК не найден';
  MAIN_DIARY_PANEL_TIME_FINGER             : string = 'Палец:';
  MAIN_DIARY_PANEL_TIME_AFTER_MEAL         : string = 'После еды:';
  MAIN_DIARY_PANEL_TIME_AFTER_INS          : string = 'После укола:';
  MAIN_DIARY_PANEL_ADD_SELECT_MEAL         : string = 'Выберите приём пищи или создайте новый';
  MAIN_DIARY_VIEW_EMPTYPAGE: string;
  MESSAGE_CONF_FIRST_WARNING  : string = 'ВНИМАНИЕ! Автор программы '+
    'не несет ответственности за любой ущерб, прямо или '+
    'косвенно связанный с использованием данной программы. '+
    'Все результаты работы программы носят '+
    'рекомендательный характер. Не забывайте о здравом '+
    'смысле. За Ваше здоровье ответственны только Вы сами.'#13#13+
    'Если Вы принимаете эти условия, нажмите "Да" (Yes)'#13+
    'Если не принимаете - нажмите "Нет" (No).';
  MESSAGE_CONF_REMOVE_DIARY_UNKNOWN   : string = 'Удалить запись?';
  MESSAGE_CONF_REMOVE_DIARY_BLOOD     : string = 'Удалить замер СК?';
  MESSAGE_CONF_REMOVE_DIARY_INS       : string = 'Удалить инъекцию?';
  MESSAGE_CONF_REMOVE_DIARY_MEAL      : string = 'Удалить приём пищи?';
  MESSAGE_CONF_REMOVE_DIARY_NOTE      : string = 'Удалить заметку?';
  MESSAGE_CONF_REMOVE_DIARY_FOOD      : string = 'Удалить <%s> ?';
  MESSAGE_CONF_REMOVE_FOOD            : string = 'Удалить продукт <%s>?';
  MESSAGE_CONF_REMOVE_FOOD_USED       : string = 'Продукт <%s> используется в блюде <%s>.'#13'Всё равно удалить?';
  MESSAGE_CONF_REMOVE_DISH            : string = 'Удалить блюдо <%s>?';
  MESSAGE_CONF_REMOVE_DISH_USED       : string = 'Блюдо <%s> используется в блюде <%s>.'#13'Всё равно удалить?';
  MESSAGE_ERROR_NO_INTERNET           : string = 'Нет подключения к сети Интернет.';
  MESSAGE_ERROR_INITIALIZATION        : string = 'Возникла критическая ошибка (этап "%s": %s). Загрузка прервана.';
  MESSAGE_ERROR_INPUT_INT_POSITIVE    : string = 'Неверное значение. Введите целое положительное число.';
  MESSAGE_ERROR_INPUT_REAL            : string = 'Неверное значение. Введите вещественное число.';
  MESSAGE_INFO_NO_UPDATES             : string = 'Вы используете самую последнюю версию программы.';
  MESSAGE_INFO_CANT_BALANCE           : string = 'Масса уменьшена до 0, но для соответствия дозы пище надо ещё что-то убрать.';
  STATUS_ACTION_LOADING_DIARY         : string = 'Загрузка дневника';
  STATUS_ACTION_LOADING_GRAPHICS      : string = 'Загрузка графики';
  STATUS_ACTION_WEB_SETUP             : string = 'Настройка веб-клиента';
  STATUS_ACTION_APPLYING_SETTINGS     : string = 'Применение пользовательских настроек';
  STATUS_ACTION_PREPARING_INFOPANELS  : string = 'Настройка информационных панелей';
  STATUS_ACTION_DOWNLOADING_FOODBASE  : string = 'Получение базы продуктов';
  STATUS_ACTION_DOWNLOADING_DISHBASE  : string = 'Получение базы блюд';
  STATUS_ACTION_CONVERT_FOODBASE      : string = 'Конвертирование базы продуктов';
  STATUS_ACTION_CONVERT_DISHBASE      : string = 'Конвертирование базы блюд';
  STATUS_ACTION_LOADING_FOODBASE      : string = 'Загрузка базы продуктов';
  STATUS_ACTION_LOADING_DISHBASE      : string = 'Загрузка базы блюд';
  STATUS_ACTION_AUTH                  : string = 'Авторизация';
  STATUS_ACTION_SYNC_DIARY            : string = 'Синхронизация дневника';
  STATUS_ACTION_SYNC_FOODBASE         : string = 'Синхронизация базы продуктов';
  STATUS_ACTION_SYNC_DISHBASE         : string = 'Синхронизация базы блюд';
  STATUS_ACTION_LOADING_MATHAN        : string = 'Загрузка модуля анализа';
  STATUS_ACTION_PREPARING_KOOFS       : string = 'Расчёт модели';
  STATUS_ACTION_UPLOADING_KOOFS       : string = 'Выгрузка коэффициентов на сервер';
  STATUS_RESULT_READY                 : string = 'Готово';
  STATUS_RESULT_LOADING_TIME          : string = 'Время запуска: %d мсек';
  STATUS_RESULT_SYNC_DONE             : string = 'Дневник синхронизирован';
  STATUS_RESULT_STATE_ONLINE          : string = 'Онлайн';
  STATUS_RESULT_STATE_OFFLINE         : string = 'Оффлайн';

const
  MESSAGE_CONF_UPDATE: array[Boolean] of string = (
    'Доступна новая версия программы. Установить сейчас?',
    'Установить обновление сейчас?'
  );
  { Кнопки }
  SAVE_CAPTION: array[Boolean] of string = ('Сохранить','Создать');

  KoofDisc: array[0..3] of string = (
    'Показывает, насколько увеличивается СК при съедении 1г углеводов',
    'Показывает, насколько уменьшается СК при введении 1 ЕД пищевого инсулина',
    'Показывает, насколько увеличивается СК при съедении 1г белков',
    'Показывает, сколько инсулина требуется для усвоения 1г углеводов и 0,25г белков');

  { пальцы }
  LongFingerNames: array[0..9] of string = (
    '[Левая]: большой',
    '[Левая]: указательный',
    '[Левая]: средний',
    '[Левая]: безымянный',
    '[Левая]: мизинец',
    '[Правая]: мизинец',
    '[Правая]: безымянный',
    '[Правая]: средний',
    '[Правая]: указательный',
    '[Правая]: большой'
  );

  ShortFingerNames: array[0..9] of string = (
    'БЛ',
    '1Л',
    '2Л',
    '3Л',
    '4Л',
    '4П',
    '3П',
    '2П',
    '1П',
    'БП'
  ); 

  MiddleFingerNames: array[0..9] of string = (
    '< большой',
    '< указат.',
    '< средний',
    '< безымян.',
    '< мизинец',
    'мизинец >',
    'безымян. >',
    'средний >',
    'указат. >',
    'большой >'
  );

  procedure LoadStringResources(const FileName: string);

implementation

{==============================================================================}
procedure LoadStringResources(const FileName: string);
{==============================================================================}

  function GetValue(const S: string): string;
  begin
    Result := Trim(TextAfter(S, '='));
  end;

var
  Map: TStringMap;
    
  function Extract(const Name: string): string;
  begin
    try
      Result := GetValue(Map[Name]);
    except
      on EKeyNotFoundException do
        raise Exception.Create(Format('Resource string "%s" not founded', [Name]));
    end;
  end;

begin
  Map := TStringMap.Create;
  try
    Map.LoadFromFile(FileName);

    APPLICATION_TITLE                   := Extract('APPLICATION_TITLE');
    BALLOON_INFO_NEW_VERSION_AVAILABLE  := Extract('BALLOON_INFO_NEW_VERSION_AVAILABLE');
    BALLOON_ERROR_ANALYZER_NOT_FOUNDED  := Extract('BALLOON_ERROR_ANALYZER_NOT_FOUNDED');
    MAIN_BASES                               := Extract('MAIN_BASES');
    MAIN_BASES_FOOD_TITLE                    := Extract('MAIN_BASES_FOOD_TITLE');
    MAIN_BASES_DISH_TITLE                    := Extract('MAIN_BASES_DISH_TITLE');
    MAIN_DIARY                               := Extract('MAIN_DIARY'); 
    MAIN_DIARY_PANEL_TIME_FINGER_NOT_FOUND   := Extract('MAIN_DIARY_PANEL_TIME_FINGER_NOT_FOUND');
    MAIN_DIARY_PANEL_TIME_FINGER             := Extract('MAIN_DIARY_PANEL_TIME_FINGER');
    MAIN_DIARY_PANEL_TIME_AFTER_MEAL         := Extract('MAIN_DIARY_PANEL_TIME_AFTER_MEAL');
    MAIN_DIARY_PANEL_TIME_AFTER_INS          := Extract('MAIN_DIARY_PANEL_TIME_AFTER_INS');
    MAIN_DIARY_PANEL_ADD_SELECT_MEAL         := Extract('MAIN_DIARY_PANEL_ADD_SELECT_MEAL');
    MESSAGE_CONF_FIRST_WARNING          := Extract('MESSAGE_CONF_FIRST_WARNING');
    MESSAGE_CONF_REMOVE_DIARY_UNKNOWN   := Extract('MESSAGE_CONF_REMOVE_DIARY_UNKNOWN');
    MESSAGE_CONF_REMOVE_DIARY_BLOOD     := Extract('MESSAGE_CONF_REMOVE_DIARY_BLOOD');
    MESSAGE_CONF_REMOVE_DIARY_INS       := Extract('MESSAGE_CONF_REMOVE_DIARY_INS');
    MESSAGE_CONF_REMOVE_DIARY_MEAL      := Extract('MESSAGE_CONF_REMOVE_DIARY_MEAL');
    MESSAGE_CONF_REMOVE_DIARY_NOTE      := Extract('MESSAGE_CONF_REMOVE_DIARY_NOTE');
    MESSAGE_CONF_REMOVE_DIARY_FOOD      := Extract('MESSAGE_CONF_REMOVE_DIARY_FOOD');
    MESSAGE_CONF_REMOVE_FOOD            := Extract('MESSAGE_CONF_REMOVE_FOOD');
    MESSAGE_CONF_REMOVE_FOOD_USED       := Extract('MESSAGE_CONF_REMOVE_FOOD_USED');
    MESSAGE_CONF_REMOVE_DISH            := Extract('MESSAGE_CONF_REMOVE_DISH');
    MESSAGE_CONF_REMOVE_DISH_USED       := Extract('MESSAGE_CONF_REMOVE_DISH_USED');
    MESSAGE_ERROR_NO_INTERNET           := Extract('MESSAGE_ERROR_NO_INTERNET');
    MESSAGE_ERROR_INITIALIZATION        := Extract('MESSAGE_ERROR_INITIALIZATION');
    MESSAGE_ERROR_INPUT_INT_POSITIVE    := Extract('MESSAGE_ERROR_INPUT_INT_POSITIVE');
    MESSAGE_ERROR_INPUT_REAL            := Extract('MESSAGE_ERROR_INPUT_REAL');
    MESSAGE_INFO_NO_UPDATES             := Extract('MESSAGE_INFO_NO_UPDATES');
    MESSAGE_INFO_CANT_BALANCE           := Extract('MESSAGE_INFO_CANT_BALANCE');
    STATUS_ACTION_LOADING_DIARY         := Extract('STATUS_ACTION_LOADING_DIARY');
    STATUS_ACTION_LOADING_GRAPHICS      := Extract('STATUS_ACTION_LOADING_GRAPHICS');
    STATUS_ACTION_WEB_SETUP             := Extract('STATUS_ACTION_WEB_SETUP');
    STATUS_ACTION_APPLYING_SETTINGS     := Extract('STATUS_ACTION_APPLYING_SETTINGS');
    STATUS_ACTION_PREPARING_INFOPANELS  := Extract('STATUS_ACTION_PREPARING_INFOPANELS');
    STATUS_ACTION_DOWNLOADING_FOODBASE  := Extract('STATUS_ACTION_DOWNLOADING_FOODBASE');
    STATUS_ACTION_DOWNLOADING_DISHBASE  := Extract('STATUS_ACTION_DOWNLOADING_DISHBASE');
    STATUS_ACTION_CONVERT_FOODBASE      := Extract('STATUS_ACTION_CONVERT_FOODBASE');
    STATUS_ACTION_CONVERT_DISHBASE      := Extract('STATUS_ACTION_CONVERT_DISHBASE');
    STATUS_ACTION_LOADING_FOODBASE      := Extract('STATUS_ACTION_LOADING_FOODBASE');
    STATUS_ACTION_LOADING_DISHBASE      := Extract('STATUS_ACTION_LOADING_DISHBASE');
    STATUS_ACTION_AUTH                  := Extract('STATUS_ACTION_AUTH');
    STATUS_ACTION_SYNC_DIARY            := Extract('STATUS_ACTION_SYNC_DIARY');
    STATUS_ACTION_SYNC_FOODBASE         := Extract('STATUS_ACTION_SYNC_FOODBASE');
    STATUS_ACTION_SYNC_DISHBASE         := Extract('STATUS_ACTION_SYNC_DISHBASE');
    STATUS_ACTION_LOADING_MATHAN        := Extract('STATUS_ACTION_LOADING_MATHAN');
    STATUS_ACTION_PREPARING_KOOFS       := Extract('STATUS_ACTION_PREPARING_KOOFS');
    STATUS_ACTION_UPLOADING_KOOFS       := Extract('STATUS_ACTION_UPLOADING_KOOFS');
    STATUS_RESULT_READY                 := Extract('STATUS_RESULT_READY');
    STATUS_RESULT_LOADING_TIME          := Extract('STATUS_RESULT_LOADING_TIME');
    STATUS_RESULT_SYNC_DONE             := Extract('STATUS_RESULT_SYNC_DONE');
    STATUS_RESULT_STATE_ONLINE          := Extract('STATUS_RESULT_STATE_ONLINE');
    STATUS_RESULT_STATE_OFFLINE         := Extract('STATUS_RESULT_STATE_OFFLINE');
    MAIN_DIARY_VIEW_EMPTYPAGE := Extract('MAIN_DIARY_VIEW_EMPTYPAGE');
  finally
    Map.Free;
  end;
end;    

end.


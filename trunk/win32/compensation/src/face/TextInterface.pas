unit TextInterface;

interface

// TODO: отрефакторить названия
// TODO: найти дубли

const
  { Заголовки окон }
  PROGRAM_TITLE             = 'Компенсация';

  { Сообщения \ Ошибки }
  MESSAGE_ERROR_NO_INTERNET         = 'Нет подключения к сети Интернет.';
  MESSAGE_ERROR_INITIALIZATION      = 'Возникла критическая ошибка (этап "%s": %s). Загрузка прервана.';
  //MESSAGE_ERROR_INPUT_INT           = 'Неверное значение. Введите целое число.';
  MESSAGE_ERROR_INPUT_INT_POSITIVE  = 'Неверное значение. Введите целое положительное число.';
  MESSAGE_ERROR_INPUT_REAL          = 'Неверное значение. Введите вещественное число.';

  { Сообщения \ Информация }
  MESSAGE_INFO_NO_UPDATES           = 'Вы используете самую последнюю версию программы.';
  MESSAGE_INFO_CANT_BALANCE         = 'Масса уменьшена до 0, но для соответствия дозы пище надо ещё что-то убрать.';

  { Сообщения \ Вопросы }
  MESSAGE_CONF_FIRST_WARNING  = 'ВНИМАНИЕ! Автор программы '+
    'не несет ответственности за любой ущерб, прямо или '+
    'косвенно связанный с использованием данной программы. '+
    'Все результаты работы программы носят '+
    'рекомендательный характер. Не забывайте о здравом '+
    'смысле. За Ваше здоровье ответственны только Вы сами.'#13#13+
    'Если Вы принимаете эти условия, нажмите "Да" (Yes)'#13+
    'Если не принимаете - нажмите "Нет" (No).';
  MESSAGE_CONF_REMOVE_RECORD: array[1..4] of string = (
    'Удалить замер СК?',
    'Удалить инъекцию?',
    'Удалить приём пищи?',
    'Удалить заметку?'
  );
  MESSAGE_CONF_UPDATE: array[Boolean] of string = (
    'Доступна новая версия программы. Установить сейчас?',
    'Установить обновление сейчас?'
  );
  MESSAGE_CONF_REMOVE_DIARY_FOOD = 'Удалить <%s> ?';
  MESSAGE_CONF_REMOVE_FOOD       = 'Удалить продукт <%s>?';
  MESSAGE_CONF_REMOVE_FOOD_USED  = 'Продукт <%s> используется в блюде <%s>.'#13'Всё равно удалить?';
  MESSAGE_CONF_REMOVE_DISH       = 'Удалить блюдо <%s>?';
  MESSAGE_CONF_REMOVE_DISH_USED  = 'Блюдо <%s> используется в блюде <%s>.'#13'Всё равно удалить?';

  { Всплывающие \ Информация }
  BALLOON_INFO_NEW_VERSION_AVAILABLE  = 'Доступна новая версия программы. Для просмотра более подробной информации нажмите на это сообщение.';
  BALOON_ERROR_ANALYZER_NOT_FOUNDED   = 'Ошибка: модуль анализа не загружен';     

  { Кнопки }
  SAVE_CAPTION: array[Boolean] of string = ('Сохранить','Создать');

  { Строка состояния }
  STATUS_ACTION_LOADING_DIARY        = 'Загрузка дневника';
  STATUS_ACTION_LOADING_GRAPHICS     = 'Загрузка графики';
  STATUS_ACTION_WEB_SETUP            = 'Настройка веб-клиента';
  STATUS_ACTION_APPLYING_SETTINGS    = 'Применение пользовательских настроек';
  //STATUS_ACTION_PREPARING_INTERFACE  = 'Косметика';
  STATUS_ACTION_PREPARING_INFOPANELS = 'Настройка информационных панелей';
  STATUS_ACTION_DOWNLOADING_FOODBASE = 'Получение базы продуктов';
  STATUS_ACTION_DOWNLOADING_DISHBASE = 'Получение базы блюд';
  STATUS_ACTION_CONVERT_FOODBASE     = 'Конвертирование базы продуктов';
  STATUS_ACTION_CONVERT_DISHBASE     = 'Конвертирование базы блюд';
  STATUS_ACTION_LOADING_FOODBASE     = 'Загрузка базы продуктов';
  STATUS_ACTION_LOADING_DISHBASE     = 'Загрузка базы блюд';
  STATUS_ACTION_AUTH                 = 'Авторизация';
  STATUS_ACTION_SYNC_DIARY           = 'Синхронизация дневника';
  STATUS_ACTION_SYNC_FOODBASE        = 'Синхронизация базы продуктов';
  STATUS_ACTION_SYNC_DISHBASE        = 'Синхронизация базы блюд';
  STATUS_ACTION_LOADING_MATHAN       = 'Загрузка модуля анализа';
  STATUS_READY                       = 'Готово';
  STATUS_LOADING_TIME                = 'Время запуска: %d мсек';
  STATUS_SYNC_DONE                   = 'Дневник синхронизирован';
  STATUS_ONLINE_STATE: array[False..True] of string = ('Оффлайн', 'Онлайн');

  { Хинты }
  HINT_FINGER_NOT_FOUND = 'Предыдущий замер СК не найден';

  { Надписи на компонентах }
  LABEL_FINGER           = 'Палец:';
  LABEL_AFTER_MEAL       = 'После еды:';
  LABEL_AFTER_INS        = 'После укола:';
  MESSAGE_SELECT_MEAL    = 'Выберите приём пищи или создайте новый';
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
  
implementation

end.

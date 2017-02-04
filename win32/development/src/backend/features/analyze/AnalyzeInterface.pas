unit AnalyzeInterface;

interface

uses
  DiaryRoutines;

{
  ***** ПРОЦЕСС АНАЛИЗА *****
  1) выделение из дневника первичных записей (TPrimeRec)
  2) небольшое форматирование в тип TAnalyzeRec
  3) отправление их в модуль анализа и получение поминутного списка коэффициентов
}

type
  { ===== ВХОДНЫЕ ДАННЫЕ ===== }

  TAnalyzeRec = record
    Prots, Fats, Carbs: real;
    Ins: real;
    BSIn, BSOut: real;
    Time: integer; // время привязки (в минутах)
    Weight: real; // для взвешивания
  end;

  TAnalyzeRecList = array of TAnalyzeRec;

  { ===== ВЫХОДНЫЕ ДАННЫЕ ===== }

  TKoof = record
    k, q, p: real;
  end;

  TKoofList = array[0..MinPerDay - 1] of TKoof;

  { ===== ФУНКЦИИ ===== }

  TCallbackProgressProc = procedure(Progress: integer);

  TAnalyzer = class
    function Analyze(const RecList: TAnalyzeRecList; out KoofList: TKoofList; CallBack: TCallbackProgressProc): boolean; virtual; abstract;
    function GetName(): PChar; virtual; abstract;
  end;
  
implementation

end.

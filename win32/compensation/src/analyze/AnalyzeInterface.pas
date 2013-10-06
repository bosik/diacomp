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

  // #dao #dll
  TAnalyzeRec = record
    Prots,Fats,Carbs: real;
    Ins: real;
    BSIn, BSOut: real;
    Time: integer; // время привязки (в минутах)
    Weight: real; // для взвешивания
  end;

  // #dao #dll
  TAnalyzeRecList = array of TAnalyzeRec;

  { ===== ВЫХОДНЫЕ ДАННЫЕ ===== }

  // #dao #dll
  TKoof = record
    k,q,p: real;
  end;

  // #dao #dll
  TKoofList = array[0..MinPerDay - 1] of TKoof;

  { ===== ИМПОРТИРУЕМАЯ ФУНКЦИЯ ===== }

  // #dao #dll
  TCallbackProgressProc = procedure(Progress: integer);

  // #dao #dll
  TInfoFunction = function(): PChar;

  // #dao #dll
  TAnalyzeFunction = function(
    const RecList: TAnalyzeRecList;
    var KoofList: TKoofList;
    CallBack: TCallbackProgressProc
  ): boolean; StdCall;

const
  AnalyzeFunctionName = 'Analyze';
  InfoFunctionName    = 'Info';

  PAR_ADAPTATION  = 0;
  //PAR_COMPRESSION = 1;
  
implementation

end.

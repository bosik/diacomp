unit AnalyzeInterface;

interface

uses
  DiaryRoutines;

{

  ***** ������� ������� *****
  1) ��������� �� �������� ��������� ������� (TPrimeRec)
  2) ��������� �������������� � ��� TAnalyzeRec
  3) ����������� �� � ������ ������� � ��������� ����������� ������ �������������

}

type
  { ===== ������� ������ ===== }

  // #dao #dll
  TAnalyzeRec = record
    Prots,Fats,Carbs: real;
    Ins: real;
    BSIn, BSOut: real;
    Time: integer; // ����� �������� (� �������)
    Weight: real; // ��� �����������
  end;

  // #dao #dll
  TAnalyzeRecList = array of TAnalyzeRec;

  { ===== �������� ������ ===== }

  // #dao #dll
  TKoof = record
    k,q,p: real;
  end;

  // #dao #dll
  TKoofList = array[0..MinPerDay - 1] of TKoof;

  { ===== ������������� ������� ===== }

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

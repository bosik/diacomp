unit AnalyzeInterface;

interface

uses
  Classes,
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

{==============================================================================}  

  { ������ }
  function GetKoofRandomation(const KoofList: TKoofList): real;
  procedure SaveRecords(const List: TAnalyzeRecList; const FileName: string);

const
  MinFinishTime = 7 * MinPerHour div 2; // 3.5 ����
  AnalyzeFunctionName = 'Analyze';
  InfoFunctionName    = 'Info';

  PAR_ADAPTATION  = 0;
  //PAR_COMPRESSION = 1;
  
implementation

{==============================================================================}
function GetKoofRandomation(const KoofList: TKoofList): real;
{==============================================================================}
  function Dist(n1,n2: integer): real;
  begin
    result := Sqrt(
      Sqr(KoofList[n1].k - KoofList[n2].k)+
      Sqr(KoofList[n1].q - KoofList[n2].q)+
      Sqr(KoofList[n1].p - KoofList[n2].p));
  end;

{var
  i: integer; }
begin
  result := 0;
  {for i := 0 to High(KoofList) do
    result := result + Dist(i,(i + 1) mod length(KoofList));  }

  { *** }
end;

{==============================================================================}
procedure SaveRecords(const List: TAnalyzeRecList; const FileName: string);
{==============================================================================}
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  try
    for i := 0 to High(List) do
      s.Add(
        TimeToStr{Colon}(List[i].Time, ':')+#9+
        RealToStr(List[i].Carbs)+#9+
        RealToStr(List[i].Prots)+#9+
        RealToStr(List[i].Ins)+#9+
        RealToStr(List[i].BSOut - List[i].BSIn)
      );
    s.SaveToFile(FileName);
  finally
    s.Free;
  end;
end;

end.

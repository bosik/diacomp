unit DiaryAnalyze;

interface

uses
  AnalyzeInterface;

type
  TAnalyzeFunction = function(
    const RecList: TAnalyzeRecList;
    var KoofList: TKoofList;
    CallBack: TCallbackProgressProc
  ): boolean; StdCall;

  TAnalyzer = class
    FName: string;

  end;

implementation

end.

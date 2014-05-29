unit DiaryDAO;

interface

uses
  SysUtils, // Exception
  DiaryRoutines, // TDate
  DiaryPage;

type
  // информация для синхронизации
  TModItem = record
    Date: TDate;
    Version: integer;
  end;

  TModList = array of TModItem;

  TDateList = array of TDate;

  // DAO
  TDiaryDAO = class
  public
    { методы }
    // TODO: make it function
    procedure GetModified(Time: TDateTime; out ModList: TModList); virtual; abstract;
    // TODO: make it function
    procedure GetVersions(const Dates: TDateList; out ModList: TModList); virtual; abstract;
    // TODO: make it function
    function GetPages(const Dates: TDateList; out Pages: TDiaryPageList): boolean; virtual; abstract;
    // TODO: make it procedure
    function PostPages(const Pages: TDiaryPageList): boolean; virtual; abstract;

    { сахар }
    function GetPage(Date: TDate): TDiaryPage;
    function PostPage(Page: TDiaryPage): boolean;
  end;

implementation

{ TDiaryDAO }

{==============================================================================}
function TDiaryDAO.GetPage(Date: TDate): TDiaryPage;
{==============================================================================}
var
  DateList: TDateList;
  PageList: TDiaryPageList;
begin
  SetLength(DateList, 1);
  DateList[0] := Date;
  GetPages(DateList, PageList);
  if (Length(PageList) = 1) then
    Result := PageList[0]
  else
    raise Exception.Create('Неверная реализация TDiaryDAO: GetPages() вернул ' + IntToStr(Length(PageList)) + ' страниц вместо одной');
end;

{==============================================================================}
function TDiaryDAO.PostPage(Page: TDiaryPage): boolean;
{==============================================================================}
var
  PageList: TDiaryPageList;
begin
  SetLength(PageList, 1);
  PageList[0] := Page;
  Result := PostPages(PageList);
end;

end.

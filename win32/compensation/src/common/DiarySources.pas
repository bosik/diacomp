unit DiarySources;

interface

uses
  SysUtils, // Exception
  DiaryRoutines, // TDate
  DiaryPage;

type
  // ���������� ��� �������������
  TModItem = record
    Date: TDate;
    Version: integer;
  end;

  TModList = array of TModItem;

  TDateList = array of TDate;

  // DAO
  IDiarySource = class
  public
    { ������ }
    function GetModList(Time: TDateTime; out ModList: TModList): boolean; virtual; abstract;
    function GetPages(const Dates: TDateList; out Pages: TDiaryPageList): boolean; virtual; abstract;
    function PostPages(const Pages: TDiaryPageList): boolean; virtual; abstract;

    { ����� }
    function GetPage(Date: TDate): TDiaryPage;
    function PostPage(Page: TDiaryPage): boolean;
  end;

implementation

{ IDiarySource }

{==============================================================================}
function IDiarySource.GetPage(Date: TDate): TDiaryPage;
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
    raise Exception.Create('�������� ���������� IDiarySource: GetPages() ������ ' + IntToStr(Length(PageList)) + ' ������� ������ �����');
end;

{==============================================================================}
function IDiarySource.PostPage(Page: TDiaryPage): boolean;
{==============================================================================}
var
  PageList: TDiaryPageList;
begin
  SetLength(PageList, 1);
  PageList[0] := Page;
  Result := PostPages(PageList);
end;

end.

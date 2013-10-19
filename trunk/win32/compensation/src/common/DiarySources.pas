unit DiarySources;

interface

uses
  SysUtils, Classes,
  AutoLog,
  Dialogs, // debug
  DiaryRoutines,
  DiaryPageSerializer {TODO: remove it};

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
    function GetPages(const Dates: TDateList; out Pages: TPageDataList): boolean; virtual; abstract;
    function PostPages(const Pages: TPageDataList): boolean; virtual; abstract;

    { ����� }
    function GetPage(Date: TDate): TPageData;
    function PostPage(Page: TPageData): boolean;
  end;

implementation

{ IDiarySource }

{==============================================================================}
function IDiarySource.GetPage(Date: TDate): TPageData;
{==============================================================================}
var
  DateList: TDateList;
  PageList: TPageDataList;
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
function IDiarySource.PostPage(Page: TPageData): boolean;
{==============================================================================}
var
  PageList: TPageDataList;
begin
  SetLength(PageList, 1);
  PageList[0] := Page;
  Result := PostPages(PageList);
end;

end.

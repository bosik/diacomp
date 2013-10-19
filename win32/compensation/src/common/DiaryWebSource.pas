unit DiaryWebSource;

interface

uses
  SysUtils,
  DiarySources,
  DiaryWeb,
  DiaryPageSerializer;

type
  TDiaryWebSource = class (IDiarySource)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    function GetModList(Time: TDateTime; out ModList: TModList): boolean; override;
    function GetPages(const Dates: TDateList; out Pages: TPageDataList): boolean; override;
    function PostPages(const Pages: TPageDataList): boolean; override;
  end;

implementation

{ TDiaryWebSource }

{==============================================================================}
constructor TDiaryWebSource.Create(Client: TDiacompClient);
{==============================================================================}
begin
  if (Client = nil) then
    raise Exception.Create('Client can''t be nil');
    
  FClient := Client;
end;

{==============================================================================}
function TDiaryWebSource.GetModList(Time: TDateTime; out ModList: TModList): boolean;
{==============================================================================}
begin
  Result := FClient.GetModList(Time, ModList);
end;

{==============================================================================}
function TDiaryWebSource.GetPages(const Dates: TDateList; out Pages: TPageDataList): boolean;
{==============================================================================}
begin
  Result := FClient.GetPages(Dates, Pages);
end;

{==============================================================================}
function TDiaryWebSource.PostPages(const Pages: TPageDataList): boolean;
{==============================================================================}
begin
  Result := PostPages(Pages);
end;

end.

unit DiaryWebSource;

interface

uses
  SysUtils,
  DiarySources,
  DiaryWeb,
  DiaryPage,
  DiaryPageSerializer,
  Classes;

type
  TDiaryWebSource = class (IDiarySource)
  private
    FClient: TDiacompClient;
  public
    constructor Create(Client: TDiacompClient);

    procedure GetModified(Time: TDateTime; out ModList: TModList); override;
    procedure GetVersions(const Dates: TDateList; out ModList: TModList); override;
    function GetPages(const Dates: TDateList; out Pages: TDiaryPageList): boolean; override;
    function PostPages(const Pages: TDiaryPageList): boolean; override;
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
procedure TDiaryWebSource.GetModified(Time: TDateTime; out ModList: TModList);
{==============================================================================}
begin
  FClient.GetModList(Time, ModList);
end;

{==============================================================================}
function TDiaryWebSource.GetPages(const Dates: TDateList; out Pages: TDiaryPageList): boolean;
{==============================================================================}
var
  Resp: string;
  S: TStrings;
  i: integer;
begin
  Result := FClient.GetPages(Dates, Resp);

  // обрабатываем
  S := TStringList.Create;
  S.Text := Resp;
  try
    TPageSerializer.ReadPages(S, DiaryWeb.WebFmt, Pages);

    // переводим время в локальное
    for i := 0 to High(Pages) do
      Pages[i].TimeStamp := FClient.ServerToLocal(Pages[i].TimeStamp);
  finally
    S.Free;
  end;
end;

{==============================================================================}
procedure TDiaryWebSource.GetVersions(const Dates: TDateList; out ModList: TModList);
{==============================================================================}
begin
  FClient.GetVersions(Dates, ModList);
end;

{==============================================================================}
function TDiaryWebSource.PostPages(const Pages: TDiaryPageList): boolean;
{==============================================================================}
var
  S: TStrings;
  i: integer;
  OldTimestamp: TDateTime;
begin
  S := TStringList.Create;

  for i := 0 to High(Pages) do
  begin
    OldTimestamp := Pages[i].TimeStamp;
    try
      Pages[i].TimeStamp := FClient.LocalToServer(Pages[i].TimeStamp);
      TPageSerializer.WritePage(Pages[i], S, WebFmt);
    finally
      Pages[i].TimeStamp := OldTimestamp;
    end;
  end;

  Result := FClient.PostPages(S.Text);
end;

end.

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

    function GetModList(Time: TDateTime; out ModList: TModList): boolean; override;
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
function TDiaryWebSource.GetModList(Time: TDateTime; out ModList: TModList): boolean;
{==============================================================================}
begin
  Result := FClient.GetModList(Time, ModList);
end;

{==============================================================================}
function TDiaryWebSource.GetPages(const Dates: TDateList; out Pages: TDiaryPageList): boolean;
{==============================================================================}
var
  Resp: string;
  S: TStrings;
  PageDataList: TPageDataList;
  i: integer;
begin
  Result := FClient.GetPages(Dates, Resp);

  // обрабатываем
  S := TStringList.Create;
  S.Text := Resp;
  try
    TPageSerializer.Read(S, DiaryWeb.WebFmt, Pages);

    // переводим время в локальное
    for i := 0 to High(Pages) do
      Pages[i].TimeStamp := FClient.ServerToLocal(Pages[i].TimeStamp);
  finally
    S.Free;
  end;
end;

{==============================================================================}
function TDiaryWebSource.PostPages(const Pages: TDiaryPageList): boolean;
{==============================================================================}
var
  P: TPageDataList;
  S: TStrings;
  i: integer;
begin
  SetLength(P, Length(Pages));
  for i := 0 to High(P) do
  begin
    P[i] := TPageData.Create;
    TPageSerializer.Write(Pages[i], P[i]);
    P[i].TimeStamp := FClient.LocalToServer(P[i].TimeStamp);
  end;

  S := TStringList.Create;
  TPageSerializer.Write(P, S, WebFmt);
  Result := FClient.PostPages(S.Text);
end;

end.

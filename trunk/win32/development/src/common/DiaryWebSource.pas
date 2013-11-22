unit DiaryWebSource;

interface

uses
  SysUtils,
  Classes,
  DiarySources,
  DiaryWeb,
  DiaryPage,
  DiaryPageSerializer,
  AutoLog;

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
begin
  try
    Resp := FClient.GetPages(Dates);
    S := TStringList.Create;
    try
      S.Text := Resp;
      TPageSerializer.ReadPages(S, DiaryWeb.WebFmt, Pages);
    finally
      S.Free;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      Log(Error, 'TDiaryWebSource.GetPages(): ' + E.Message);
      Result := False;                                       
    end;
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
  //OldTimestamp: TDateTime;
begin
  S := TStringList.Create;

  for i := 0 to High(Pages) do
  begin
    //OldTimestamp := Pages[i].TimeStamp;
    //try
      //Pages[i].TimeStamp := FClient.LocalToServer(Pages[i].TimeStamp);
      TPageSerializer.WritePage(Pages[i], S, WebFmt);
    //finally
    //  Pages[i].TimeStamp := OldTimestamp;
    //end;
  end;

  Result := FClient.PostPages(S.Text);
end;

end.

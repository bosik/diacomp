unit DiaryLocalSource;

interface

uses
  Windows,
  SysUtils, // FileExists(), Now()
  Classes,
  Dialogs {update warnings},
  DiaryRoutines,
  DiarySources,
  DiaryPage,
  DiaryPageSerializer;

type
  TPageData = class;
  TPageDataList = array of TPageData;

  // частично распарсенная страница
  TPageData = class
  private
    procedure CopyFrom(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string); overload;
    procedure CopyFrom(APage: TPageData); overload;
  public
    Date: TDate;
    TimeStamp: TDateTime;
    Version: integer;
    Page: string;

    constructor Create(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string); overload;
    constructor Create(APage: TPageData); overload;

    function Write(F: TFormatSettings): string; overload;
    function WriteHeader(F: TFormatSettings): string; deprecated;

    class procedure Read(const S: string; Page: TPageData; F: TFormatSettings); overload;
    {L} class procedure Read(S: TStrings; F: TFormatSettings; out Pages: TPageDataList); overload;
    {L} class procedure Read(PageData: TPageData; Page: TDiaryPage); overload;
    class procedure ReadHeader(const S: string; PageData: TPageData; F: TFormatSettings); overload;
    {L} class procedure Write(Page: TDiaryPage; PageData: TPageData); overload;
    {L} class procedure Write(const Pages: TPageDataList; S: TStrings; F: TFormatSettings); overload;
  end;

  TDiaryLocalSource =  class (IDiarySource)
  private
    FPages: TPageDataList;
    FModified: boolean;
    FFileName: string;

    function Add(Page: TPageData): integer;
    procedure Clear;
    function CreatePage(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer): integer;
    function GetPageIndex(Date: TDate): integer;
    function TraceLastPage: integer;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure GetModified(Time: TDateTime; out ModList: TModList); override;
    procedure GetVersions(const Dates: TDateList; out ModList: TModList); override;
    function GetPages(const Dates: TDateList; out Pages: TDiaryPageList): boolean; override;
    function PostPages(const Pages: TDiaryPageList): boolean; override;

    // свойства
    // TODO: think about it
    //property Modified: boolean read FModified {write FModified};
 end;

implementation

var
  LocalFmt: TFormatSettings;
  
const
  ShowUpdateWarning = True;

{ TPageData }

{==============================================================================}
procedure TPageData.CopyFrom(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string);
{==============================================================================}
begin
  Date := ADate;
  TimeStamp := ATimeStamp;
  Version := AVersion;
  Page := APage;
end;

{==============================================================================}
procedure TPageData.CopyFrom(APage: TPageData);
{==============================================================================}
begin
  CopyFrom(APage.Date, APage.TimeStamp, APage.Version, APage.Page);
end;

{==============================================================================}
constructor TPageData.Create(ADate: TDate; ATimeStamp: TDateTime;
  AVersion: integer; const APage: string);
{==============================================================================}
begin
  CopyFrom(ADate, ATimeStamp, AVersion, APage);
end;

{==============================================================================}
constructor TPageData.Create(APage: TPageData);
{==============================================================================}
begin
  CopyFrom(APage);
end;

{==============================================================================}
function TPageData.Write(F: TFormatSettings): string;
{==============================================================================}
var
  Header: string;
  Body: string;
begin
  //TPageSerializer.WriteHeader(Self, F, Header);
  Header := WriteHeader(F);
  Body := Page;
  Result := Header + #13 + Body;
end;

{==============================================================================}
function TPageData.WriteHeader(F: TFormatSettings): string;
{==============================================================================}
begin
  Result := Format('=== %s ===|%s|%d', [DateToStr(Date, F), DateTimeToStr(TimeStamp, F), Version]);
end;

{==============================================================================}
class procedure TPageData.Read(const S: string; Page: TPageData; F: TFormatSettings);
{==============================================================================}
var
  header, body: string;
begin
  Separate(S, header, #13, body);
  TPageData.ReadHeader(header, Page, F);
  Page.Page := body;
end;

{==============================================================================}
class procedure TPageData.Read(PageData: TPageData; Page: TDiaryPage);
{==============================================================================}
begin
  with Page do
  begin
    Date := PageData.Date;
    TimeStamp := PageData.TimeStamp;
    Version := PageData.Version;
    TPageSerializer.ReadBody(PageData.Page, Page);
  end;


  {

  Switch: boolean;

  ------

  Switch := Switch and (not SilentMode);
  SilentMode := SilentMode or Switch;

  ...

  SilentMode := SilentMode and (not Switch);

  ------

  OldMode := SilentMode;
  if Switch then SilentMode := True;

  ...

  SilentMode := OldMode;

  }
end;

{==============================================================================}
class procedure TPageData.ReadHeader(const S: string; PageData: TPageData; F: TFormatSettings);
{==============================================================================}
begin
  TPageSerializer.ReadHeader(S, F, PageData.Date, PageData.TimeStamp, PageData.Version);
end;

{==============================================================================}
class procedure TPageData.Read(S: TStrings; F: TFormatSettings; out Pages: TPageDataList);
{==============================================================================}
var
  PageList: TStringsArray;
  i: integer;
begin
  TPageSerializer.SeparatePages(S, PageList);

  SetLength(Pages, Length(PageList));
  for i := 0 to High(PageList) do
  begin
    Pages[i] := TPageData.Create;
    TPageData.Read(PageList[i].Text, Pages[i], F);
  end;
end;

{==============================================================================}
class procedure TPageData.Write(Page: TDiaryPage; PageData: TPageData);
{==============================================================================}
begin
  with Page do
  begin
    PageData.Date := Date;
    PageData.TimeStamp := TimeStamp;
    PageData.Version := Version;
    TPageSerializer.WriteBody(Page, PageData.Page);
  end;
end;

{==============================================================================}
class procedure TPageData.Write(const Pages: TPageDataList; S: TStrings; F: TFormatSettings);
{==============================================================================}
var
  i: integer;
begin
  // пустые страницы могут появляться после интенсивного тыкания по календарю,
  // поэтому сохраняем только страницы с записями - МОЛОДЕЦ! :-)
  // ---
  // No. С таким подходом я не смогу перезаписать страницу на пустую. Пустое содержание - тоже содержание
  // ---
  /// Yes. Проверять нужно номер версии, а не содержание.

  for i := 0 to High(Pages) do
  // (Trim(Pages[i].Page) <> '') then
  if (Pages[i].Version > 0) then
      s.Add(Pages[i].Write(F));
end;

{ TDiaryLocalSource }

{==============================================================================}
function TDiaryLocalSource.Add(Page: TPageData): integer;
{==============================================================================}
begin
  // 1. Проверка дублей
  // 2. Сортировка

  Result := GetPageIndex(Page.Date);
  if (Result = -1) then
  begin
    Result := Length(FPages);
    SetLength(FPages, Result + 1);
    FPages[Result] := Page;
    Result := TraceLastPage();
  end else
  if (Page <> FPages[Result]) then
  begin
    FPages[Result].Free;
    FPages[Result] := Page;
  end;
end;

{==============================================================================}
procedure TDiaryLocalSource.Clear;
{==============================================================================}
var
  i: integer;
begin
  { вызывается в деструкторе и перед загрузкой из файла }

  if (Length(FPages) > 0) then
    FModified := True;

  for i := 0 to High(FPages) do
    FPages[i].Free;
  SetLength(FPages, 0);
end;

{==============================================================================}
constructor TDiaryLocalSource.Create(const FileName: string);
{==============================================================================}
begin
  FModified := False;
  FFileName := FileName;

  if (FileExists(FileName)) then
    LoadFromFile(FileName);
end;

{==============================================================================}
function TDiaryLocalSource.CreatePage(ADate: TDate; ATimeStamp: TDateTime;
  AVersion: integer): integer;
{==============================================================================}
begin
  Result := GetPageIndex(ADate);
  if (Result = -1) then
  begin
    Result := Length(FPages);
    SetLength(FPages, Result + 1);
    FPages[Result] := TPageData.Create(ADate, ATimeStamp, AVersion, '');
    Result := TraceLastPage();
  end;
end;

{==============================================================================}
destructor TDiaryLocalSource.Destroy;
{==============================================================================}
begin
  Clear;
  inherited;
end;

{==============================================================================}
procedure TDiaryLocalSource.GetModified(Time: TDateTime; out ModList: TModList);
{==============================================================================}
var
  i, Count: integer;
begin
  Count := 0;
  SetLength(ModList, 1);
  for i := 0 to High(FPages) do
  if (FPages[i].TimeStamp > Time) {and (FPages[i].Version > 0)} then
  begin
    if (Count = Length(ModList)) then
      SetLength(ModList, Length(ModList) * 2);
    ModList[Count].Date := FPages[i].Date;
    ModList[Count].Version := FPages[i].Version;
    inc(Count);
  end;
  SetLength(ModList, Count);
end;

{==============================================================================}
procedure TDiaryLocalSource.GetVersions(const Dates: TDateList; out ModList: TModList);
{==============================================================================}
var
  i, k: integer;
begin
  SetLength(ModList, Length(Dates));
  for i := 0 to High(Dates) do
  begin
    ModList[i].Date := Dates[i];

    k := GetPageIndex(Dates[i]);
    if (k > -1) then
      ModList[i].Version := FPages[k].Version
    else
      ModList[i].Version := 0;
  end;
end;

{==============================================================================}
function TDiaryLocalSource.GetPageIndex(Date: TDate): integer;
{==============================================================================}
var
  L,R: integer;
begin
  L := 0;
  R := High(FPages);
  while (L <= R) do
  begin
    Result := (L + R) div 2;
    if (FPages[Result].Date < Date) then L := Result + 1 else
    if (FPages[Result].Date > Date) then R := Result - 1 else
      Exit;
  end;
  Result := -1;
end;

{==============================================================================}
function TDiaryLocalSource.GetPages(const Dates: TDateList; out Pages: TDiaryPageList): boolean;
{==============================================================================}
const
  INITIAL_PAGE_VERSION = 0;
var
  i, Index: integer;
begin
  SetLength(Pages, Length(Dates));
  for i := 0 to High(Dates) do
  begin
    // TODO: try to move Now() and initial version number to CreatePage() method
    Index := CreatePage(Dates[i], Now, INITIAL_PAGE_VERSION);

    Pages[i] := TDiaryPage.Create;
    TPageData.Read(FPages[Index], Pages[i]);
  end;
  Result := True;
end;

{==============================================================================}
procedure TDiaryLocalSource.LoadFromFile(const FileName: string);
{==============================================================================}
var
  s: TStrings;
  i: integer;
  Pages: TPageDataList;
begin
  Clear;

  s := TStringList.Create;

  try
    s.LoadFromFile(FileName);               // <-- берёшь

    if (s.Count >= 2) and (s[0] = 'DIARYFMT') then
    begin
      s.Delete(0);
      s.Delete(0);
    end;

    TPageData.Read(S, LocalFmt, Pages);   // и загружаешь -->

    // для проверки сортировки и дублей используем вспомогательный список
    for i := 0 to High(Pages) do
      Add(Pages[i]);
  finally
    s.Free;
  end;

  FModified := False;  // да)
end;

{==============================================================================}
function TDiaryLocalSource.PostPages(const Pages: TDiaryPageList): boolean;
{==============================================================================}

  function PureLength(const S: string): integer;
  var
    i: integer;
  begin
    Result := Length(S);
    for i := 1 to Length(S) do
      if (S[i] = #10) or (S[i] = #13) then
        dec(Result);
  end;

  function Worry(PageOld, PageNew: TPageData): boolean;
  var
    Msg: string;
    MsgType: TMsgDlgType;
  begin
    Msg := '';
    MsgType := mtConfirmation;

    if (PageOld.Version > PageNew.Version) then
    begin
      MsgType := mtWarning;
      Msg := Msg + '* Версия: ' + IntToStr(PageOld.Version) + ' --> ' + IntToStr(PageNew.Version) + #13;
    end;

    if (PageOld.TimeStamp > PageNew.TimeStamp) then
      Msg := Msg + '* Время: ' + DateTimeToStr(PageOld.TimeStamp) + ' --> ' + DateTimeToStr(PageNew.TimeStamp) + #13;

    if (PureLength(PageNew.Page) - PureLength(PageOld.Page) < -20) then
      Msg := Msg + '* Размер: ' + IntToStr(Length(PageOld.Page)) + ' --> ' + IntToStr(Length(PageNew.Page)) + #13;

    Result :=
      (Msg <> '') and
      (MessageDlg(
        'Страница ' + DateToStr(PageOld.Date)+' будет перезаписана.'+#13+
        'Обнаружены подозрения:' + #13 +
        Msg + #13+
        'Продолжить?',
         MsgType, [mbYes,mbNo],0) <> 6); // mrYes
  end;

var
  i, Index: integer;
  PageData: TPageData;
begin
  for i := 0 to High(Pages) do
  begin
    // сериализуем
    PageData := TPageData.Create();
    TPageData.Write(Pages[i], PageData);

    // ищем
    Index := GetPageIndex(Pages[i].Date);
    if (Index = -1) then
    begin
      SetLength(FPages, Length(FPages) + 1);
      FPages[High(FPages)] := PageData;
      TraceLastPage();
    end else
    begin   
      if (not ShowUpdateWarning) or (not Worry(FPages[Index], PageData)) then
      begin
        FPages[Index].Free;
        FPages[Index] := PageData;
      end;
    end;
  end;
  if (Length(Pages) > 0) then
  begin
    FModified := True;
    SaveToFile(FFileName);
  end;

  Result := True;
end;

{==============================================================================}
procedure TDiaryLocalSource.SaveToFile(const FileName: string);
{==============================================================================}
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    TPageData.Write(FPages, S, LocalFmt);
    s.SaveToFile(FileName);
  finally
    s.Free;
  end;
  FModified := False;
end;

{==============================================================================}
function TDiaryLocalSource.TraceLastPage: integer;
{==============================================================================}
var
  Temp: TPageData;
begin
  Result := High(FPages);
  if (Result > -1) then
  begin
    Temp := FPages[Result];
    while (Result > 0) and (FPages[Result-1].Date > Temp.Date) do
    begin
      FPages[Result] := FPages[Result-1];
      dec(Result);
    end;
    FPages[Result] := Temp;
  end;
end;

initialization
  // 02.04.1992 09:45:00
  GetLocaleFormatSettings(GetThreadLocale, LocalFmt);
  LocalFmt.DateSeparator := '.';
  LocalFmt.TimeSeparator := ':';
  LocalFmt.ShortDateFormat := 'dd.mm.yyyy';
  LocalFmt.LongTimeFormat := 'hh:nn:ss';
end.

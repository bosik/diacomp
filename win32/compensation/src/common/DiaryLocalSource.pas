unit DiaryLocalSource;

interface

uses
  DiaryRoutines, DiaryDatabase, DiarySources, SysUtils, Classes, Dialogs {update warnings},
  XMLDoc, XMLIntf;

type
  ELoadingError = class(EInOutError);
  TSucceedEvent = procedure(Sender: TObject; OK: boolean) of object;
  
  // {#} - реализация интерфейса
  TDiaryLocalSource =  class (IDiarySource)
  private
    FPages: TPageList;
    FModified: boolean;
    {!}FUpdateWarning: boolean;
    FFileName: string;

    function Add(Page: TPageData): integer;
    procedure Clear;
    function CreatePage(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer): integer;
    function GetPageIndex(Date: TDate): integer;
    function TraceLastPage: integer;

    procedure LoadFromFile(const FileName: string);
    procedure Flush();
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    // TODO: not used, remove it
    function _GetFirstDate(var Date: TDate): boolean; deprecated;
    function _GetLastDate(var Date: TDate): boolean; deprecated;

    {#}function GetModList(Time: TDateTime; out ModList: TModList): boolean; override;
    {#}function GetPages(const Dates: TDateList; out Pages: TPageList): boolean; override;
    {#}function PostPages(const Pages: TPageList): boolean; override;

    // свойства
    property Modified: boolean read FModified {write FModified};
    property ShowUpdateWarning: boolean read FUpdateWarning write FUpdateWarning;

    // события
    //property OnLoad: TSucceedEvent read FOnLoad write FOnLoad;
    //property OnSave: TSucceedEvent read FOnSave write FOnSave;
 end;

implementation

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

  for i := 0 to high(FPages) do
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
function TDiaryLocalSource._GetFirstDate(var Date: TDate): boolean;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to high(FPages) do
  if (FPages[i].Page <> '') then
  begin
    Date := FPages[i].Date;
    Result := True;
    Exit;
  end;

  Result := False;
end;

{==============================================================================}
function TDiaryLocalSource._GetLastDate(var Date: TDate): boolean;
{==============================================================================}
var
  i: integer;
begin
  for i := high(FPages) downto 0 do
  if (FPages[i].Page <> '') then
  begin
    Date := FPages[i].Date;
    Result := True;
    Exit;
  end;

  Result := False;
end;

{==============================================================================}
function TDiaryLocalSource.GetModList(Time: TDateTime; out ModList: TModList): boolean;
{==============================================================================}
var
  i, Count: integer;
begin
  Count := 0;
  SetLength(ModList, 1);
  for i := 0 to high(FPages) do
  if (FPages[i].TimeStamp > Time) then
  begin
    if (Count = Length(ModList)) then
      SetLength(ModList, Length(ModList) * 2);
    ModList[Count].Date := FPages[i].Date;
    ModList[Count].Version := FPages[i].Version;
    inc(Count);
  end;
  SetLength(ModList, Count);
  Result := True;
end;

{==============================================================================}
function TDiaryLocalSource.GetPageIndex(Date: TDate): integer;
{==============================================================================}
var
  L,R: integer;
begin
  L := 0;
  R := high(FPages);
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
function TDiaryLocalSource.GetPages(const Dates: TDateList; out Pages: TPageList): boolean;
{==============================================================================}
var
  i, Index: integer;
begin
  SetLength(Pages, Length(Dates));
  for i := 0 to High(Dates) do
  begin
    Index := CreatePage(Dates[i], Now, 1);
    Pages[i] := TPageData.Create(FPages[Index]);
  end;
  Result := True;
end;

{==============================================================================}
procedure TDiaryLocalSource.LoadFromFile(const FileName: string);
{==============================================================================}
var
  s: TStringList;
  i: integer;
  Pages: TPageList;
begin
  Clear;

 { if not FileExists(FileName) then
  begin
    Result := CanCreate and CreateFile(FileName);
    NotifyLoad(Result);
    Exit;
  end;      }

  s := TStringList.Create;

  try
    s.LoadFromFile(FileName);               // <-- берёшь

    if (s.Count >= 2) and (s[0] = 'DIARYFMT') then
    begin
      s.Delete(0);
      s.Delete(0);
    end;

    TPageData.MultiRead(S, False, Pages);   // и загружаешь -->

    // для проверки сортировки и дублей используем вспомогательный список
    for i := 0 to high(Pages) do
      Add(Pages[i]);
  finally
    s.Free;
  end;

  FModified := false;  // да)
  //NotifyLoad(Result);
end;

(*
{==============================================================================}
procedure TDiaryLocalSource.NotifyLoad(OK: boolean);
{==============================================================================}
begin
  // TODO: never used
  //if Assigned(FOnLoad) then FOnLoad(Self, OK);
end;

{==============================================================================}
procedure TDiaryLocalSource.NotifySave(OK: boolean);
{==============================================================================}
begin
  // TODO: never used
  //if Assigned(FOnSave) then FOnSave(Self, OK);
end;
*)

{==============================================================================}
function TDiaryLocalSource.PostPages(const Pages: TPageList): boolean;
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

    if (PureLength(PageOld.Page) > PureLength(PageNew.Page)) then
      Msg := Msg + '* Размер: ' + IntToStr(Length(PageOld.Page)) + ' --> ' + IntToStr(Length(PageNew.Page)) + #13;

    Result :=
      (Msg <> '') and
      (MessageDlg(
        'Страница ' + DateToStr(PageOld.Date)+' будет загружена с сервера.'+#13+
        'Обнаружены подозрения:' + #13 +
        Msg + #13+
        'Продолжить?',
         MsgType, [mbYes,mbNo],0) <> 6); // mrYes
  end;

var
  i, Index: integer;
begin
  for i := 0 to high(Pages) do
  begin
    Index := GetPageIndex(Pages[i].Date);
    if (Index = -1) then
    begin
      SetLength(FPages, Length(FPages) + 1);
      FPages[High(FPages)] := TPageData.Create(Pages[i]);
      TraceLastPage();
    end else
    begin   
      if (not ShowUpdateWarning) or (not Worry(FPages[Index], Pages[i])) then
        FPages[Index].CopyFrom(Pages[i]);
    end;
  end;
  if (Length(Pages) > 0) then
    FModified := True;

  Flush();

  Result := True;
end;

{==============================================================================}
procedure TDiaryLocalSource.Flush();
{==============================================================================}
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    TPageData.MultiWrite(S, False, FPages);
    s.SaveToFile(FFileName);
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

end.

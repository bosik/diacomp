unit DiarySync;

{ Синхронизация }

interface

uses
  Classes,
  SysUtils,
  DiaryRoutines,
  DiarySources,
  DiaryPage,
  Autolog;

  { возвращается количество синхронизированных страниц }
  function SyncSources(Source1, Source2: IDiarySource; Since: TDateTime): integer;

implementation

type
  TSyncInfo = class
    Date: TDate;
    Version1: integer;
    Version2: integer;
    constructor Create(Date: TDate; Version1, Version2: integer);
  end;

  TObjDate = class
    Date: TDate;
    constructor Create(Date: TDate);
  end;

{ TSyncInfo }

{==============================================================================}
constructor TSyncInfo.Create(Date: TDate; Version1, Version2: integer);
{==============================================================================}
begin
  Self.Date := Date;
  Self.Version1 := Version1;
  Self.Version2 := Version2;
end;


{ TObjDate }

{==============================================================================}
constructor TObjDate.Create(Date: TDate);
{==============================================================================}
begin
  Self.Date := Date;
end;

{==============================================================================}
function ConvertToDateList(List: TList): TDateList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, List.Count);
  for i := 0 to List.Count - 1 do
    Result[i] := TObjDate(List[i]).Date;
end;

{==============================================================================}
procedure SortModList(var List: TModList);
{==============================================================================}

  procedure qsort(l, r: integer);
  var
    i, j: integer;
    x: TDate;
    y: TModItem;
  begin
    i := l;
    j := r;
    x := List[(l+r) div 2].Date;
    repeat
      while List[i].Date < x do inc(i);
      while List[j].Date > x do dec(j);
      if i <= j then
      begin
        if (List[i].Date <> List[j].Date) then
        begin
          y := List[i];
          List[i] := List[j];
          List[j] := y;
        end;
        inc(i);
        dec(j);
      end;
    until i > j;
    if l < j then qsort(l, j);
    if i < r then qsort(i, r);
  end;

  function Ordered(const List: TModList): boolean;
  var
    i: integer;
  begin
    for i := 0 to High(List) - 1 do
    if (List[i].Date > List[i + 1].Date) then
    begin
      Result := False;
      Exit;
    end;
    Result := True;
  end;

begin
  if (Length(List) > 0) and
     (not Ordered(List)) then
    qsort(0, High(List));
end;

{==============================================================================}
function Merge(var ModList1, ModList2: TModList; out Miss1, Miss2: TList  {of TObjDate}): TList {of TSyncInfo};
{==============================================================================}

  procedure AddUniq1(const Item: TModItem);
  begin
    Result.Add(TSyncInfo.Create(Item.Date, Item.Version, -1));
    Miss2.Add(TObjDate.Create(Item.Date));
  end;

  procedure AddUniq2(const Item: TModItem);
  begin
    Result.Add(TSyncInfo.Create(Item.Date, -1, Item.Version));
    Miss1.Add(TObjDate.Create(Item.Date));
  end;

var
  i, j: integer;
begin
  Result := TList.Create();
  Miss1 := TList.Create();
  Miss2 := TList.Create();

  // готовимся
  SortModList(ModList1);
  SortModList(ModList2);

  i := Low(ModList1);
  j := Low(ModList2);

  // параллельная обработка
  while (i <= High(ModList1)) and (j <= High(ModList2)) do
  begin
    if (ModList1[i].Date < ModList2[j].Date) then
    begin
      AddUniq1(ModList1[i]);
      inc(i);
    end else
    if (ModList1[i].Date > ModList2[j].Date) then
    begin
      AddUniq2(ModList2[j]);
      inc(j);
    end else
    begin
      Result.Add(TSyncInfo.Create(ModList1[i].Date, ModList1[i].Version, ModList2[j].Version));
      inc(i);
      inc(j);
    end;
  end;

  // добиваем первый
  while (i <= High(ModList1)) do
  begin
    AddUniq1(ModList1[i]);
    inc(i);
  end;

  // добиваем второй
  while (j <= High(ModList2)) do
  begin
    AddUniq2(ModList2[j]);
    inc(j);
  end;
end;

{==============================================================================}
procedure ExtractDiffs(SyncList: TList; out Newer1, Newer2: TList);
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to SyncList.Count - 1 do
  begin
    if (TSyncInfo(SyncList[i]).Version1 > TSyncInfo(SyncList[i]).Version2) then Newer1.Add(TObjDate.Create(TSyncInfo(SyncList[i]).Date)) else
    if (TSyncInfo(SyncList[i]).Version1 < TSyncInfo(SyncList[i]).Version2) then Newer2.Add(TObjDate.Create(TSyncInfo(SyncList[i]).Date)) else
    // nothing to do here;
  end;
end;

{==============================================================================}
function SyncSources(Source1, Source2: IDiarySource; Since: TDateTime): integer;
{==============================================================================}
var
  ModList1, ModList2: TModList;
  Newer1, Newer2: TList;
  Add1, Add2: TModList;
  Miss1, Miss2: TList;
  Pages: TDiaryPageList;
  SyncList: TList;

  i, j: integer;
begin
  Source1.GetModified(Since, ModList1);
  Source2.GetModified(Since, ModList2);
  SyncList := Merge(ModList1, ModList2, Miss1, Miss2);

  Source1.GetVersions(ConvertToDateList(Miss1), Add1);
  Source2.GetVersions(ConvertToDateList(Miss2), Add2);

  for i := 0 to High(Add1) do
  for j := 0 to SyncList.Count - 1 do
  begin
    if (TSyncInfo(SyncList[j]).Date = Add1[i].Date) and
       (TSyncInfo(SyncList[j]).Version1 = -1) then
    begin
      TSyncInfo(SyncList[j]).Version1 := Add1[i].Version;
      break;
    end;
  end;

  for i := 0 to High(Add2) do
  for j := 0 to SyncList.Count - 1 do
  begin
    if (TSyncInfo(SyncList[j]).Date = Add2[i].Date) and
       (TSyncInfo(SyncList[j]).Version2 = -1) then
    begin
      TSyncInfo(SyncList[j]).Version2 := Add2[i].Version;
      break;
    end;
  end;

  for i := 0 to SyncList.Count - 1 do
  begin
    Log(DEBUG, Format('SyncList'#9'%s'#9'%d'#9'%d', [
      DateToStr(TSyncInfo(SyncList[i]).Date),
      TSyncInfo(SyncList[i]).Version1,
      TSyncInfo(SyncList[i]).Version2
      ] ));
  end;

  Newer1 := TList.Create;
  Newer2 := TList.Create;
  try
    ExtractDiffs(SyncList, Newer1, Newer2);

    for i := 0 to Newer1.Count - 1 do
    begin
      Log(DEBUG, Format('SyncList'#9'1->2'#9'%s', [
        DateToStr(TSyncInfo(Newer1[i]).Date)
        ] ));
    end;
    for i := 0 to Newer2.Count - 1 do
    begin
      Log(DEBUG, Format('SyncList'#9'2->1'#9'%s', [
        DateToStr(TSyncInfo(Newer2[i]).Date)
        ] ));
    end;

    if Source1.GetPages(ConvertToDateList(Newer1), Pages) then Source2.PostPages(Pages);
    if Source2.GetPages(ConvertToDateList(Newer2), Pages) then Source1.PostPages(Pages);

    Result := Newer1.Count + Newer2.Count;

  finally
    Newer1.Free;
    Newer2.Free;
  end;

  // TODO: раньше здесь стояло определение минимальной даты загруженной
  // страницы и последующее UpdatePostPrand от неё
end;

(*
{==============================================================================}
procedure UnionDateLists(const ServerList, LocalList: TModList; out Result: TDatePairModList);
{==============================================================================}

  function Find(D: TDate): integer;
  var
    i: integer;
  begin
    for i := 0 to High(ServerList) do
    if ServerList[i].Date = D then
    begin
      Result := i;
      Exit;
    end;
    Result := -1;
  end;

var
  i,j,k: integer;
begin
  // TODO: пока тупой вариант
  SetLength(Result, Length(ServerList) + Length(LocalList));
  for i := 0 to High(ServerList) do
  begin
    Result[i].Date := ServerList[i].Date;
    Result[i].ServerVersion := ServerList[i].Version;
    Result[i].LocalVersion := 0;
  end;

  k := Length(ServerList);
  for i := 0 to High(LocalList) do
  begin
    j := Find(LocalList[i].Date);
    if j = -1 then
    begin
      Result[k].Date := LocalList[i].Date;
      Result[k].ServerVersion := 0;
      Result[k].LocalVersion := LocalList[i].Version;
      inc(k);
    end else
      Result[j].LocalVersion := LocalList[i].Version;
  end;

  SetLength(Result, k);
end;

{==============================================================================}
function SyncPage(Date: TDate; ServerStamp, LocalStamp: TDateTime): TSyncResult; overload;
{==============================================================================}
{ TimeStamp'ы должны быть относительно сервера }

  function More(Stamp1, Stamp2: TDateTime): boolean;
  const
    EPS = 1 / 1440; // одна минута !!!
  begin
    Result := ((Stamp1 - Stamp2) > EPS);
  end;

var
  Source: string;
  OK: boolean;
begin
  try
    if More(ServerStamp, LocalStamp) then
    begin
      OK := Web.DownloadPage(Date, Source);

      if ok and
         (not(   (Source = '') and (DiaryBase[Date].Count = 0))) and// замена пустой на пустую

        (MessageDlg('Страница '+DateToStr(Date)+' будет загружена с сервера. Продолжить?', mtConfirmation, [mbYes,mbNo],0) = 6{yes})

      then

      begin
        Result := srDoneDownload;
        DiaryBase[Date].ReadFrom(Source);
        DiaryBase[Date].TimeStamp := Web.ServerToLocal(ServerStamp);
        DiaryBase.Modified := True; // for saving on disk
      end else
        Result := srFailDownload;
      //sleep(500);
    end else
    if More(LocalStamp, ServerStamp) then
    begin
      // уже приведено
      DiaryBase[Date].WriteTo(Source);
      if Web.UploadPage(Date, Source, LocalStamp) then
        Result := srDoneUpload
      else
        Result := srFailUpload;
      //sleep(500);
    end else
      Result := srDoneEqual;
  except
    Result := srFailCommon;
  end;
end;

{==============================================================================}
function SyncPage(Date: TDate): TSyncResult; overload;
{==============================================================================}
var
  ServerStamp: TDateTime;
  LocalStamp: TDateTime;
begin
  try
    Web.RequestStamp(Date, ServerStamp);
    //ServerStamp := 0; //!!!
    LocalStamp := Web.LocalToServer(DiaryBase[Date].TimeStamp);
    Result := SyncPage(Date, ServerStamp, LocalStamp);
  except
    Result := srFailCommon;
  end;
end;
 *)

{
srDoneEqual: //Form1.StatusBar.Panels[3].Text := 'Уже синхронизировано';
srDoneDownload
srDoneUpload: //Form1.StatusBar.Panels[3].Text := 'Дневник загружен на сервер';
srFailCommon: //Form1.StatusBar.Panels[3].Text := 'Ошибка синхронизации';
srFailDownload: //Form1.StatusBar.Panels[3].Text := 'Ошибка загрузки (down)';
srFailUpload: //Form1.StatusBar.Panels[3].Text := 'Ошибка выгрузки (up)';
}

end.

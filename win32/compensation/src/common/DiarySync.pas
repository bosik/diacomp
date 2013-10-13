unit DiarySync;

{ Синхронизация }

interface

uses
  DiaryRoutines, DiarySources;

  { возвращается количество синхронизированных страниц }
  function SyncSources(Source1, Source2: IDiarySource; Since: TDateTime): integer;

implementation

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
procedure GetOverLists(var ModList1, ModList2: TModList; out Over1, Over2: TDateList);
{==============================================================================}

  procedure Init(var List: TDateList; var Counter: integer);
  begin
    SetLength(List, 4);
    Counter := 0;
  end;

  procedure Add(const Item: TDate; var List: TDateList; var Counter: integer);
  begin
    if (Counter = Length(List)) then
      SetLength(List, Length(List) * 2);
    List[Counter] := Item;
    inc(Counter);
  end;

  procedure Cut(var List: TDateList; Counter: integer);
  begin
    SetLength(List, Counter);
  end;

var
  i, j: integer;
  c1, c2: integer;
begin
  // готовимся
  SortModList(ModList1);
  SortModList(ModList2);
  Init(Over1, c1);
  Init(Over2, c2);
  i := Low(ModList1);
  j := Low(ModList2);

  // параллельная обработка
  while (i <= High(ModList1)) and (j <= High(ModList2)) do
  begin
    if (ModList1[i].Date < ModList2[j].Date) then
    begin
      Add(ModList1[i].Date, Over1, c1);
      inc(i);
    end else
    if (ModList1[i].Date > ModList2[j].Date) then
    begin
      Add(ModList2[j].Date, Over2, c2);
      inc(j);
    end else
    begin
      if (ModList1[i].Version > ModList2[j].Version) then
        Add(ModList1[i].Date, Over1, c1) else
      if (ModList1[i].Version < ModList2[j].Version) then
        Add(ModList2[j].Date, Over2, c2);
      inc(i);
      inc(j);
    end;
  end;

  // добиваем первый
  while (i <= High(ModList1)) do
  begin
    Add(ModList1[i].Date, Over1, c1);
    inc(i);
  end;

  // добиваем второй
  while (j <= High(ModList2)) do
  begin
    Add(ModList2[j].Date, Over2, c2);
    inc(j);
  end;

  // обрезаем
  Cut(Over1, c1);
  Cut(Over2, c2);
end;

{==============================================================================}
function SyncSources(Source1, Source2: IDiarySource; Since: TDateTime): integer;
{==============================================================================}
var
  ModList1, ModList2: TModList;
  Over1, Over2: TDateList;
  Pages: TPageDataList;
begin
  Result := 0;

  if not Source1.GetModList(Since, ModList1) then Exit;
  if not Source2.GetModList(Since, ModList2) then Exit;

  GetOverLists(ModList1, ModList2, Over1, Over2);

  if Source1.GetPages(Over1, Pages) then Source2.PostPages(Pages);
  if Source2.GetPages(Over2, Pages) then Source1.PostPages(Pages);

  Result := Length(Over1) + Length(Over2);

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

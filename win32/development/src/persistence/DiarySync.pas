unit DiarySync;

{ Синхронизация }

interface

uses
  DAO, //
  BusinessObjects; //

  { возвращается количество синхронизированных объектов }
  function SyncSources(Source1, Source2: TDAO; Since: TDateTime): integer;

implementation

{==============================================================================}
procedure SortVersioned(var List: TVersionedList);
{==============================================================================}

  // sort by ID

  procedure qsort(l, r: integer);
  var
    i, j: integer;
    x: string;
    y: TVersioned;
  begin
    i := l;
    j := r;
    x := List[(l+r) div 2].ID;
    repeat
      while List[i].ID < x do inc(i);
      while List[j].ID > x do dec(j);
      if i <= j then
      begin
        if (List[i].ID <> List[j].ID) then
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

  function Ordered(const List: TVersionedList): boolean;
  var
    i: integer;
  begin
    for i := 0 to High(List) - 1 do
    if (List[i].ID > List[i + 1].ID) then
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
procedure Add(const Item: TVersioned; var List: TVersionedList);
{==============================================================================}
begin
  SetLength(List, Length(List) + 1);
  List[High(List)] := Item;
end;

{==============================================================================}
procedure GetOverLists(
  var items1, items2: TVersionedList;
  var newer1, newer2: TVersionedList;
  var only1, only2: TVersionedList
);
{==============================================================================}
var
  i, j: integer;
begin
  SortVersioned(items1);
  SortVersioned(items2);

  SetLength(newer1, 0);
  SetLength(newer2, 0);
  SetLength(only1, 0);
  SetLength(only2, 0);

  i := Low(items1);
  j := Low(items2);

  // parallel processing
  while (i <= High(items1)) and (j <= High(items2)) do
  begin
    if (items1[i].ID < items2[j].ID) then
    begin
      Add(items1[i], only1);
      inc(i);
    end else
    if (items1[i].ID > items2[j].ID) then
    begin
      Add(items2[j], only2);
      inc(j);
    end else
    begin
      if (items1[i].Version > items2[j].Version) then
        Add(items1[i], newer1) else
      if (items1[i].Version < items2[j].Version) then
        Add(items2[j], newer2);

      inc(i);
      inc(j);
    end;
  end;

  // finish first list
  while (i <= High(items1)) do
  begin
    Add(items1[i], only1);
    inc(i);
  end;

  // finish second list
  while (j <= High(items2)) do
  begin
    Add(items2[j], only2);
    inc(j);
  end;
end;

{==============================================================================}
function SyncSources(Source1, Source2: TDAO; Since: TDateTime): integer;
{==============================================================================}
var
  Items1, Items2: TVersionedList;
  Newer1, Newer2: TVersionedList;
  Only1, Only2: TVersionedList;
  i, j: integer;
  T1, T2: TVersioned;
begin
  Items1 := Source1.FindChanged(Since);
  Items2 := Source2.FindChanged(Since);
  GetOverLists(Items1, Items2, Newer1, Newer2, Only1, Only2);

  for i := 0 to High(Only1) do
  begin
    T1 := Only1[i];
    T2 := Source2.FindById(T1.ID);

    if (T2 = nil) or (T2.Version < T1.Version) then
      Add(T1, Newer1) else
    if (T2.Version > T1.Version) then
      Add(T2, Newer2);
  end;

  for j := 0 to High(Only2) do
  begin
    T2 := Only2[j];
    T1 := Source1.FindById(T2.ID);

    if (T1 = nil) or (T1.Version < T2.Version) then
      Add(T2, Newer2) else
    if (T1.Version > T2.Version) then
      Add(T1, Newer1);
  end;

  Source1.Post(Newer2);
  Source2.Post(Newer1);
  Result := Length(Newer1) + Length(Newer2);

  // TODO: раньше здесь стояло определение минимальной даты загруженной
  // страницы и последующее UpdatePostPrand от неё
end;

end.


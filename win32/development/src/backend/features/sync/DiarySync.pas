unit DiarySync;

{ Синхронизация }

interface

uses
  ObjectService, //
  BusinessObjects, //
  HashService, //
  DiaryRoutines, //
  SysUtils; //

  { возвращается количество синхронизированных объектов }
  function SyncSources(Source1, Source2: TObjectService; Since: TDateTime): integer; overload;
  function SyncSources(Source1, Source2: TObjectService; Callback: TCallbackProgress = nil): integer; overload;

implementation

{======================================================================================================================}
procedure SortVersioned(var List: TVersionedList);
{======================================================================================================================}

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

{======================================================================================================================}
procedure Add(const Item: TVersioned; var List: TVersionedList; var CurrentSize: integer);
{======================================================================================================================}
begin
  if (CurrentSize >= Length(List)) then
    SetLength(List, Length(List) * 2 + 1);

  List[CurrentSize] := Item;
  inc(CurrentSize);
end;

{======================================================================================================================}
procedure GetOverLists(
  var items1, items2: TVersionedList;
  var newer1, newer2: TVersionedList;
  var only1, only2: TVersionedList
);
{======================================================================================================================}
var
  i, j: integer;
  NewerSize1, NewerSize2: integer;
  OnlySize1, OnlySize2: integer;
begin
  SortVersioned(items1);
  SortVersioned(items2);

  SetLength(newer1, 16);
  SetLength(newer2, 16);
  SetLength(only1, 16);
  SetLength(only2, 16);

  NewerSize1 := 0;
  NewerSize2 := 0;
  OnlySize1 := 0;
  OnlySize2 := 0;

  i := Low(items1);
  j := Low(items2);

  // parallel processing
  while (i <= High(items1)) and (j <= High(items2)) do
  begin
    if (items1[i].ID < items2[j].ID) then
    begin
      Add(items1[i], only1, OnlySize1);
      inc(i);
    end else
    if (items1[i].ID > items2[j].ID) then
    begin
      Add(items2[j], only2, OnlySize2);
      inc(j);
    end else
    begin
      if (items1[i].Version > items2[j].Version) then
        Add(items1[i], newer1, NewerSize1) else
      if (items1[i].Version < items2[j].Version) then
        Add(items2[j], newer2, NewerSize2);

      inc(i);
      inc(j);
    end;
  end;

  // finish first list
  while (i <= High(items1)) do
  begin
    Add(items1[i], only1, OnlySize1);
    inc(i);
  end;

  // finish second list
  while (j <= High(items2)) do
  begin
    Add(items2[j], only2, OnlySize2);
    inc(j);
  end;

  SetLength(Newer1, NewerSize1);
  SetLength(Newer2, NewerSize2);
  SetLength(Only1, OnlySize1);
  SetLength(Only2, OnlySize2);
end;

{======================================================================================================================}
function SyncSources(Source1, Source2: TObjectService; Since: TDateTime): integer;
{======================================================================================================================}
const
  MAX_BLOCK_SIZE = 100;

  procedure BlockSave(Data: TVersionedList; Source: TObjectService);
  var
    Start, Count: integer;
  begin
    Start := 0;
    while (Start <= High(Data)) do
    begin
      if (Start + MAX_BLOCK_SIZE <= High(Data)) then
        Count := MAX_BLOCK_SIZE
      else
        Count := High(Data) - Start + 1;
      Source.Save(Copy(Data, Start, Count));
      Start := Start + MAX_BLOCK_SIZE;
    end;
  end;

var
  Items1, Items2: TVersionedList;
  Newer1, Newer2: TVersionedList;
  Only1, Only2: TVersionedList;
  i, j: integer;
  T1, T2: TVersioned;
  NewerSize1, NewerSize2: integer;
begin
  Items1 := Source1.FindChanged(Since);
  Items2 := Source2.FindChanged(Since);
  GetOverLists(Items1, Items2, Newer1, Newer2, Only1, Only2);

  NewerSize1 := Length(Newer1);
  NewerSize2 := Length(Newer2);

  for i := 0 to High(Only1) do
  begin
    T1 := Only1[i];
    T2 := Source2.FindById(T1.ID);

    if (T2 = nil) or (T2.Version < T1.Version) then
      Add(T1, Newer1, NewerSize1) else
    if (T2.Version > T1.Version) then
      Add(T2, Newer2, NewerSize2);
  end;

  for j := 0 to High(Only2) do
  begin
    T2 := Only2[j];
    T1 := Source1.FindById(T2.ID);

    if (T1 = nil) or (T1.Version < T2.Version) then
      Add(T2, Newer2, NewerSize2) else
    if (T1.Version > T2.Version) then
      Add(T1, Newer1, NewerSize1);
  end;

  SetLength(Newer1, NewerSize1);
  SetLength(Newer2, NewerSize2);

  BlockSave(Newer2, Source1);
  BlockSave(Newer1, Source2);
  Result := Length(Newer1) + Length(Newer2);

  for i := 0 to High(Newer1) do
    Newer1[i].Free;       
  for i := 0 to High(Newer2) do
    Newer2[i].Free;

  // TODO: раньше здесь стояло определение минимальной даты загруженной
  // страницы и последующее UpdatePostPrand от неё
end;

{======================================================================================================================}
function SyncSources(Source1, Source2: TObjectService; Callback: TCallbackProgress = nil): integer;
{======================================================================================================================}
const
  MAX_BLOCK_SIZE = 100;

  procedure BlockSave(Data: TVersionedList; Source: TObjectService);
  var
    Start, Count: integer;
  begin
    if (Length(Data) <= MAX_BLOCK_SIZE) then
    begin
      Source.Save(Data);
    end else
    begin
      Start := 0;
      while (Start <= High(Data)) do
      begin
        if (Start + MAX_BLOCK_SIZE <= High(Data)) then
          Count := MAX_BLOCK_SIZE
        else
          Count := High(Data) - Start + 1;
        Source.Save(Copy(Data, Start, Count));
        Start := Start + MAX_BLOCK_SIZE;
      end;
    end;
  end;

  function CalculateProgress(const Prefix: string): integer;

    function CharToInt(const C: char): integer;
    begin
      case C of
        '0': Result := 0;
        '1': Result := 1;
        '2': Result := 2;
        '3': Result := 3;
        '4': Result := 4;
        '5': Result := 5;
        '6': Result := 6;
        '7': Result := 7;
        '8': Result := 8;
        '9': Result := 9;
        'a': Result := 10;
        'b': Result := 11;
        'c': Result := 12;
        'd': Result := 13;
        'e': Result := 14;
        'f': Result := 15;
      end;
    end;

  var
    p, d, i: integer;
  begin
    p := 0;
    d := 1;

    for i := 0 to 1 do
    if (Length(Prefix) > i) then
    begin
      p := p * 16 + CharToInt(Prefix[i + 1]);
      d := d * 16;
    end;

    Result := p * 100 div d;
  end;

  function Sync(const Prefix: string): integer;
  (*var
    Hash1, Hash2: string;

    Items1, Items2: TVersionedList;
    Newer1, Newer2: TVersionedList;
    Only1, Only2: TVersionedList;
    i, j: integer;
    T1, T2: TVersioned;
    NewerSize1, NewerSize2: integer; *)
  begin
   (* Result := 0;
    Hash1 := Source1.GetHash(Prefix);
    Hash2 := Source2.GetHash(Prefix);

    if (Hash1 = Hash2) then
    begin
      Exit;
    end;

    if (Length(Prefix) < MAX_PREFIX_SIZE) then
    begin
      for i := 0 to 15 do
      begin
        // TODO: request bunch of 16 hashes at once
        Result := Result + Sync(Prefix + LETTERS[i]);

        if (Length(Prefix) <= 2) and (Assigned(Callback)) then
          Callback(CalculateProgress(Prefix + LETTERS[i]));
      end;
    end else
    begin
      Items1 := Source1.FindByIdPrefix(Prefix);
      Items2 := Source2.FindByIdPrefix(Prefix);

      GetOverLists(Items1, Items2, Newer1, Newer2, Only1, Only2);
      NewerSize1 := Length(Newer1);
      NewerSize2 := Length(Newer2);

      for i := 0 to High(Only1) do
      begin
        T1 := Only1[i];
        T2 := Source2.FindById(T1.ID);

        if (T2 = nil) or (T2.Version < T1.Version) then
          Add(T1, Newer1, NewerSize1) else
        if (T2.Version > T1.Version) then
          Add(T2, Newer2, NewerSize2);
      end;

      for j := 0 to High(Only2) do
      begin
        T2 := Only2[j];
        T1 := Source1.FindById(T2.ID);

        if (T1 = nil) or (T1.Version < T2.Version) then
          Add(T2, Newer2, NewerSize2) else
        if (T1.Version > T2.Version) then
          Add(T1, Newer1, NewerSize1);
      end;

      SetLength(Newer1, NewerSize1);
      SetLength(Newer2, NewerSize2);

      BlockSave(Newer2, Source1);
      BlockSave(Newer1, Source2);
      Result := Length(Newer1) + Length(Newer2);

      for i := 0 to High(Items1) do
        Items1[i].Free;
      for i := 0 to High(Items2) do
        Items2[i].Free;  
    end;       *)
  end;

begin
  if (Assigned(Callback)) then
    Callback(0);

  Result := Sync('');

  if (Assigned(Callback)) then
    Callback(100);

  // TODO: раньше здесь стояло определение минимальной даты загруженной
  // страницы и последующее UpdatePostPrand от неё
end;

end.


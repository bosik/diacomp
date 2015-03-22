unit DiarySync;

{ Синхронизация }

interface

uses
  ObjectService, //
  BusinessObjects, //
  HashService, //
  DiaryRoutines, //
  SysUtils, //
  MerkleTree;

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
procedure Add(const Item: TVersioned; var List: TVersionedList; var CurrentSize: integer); overload;
{======================================================================================================================}
begin
  if (CurrentSize >= Length(List)) then
    SetLength(List, Length(List) * 2 + 1);

  List[CurrentSize] := Item;
  inc(CurrentSize);
end;

{======================================================================================================================}
procedure Add(const Source: TVersionedList; var Dest: TVersionedList); overload;
{======================================================================================================================}
var
  OldSize, i: integer;
begin
  OldSize := Length(Dest);
  SetLength(Dest, OldSize + Length(Source));
  for i := 0 to High(Source) do
  begin
    Dest[OldSize + i] := Source[i];
  end;
end;

{======================================================================================================================}
procedure Free(var List: TVersionedList);
{======================================================================================================================}
var
  i: integer;
begin
  for i := Low(List) to High(List) do
    List[i].Free;
  SetLength(List, 0);
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
        Add(items2[j], newer2, NewerSize2) else
      if (items1[i].Hash <> items2[j].Hash) then
      begin
        // to fix deadlocks
        if (items1[i].TimeStamp > items2[j].TimeStamp) then
          Add(items1[i], newer1, NewerSize1)
        else
          Add(items2[j], newer2, NewerSize2);
      end;

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
procedure BlockSave(Data: TVersionedList; Source: TObjectService; MaxBlockSize: integer);
{======================================================================================================================}
var
  Start, Count: integer;
begin
  if (Length(Data) <= MaxBlockSize) then
  begin
    Source.Save(Data);
  end else
  begin
    Start := 0;
    while (Start <= High(Data)) do
    begin
      if (Start + MaxBlockSize <= High(Data)) then
        Count := MaxBlockSize
      else
        Count := High(Data) - Start + 1;
      Source.Save(Copy(Data, Start, Count));
      Start := Start + MaxBlockSize;
    end;
  end;
end;

{======================================================================================================================}
function ProcessItems(Source1, Source2: TObjectService; Items1, Items2: TVersionedList): integer;
{======================================================================================================================}
var
  Newer1, Newer2: TVersionedList;
  Only1, Only2: TVersionedList;
begin
  GetOverLists(Items1, Items2, Newer1, Newer2, Only1, Only2);
  Add(Only1, Newer1);
  Add(Only2, Newer2);

  BlockSave(Newer2, Source1, 100);
  BlockSave(Newer1, Source2, 100);

  Result := Length(Newer1) + Length(Newer2);
end;

{======================================================================================================================}
function SyncSources(Source1, Source2: TObjectService; Since: TDateTime): integer;
{======================================================================================================================}
var
  Items1, Items2: TVersionedList;
begin
  Items1 := Source1.FindChanged(Since);
  Items2 := Source2.FindChanged(Since);

  Result := ProcessItems(Source1, Source2, Items1, Items2);

  Free(Items1);
  Free(Items2);
end;

{======================================================================================================================}
function SyncSources(Source1, Source2: TObjectService; Callback: TCallbackProgress = nil): integer;
{======================================================================================================================}

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

    for i := 0 to 2 do
    if (Length(Prefix) > i) then
    begin
      p := p * 16 + CharToInt(Prefix[i + 1]);
      d := d * 16;
    end;

    Result := p * 100 div d;
  end;

  function SynchronizeChildren(Source1: TObjectService; Tree1: THashTree; Source2: TObjectService; Tree2: THashTree;
    const Prefix: string; Callback: TCallbackProgress): integer;
  var
    Items1, Items2: TVersionedList;
    i: integer;
    Hashes1, Hashes2: TStringMap;
    Key: string;
    Hash1, Hash2: string;
  begin
    if (Assigned(Callback)) then
    case Length(Prefix) of
      0: Callback(0);
      1: Callback(CalculateProgress(Prefix));
      2: Callback(CalculateProgress(Prefix));
      3: Callback(CalculateProgress(Prefix));
    end;

    if (Length(Prefix) < MAX_PREFIX_SIZE) then
    begin
      Hashes1 := Tree1.GetHashChildren(Prefix);
      Hashes2 := Tree2.GetHashChildren(Prefix);
      Result := 0;
      for i := 0 to 15 do
      begin
        Key := Prefix + LETTERS[i];
        Hash1 := Hashes1[Key];
        Hash2 := Hashes2[Key];

        if (Hash1 <> Hash2) then
        begin
          Result := Result + SynchronizeChildren(Source1, Tree1, Source2, Tree2, Key, Callback);
        end;
      end;
    end else
    begin
      Items1 := Source1.FindByIdPrefix(Prefix);
      Items2 := Source2.FindByIdPrefix(Prefix);

      Result := ProcessItems(Source1, Source2, Items1, Items2);

      Free(Items1);
      Free(Items2);
    end;
  end;

var
  Tree1, Tree2: THashTree;
  Hash1, Hash2: string;
begin
  if (Assigned(Callback)) then
    Callback(0);

  Tree1 := Source1.GetHashTree();
  Tree2 := Source2.GetHashTree();

  Hash1 := Tree1.GetHash('');
  Hash2 := Tree2.GetHash('');
  if (Hash1 <> Hash2) then
  begin
    Result := SynchronizeChildren(Source1, Tree1, Source2, Tree2, '', Callback);
  end else
  begin
    Result := 0;
  end;

  if (Assigned(Callback)) then
    Callback(100);
end;

end.


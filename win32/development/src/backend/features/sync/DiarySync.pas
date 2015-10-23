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

type
  TAppendableVersionedList = class
  private
    FData: TVersionedList;
    FSize: integer;
  public
    procedure Add(const Item: TVersioned);
    function Count(): integer;
    constructor Create();
    destructor Destroy(); override;
    function Get(Index: integer): TVersioned;
    function ToArray(): TVersionedList;
    procedure Truncate(); 
  end;

  { возвращается количество синхронизированных объектов }
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
procedure GetOverLists(
  var Items1, Items2: TVersionedList;
  Newer1, Newer2, Garbage: TAppendableVersionedList
);
{======================================================================================================================}
var
  i, j: integer;
begin
  SortVersioned(Items1);
  SortVersioned(Items2);

  i := Low(Items1);
  j := Low(Items2);

  // parallel processing
  while (i <= High(Items1)) and (j <= High(Items2)) do
  begin
    if (Items1[i].ID < Items2[j].ID) then
    begin
      Newer1.Add(Items1[i]);
      inc(i);
    end else
    if (Items1[i].ID > Items2[j].ID) then
    begin
      Newer2.Add(Items2[j]);
      inc(j);
    end else
    begin
      if (Items1[i].Version > Items2[j].Version) then
      begin
        Newer1.Add(Items1[i]);
        Garbage.Add(Items2[j]);
      end else
      if (Items1[i].Version < Items2[j].Version) then
      begin
        Newer2.Add(Items2[j]);
        Garbage.Add(Items1[i]);
      end else
      if (Items1[i].Hash <> Items2[j].Hash) then
      begin
        // to fix deadlocks
        if (Items1[i].TimeStamp > Items2[j].TimeStamp) then
        begin
          Newer1.Add(Items1[i]);
          Garbage.Add(Items2[j]);
        end else
        begin
          Newer2.Add(Items2[j]);
          Garbage.Add(Items1[i]);
        end;
      end else
      begin
        Garbage.Add(Items1[i]);
        Garbage.Add(Items2[j]);
      end;

      inc(i);
      inc(j);
    end;
  end;

  // finish first list
  while (i <= High(Items1)) do
  begin
    Newer1.Add(Items1[i]);
    inc(i);
  end;

  // finish second list
  while (j <= High(Items2)) do
  begin
    Newer2.Add(Items2[j]);
    inc(j);
  end;
end;

{======================================================================================================================}
procedure BlockSave(Data: TAppendableVersionedList; Source: TObjectService; MaxBlockSize: integer);
{======================================================================================================================}
var
  Start, Count: integer;
begin
  if (Data.Count() <= MaxBlockSize) then
  begin
    Source.Save(Data.ToArray());
  end else
  begin
    Start := 0;
    while (Start < Data.Count()) do
    begin
      if (Start + MaxBlockSize < Data.Count()) then
        Count := MaxBlockSize
      else
        Count := Data.Count() - Start;
      Source.Save(Copy(Data.ToArray(), Start, Count));
      Start := Start + MaxBlockSize;
    end;
  end;
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
        else raise Exception.Create('Failed to convert char to int, invalid value: ' + C);
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

  procedure SynchronizeChildren(Source1: TObjectService; Tree1: THashTree; Source2: TObjectService; Tree2: THashTree;
    const Prefix: string; var Newer1, Newer2: TAppendableVersionedList; Callback: TCallbackProgress);
  var
    Items1, Items2: TVersionedList;
    Garbage: TAppendableVersionedList;
    i: integer;
    Hashes1, Hashes2: TStringMap;
    Key: string;
    Hash1, Hash2: string;
    Count1, Count2: integer;
  begin
    if (Assigned(Callback)) then
    case Length(Prefix) of
      0: Callback(0);
      1: Callback(CalculateProgress(Prefix));
      2: Callback(CalculateProgress(Prefix));
      3: Callback(CalculateProgress(Prefix));
    end;

    Count1 := Source1.Count(Prefix);
    Count2 := Source2.Count(Prefix);

    if (Count1 + Count2 > 500) and (Length(Prefix) < MAX_PREFIX_SIZE) then
    begin
      Hashes1 := Tree1.GetHashChildren(Prefix);
      Hashes2 := Tree2.GetHashChildren(Prefix);
      try
        for i := 0 to 15 do
        begin
          Key := Prefix + LETTERS[i];
          Hash1 := Hashes1[Key];
          Hash2 := Hashes2[Key];

          if (Hash1 <> Hash2) then
          begin
            SynchronizeChildren(Source1, Tree1, Source2, Tree2, Key, Newer1, Newer2, Callback);
          end;
        end;
      finally
        FreeAndNil(Hashes1);
        FreeAndNil(Hashes2);
      end;
    end else
    begin
      Items1 := Source1.FindByIdPrefix(Prefix);
      Items2 := Source2.FindByIdPrefix(Prefix);

      Garbage := TAppendableVersionedList.Create();
      GetOverLists(Items1, Items2, Newer1, Newer2, Garbage);

      Garbage.Free();
    end;
  end;

var
  Tree1, Tree2: THashTree;
  Hash1, Hash2: string;
  Newer1: TAppendableVersionedList;
  Newer2: TAppendableVersionedList;
begin
  if (Assigned(Callback)) then
    Callback(0);

  Tree1 := Source1.GetHashTree();
  Tree2 := Source2.GetHashTree();

  try
    Hash1 := Tree1.GetHash('');
    Hash2 := Tree2.GetHash('');
    Newer1 := TAppendableVersionedList.Create();
    Newer2 := TAppendableVersionedList.Create();

    if (Hash1 <> Hash2) then
    begin
      SynchronizeChildren(Source1, Tree1, Source2, Tree2, '', Newer1, Newer2, Callback);
      Result := Newer1.Count() + Newer2.Count();

      BlockSave(Newer1, Source2, 250);
      BlockSave(Newer2, Source1, 250);
    end else
    begin
      Result := 0;
    end;

    if (Assigned(Callback)) then
      Callback(100);
  finally
    FreeAndNil(Tree1);
    FreeAndNil(Tree2);
    Newer1.Free;
    Newer2.Free;
  end;
end;

{ TAppendableVersionedList }

{======================================================================================================================}
procedure TAppendableVersionedList.Add(const Item: TVersioned);
{======================================================================================================================}
begin
  if (FSize >= Length(FData)) then
    SetLength(FData, Length(FData) * 2 + 1);

  FData[FSize] := Item;
  inc(FSize);
end;

{======================================================================================================================}
function TAppendableVersionedList.Count(): integer;
{======================================================================================================================}
begin
  Result := FSize;
end;

{======================================================================================================================}
constructor TAppendableVersionedList.Create();
{======================================================================================================================}
begin
  SetLength(FData, 16);
  FSize := 0;
end;

{======================================================================================================================}
destructor TAppendableVersionedList.Destroy();
{======================================================================================================================}
var
  i: integer;
begin
  for i := 0 to High(FData) do
    FreeAndNil(FData[i]);
  SetLength(FData, 0);

  inherited;
end;

{======================================================================================================================}
function TAppendableVersionedList.Get(Index: integer): TVersioned;
{======================================================================================================================}
begin
  Result := FData[Index];
end;

{======================================================================================================================}
function TAppendableVersionedList.ToArray(): TVersionedList;
{======================================================================================================================}
begin
  Truncate();
  Result := FData;
end;

{======================================================================================================================}
procedure TAppendableVersionedList.Truncate();
{======================================================================================================================}
begin
  SetLength(FData, FSize);
end;

end.


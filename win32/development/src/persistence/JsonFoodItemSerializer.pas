unit JsonFoodItemSerializer;

interface

uses
  uLkJSON,
  JsonSerializer,
  JsonVersionedSerializer,
  BusinessObjects;

  function ParseVersionedFoodItem(json: TlkJSONbase): TFood;
  function ParseVersionedFoodItems(json: TlkJSONlist): TFoodItemList;

  function SerializeVersionedFoodItem(Item: TFood): TlkJSONobject;
  function SerializeVersionedFoodItems(Items: TFoodItemList): TlkJSONlist;

implementation

{==============================================================================}
function ParseFoodItem(json: TlkJSONobject): TFood;
{==============================================================================}
begin
  Result := TFood.Create();
  Result.Name      := (json['name']  as TlkJSONstring).Value;
  Result.RelProts  := (json['prots'] as TlkJSONnumber).Value;
  Result.RelFats   := (json['fats']  as TlkJSONnumber).Value;
  Result.RelCarbs  := (json['carbs'] as TlkJSONnumber).Value;
  Result.RelValue  := (json['value'] as TlkJSONnumber).Value;
  Result.Tag       := (json['tag']   as TlkJSONnumber).Value;
  Result.FromTable := (json['table'] as TlkJSONboolean).Value;
end;

{==============================================================================}
function ParseVersionedFoodItem(json: TlkJSONbase): TFood;
{==============================================================================}
var
  JsonObj: TlkJSONobject;
begin
  JsonObj := json as TlkJSONobject;

  Result := ParseFoodItem(JsonObj[REC_DATA] as TlkJSONobject);
  ReadVersioned(JsonObj, Result);
end;

{==============================================================================}
function ParseVersionedFoodItems(json: TlkJSONlist): TFoodItemList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, json.Count);
  for i := 0 to json.Count - 1 do
    Result[i] := ParseVersionedFoodItem(json.Child[i] as TlkJSONobject);
end;

{==============================================================================}
function SerializeFoodItem(Item: TFood): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add('name', Item.Name);
  Result.Add('prots', Item.RelProts);
  Result.Add('fats', Item.RelFats);
  Result.Add('carbs', Item.RelCarbs);
  Result.Add('value', Item.RelValue);
  Result.Add('tag', Item.Tag);
  Result.Add('table', Item.FromTable);
end;

{==============================================================================}
function SerializeVersionedFoodItem(Item: TFood): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  WriteVersioned(Item, Result);
  Result.Add(REC_DATA, SerializeFoodItem(Item));
end;

{==============================================================================}
function SerializeVersionedFoodItems(Items: TFoodItemList): TlkJSONlist;
{==============================================================================}
var
  i: integer;
begin
  Result := TlkJSONlist.Create;          
  for i := Low(Items) to High(Items) do
    Result.Add(SerializeVersionedFoodItem(Items[i]));
end;

end.

unit DiaryPageSerializer;

interface

uses
  SysUtils, // StrToDate, etc.
  Classes, // TStrings
  DiaryRecords,
  BusinessObjects,
  DiaryRoutines, // TDate
  uLkJSON,
  JsonVersionedSerializer;

type
  TStringsArray = array of TStrings;

  {
  function ParseBlood(json: TlkJSONobject): TBloodRecord;
  function ParseIns(json: TlkJSONobject): TInsRecord;
  function ParseFoodMassed(json: TlkJSONobject): TFoodMassed;
  function ParseMeal(json: TlkJSONobject): TMealRecord;
  function ParseNote(json: TlkJSONobject): TNoteRecord; }
  function ParseDiaryRecord(json: TlkJSONobject): TCustomRecord;
  
  function ParseVersionedDiaryRecord(json: TlkJSONbase): TCustomRecord;
  function ParseVersionedDiaryRecords(json: TlkJSONlist): TRecordList;
  function ParseVersionedFoodItem(json: TlkJSONbase): TFoodItem;
  function ParseVersionedFoodItems(json: TlkJSONlist): TFoodItemList;
  function ParseVersionedDishItem(json: TlkJSONbase): TDishItem;
  function ParseVersionedDishItems(json: TlkJSONlist): TDishItemList;

  function SerializeBlood(R: TBloodRecord): TlkJSONobject;
  function SerializeIns(R: TInsRecord): TlkJSONobject;
  function SerializeFoodMassed(R: TFoodMassed): TlkJSONobject;
  function SerializeMeal(R: TMealRecord): TlkJSONobject;
  function SerializeNote(R: TNoteRecord): TlkJSONobject;
  function SerializeDiaryRecord(R: TCustomRecord): TlkJSONobject;
  function SerializeVersionedDiaryRecord(R: TCustomRecord): TlkJSONobject;
  function SerializeVersionedDiaryRecords(List: TRecordList): TlkJSONlist;

  function SerializeVersionedFoodItem(Item: TFoodItem): TlkJSONobject;
  function SerializeVersionedFoodItems(Items: TFoodItemList): TlkJSONlist;
  function SerializeVersionedDishItem(Item: TDishItem): TlkJSONobject;
  function SerializeVersionedDishItems(Items: TDishItemList): TlkJSONlist;

const
  REC_TYPE            = 'type';
  REC_TYPE_BLOOD      = 'blood';
  REC_TYPE_INS        = 'ins';
  REC_TYPE_MEAL       = 'meal';
  REC_TYPE_NOTE       = 'note';

  REC_TIME            = 'time';
  REC_BLOOD_VALUE     = 'value';
  REC_BLOOD_FINGER    = 'finger';
  REC_INS_VALUE       = 'value';
  REC_MEAL_SHORT      = 'short';
  REC_MEAL_CONTENT    = 'content';
  REC_MEAL_FOOD_NAME  = 'name';
  REC_MEAL_FOOD_PROTS = 'prots';
  REC_MEAL_FOOD_FATS  = 'fats';
  REC_MEAL_FOOD_CARBS = 'carbs';
  REC_MEAL_FOOD_VALUE = 'value';
  REC_MEAL_FOOD_MASS  = 'mass';
  REC_NOTE_TEXT       = 'text';

implementation

{==============================================================================}
function ParseBlood(json: TlkJSONobject): TBloodRecord;
{==============================================================================}
begin
  Result := TBloodRecord.Create();
  Result.NativeTime := StrToDateTime((json[REC_TIME] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Result.Value := (json[REC_BLOOD_VALUE] as TlkJSONnumber).Value;
  Result.Finger := (json[REC_BLOOD_FINGER] as TlkJSONnumber).Value;
end;

{==============================================================================}
function ParseIns(json: TlkJSONobject): TInsRecord;
{==============================================================================}
begin
  Result := TInsRecord.Create();
  Result.NativeTime := StrToDateTime((json[REC_TIME] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Result.Value := (json[REC_INS_VALUE] as TlkJSONnumber).Value;
end;

{==============================================================================}
function ParseFoodMassed(json: TlkJSONobject): TFoodMassed;
{==============================================================================}
begin
  Result := TFoodMassed.Create();
  Result.Name     := (json[REC_MEAL_FOOD_NAME]  as TlkJSONstring).Value;
  Result.RelProts := (json[REC_MEAL_FOOD_PROTS] as TlkJSONnumber).Value;
  Result.RelFats  := (json[REC_MEAL_FOOD_FATS]  as TlkJSONnumber).Value;
  Result.RelCarbs := (json[REC_MEAL_FOOD_CARBS] as TlkJSONnumber).Value;
  Result.RelValue := (json[REC_MEAL_FOOD_VALUE] as TlkJSONnumber).Value;
  Result.Mass     := (json[REC_MEAL_FOOD_MASS]  as TlkJSONnumber).Value;
end;

{==============================================================================}
function ParseMeal(json: TlkJSONobject): TMealRecord;
{==============================================================================}
var
  content: TlkJSONlist;
  i: integer;
  Food: TFoodMassed;
begin
  Result := TMealRecord.Create();
  Result.NativeTime := StrToDateTime((json[REC_TIME] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Result.ShortMeal := (json[REC_MEAL_SHORT] as TlkJSONboolean).Value;

  content := (json[REC_MEAL_CONTENT] as TlkJSONlist);
  for i := 0 to content.Count - 1 do
  begin
    Food := ParseFoodMassed((content.Child[i] as TlkJSONobject));
    Result.Add(food);
  end;
end;

{==============================================================================}
function ParseNote(json: TlkJSONobject): TNoteRecord;
{==============================================================================}
begin
  Result := TNoteRecord.Create();

  if (Assigned(json[REC_TIME])) then
    Result.NativeTime := StrToDateTime((json[REC_TIME] as TlkJSONstring).Value, STD_DATETIME_FMT)
  else
    raise Exception.Create('Failed to read JSON: missing field: ' + REC_TIME);

  if (Assigned(json[REC_NOTE_TEXT])) then
    Result.Text := (json[REC_NOTE_TEXT] as TlkJSONstring).Value
  else
    raise Exception.Create('Failed to read JSON: missing field: ' + REC_NOTE_TEXT);
end;

{==============================================================================}
function ParseDiaryRecord(json: TlkJSONobject): TCustomRecord;
{==============================================================================}
var
  RecType: string;
begin
  RecType := (json[REC_TYPE] as TlkJSONstring).Value;

  if (RecType = REC_TYPE_BLOOD) then Result := ParseBlood(json as TlkJSONobject) else
  if (RecType = REC_TYPE_INS)   then Result := ParseIns  (json as TlkJSONobject) else
  if (RecType = REC_TYPE_MEAL)  then Result := ParseMeal (json as TlkJSONobject) else
  if (RecType = REC_TYPE_NOTE)  then Result := ParseNote (json as TlkJSONobject) else
    raise Exception.Create('Unsupported record type: ' + RecType);
end;

{==============================================================================}
function ParseVersionedDiaryRecord(json: TlkJSONbase): TCustomRecord;
{==============================================================================}
var
  JsonObj: TlkJSONobject;
begin
  JsonObj := json as TlkJSONobject;

  Result := ParseDiaryRecord(JsonObj[REC_DATA] as TlkJSONobject);
  Result.ID := (JsonObj[REC_ID] as TlkJSONstring).Value;
  Result.TimeStamp := StrToDateTime((JsonObj[REC_TIMESTAMP] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Result.Version := (JsonObj[REC_VERSION] as TlkJSONnumber).Value;
  Result.Deleted := (JsonObj[REC_DELETED] as TlkJSONboolean).Value;
end;

{==============================================================================}
function ParseVersionedDiaryRecords(json: TlkJSONlist): TRecordList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, json.Count);
  for i := 0 to json.Count - 1 do
    Result[i] := ParseVersionedDiaryRecord(json.Child[i] as TlkJSONobject);
end;

{==============================================================================}
function ParseDishItem(json: TlkJSONobject): TDishItem;
{==============================================================================}
var
  Content: TlkJSONlist;
  i: integer;
begin
  Result := TDishItem.Create();
  
  Result.Name      := (json['name']  as TlkJSONstring).Value;
  Result.Tag       := (json['tag']   as TlkJSONnumber).Value;

  if (json['mass'] <> nil) then
    Result.SetResultMass((json['mass'] as TlkJSONnumber).Value)
  else
    Result.EraseResultMass;

  if (json['content'] <> nil) then
  begin
    Content := json['content'] as TlkJSONlist;
    
    for i := 0 to Content.Count - 1 do
      Result.Add(ParseFoodMassed(Content.Child[i] as TlkJSONobject));
  end;
end;

{==============================================================================}
function ParseFoodItem(json: TlkJSONobject): TFoodItem;
{==============================================================================}
begin
  Result := TFoodItem.Create();
  Result.Name      := (json['name']  as TlkJSONstring).Value;
  Result.RelProts  := (json['prots'] as TlkJSONnumber).Value;
  Result.RelFats   := (json['fats']  as TlkJSONnumber).Value;
  Result.RelCarbs  := (json['carbs'] as TlkJSONnumber).Value;
  Result.RelValue  := (json['value'] as TlkJSONnumber).Value;
  Result.Tag       := (json['tag']   as TlkJSONnumber).Value;
  Result.FromTable := (json['table'] as TlkJSONboolean).Value;
end;

{==============================================================================}
function ParseVersionedFoodItem(json: TlkJSONbase): TFoodItem;
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
function ParseVersionedDishItem(json: TlkJSONbase): TDishItem;
{==============================================================================}
var
  JsonObj: TlkJSONobject;
begin
  JsonObj := json as TlkJSONobject;

  Result := ParseDishItem(JsonObj[REC_DATA] as TlkJSONobject);
  ReadVersioned(JsonObj, Result);
end;

{==============================================================================}
function ParseVersionedDishItems(json: TlkJSONlist): TDishItemList;
{==============================================================================}
var
  i: integer;
begin
  SetLength(Result, json.Count);
  for i := 0 to json.Count - 1 do
    Result[i] := ParseVersionedDishItem(json.Child[i] as TlkJSONobject);
end;

{==============================================================================}
function SerializeBlood(R: TBloodRecord): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_TYPE, REC_TYPE_BLOOD);
  Result.Add(REC_TIME, DateTimeToStr(R.NativeTime, STD_DATETIME_FMT));
  Result.Add(REC_BLOOD_VALUE, R.Value);
  Result.Add(REC_BLOOD_FINGER, R.Finger);
end;

{==============================================================================}
function SerializeIns(R: TInsRecord): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_TYPE, REC_TYPE_INS);
  Result.Add(REC_TIME, DateTimeToStr(R.NativeTime, STD_DATETIME_FMT));
  Result.Add(REC_INS_VALUE, R.Value);
end;

{==============================================================================}
function SerializeFoodMassed(R: TFoodMassed): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_MEAL_FOOD_NAME, R.Name);
  Result.Add(REC_MEAL_FOOD_PROTS, R.RelProts);
  Result.Add(REC_MEAL_FOOD_FATS, R.RelFats);
  Result.Add(REC_MEAL_FOOD_CARBS, R.RelCarbs);
  Result.Add(REC_MEAL_FOOD_VALUE, R.RelValue);
  Result.Add(REC_MEAL_FOOD_MASS, R.Mass);
end;

{==============================================================================}
function SerializeMeal(R: TMealRecord): TlkJSONobject;
{==============================================================================}
var
  Content: TlkJSONlist;
  i: integer;
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_TYPE, REC_TYPE_MEAL);
  Result.Add(REC_TIME, DateTimeToStr(R.NativeTime, STD_DATETIME_FMT));
  Result.Add(REC_MEAL_SHORT, R.ShortMeal);

  Content := TlkJSONlist.Create();
  for i := 0 to R.Count - 1 do
    Content.Add(SerializeFoodMassed(R[i]));

  Result.Add(REC_MEAL_CONTENT, Content);
end;

{==============================================================================}
function SerializeNote(R: TNoteRecord): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_TYPE, REC_TYPE_NOTE);
  Result.Add(REC_TIME, DateTimeToStr(R.NativeTime, STD_DATETIME_FMT));
  Result.Add(REC_NOTE_TEXT, R.Text);
end;

{==============================================================================}
function SerializeDiaryRecord(R: TCustomRecord): TlkJSONobject;
{==============================================================================}
begin
  if (R.RecType = TBloodRecord) then Result := SerializeBlood(R as TBloodRecord) else
  if (R.RecType = TInsRecord)   then Result := SerializeIns(R as TInsRecord) else
  if (R.RecType = TMealRecord)  then Result := SerializeMeal(R as TMealRecord) else
  if (R.RecType = TNoteRecord)  then Result := SerializeNote(R as TNoteRecord) else
    raise Exception.Create('Unsupported record type: ' + R.ClassName);
end;

{==============================================================================}
function SerializeVersionedDiaryRecord(R: TCustomRecord): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  Result.Add(REC_ID, R.ID);
  Result.Add(REC_TIMESTAMP, DateTimeToStr(R.TimeStamp, STD_DATETIME_FMT));
  Result.Add(REC_VERSION, R.Version);
  Result.Add(REC_DELETED, R.Deleted);
  Result.Add(REC_DATA, SerializeDiaryRecord(R));
end;


{==============================================================================}
function SerializeVersionedDiaryRecords(List: TRecordList): TlkJSONlist;
{==============================================================================}
var
  i: integer;
begin
  Result := TlkJSONlist.Create;
  for i := Low(List) to High(List) do
    Result.Add(SerializeVersionedDiaryRecord(List[i]));
end;

{==============================================================================}
function SerializeDishItem(Item: TDishItem): TlkJSONobject;
{==============================================================================}
var
  Content: TlkJSONlist;
  i: integer;
begin
  Result := TlkJSONobject.Create();
  Result.Add('name', Item.Name);
  Result.Add('tag', Item.Tag);

  if (Item.FixedMass) then
    Result.Add('mass', Item.ResultMass);

  Content := TlkJSONlist.Create;
  for i := 0 to Item.Count - 1 do
    Content.Add(SerializeFoodMassed(Item.Content[i]));

  Result.Add('content', Content);
end;

{==============================================================================}
function SerializeFoodItem(Item: TFoodItem): TlkJSONobject;
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
function SerializeVersionedFoodItem(Item: TFoodItem): TlkJSONobject;
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

{==============================================================================}
function SerializeVersionedDishItem(Item: TDishItem): TlkJSONobject;
{==============================================================================}
begin
  Result := TlkJSONobject.Create();
  WriteVersioned(Item, Result);
  Result.Add(REC_DATA, SerializeDishItem(Item));
end;

{==============================================================================}
function SerializeVersionedDishItems(Items: TDishItemList): TlkJSONlist;
{==============================================================================}
var
  i: integer;
begin
  Result := TlkJSONlist.Create;          
  for i := Low(Items) to High(Items) do
    Result.Add(SerializeVersionedDishItem(Items[i]));
end;

end.
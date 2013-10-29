unit DiaryUnit;

interface

uses
  Windows, SysUtils, Classes, Controls, Grids, Dialogs;

  {====== ОСНОВНЫЕ КЛАССЫ ======}
  
type
  { Молоко }
  TFood = record
    Name: string;
    Prots, Fats, Carbs: real;
    Value,GI: real;
  end;

  TFoodMassed = record
    ProductType: integer;
    Mass: real;
  end;

  TDishMassed = record
    DishType: integer;
    Mass: real;
  end;

  { Гречневая каша }
  TDish = record
    Name: string;
    Content: array of TFoodMassed;
    { вычисляются после загрузки }
    Prots, Fats, Carbs: real;
    Value,GI: real;
    Mass: real;
  end;

  TDiaryBlood = record
    Time: integer; // в мин
    Value: real;
  end;

  TDiaryIns = record
    Time: integer;
    Value: integer;
  end;

  TDiaryFood = record
    Time: integer;
    Dish: array of TDish;
    { вычисляются после загрузки }
    Prots, Fats, Carbs: real;
    Value,GI: real;
    Mass: real;
  end;

  TDiaryPage = record
    Date: TDate;
    Blood: array of TDiaryBlood;
    Ins: array of TDiaryIns;
    Meal: array of TDiaryFood;
  end;


  {====== ОСНОВНЫЕ ПРОЦЕДУРЫ ======}

  { работа с базами }
  procedure LoadFoodBase;
  procedure SaveFoodBase;
  procedure ShowFoodBase(const Table: TStringGrid);

  procedure CalculateDish(var D: TDish);
  procedure LoadDishBase;
  procedure SaveDishBase;
  procedure ShowDishBase(const Table: TStringGrid;
    On100g: boolean);

  {====== ДОПОЛНИТЕЛЬНЫЕ ПРОЦЕДУРЫ ======}
  function RealToStr(x: real): string;
  function TimeToStr(time: integer): string;
  procedure ErrorMessage(const Text: string);

  {====== ОСНОВНЫЕ ПЕРЕМЕННЫЕ ======}

var
  Food: array of TFood;
  Dish: array of TDish;
  Diary: array of TDiaryPage;
implementation

{=========================================================}
function RealToStr(x: real): string;
{=========================================================}
begin
  Result:=FloatToStr(Round(x*10)/10);
end;

{=========================================================}
function TimeToStr(time: integer): string;
{=========================================================}
begin
  if time<600 then result:='0' else result:='';
  result:=result+IntToStr(time div 60)+':';
  if (time mod 60)<10 then result:=result+'0';
  result:=result+IntToStr(time mod 60);
end;

{=========================================================}
procedure ErrorMessage(const Text: string);
{=========================================================}
begin
  MessageDlg(Text,mtError,[mbOK],0);
end;

{*********************************************************}
{*********************************************************}

{=========================================================}
procedure LoadFoodBase;
{=========================================================}
var
  s: TStrings;
  line,buf: string;
  i,j: integer;
begin
  s:=TStringList.Create;
  try
    s.LoadFromFile('FoodBase.txt');
    SetLength(Food,s.Count);
    for i:=0 to s.Count-1 do
    begin
      line:=s[i];
      j:=pos('[',s[i]);
      Food[i].Name:=Copy(line,1,j-1);
      inc(j);

      buf:='';
      while (line[j]<>'/') do
      begin
        buf:=buf+line[j];
        inc(j);
      end;
      Food[i].Prots:=StrToFloat(buf);

      buf:='';
      inc(j);
      while (line[j]<>'/') do
      begin
        buf:=buf+line[j];
        inc(j);
      end;
      Food[i].Fats:=StrToFloat(buf);

      buf:='';
      inc(j);
      while (line[j]<>'/') do
      begin
        buf:=buf+line[j];
        inc(j);
      end;
      Food[i].Carbs:=StrToFloat(buf);

      buf:='';
      inc(j);
      while (line[j]<>'/') do
      begin
        buf:=buf+line[j];
        inc(j);
      end;
      Food[i].Value:=StrToFloat(buf);

      buf:='';
      inc(j);
      while (line[j]<>']') do
      begin
        buf:=buf+line[j];
        inc(j);
      end;
      Food[i].GI:=StrToFloat(buf);
    end;
  finally
    s.Free;
  end;
end;

{=========================================================}
procedure SaveFoodBase;
{=========================================================}
var
  s: TStrings;
  line: string;
  i: integer;
begin
  s:=TStringList.Create;
  try
    for i:=0 to high(Food) do
    begin
      line:=
        Food[i].Name+'['+
        FloatToStr(Food[i].Prots)+'/'+
        FloatToStr(Food[i].Fats)+'/'+
        FloatToStr(Food[i].Carbs)+'/'+
        FloatToStr(Food[i].Value)+'/'+
        FloatToStr(Food[i].GI)+']';
      s.Add(line);
    end;
    s.SaveToFile('FoodBase.txt');
  finally
    s.Free;
  end;
end;

{=========================================================}
procedure ShowFoodBase(const Table: TStringGrid);
{=========================================================}
var
  i,n: integer;
begin
  with Table do
  begin
    n:=Row-1;
    ColCount:=6;
    RowCount:=length(Food)+1;

    Cells[0,0]:='  Наименование';
    Cells[1,0]:='  Белки';
    Cells[2,0]:='  Жиры';
    Cells[3,0]:='  Угл.';
    Cells[4,0]:='  ккал';
    Cells[5,0]:='  ГИ';

    for i:=0 to high(Food) do
    begin
      Cells[0,i+1]:=Food[i].Name;
      Cells[1,i+1]:=RealToStr(Food[i].Prots);
      Cells[2,i+1]:=RealToStr(Food[i].Fats);
      Cells[3,i+1]:=RealToStr(Food[i].Carbs);
      Cells[4,i+1]:=RealToStr(Food[i].Value);
      Cells[5,i+1]:=IntToStr(Round(Food[i].GI*100));
    end;
    Row:=n+1;
  end;
end;

{=========================================================}
procedure CalculateDish(var D: TDish);
{=========================================================}
var
  i: integer;
begin
  D.Prots:=0;
  D.Fats:=0;
  D.Carbs:=0;
  D.Value:=0;
  D.GI:=0;
  D.Mass:=0;
  for i:=0 to high(D.Content) do
  begin
    D.Prots:=D.Prots +
      food[D.Content[i].ProductType].Prots/100*
      D.Content[i].Mass;

    D.Fats:=D.Fats +
      food[D.Content[i].ProductType].Fats/100*
      D.Content[i].Mass;

    D.Carbs:=D.Carbs +
      food[D.Content[i].ProductType].Carbs/100*
      D.Content[i].Mass;

    D.Value:=D.Value +
      food[D.Content[i].ProductType].Value/100*
      D.Content[i].Mass;

    D.GI:=D.GI +
      food[D.Content[i].ProductType].GI*
      D.Content[i].Mass;

    D.Mass:=D.Mass+D.Content[i].Mass;
  end;
  if D.Mass>0 then
    D.GI:=D.GI/D.Mass
  else
    D.GI:=0;
end;

{=========================================================}
procedure LoadDishBase;
{=========================================================}
var
  s: TStrings;
  buf: string;
  i,j,n: integer;
  founded: boolean;
begin
  s:=TStringList.Create;
  try
    s.LoadFromFile('DishBase.txt');
    SetLength(Dish,0);
    n:=-1;
    for i:=0 to s.Count-1 do
    if s[i][1]='#' then
    begin
      inc(n);
      SetLength(Dish,n+1);
      Dish[n].Name:=Copy(s[i],2,length(s[i])-1);
    end else
    if s[i][1]<>'=' then
    begin
      SetLength(Dish[n].Content,length(Dish[n].Content)+1);
      buf:=Copy(s[i],1,pos(':',s[i])-1);
      founded:=false;
      for j:=0 to high(food) do
      if food[j].Name=buf then
      begin
        founded:=true;
        Dish[n].Content[high(Dish[n].Content)].ProductType:=j;
        break;
      end;
      if not founded then
        ErrorMessage(
          'Продукт <'+buf+'> из блюда <'+
          Dish[n].Name+'> не найден');
      Dish[n].Content[high(Dish[n].Content)].Mass:=
        StrToFloat(Copy(s[i],pos(':',s[i])+1,length(s[i])-pos(':',s[i])));
    end;
  finally
    s.Free;
  end;

  { вычисление суммарных значений }
  for i:=0 to high(Dish) do
    CalculateDish(Dish[i]);
end;

{=========================================================}
procedure SaveDishBase;
{=========================================================}
var
  s: TStrings;
  i,j: integer;
begin
  s:=TStringList.Create;
  try
    for i:=0 to high(Dish) do
    begin
      s.Add('#'+Dish[i].Name);
      for j:=0 to high(Dish[i].Content) do
        s.Add(Food[Dish[i].Content[j].ProductType].Name+
        ':'+RealToStr(Dish[i].Content[j].Mass));
      s.Add('==============================');
    end;
    s.SaveToFile('DishBase.txt');
  finally
    s.Free;
  end;
end;

{=========================================================}
procedure ShowDishBase(const Table: TStringGrid;
  On100g: boolean);
{=========================================================}
var
  i,n: integer;
begin
  with Table do
  begin
    n:=Row-1;
    RowCount:=length(Dish)+1;
    ColCount:=7;

    Cells[0,0]:='  Наименование';
    Cells[1,0]:='  Масса';
    Cells[2,0]:='  Белки';
    Cells[3,0]:='  Жиры';
    Cells[4,0]:='  Угл.';
    Cells[5,0]:='  ккал';
    Cells[6,0]:='    ГИ';

    for i:=0 to high(Dish) do
    begin
      Cells[0,i+1]:=Dish[i].Name;
      Cells[1,i+1]:=RealToStr(Dish[i].Mass);

      if (On100g)and
         (Dish[i].Mass<>0) then
      begin
        Cells[2,i+1]:=RealToStr(Dish[i].Prots*100/Dish[i].Mass);
        Cells[3,i+1]:=RealToStr(Dish[i].Fats*100/Dish[i].Mass);
        Cells[4,i+1]:=RealToStr(Dish[i].Carbs*100/Dish[i].Mass);
        Cells[5,i+1]:=IntToStr(Round(Dish[i].Value*100/Dish[i].Mass));
      end else
      begin
        Cells[2,i+1]:=RealToStr(Dish[i].Prots);
        Cells[3,i+1]:=RealToStr(Dish[i].Fats);
        Cells[4,i+1]:=RealToStr(Dish[i].Carbs);
        Cells[5,i+1]:=IntToStr(Round(Dish[i].Value));
      end;
      Cells[6,i+1]:=IntToStr(Round(Dish[i].GI*100));
    end;
    Row:=n+1;
  end;
end;

end.

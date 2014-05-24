unit BasesTest;

interface

uses
  TestFrameWork,
  SysUtils,
  BusinessObjects,
  Bases;

type
  TFoodBaseTest = class(TTestCase)
  private
    FoodBase: TFoodBase;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddNil;
    procedure TestDelete;
  end;

  TDishBaseTest = class(TTestCase)
  private
    DishBase: TDishBase;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddNil;
    procedure TestDelete;
  end;

implementation

{ TFoodBaseTest }

procedure TFoodBaseTest.SetUp;
begin
  inherited;
  FoodBase := TFoodBase.Create();
end;

procedure TFoodBaseTest.TearDown;
begin
  inherited;
  FoodBase.Free;
end;

procedure TFoodBaseTest.TestAdd;
var
  FoodBase: TFoodBase;
  Food: TFood;
  Index: integer;
begin
  FoodBase := TFoodBase.Create();

  Food := TFood.Create;
  Food.Name := 'яблоко';
  Index := FoodBase.Add(Food);
  CheckEquals(0, Index);

  Food := TFood.Create;
  Food.Name := 'мандарин';
  Index := FoodBase.Add(Food);
  CheckEquals(0, Index);

  Food := TFood.Create;
  Food.Name := 'јбрикос';
  Index := FoodBase.Add(Food);
  CheckEquals(0, Index);

  Food := TFood.Create;
  Food.Name := '€йцо';
  Index := FoodBase.Add(Food);
  CheckEquals(3, Index);

  CheckEquals(4, FoodBase.Count, 'Count check failed');

  CheckEqualsString('јбрикос', FoodBase[0].Name);
  CheckEqualsString('мандарин', FoodBase[1].Name);
  CheckEqualsString('яблоко', FoodBase[2].Name);
  CheckEqualsString('€йцо', FoodBase[3].Name);

  FoodBase.Free;
end;

procedure TFoodBaseTest.TestAddNil;
begin
  ExpectedException := EInvalidPointer;
  FoodBase.Add(nil);
  ExpectedException := nil;
end;

procedure TFoodBaseTest.TestDelete;
var
  FoodBase: TFoodBase;
  Food: TFood;
begin
  FoodBase := TFoodBase.Create();

  Food := TFood.Create;
  Food.Name := 'яблоко';
  FoodBase.Add(Food);

  Food := TFood.Create;
  Food.Name := 'мандарин';
  FoodBase.Add(Food);

  Food := TFood.Create;
  Food.Name := 'јбрикос';
  FoodBase.Add(Food);

  Food := TFood.Create;
  Food.Name := '€йцо';
  FoodBase.Add(Food);

  CheckEquals(4, FoodBase.Count, 'Count check failed');

  FoodBase.Free;
end;

{ TDishBaseTest }

procedure TDishBaseTest.SetUp;
begin
  inherited;
  DishBase := TDishBase.Create();
end;

procedure TDishBaseTest.TearDown;
begin
  inherited;
  DishBase.Free;
end;

procedure TDishBaseTest.TestAdd;
var
  DishBase: TDishBase;
  Dish: TDish;
  Index: integer;
begin
  DishBase := TDishBase.Create();

  Dish := TDish.Create;
  Dish.Name := 'яблочный пирог';
  Index := DishBase.Add(Dish);
  CheckEquals(0, Index);

  Dish := TDish.Create;
  Dish.Name := 'макароны';
  Index := DishBase.Add(Dish);
  CheckEquals(0, Index);

  Dish := TDish.Create;
  Dish.Name := 'Ѕлинчики';
  Index := DishBase.Add(Dish);
  CheckEquals(0, Index);

  Dish := TDish.Create;
  Dish.Name := '€ичница';
  Index := DishBase.Add(Dish);
  CheckEquals(3, Index);

  CheckEquals(4, DishBase.Count, 'Count check failed');

  CheckEqualsString('Ѕлинчики', DishBase[0].Name);
  CheckEqualsString('макароны', DishBase[1].Name);
  CheckEqualsString('яблочный пирог', DishBase[2].Name);
  CheckEqualsString('€ичница', DishBase[3].Name);

  DishBase.Free;
end;

procedure TDishBaseTest.TestAddNil;
begin
  ExpectedException := EInvalidPointer;
  DishBase.Add(nil);
  ExpectedException := nil;
end;

procedure TDishBaseTest.TestDelete;
var
  DishBase: TDishBase;
  Dish: TDish;
begin
  DishBase := TDishBase.Create();

  Dish := TDish.Create;
  Dish.Name := 'яблочный пирог';
  DishBase.Add(Dish);

  Dish := TDish.Create;
  Dish.Name := 'макароны';
  DishBase.Add(Dish);

  Dish := TDish.Create;
  Dish.Name := 'Ѕлинчики';
  DishBase.Add(Dish);

  Dish := TDish.Create;
  Dish.Name := '€ичница';
  DishBase.Add(Dish);

  CheckEquals(4, DishBase.Count, 'Count check failed');

  DishBase.Free;
end;


initialization
  TestFramework.RegisterTest(TFoodBaseTest.Suite);
  TestFramework.RegisterTest(TDishBaseTest.Suite);
end.

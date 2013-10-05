unit BusinessObjectsTest;

interface

uses
  TestFrameWork, BusinessObjects;

type
  TFoodDataTest = class(TTestCase)
  private
    Food: TFoodRelative;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFields;
    procedure TestValidator;
  end;

  TFoodMassedTest = class(TTestCase)
  private
    Food: TFoodMassed;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestWrite;
  end;


implementation

{ TFoodDataTest }

procedure TFoodDataTest.SetUp;
begin
  inherited;
  Food := TFoodRelative.Create;
end;

procedure TFoodDataTest.TearDown;
begin
  inherited;
  Food.Free;
end;

procedure TFoodDataTest.TestFields;
begin
  Food.Name := 'TheName'; CheckEqualsString('TheName', Food.Name);
  Food.RelProts := 10;    CheckEquals(10, Food.RelProts);
  Food.RelFats  := 11;    CheckEquals(11, Food.RelFats);
  Food.RelCarbs := 12;    CheckEquals(12, Food.RelCarbs);
  Food.RelValue := 13;    CheckEquals(13, Food.RelValue);
end;

procedure TFoodDataTest.TestValidator;
begin
  Check(Food.IsCorrectRel(0));
  Check(Food.IsCorrectRel(0.1));
  Check(Food.IsCorrectRel(80));
  Check(Food.IsCorrectRel(100));

  CheckFalse(Food.IsCorrectRel(-0.1));
  CheckFalse(Food.IsCorrectRel(100.1));
  CheckFalse(Food.IsCorrectRel(100500));
end;

{ TFoodMassedTest }

procedure TFoodMassedTest.SetUp;
begin
  inherited;
  Food := TFoodMassed.Create;
end;

procedure TFoodMassedTest.TearDown;
begin
  inherited;
  Food.Free;
end;

procedure TFoodMassedTest.TestCopy;
var
  Temp: TFoodMassed;
begin
  Food.Name := 'Колбаса';
  Food.RelProts := 0.1;
  Food.RelFats := 0.2;
  Food.RelCarbs := 0.3;
  Food.RelValue := 0.4;
  Food.Mass := 200;

  Temp := TFoodMassed.Create;
  Temp.CopyFrom(Food);

  CheckEquals(Food.Name,     Temp.Name);
  CheckEquals(Food.RelProts, Temp.RelProts);
  CheckEquals(Food.RelFats , Temp.RelFats);
  CheckEquals(Food.RelCarbs, Temp.RelCarbs);
  CheckEquals(Food.RelValue, Temp.RelValue);
end;

procedure TFoodMassedTest.TestWrite;
begin
  Food.Name := 'Колбаса';
  Food.RelProts := 0.1;
  Food.RelFats := 0.2;
  Food.RelCarbs := 0.3;
  Food.RelValue := 0.4;
  Food.Mass := 200;

  CheckEqualsString('Колбаса[0,1|0,2|0,3|0,4]:200', Food.Write());
end;

initialization
  TestFramework.RegisterTest(TFoodDataTest.Suite);
  TestFramework.RegisterTest(TFoodMassedTest.Suite);
end.

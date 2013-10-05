unit BusinessObjectsTest;

interface

uses
  TestFrameWork, BusinessObjects;

type
  TFoodDataTest = class(TTestCase)
  private
    FoodData: TFoodData;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFirst;
  end;


implementation

{ TFoodDataTest }

procedure TFoodDataTest.SetUp;
begin
  inherited;
  FoodData := TFoodData.Create;
end;

procedure TFoodDataTest.TearDown;
begin
  inherited;
  FoodData.Free;
end;

procedure TFoodDataTest.TestFirst;
begin
  FoodData.Name := 'TheName'; CheckEqualsString('TheName', FoodData.Name);
  FoodData.RelProts := 10;    CheckEquals(10, FoodData.RelProts);
  FoodData.RelFats  := 11;    CheckEquals(11, FoodData.RelFats);
  FoodData.RelCarbs := 10;    CheckEquals(12, FoodData.RelCarbs);
  FoodData.RelValue := 13;    CheckEquals(13, FoodData.RelValue);
end;

initialization
  TestFramework.RegisterTest(TFoodDataTest.Suite);
end.

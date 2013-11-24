unit FoodbaseDAO;

interface

uses
  SysUtils,
  BusinessObjects;

type
  TFoodList = array of TFood;

  EItemNotFoundException = class(Exception)
    constructor Create(Food: TFood);
  end;

  EDuplicateException = class(Exception)
    constructor Create(Food: TFood);
  end;

  TFoodbaseDAO = class
    // ��������� ������� � ����; � ������ ������������� ����������� ���������� EDuplicateException
    procedure Add(Food: TFood); virtual; abstract;

    // ������� ��������� ������� �� ����; � ������ ���������� ����������� ���������� EItemNotFoundException
    procedure Delete(Food: TFood); virtual; abstract;

    // ���������� ��� ��������� � ���� ��������
    function FindAll(): TFoodList; virtual; abstract;

    // ���� ��� ��������, ���������� ��������� ������ � �������� (��� ����� ��������)
    function FindAny(const Filter: string): TFoodList; virtual; abstract;

    // ���� ������� � ������ ���������; � ������ ���������� ���������� nil
    function FindOne(const Name: string): TFood; virtual; abstract;

    // �������� ��� ���� ��������� �������, ����� ������ �������� �� ���������
    {*}procedure ReplaceAll(const NewList: TFoodList; NewVersion: integer); virtual; abstract;

    // ��������� ������� � ����; � ������ ���������� ����������� ���������� EItemNotFoundException
    procedure Update(Food: TFood); virtual; abstract;

    // �������� ����� ������ ����
    {*}function Version(): integer; virtual; abstract;
  end;

implementation

{ EItemNotFoundException }

constructor EItemNotFoundException.Create(Food: TFood);
begin
  inherited CreateFmt('������� #%d (%s) �� ������', [Food.ID, Food.Name]);
end;

{ EDuplicateException }

constructor EDuplicateException.Create(Food: TFood);
begin
  inherited CreateFmt('������� #%d (%s) ��� ����������', [Food.ID, Food.Name]);
end;

end.

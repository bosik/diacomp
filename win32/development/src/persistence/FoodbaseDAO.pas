unit FoodbaseDAO;

interface

uses
  SysUtils,
  BusinessObjects,
  DiaryRoutines;

type
  TFoodList = array of TFood;

  EItemNotFoundException = class(Exception)
    constructor Create(ID: TCompactGUID);
  end;

  EDuplicateException = class(Exception)
    constructor Create(Food: TFood);
  end;

  TFoodbaseDAO = class
    // ��������� ������� � ���� � ���������� ��� ������������� (��� ���������);
    // � ������ ������������� �������� � ��� �� ID ����������� ���������� EDuplicateException
    function Add(Food: TFood): TCompactGUID; virtual; abstract;

    // ������� ��������� ������� �� ����; � ������ ���������� ����������� ���������� EItemNotFoundException
    procedure Delete(ID: TCompactGUID); virtual; abstract;

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

constructor EItemNotFoundException.Create(ID: TCompactGUID);
begin
  inherited CreateFmt('������� {%s} �� ������', [ID]);
end;

{ EDuplicateException }

constructor EDuplicateException.Create(Food: TFood);
begin
  inherited CreateFmt('������� "%s" (#%s) ��� ����������', [Food.Name, Food.ID]);
end;

end.

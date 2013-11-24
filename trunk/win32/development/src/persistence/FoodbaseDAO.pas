unit FoodbaseDAO;

interface

uses
  SysUtils,
  BusinessObjects;

type
  TFoodList = array of TFood;

  EItemNotFoundException = class(Exception);

  TFoodbaseDAO = class
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

    // ��������� (create) ��� ��������� (update) ������� � ����
    procedure Save(Food: TFood); virtual; abstract;

    // �������� ����� ������ ����
    {*}function Version(): integer; virtual; abstract;
  end;

implementation

end.

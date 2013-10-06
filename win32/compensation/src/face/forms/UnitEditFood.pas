unit UnitEditFood;

{ Модуль редактора продуктов }

interface

uses
  Windows, SysUtils, Classes, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Grids, ValEdit, Menus,
  DiaryCore, UnitDataInterface,
  BusinessObjects, Bases, DiaryInterface, DiaryRoutines;

type
  TFormFood = class(TAutosetupForm)
    GroupBoxName: TGroupBox;
    EditName: TEdit;
    GroupBoxContent: TGroupBox;
    Shape1: TShape;
    Shape2: TShape;
    Panel1: TPanel;
    ButtonCancel: TBitBtn;
    ButtonSave: TBitBtn;
    Panel2: TPanel;
    FoodEditor: TValueListEditor;
    MenuHidden: TMainMenu;
    HiddenCuts1: TMenuItem;
    Item_Save: TMenuItem;
    GroupBoxSource: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure EditNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FoodEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FoodEditorKeyPress(Sender: TObject; var Key: Char);
    procedure Item_SaveClick(Sender: TObject);
    procedure EditNameKeyPress(Sender: TObject; var Key: Char);
    procedure RadioButtonKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    procedure Designer; override;
  public
    function OpenFoodEditor(var Food: TFood; New: boolean; ShowInRect: TRect): boolean;
    procedure ShowFoodInEditor;
  end;

var
  FormFood: TFormFood;
  AFood: TFood;
  ModeNew: boolean;
  OK: boolean;

const
  VAL_PROTS = 'Белки (г)';
  VAL_FATS  = 'Жиры (г)';
  VAL_CARBS = 'Углеводы (г)';
  VAL_VALUE = 'Калорийность (ккал)';
  //VAL_GI    = 'ГИ';
  FIELDS: array[1..4] of string = (VAL_PROTS, VAL_FATS, VAL_CARBS, VAL_VALUE{, VAL_GI});
  SAVE_CAPTION: array[Boolean] of string = ('Сохранить','Создать');

implementation

uses MainUnit, UnitShadow;

{$R *.dfm}

{==============================================================================}
function TFormFood.OpenFoodEditor(var Food: TFood; New: boolean; ShowInRect: TRect): boolean;
{==============================================================================}
begin
  { установка размеров окна }
  SetupInterface;
  Left := (ShowInRect.Right+ShowInRect.Left-Width) div 2;
  Top := (ShowInRect.Bottom+ShowInRect.Top-Height) div 2;

  { корректировка положения }
  if (Left < 0) then Left := 0;
  {if Top+Height>Screen.Height then
    Top := Screen.Height-Height;  }

  { вывод }
  AFood := TFood.Create;
  if New then
    Food := TFood.Create
  else
    AFood.CopyFrom(Food);

  {if New then
    FreeFood(AFood)
  else
    AFood := Food;   }
  ModeNew := New;   
  ShowFoodInEditor;
  OK := false;
  ButtonSave.Caption := SAVE_CAPTION[New];

  { запускаем пользователя }
  Shadow(true, $FFFFFF);
  ShowModal;
  Shadow(false);

  { возвращаем результат }
  Result := OK;
  if OK then
    Food := AFood;
end;

{==============================================================================}
procedure TFormFood.Designer;
{==============================================================================}
begin
  EditName.Left := Bord;
  EditName.Top := 2*Bord;
  EditName.Width := GroupBoxName.Width-2*Bord;
  GroupBoxName.Height := 3*Bord+EditName.Height;
  ButtonSave.Left := Bord;
  ButtonSave.Top := Bord;
  ButtonCancel.Top := Bord;
  ButtonCancel.Left := Panel1.Width-ButtonCancel.Width-Bord;
  Panel1.Height := 2*Bord+ButtonSave.Height;

  ClientHeight := ClientHeight +
    (2 + Length(FIELDS))*FoodEditor.DefaultRowHeight - FoodEditor.Height;
end;

{==============================================================================}
procedure TFormFood.ShowFoodInEditor;
{==============================================================================}
begin
  EditName.Text := AFood.Name;

  //RadioButtonTable_.Checked := (not ModeNew)and(AFood.FromTable);
  //RadioButtonLabel_.Checked := (not ModeNew)and(not AFood.FromTable);

  if (ModeNew) then
    GroupBoxSource.ItemIndex := -1
  else
    GroupBoxSource.ItemIndex := Byte(AFood.FromTable);

  with FoodEditor do
  begin
    Values[VAL_PROTS] := RealToStr(AFood.RelProts);
    Values[VAL_FATS] := RealToStr(AFood.RelFats);
    Values[VAL_CARBS] := RealToStr(AFood.RelCarbs);
    Values[VAL_VALUE] := RealToStr(AFood.RelValue);
    //Values[VAL_GI] := IntToStr(AFood.GI);
  end;
end;

{==============================================================================}
procedure TFormFood.FormCreate(Sender: TObject);
{==============================================================================}
var
  i: integer;
begin
  with FoodEditor.Strings do
  begin
    Clear;
    for i := Low(FIELDS) to High(FIELDS) do
      Add(FIELDS[i]+'=0');
  end;
end;

{==============================================================================}
procedure TFormFood.ButtonSaveClick(Sender: TObject);
{==============================================================================}
var
  i: integer;
  ext: extended;
begin
  EditName.Text := Trim(EditName.Text);

  if (EditName.Text = '') then
  begin
    ErrorMessage('Введите название продукта');
    EditName.SetFocus;
    Exit;
  end;

  EditName.Text := UppercaseFirst(EditName.Text);

  if (DishBase.Find(EditName.Text{, True})>-1) then
  begin
    ErrorMessage('Блюдо с таким названием уже существует');
    EditName.SetFocus;
  end else
  if (ModeNew or (EditName.Text <> AFood.Name)) and
     (FoodBase.Find(EditName.Text{, True})>-1) then
  begin
    ErrorMessage('Продукт с таким названием уже существует');
    EditName.SetFocus;
  end else
  begin
    for i := 1 to 4 do
    if not TryStrToFloat(CheckDot(FoodEditor.Values[Fields[i]]),ext) then
    begin
      ErrorMessage('Некорректное значение');
      FoodEditor.Row := i;
      FoodEditor.SetFocus;
      Exit;
    end;

    { TryStrToFloat / TryStrToInt }
    {if not TryStrToInt(FoodEditor.Values[VAL_GI],i) then
    begin
      ErrorMessage('Некорректное значение');
      FoodEditor.Row := 5;
      FoodEditor.SetFocus;
      Exit;
    end;  }

    //if (not RadioButtonLabel.Checked)and(not RadioButtonTable.Checked)then
    if (GroupBoxSource.ItemIndex = -1) then
    begin
      ErrorMessage('Выберите источник данных');
      Exit;
    end;

    AFood.Name := EditName.Text;
    AFood.FromTable := (GroupBoxSource.ItemIndex = 1);//RadioButtonTable.Checked;

    AFood.RelProts := StrToFloat(CheckDot(FoodEditor.Values[VAL_PROTS]));
    AFood.RelFats  := StrToFloat(CheckDot(FoodEditor.Values[VAL_FATS]));
    AFood.RelCarbs := StrToFloat(CheckDot(FoodEditor.Values[VAL_CARBS]));
    AFood.RelValue := StrToFloat(CheckDot(FoodEditor.Values[VAL_VALUE]));
    //AFood.GI := StrToInt(FoodEditor.Values[VAL_GI]);

    OK := true;
    Close;
  end;
end;

{==============================================================================}
procedure TFormFood.FormShow(Sender: TObject);
{==============================================================================}
begin
  FoodEditor.Row := 1;
  EditName.SetFocus;
  EditName.SelectAll;
end;

{==============================================================================}
procedure TFormFood.ButtonCancelClick(Sender: TObject);
{==============================================================================}
begin
  OK := false;
end;

{==============================================================================}
procedure TFormFood.EditNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{==============================================================================}
begin
  if (Key=VK_RETURN)or(KEY=VK_DOWN) then
  begin
    FoodEditor.SetFocus;
    FoodEditor.Row := 1;
  end;
end;

{==============================================================================}
procedure TFormFood.FoodEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{==============================================================================}
begin
  if (Key = VK_RETURN) then
  begin
    if (FoodEditor.Row < FoodEditor.RowCount-1) then
    begin
      FoodEditor.Row := FoodEditor.Row + 1;
      FoodEditor.EditorMode := true;
    end else
    begin
      GroupBoxSource.SetFocus;
      {if RadioButtonTable.Checked then
        RadioButtonTable.SetFocus
      else
        RadioButtonLabel.SetFocus;}
    end;
  end else
  if (Key = VK_UP)and(FoodEditor.Row = 1) then
  begin
    EditName.SetFocus;
    EditName.SelectAll;
  end;
end;

{==============================================================================}
procedure TFormFood.FoodEditorKeyPress(Sender: TObject; var Key: Char);
{==============================================================================}
begin
  if Key in ['.',','] then Key := Decimal else
  if not (Key in ['0'..'9',Decimal,#8,#13]) then Key := #0 {else
    ButtonSave.Enabled := true};
end;

{==============================================================================}
procedure TFormFood.Item_SaveClick(Sender: TObject);
{==============================================================================}
begin
  ButtonSaveClick(nil);
end;

{==============================================================================}
procedure TFormFood.EditNameKeyPress(Sender: TObject; var Key: Char);
{==============================================================================}
begin
  if (Key in FOODBASE_RESERVED) then Key := #0;
end;

{==============================================================================}
procedure TFormFood.RadioButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{==============================================================================}
begin
  if Key = vk_Return then
    ButtonSaveClick(nil);
end;

end.

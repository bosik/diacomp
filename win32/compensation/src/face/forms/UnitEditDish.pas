unit UnitEditDish;

{ Модуль редактора блюд }

interface

uses
  Windows, SysUtils, Classes,
  Graphics,
  Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  Grids, ValEdit, ComCtrls, Menus,
  DiaryInterface, BusinessObjects, DiaryRoutines,
  ShellApi, ActnPopupCtrl, ACCombo,

  DiaryCore, UnitShadow, UnitDataInterface, DiaryView;
  
type
  TFormDish = class(TAutosetupForm)
    GroupBoxName: TGroupBox;
    GroupContent: TGroupBox;
    EditName: TEdit;
    Shape1: TShape;
    Panel1: TPanel;
    ButtonCancel: TBitBtn;
    ButtonSave: TBitBtn;
    PanelTable: TPanel;
    TableDishContent: TListView;
    MenuHidden: TMainMenu;
    HiddenCuts1: TMenuItem;
    Shortcut_ShowDishMass: TMenuItem;
    Item_Save: TMenuItem;
    GroupFood: TGroupBox;
    ButtonAddFood: TSpeedButton;
    Shape2: TShape;
    Shape3: TShape;
    PopupContent: TPopupActionBarEx;
    Item_ChangeMass: TMenuItem;
    Item_Remove: TMenuItem;
    EditFoodMass: TEditNumb;
    ComboFood: TACComboBox;
    GroupBoxMass: TGroupBox;
    ButtonRealMass: TSpeedButton;
    ButtonRunCalc: TSpeedButton;
    Shape4: TShape;
    CheckFixedMass_: TCheckBox;
    EditResultMass_: TEdit;
    ButtonSimpleMass: TSpeedButton;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonAddFoodClick(Sender: TObject);
    procedure ComboFoodCloseUp(Sender: TObject);
    procedure ValueEditKeyPress(Sender: TObject; var Key: Char);
    procedure EditMassKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckFixedMass_Click(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure EditResultMass_KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TableDishContentDblClick(Sender: TObject);
    procedure TableDishContentKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditNameKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure ButtonRunCalcClick(Sender: TObject);
    procedure EditResultMass_Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Shortcut_ShowDishMassClick(Sender: TObject);
    procedure Item_SaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ResetTabStop(Sender: TObject);
    procedure ButtonAddDish_Click(Sender: TObject);
    procedure Item_RemoveClick(Sender: TObject);
    procedure Item_ChangeMassClick(Sender: TObject);
    procedure TableDishContentMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditNameChange(Sender: TObject);
    procedure EditNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboFoodDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure EditResultMass_Change(Sender: TObject);
    procedure ButtonRealMassClick(Sender: TObject);
    procedure ButtonSimpleMassClick(Sender: TObject);
  protected
    procedure Designer; override;
  private
    { Private declarations }
    Modified: boolean;
    ADish: TDish;
    OK: boolean;
    ModeNew: boolean;

    { общие }
    procedure ShowDishInEditor;
    procedure ShowKoof;
    procedure ShowFood(Food: TFoodMassed; Item: TListItem);
    function ReadAttributes(): boolean;

    procedure RemoveSelected;
    procedure EditSelected;
    procedure SetRealMass;
    procedure ResetRealMass;

    { food/dish }
    procedure AddFood;
  public
    function OpenDishEditor(var Dish: TDish; New: boolean; ShowInRect: TRect): boolean;
  end;

var
  FormDish: TFormDish;
  DishMultiMap: TMultimap;
  
const
  SAVE_CAPTION: array[Boolean] of string = ('Сохранить','Создать');
  AddHint: array[Boolean] of string = ('Сначала выберите продукт и введите массу', 'Добавить указанный продукт (Enter)');
  RepHint: array[Boolean] of string = ('Сначала выберите продукт и введите массу', 'Заменить указанный продукт (Ctrl+Enter)');
  RemHint: array[Boolean] of string = ('Сначала выберите продукт', 'Удалить указанный продукт (Delete)');
  CAPTION_NO_REAL_MASS = 'Указать массу ГБ'; //'<Масса ГБ не указана>';
  CALC_PATH = 'C:\WINDOWS\system32\Calc.exe';
  
implementation

{$R *.dfm}

{ TFormEditDish }

{==============================================================================}
function TFormDish.OpenDishEditor(var Dish: TDish; New: boolean; ShowInRect: TRect): boolean;
{==============================================================================}
begin
  { установка размеров окна }
  SetupInterface;
  Left := (ShowInRect.Right+ShowInRect.Left-Width) div 2;
  Top := (ShowInRect.Bottom+ShowInRect.Top-Height) div 2;

  { корректировка положения }
  if Left+Width>Screen.Width then
    Left := Screen.Width-Width;
  if Top+Height>Screen.Height then
    Top := Screen.Height-Height;

  { вывод }
  ADish := TDish.Create;
  if New then
    Dish := TDish.Create
  else
    ADish.CopyFrom(Dish);

  ShowDishInEditor;
  OK := false;
  ModeNew := New;
  ButtonSave.Caption := SAVE_CAPTION[New];
  ButtonSave.Hint := SAVE_CAPTION[New] + ' блюдо'#13'Сочетание клавиш: Alt+S';
  ButtonCancel.Hint := 'Отменить редактирование'#13'Сочетание клавиш: Esc';
  ButtonRealMass.Hint :=
    'Укажите массу готового блюда (ГБ), если'#13+
    'она не совпадает с суммой масс его'#13+
    'компонентов (например, в ходе '#13+
    'приготовления блюдо разварилось)'#13#13+
    'Сочетание клавиш: Alt+G';
  ButtonSimpleMass.Hint :=
    'Отменить ГБ';

  Modified := False;

  { запускаем пользователя }
  Shadow(true, $FFFFFF);
  ShowModal;
  Shadow(false);

  { возвращаем результат }
  Result := OK;
  if OK then
    Dish.CopyFrom(ADish);

  ADish.Free;
end;

{==============================================================================}
procedure TFormDish.ShowDishInEditor;
{==============================================================================}
var
  i: integer;
begin
  EditName.Text := ADish.Name;

  //EditResultMass.Enabled := ADish.FixedMass;
  //EditResultMass.Text := RealToStr(ADish.RealMass);
  //CheckFixedMass.Checked := ADish.FixedMass;

  if ADish.FixedMass then
  begin
    //ButtonSimpleMass.Down := False;
    //ButtonRealMass.Down := True;
    ButtonRealMass.Flat := True;
    ButtonRealMass.Caption := 'ГБ: ' + FloatToStr(ADish.RealMass);
    ButtonSimpleMass.Show;
  end else
  begin
    //ButtonSimpleMass.Down := True;
    //ButtonRealMass.Down := False;
    ButtonRealMass.Flat := False;
    ButtonRealMass.Caption := CAPTION_NO_REAL_MASS;
    ButtonSimpleMass.Hide;
  end;

  ButtonRealMass.Down := False;

  ComboFood.Text := '';
  EditFoodMass.Text := '';
  //ComboDish.Text := '';
  //EditDishMass.Text := '';

  ShowKoof;

  TableDishContent.Items.Clear;
  for i := 0 to ADish.Count-1 do
    ShowFood(ADish.Content[i], TableDishContent.Items.Add);
end;

{==============================================================================}
procedure TFormDish.ButtonAddFoodClick(Sender: TObject);
{==============================================================================}
begin
  AddFood;
end;

{==============================================================================}
procedure TFormDish.ButtonSaveClick(Sender: TObject);
{==============================================================================}
begin
  if (ReadAttributes) then
  begin
    OK := True;
    Close;
  end;
end;

{==============================================================================}
procedure TFormDish.ValueEditKeyPress(Sender: TObject;
  var Key: Char);
{==============================================================================}
begin
  if Key in ['.',','] then Key := Decimal else
  if not (Key in ['0'..'9',Decimal,#8]) then Key := #0;
end;

{==============================================================================}
procedure TFormDish.EditMassKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  case TWinControl(Sender).Tag of
    1:  if Key = vk_Return then
          AddFood else
        if (key = vk_left)and(EditFoodMass.SelStart=0) then
        begin
          ComboFood.SetFocus;
          //key := 0;
        end;
   { 2:  if Key = vk_Return then
          AddDish else
        if (key = vk_left)and(EditDishMass.SelStart=0) then
        begin
          ComboDish.SetFocus;
          //key := 0;
        end; }
  end;
end;

{==============================================================================}
procedure TFormDish.CheckFixedMass_Click(Sender: TObject);
{==============================================================================}
begin
  {if CheckFixedMass.Checked then
  begin
    //ADish.FixedMass := true;
    EditResultMass.Enabled := true;
    if FormDish.Visible then
      EditResultMass.SetFocus;
    EditResultMass.Color := clWhite;
  end else
  begin
    //ADish.FixedMass := false;
    EditResultMass.Enabled := false;
    EditResultMass.Color := clSilver;
  end;
  Modified := true;}
end;

{==============================================================================}
procedure TFormDish.ButtonCancelClick(Sender: TObject);
{==============================================================================}
begin
  Close;
end;

{==============================================================================}
procedure TFormDish.EditResultMass_KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  //if Key = vk_Return then EditResultMass.SelectAll; // затычка :) - так мило...
end;

{==============================================================================}
procedure TFormDish.ComboKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if (Key=vk_Return)or
     ((Key=vk_right)and(TACComboBox(Sender).SelStart = Length(TACComboBox(Sender).Text)))then
  TACComboBox(Sender).OnCloseUp(Sender);
end;

{==============================================================================}
procedure TFormDish.ComboFoodCloseUp(Sender: TObject);
{==============================================================================}
{var
  n: integer; }
begin
 (* case TWinControl(Sender).Tag of
    1:  begin
          n := FoodBase.Find(Trim(TACComboBox(Sender).Text){, True});
          if n <> -1 then
          begin
            TACComboBox(Sender).Text := FoodBase[n].Name;
            FocusEdit(EditFoodMass);
          end;
        end;
    2:  begin
          n := DishBase.Find(Trim(TACComboBox(Sender).Text){, True});
          if n <> -1 then
          begin
            TACComboBox(Sender).Text := DishBase[n].Name;
            FocusEdit(EditDishMass);
          end;
        end;
  end; *)
  FocusEdit(EditFoodMass);
  // TODO: сделать как было?
end;

{==============================================================================}
procedure TFormDish.TableDishContentDblClick(Sender: TObject);
{==============================================================================}
begin
  EditSelected;
end;

{==============================================================================}
procedure TFormDish.TableDishContentKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
{==============================================================================}
begin
  if (TableDishContent.ItemIndex<>-1) then
  begin
    if (Key = vk_Delete) then RemoveSelected else
    if (Key = vk_Return) then EditSelected;
   { if Key in [45..57] then
    begin
      TableDishContentDblClick(nil);
      EditFoodMass.Text := char(Key-48+Ord('0'));
      EditFoodMass.SelStart := 1;
    end else
    if Key in [96..105] then
    begin
      TableDishContentDblClick(nil);
      EditFoodMass.Text := char(Key-96+Ord('0'));
      EditFoodMass.SelStart := 1;
    end else;  }
  end;
end;

{==============================================================================}
procedure TFormDish.EditNameKeyPress(Sender: TObject; var Key: Char);
{==============================================================================}
begin
  if (Key in DISHBASE_RESERVED) then Key := #0;
end;

{==============================================================================}
procedure TFormDish.EditNameChange(Sender: TObject);
{==============================================================================}
begin
  Modified := true;
end;

{==============================================================================}
procedure TFormDish.EditNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{==============================================================================}
begin
  if Key = vk_Return then
  begin
    if TableDishContent.Items.Count > 0 then
    begin
      ShowTableItem(TableDishContent, 0);
      TableDishContent.SetFocus;
    end else
      ComboFood.SetFocus;
  end;
end;

{==============================================================================}
procedure TFormDish.FormShow(Sender: TObject);
{==============================================================================}
begin
  //EditName.SetFocus;
  //EditName.SelectAll;
  ButtonRunCalc.Enabled := FileExists(CALC_PATH);
  ComboFood.SetFocus;

  if (ADish.Count > 0) then
  begin
    ShowTableItem(TableDishContent, 0);
    TableDishContent.SetFocus;
  end else
  begin
    TableDishContent.ItemIndex := -1;
    EditName.SetFocus;
    EditName.SelectAll;
  end;
end;

{==============================================================================}
procedure TFormDish.ButtonRunCalcClick(Sender: TObject);
{==============================================================================}
begin
  ShellExecute(Handle,'open',CALC_PATH, nil, nil, SW_SHOWNORMAL);
end;

{==============================================================================}
procedure TFormDish.EditResultMass_Click(Sender: TObject);
{==============================================================================}
begin
  //EditResultMass.SelectAll;
end;

{==============================================================================}
procedure TFormDish.FormResize(Sender: TObject);
{==============================================================================}
{var
  t: integer; }
begin
  {t := (PanelEditor.Width - 4*BORD) div 3;
  ButtonAddFood.Width := t;
  ButtonReplaceFood.Width := t;
  ButtonRemoveFood.Width := t;
  ButtonAddFood.Left := BORD;
  ButtonReplaceFood.Left := t + 2*BORD;
  ButtonRemoveFood.Left := 2*t + 3*BORD; }
end;

{==============================================================================}
procedure TFormDish.Shortcut_ShowDishMassClick(Sender: TObject);
{==============================================================================}
begin
  //CheckFixedMass.Checked := true;
  //EditResultMass.SetFocus;
  SetRealMass;
end;

{==============================================================================}
procedure TFormDish.Item_SaveClick(Sender: TObject);
{==============================================================================}
begin
  ButtonSaveClick(nil);
end;

{==============================================================================}
procedure TFormDish.AddFood;
{==============================================================================}

  procedure IncorrectItem(const Msg: string);
  begin
    ErrorMessage(Msg);
    ComboFood.SetFocus;
    ComboFood.SelStart := Length(ComboFood.Text);
    ComboFood.ShowAC;
  end;

  procedure IncorrectMass(const Msg: string);
  begin
    ErrorMessage(Msg);
    EditFoodMass.SetFocus;
    EditFoodMass.SelectAll;
  end;

var
  N: integer;
  Mass: extended;
  Temp: TFoodMassed;

  ItemType: TItemType;
begin
  { Food должен быть создан }

 (* N := FoodBase.Find(Trim(ComboFood.Text){, True});

  if (N = -1) then
  begin
    IncorrectItem('Продукт "' + Trim(ComboFood.Text) + '" не найден');
  end else
  if (N<0)or(N > FoodBase.Count-1) then
  begin
    IncorrectItem('Неверный продукт (выход за границы массива)');
  end else
  if (Trim(EditFoodMass.Text) = '') then
  begin
    IncorrectMass('Введите массу');
  end else
  if not TryToCalculate(CheckDot(EditFoodMass.Text), Mass) then
  begin
    IncorrectMass('Введите корректную массу');
  end else
  if (Mass < 0) then
  begin
    IncorrectMass('Масса должна быть неотрицательной');
  end else
  begin
    Temp := FoodBase[N].AsFoodMassed(Mass);
    ADish.Add(Temp);
    ShowFood(Temp, TableDishContent.Items.Add);
    ShowKoof;

    TableDishContent.ItemIndex := ADish.Count-1;
    ComboFood.Text := '';
    EditFoodMass.Text := '';
    ComboFood.SetFocus;
    Modified := True;
  end;  *)

  ComboFood.Text := Trim(ComboFood.Text);
  EditFoodMass.Text := Trim(EditFoodMass.Text);
  ItemType := IdentifyItem(ComboFood.Text, n);

  if (ItemType = itUnknown) then
  begin
    if (ComboFood.Text = '') then
      ErrorMessage('Введите название продукта или блюда')
    else
      ErrorMessage('Наименование "' +  ComboFood.Text + '" не найдено в базах');
    ComboFood.SetFocus;
    ComboFood.SelStart := Length(ComboFood.Text);
    ComboFood.ShowAC;
  end else
  begin
    EditFoodMass.Text := CheckDot(EditFoodMass.Text);
    if (EditFoodMass.Text = '') then
    begin
      IncorrectMass('Введите массу');
    end else
    if (not TryToCalculate(EditFoodMass.Text, Mass)) then
    begin
      IncorrectMass('Введите корректную массу');
    end else
    if (Mass < 0) then
    begin
      IncorrectMass('Масса должна быть неотрицательной');
    end else
    begin
      case ItemType of
        itFood: Temp := FoodBase[N].AsFoodMassed(Mass);
        itDish: Temp := DishBase[N].AsFoodMassed(Mass);
      end;

      ADish.Add(Temp);
      ShowFood(Temp, TableDishContent.Items.Add);
      ShowKoof;

      TableDishContent.ItemIndex := ADish.Count-1;
      ComboFood.Text := '';
      EditFoodMass.Text := '';
      ComboFood.SetFocus;
      Modified := True;
    end;
  end;
end;

{==============================================================================}
procedure TFormDish.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
{==============================================================================}
begin
  if Modified then
  case MessageDlg('Блюдо было изменено. Сохранить?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
    mrYes: begin
             OK := ReadAttributes;
             CanClose := OK;
           end;  
    mrCancel: CanClose := false;
  end;
end;

{==============================================================================}
procedure TFormDish.ShowFood(Food: TFoodMassed; Item: TListItem);
{==============================================================================}
var
  n: integer;
  s: string;
begin
  Item.Caption := Food.Name;
  Item.SubItems.Clear;
  Item.SubItems.Add(RealToStr(Food.Mass));

  // небольшой хак
  s := Food.Name;
  n := pos(' <', s);
  if (n > 0) then
    s := Copy(s, 1, n - 1);

  n := FoodBase.Find(s);
  if n <> -1 then
    Item.ImageIndex := Byte(FoodBase[n].FromTable) else
  if DishBase.Find(s) > -1 then
    Item.ImageIndex := 2
  else
  if (s <> Food.Name) then
  begin
    { если пользователь создаст продукт/блюдо с символами <> в названии }
    s := Food.Name;
    n := FoodBase.Find(s);
    if n <> -1 then
      Item.ImageIndex := Byte(FoodBase[n].FromTable) else
    if DishBase.Find(s) > -1 then
      Item.ImageIndex := 2
    else
      Item.ImageIndex := 3;
  end else
    Item.ImageIndex := 3;
end;

function TFormDish.ReadAttributes(): boolean;
begin
  Result := false;
  EditName.Text := Trim(EditName.Text);

  if EditName.Text = '' then
  begin
    ErrorMessage('Введите название блюда');
    EditName.SetFocus;
    exit;
  end;

  EditName.Text := UppercaseFirst(EditName.Text);

  if (FoodBase.Find(EditName.Text{, True})>-1) then
  begin
    ErrorMessage('Продукт с таким названием уже существует');
    EditName.SetFocus;
  end else
  if (ModeNew or (EditName.Text <> ADish.Name))and
     (DishBase.Find(EditName.Text{, True})>-1) then
  begin
    ErrorMessage('Блюдо с таким названием уже существует');
    EditName.SetFocus;
  end else

  {if (CheckFixedMass.Checked)and(Trim(EditResultMass.Text)='')then
  begin
    ErrorMessage('Введите массу готового блюда');
    EditResultMass.SetFocus;
  end else
  if (CheckFixedMass.Checked)and
     ((not TryStrToFloat(CheckDot(EditResultMass.Text),Mass))or
     (Mass<=0)) then
  begin
    ErrorMessage(
      'Неверная масса готового блюда'+#13+
      '(должна быть больше нуля)');
    EditResultMass.SetFocus;
  end else
  if (CheckFixedMass.Checked)and(ADish.SummMass = Mass)and
     (MessageDlg('Масса готового блюда совпадает с суммой масс его компонентов. Продолжить?',
      mtConfirmation, [mbYes, mbNo], 0) = mrNo )then
  begin
    EditResultMass.SetFocus;
  end else }
  begin
    ADish.Name := EditName.Text;
    {if CheckFixedMass.Checked then
      ADish.ResultMass := Mass
    else
      ADish.EraseResultMass;   }

    Result := true;
    Modified := false;
  end;
end;

{==============================================================================}
procedure TFormDish.RemoveSelected;
{==============================================================================}
var
  k: integer;
begin
  k := TableDishContent.ItemIndex;
  if (k<>-1) then
  begin
    ADish.Delete(k);
    TableDishContent.Items.Delete(k);

    if ADish.Count>0 then
    begin
      if k>ADish.Count-1 then
        k := ADish.Count-1;
      TableDishContent.ItemIndex := k;
    end;
    ShowKoof;
    Modified := true;
  end;
end;

{==============================================================================}
procedure TFormDish.ResetTabStop(Sender: TObject);
{==============================================================================}
begin
  TWinControl(Sender).TabStop := false;
end;

{==============================================================================}
procedure TFormDish.ButtonAddDish_Click(Sender: TObject);
{==============================================================================}
begin
  //AddDish;
end;

{==============================================================================}
procedure TFormDish.Item_RemoveClick(Sender: TObject);
{==============================================================================}
begin
  RemoveSelected;
end;

{==============================================================================}
procedure TFormDish.EditSelected;
{==============================================================================}
var
  k: integer;
  NewMass: real;
begin
  k := TableDishContent.ItemIndex;
  if k <> -1 then
  begin
    NewMass := ADish.Content[k].Mass;
    if DialogFoodMass(dtChangeMass, ADish.Content[k].Name, NewMass) then
    begin
      ADish.Content[k].Mass := NewMass;
      ShowFood(ADish.Content[k], TableDishContent.Items[k]);
      ShowKoof;
      Modified := True;
    end;
  end;
end;

{==============================================================================}
procedure TFormDish.Item_ChangeMassClick(Sender: TObject);
{==============================================================================}
begin
  EditSelected;
end;

{==============================================================================}
procedure TFormDish.TableDishContentMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{==============================================================================}
var
  Item : TListItem;
  A: TPoint;
begin
  if Button = mbRight then
  begin
    Item := TListView(Sender).GetItemAt(X, Y);
    if Item <> nil then
    begin
      Item.Focused := True;
      Item.Selected := True;
      GetCursorPos(A);
      PopupContent.Popup(A.X, A.Y);
    end;
  end;
end;

{==============================================================================}
procedure TFormDish.Designer;
{==============================================================================}
begin
  BorderWidth := BORD;
  PanelTable.BorderWidth := BORD;

  EditName.Left := Bord;
  EditName.Top := 3*Bord; // need it
  EditName.Width := GroupBoxName.Width-2*Bord;

  //CheckFixedMass.Left := Bord;
  //CheckFixedMass.Top := EditName.Top+EditName.Height+BORD;
  //EditResultMass.Top := CheckFixedMass.Top+CheckFixedMass.Height+Bord;
  //EditResultMass.Left := 4*Bord;
  //EditResultMass.Width := MASS_EDIT_WIDTH;
  GroupBoxName.Height := EditName.Top+EditName.Height+BORD; //EditResultMass.Top + EditResultMass.Height + BORD;
  //ButtonRunCalc.Left := GroupBoxName.Width - ButtonRunCalc.Width - BORD;
  //ButtonRunCalc.Top := EditResultMass.Top;

  GroupFood.Height := ComboFood.Height + 3*BORD;
  ButtonAddFood.Left := GroupFood.Width - ButtonAddFood.Width - BORD;
  ButtonAddFood.Top := 2*BORD;
  EditFoodMass.Left := ButtonAddFood.Left - MASS_EDIT_WIDTH - BORD;
  EditFoodMass.Top := 2*BORD;
  EditFoodMass.Width := MASS_EDIT_WIDTH;
  ComboFood.Left := BORD;
  ComboFood.Top := 2*BORD;
  ComboFood.Width := EditFoodMass.Left - ComboFood.Left - BORD;

  {GroupDish.Height := ComboDish.Height + 3*BORD;
  ButtonAddDish.Left := GroupDish.Width - ButtonAddDish.Width - BORD;
  ButtonAddDish.Top := 2*BORD;
  EditDishMass.Left := ButtonAddDish.Left - MASS_EDIT_WIDTH - BORD;
  EditDishMass.Top := 2*BORD;
  EditDishMass.Width := MASS_EDIT_WIDTH;
  ComboDish.Left := BORD;
  ComboDish.Top := 2*BORD;
  ComboDish.Width := EditDishMass.Left - ComboDish.Left - BORD; }


  {
  t := PanelEditor.Width;
  dec(t,Bord+EditFoodMass.Width);
  EditFoodMass.Left := t;
  LabelFoodMass.Left := t;
  dec(t,Bord);
  ComboFood.Left := BORD;
  ComboFood.Width := t-BORD; }

  { положение кнопок корректируется в OnResize формы }
  ButtonSave.Left := Bord;
  ButtonSave.Top := Bord;
  ButtonCancel.Top := Bord;
  ButtonCancel.Left := Panel1.Width-ButtonCancel.Width-Bord;
  Panel1.Height := 2*Bord+ButtonSave.Height;
end;

{==============================================================================}
procedure TFormDish.ComboFoodDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
{==============================================================================}
{var
  bitmap: TBitMap;}
begin
 (* with (Control as TListBox).Canvas do
  begin
    if odSelected in State then
    begin
      Brush.Color := clMenuHighlight;
      Font.Color := clHighlightText;
    end else
    begin
      Brush.Color := clWindow;
      Font.Color := clWindowText;
    end;
    Windows.FillRect(Handle, Rect, Brush.Handle);
    TextOut(17 + Rect.Left,Rect.Top, (Control as TListBox).Items[Index]);
    bitmap := TBitMap.Create;

    {case Multimap[ComboDiaryNewFood.ShowedIndex[Index]].ItemType of
      itFood: DataInterface.Images_BaseContent.GetBitmap(0, bitmap);
      itDish: DataInterface.Images_BaseContent.GetBitmap(2, bitmap);
    end;   }

    if ComboFood.Focused then
      DataInterface.Images_BaseContent.GetBitmap(0, bitmap)
    else
      DataInterface.Images_BaseContent.GetBitmap(2, bitmap);

    Draw(Rect.Left, Rect.Top, bitmap);
    bitmap.Free;
  end;   *)
  DrawACItem(
    Control,
    Rect,
    DishMultiMap[ComboFood.ShowedIndex[Index]],
    odSelected in State);
end;

{==============================================================================}
procedure TFormDish.EditResultMass_Change(Sender: TObject);
{==============================================================================}
begin
  Modified := True;
end;

{==============================================================================}
procedure TFormDish.ButtonRealMassClick(Sender: TObject);
{==============================================================================}
begin
  SetRealMass;
end;

{==============================================================================}
procedure TFormDish.ButtonSimpleMassClick(Sender: TObject);
{==============================================================================}
begin
  ResetRealMass;
end;

{==============================================================================}
procedure TFormDish.ShowKoof;
{==============================================================================}
var
  Summ: Real;
  i,Count: integer;
begin
  Summ := ADish.SummMass;
  if (Summ > 0) and (ADish.FixedMass) then
    GroupBoxMass.Caption := 'Масса' + ' (' + IntToStr(Round(ADish.RealMass / Summ * 100)) + '%)'
  else
    GroupBoxMass.Caption := 'Масса';

  Count := 0;
  for i := 0 to ADish.Count-1 do
    if ADish.Content[i].Mass > 0 then
      inc(Count);    
  GroupContent.Caption := 'Состав (' + IntToStr(Count) + ')';
end;

{==============================================================================}
procedure TFormDish.SetRealMass;
{==============================================================================}

  procedure RollBack;
  begin

  end;

var
  RealMass: Real;
begin
  ButtonRealMass.Down := False;//ADish.FixedMass;

  RealMass := ADish.RealMass;
  if DialogFoodMass(dtChangeMass, 'Масса готового блюда', RealMass) then
  begin
    if (RealMass <=0) then
    begin
      ErrorMessage(
        'Неверная масса готового блюда'+#13+
        '(должна быть больше нуля)');
      RollBack;
    end else

    if (ADish.SummMass = RealMass) and
      (MessageDlg('Масса готового блюда совпадает с суммой масс его компонентов. Продолжить?',
      mtConfirmation, [mbYes, mbNo], 0) = mrNo )then
      RollBack
    else      

    begin    
      ADish.ResultMass := RealMass;
      ButtonSimpleMass.Show;
      //ButtonRealMass.Down := True;
      ShowKoof;
      Modified := True;
      ButtonRealMass.Caption := 'ГБ: ' + FloatToStr(RealMass);
    end;
  end else
    RollBack;

   //ButtonRealMass.Down := False;//ADish.FixedMass;
   ButtonRealMass.Flat := ADish.FixedMass;
end;

{==============================================================================}
procedure TFormDish.ResetRealMass;
{==============================================================================}
begin
  if ADish.FixedMass then
  begin
    ADish.EraseResultMass;
    ButtonRealMass.Caption := CAPTION_NO_REAL_MASS;
    ButtonRealMass.Down := False;
    ButtonRealMass.Flat := False;
    ButtonSimpleMass.Hide;
    ShowKoof;
    Modified := True;
  end;
end;

end.

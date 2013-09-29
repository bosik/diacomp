unit UnitDMealBalancer;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Buttons,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus;

type
  TDMealBalancer = class(TForm)
    GroupBoxList: TGroupBox;
    ListMeal: TListView;
    PanelBottom: TPanel;
    ButtonSave: TBitBtn;
    ButtonCancel: TBitBtn;
    Shape4: TShape;
    PanelTools: TPanel;
    ButtonSelectAll: TSpeedButton;
    ButtonDiselectAll: TSpeedButton;
    ButtonCalc: TSpeedButton;
    Shape5: TShape;
    PopupColSettings: TPopupMenu;
    N0: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    ButtonRemove: TSpeedButton;
    GroupBoxInfo: TGroupBox;
    PanelInfo: TPanel;
    LabelProts: TLabel;
    LabelFats: TLabel;
    LabelCarbs: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonChangeSelectClick(Sender: TObject);
    procedure ListMealColumnRightClick(Sender: TObject;
      Column: TListColumn; Point: TPoint);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
  
  end;

var
  DMealBalancer: TDMealBalancer;
  //TempList: TDishList;

implementation

uses MainUnit, DiaryInterface;

{$R *.dfm}

{================================================================}
procedure TDMealBalancer.ButtonCancelClick(Sender: TObject);
{================================================================}
begin
  Close;
end;

{================================================================}
procedure TDMealBalancer.ButtonChangeSelectClick(Sender: TObject);
{================================================================}
var
  i: integer;
begin
  for i:=0 to ListMeal.Items.Count-1 do
    ListMeal.Items[i].Checked:=(TSpeedButton(Sender).Tag=1);
end;

{================================================================}
procedure TDMealBalancer.ListMealColumnRightClick(Sender: TObject;
  Column: TListColumn; Point: TPoint);
{================================================================}
begin
  GetCursorPos(Point);
  PopupColSettings.Popup(
    Point.X,
    Point.Y);
end;

procedure TDMealBalancer.FormShow(Sender: TObject);
begin
  PlaceCenter(DMealBalancer);
end;

end.

unit UnitDataInterface;

interface

uses
  SysUtils, Classes, ExtCtrls, ImgList, Controls, Menus,
  ActnPopupCtrl,  ThreadExecutor, XPMan, DiaryRecords;

type
  TDataInterface = class(TDataModule)
    Image_MinMax: TImageList;
    Images_Main_Tabs: TImageList;
    Images_BaseContent: TImageList;
    Images_Menu: TImageList;
    PopupTray: TPopupActionBarEx;
    ItemShowWindow: TMenuItem;
    ItemHideWindow: TMenuItem;
    Item_Sep1: TMenuItem;
    ItemMinToTray: TMenuItem;
    ItemTrayClose: TMenuItem;
    PopupConfigNorms: TPopupActionBarEx;
    ItemBalanceOnOff: TMenuItem;
    N3: TMenuItem;
    PopupDiaryBlood: TPopupActionBarEx;
    ItemEditBlood: TMenuItem;
    ItemSep_1: TMenuItem;
    ItemRemoveBlood: TMenuItem;
    PopupDiaryIns: TPopupActionBarEx;
    ItemEditIns: TMenuItem;
    ItemSep_2: TMenuItem;
    ItemRemoveIns: TMenuItem;
    PopupDiaryMeal: TPopupActionBarEx;
    ItemEditMeal: TMenuItem;
    Item_ShortMeal: TMenuItem;
    ItemBestMasses: TMenuItem;
    ItemSep_3: TMenuItem;
    ItemRemoveMeal: TMenuItem;
    PopupDiaryNote: TPopupActionBarEx;
    ItemEditNote: TMenuItem;
    ItemSep_4: TMenuItem;
    ItemRemoveNote: TMenuItem;
    PopupDiaryDish: TPopupActionBarEx;
    ItemEditFood: TMenuItem;
    ItemCalcMass: TMenuItem;
    ItemDelta: TMenuItem;
    N12: TMenuItem;
    ItemRemoveFood: TMenuItem;
    PopupDiary: TPopupActionBarEx;
    ItemPopupAddBlood: TMenuItem;
    ItemPopupAddIns: TMenuItem;
    ItemPopupAddMeal: TMenuItem;
    ItemPopupAddNote: TMenuItem;
    ItemSep1: TMenuItem;
    Item_SettingsImage: TMenuItem;
    XPManifest1: TXPManifest;
    procedure PopupDiaryMealPopup(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataInterface: TDataInterface;

implementation

uses MainUnit;

{$R *.dfm}

{==============================================================================}
procedure TDataInterface.PopupDiaryMealPopup(Sender: TObject);
{==============================================================================}
begin
  if (Form1.DiaryView.SelectedRecord is TMealRecord) then
    Form1.ActionShortMeal.Checked := TMealRecord(Form1.DiaryView.SelectedRecord).ShortMeal;
end;

end.

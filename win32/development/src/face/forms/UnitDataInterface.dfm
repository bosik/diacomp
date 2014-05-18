object DataInterface: TDataInterface
  OldCreateOrder = False
  Left = 534
  Top = 101
  Height = 624
  Width = 551
  object Image_MinMax: TImageList
    BlendColor = clFuchsia
    BkColor = clWhite
    AllocBy = 2
    DrawingStyle = dsTransparent
    Width = 24
    Left = 69
    Top = 19
  end
  object Images_Main_Tabs: TImageList
    BlendColor = clFuchsia
    BkColor = 14215660
    AllocBy = 1
    Left = 68
    Top = 75
  end
  object Images_BaseContent: TImageList
    BlendColor = clFuchsia
    BkColor = clWhite
    AllocBy = 1
    DrawingStyle = dsTransparent
    Left = 64
    Top = 128
  end
  object Images_Menu: TImageList
    BlendColor = clFuchsia
    BkColor = clWhite
    AllocBy = 1
    Left = 63
    Top = 179
  end
  object PopupTray: TPopupActionBarEx
    ActionManager = Form1.ActionManager
    Left = 210
    Top = 307
    object ItemShowWindow: TMenuItem
      Action = Form1.ActionShowWindow
      Default = True
    end
    object ItemHideWindow: TMenuItem
      Action = Form1.ActionHideWindow
    end
    object Item_Sep1: TMenuItem
      Caption = '-'
    end
    object ItemMinToTray: TMenuItem
      Action = Form1.ActionIsMinToTray
    end
    object ItemTrayClose: TMenuItem
      Action = Form1.ActionExit
    end
  end
  object PopupConfigNorms: TPopupActionBarEx
    Left = 210
    Top = 363
    object ItemBalanceOnOff: TMenuItem
      Action = Form1.ActionBalanceOnOff
    end
    object N3: TMenuItem
      Action = Form1.ActionSettingsStat
    end
  end
  object PopupDiaryBlood: TPopupActionBarEx
    ActionManager = Form1.ActionManager
    Left = 210
    Top = 19
    object ItemEditBlood: TMenuItem
      Action = Form1.ActionEditBlood
    end
    object ItemSep_1: TMenuItem
      Caption = '-'
    end
    object ItemRemoveBlood: TMenuItem
      Action = Form1.ActionRemovePanel
    end
  end
  object PopupDiaryIns: TPopupActionBarEx
    ActionManager = Form1.ActionManager
    Left = 210
    Top = 67
    object ItemEditIns: TMenuItem
      Action = Form1.ActionEditIns
    end
    object ItemSep_2: TMenuItem
      Caption = '-'
    end
    object ItemRemoveIns: TMenuItem
      Action = Form1.ActionRemovePanel
    end
  end
  object PopupDiaryMeal: TPopupActionBarEx
    OnPopup = PopupDiaryMealPopup
    ActionManager = Form1.ActionManager
    Left = 210
    Top = 120
    object ItemEditMeal: TMenuItem
      Action = Form1.ActionEditMeal
    end
    object Item_ShortMeal: TMenuItem
      Action = Form1.ActionShortMeal
      AutoCheck = True
    end
    object ItemBestMasses: TMenuItem
      Action = Form1.ActionBalanceMeal
    end
    object ItemSep_3: TMenuItem
      Caption = '-'
    end
    object ItemRemoveMeal: TMenuItem
      Action = Form1.ActionRemovePanel
    end
  end
  object PopupDiaryNote: TPopupActionBarEx
    ActionManager = Form1.ActionManager
    Left = 210
    Top = 171
    object ItemEditNote: TMenuItem
      Action = Form1.ActionEditNote
    end
    object ItemSep_4: TMenuItem
      Caption = '-'
    end
    object ItemRemoveNote: TMenuItem
      Action = Form1.ActionRemovePanel
    end
  end
  object PopupDiaryDish: TPopupActionBarEx
    ActionManager = Form1.ActionManager
    Left = 210
    Top = 227
    object ItemEditFood: TMenuItem
      Action = Form1.ActionEditFood
    end
    object ItemCalcMass: TMenuItem
      Action = Form1.ActionCalcMass
    end
    object ItemDelta: TMenuItem
      Action = Form1.ActionDeltaMass
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object ItemRemoveFood: TMenuItem
      Action = Form1.ActionRemoveFood
    end
  end
  object PopupDiary: TPopupActionBarEx
    ActionManager = Form1.ActionManager
    Left = 210
    Top = 435
    object ItemPopupAddBlood: TMenuItem
      Action = Form1.ActionAddBlood
    end
    object ItemPopupAddIns: TMenuItem
      Action = Form1.ActionAddIns
    end
    object ItemPopupAddMeal: TMenuItem
      Action = Form1.ActionAddMeal
    end
    object ItemPopupAddNote: TMenuItem
      Action = Form1.ActionAddNote
    end
    object ItemSep1: TMenuItem
      Caption = '-'
    end
    object Item_SettingsImage: TMenuItem
      Action = Form1.ActionSettingsImage
    end
  end
  object XPManifest1: TXPManifest
    Left = 328
    Top = 24
  end
end

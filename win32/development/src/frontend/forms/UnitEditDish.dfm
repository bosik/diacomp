object FormDish: TFormDish
  Left = 401
  Top = 158
  Width = 662
  Height = 695
  BorderIcons = [biSystemMenu]
  BorderWidth = 8
  Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1073#1083#1102#1076
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0555550000000000055555500000000055555550000000005555555500000005
    50000005000000000FFFFFF00000000FFFFFFFFFF00000000FFFFFF000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000F83F0000F01F0000F00F0000E00F0000E0070000C007
    0000C0030000C0030000E0070000F81F0000FFFF0000FFFF0000FFFF0000}
  Menu = MenuHidden
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Shape1: TShape
    Left = 0
    Top = 65
    Width = 638
    Height = 8
    Align = alTop
    Brush.Style = bsClear
    Pen.Style = psClear
  end
  object Shape4: TShape
    Left = 0
    Top = 137
    Width = 638
    Height = 8
    Align = alTop
    Brush.Style = bsClear
    Pen.Style = psClear
  end
  object Panel1: TPanel
    Left = 0
    Top = 600
    Width = 638
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      638
      47)
    object ButtonCancel: TBitBtn
      Left = 493
      Top = 12
      Width = 138
      Height = 34
      Hint = 'Esc'
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TabStop = False
      OnClick = ButtonCancelClick
      OnEnter = ResetTabStop
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333FFFFF333333000033333388888833333333333F888888FFF333
        000033338811111188333333338833FFF388FF33000033381119999111833333
        38F338888F338FF30000339119933331111833338F388333383338F300003391
        13333381111833338F8F3333833F38F3000039118333381119118338F38F3338
        33F8F38F000039183333811193918338F8F333833F838F8F0000391833381119
        33918338F8F33833F8338F8F000039183381119333918338F8F3833F83338F8F
        000039183811193333918338F8F833F83333838F000039118111933339118338
        F3833F83333833830000339111193333391833338F33F8333FF838F300003391
        11833338111833338F338FFFF883F83300003339111888811183333338FF3888
        83FF83330000333399111111993333333388FFFFFF8833330000333333999999
        3333333333338888883333330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object ButtonSave: TBitBtn
      Left = 8
      Top = 12
      Width = 138
      Height = 34
      Hint = 'Alt+S'
      Anchors = [akLeft, akBottom]
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      TabStop = False
      OnClick = ButtonSaveClick
      OnEnter = ResetTabStop
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
  end
  object GroupContent: TGroupBox
    Left = 0
    Top = 145
    Width = 638
    Height = 455
    Align = alClient
    Caption = #1057#1086#1089#1090#1072#1074
    TabOrder = 0
    object PanelTable: TPanel
      Left = 2
      Top = 18
      Width = 634
      Height = 435
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 8
      TabOrder = 0
      object Shape2: TShape
        Left = 8
        Top = 357
        Width = 618
        Height = 8
        Align = alBottom
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object Shape3: TShape
        Left = 8
        Top = 419
        Width = 618
        Height = 8
        Align = alBottom
        Brush.Style = bsClear
        Pen.Style = psClear
        Visible = False
      end
      object TableDishContent: TListView
        Left = 8
        Top = 8
        Width = 618
        Height = 349
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = #1050#1086#1084#1087#1086#1085#1077#1085#1090
            MinWidth = 70
          end
          item
            Caption = #1041
            MaxWidth = 60
            MinWidth = 40
            Width = 40
          end
          item
            Caption = #1052#1072#1089#1089#1072
            MaxWidth = 60
            MinWidth = 40
            Width = 60
          end>
        FullDrag = True
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SmallImages = DataInterface.Images_BaseContent
        TabOrder = 0
        TabStop = False
        ViewStyle = vsReport
        OnDblClick = TableDishContentDblClick
        OnEnter = ResetTabStop
        OnKeyDown = TableDishContentKeyDown
        OnMouseDown = TableDishContentMouseDown
      end
      object GroupFood: TGroupBox
        Left = 8
        Top = 365
        Width = 618
        Height = 54
        Align = alBottom
        Caption = #1055#1088#1086#1076#1091#1082#1090#1099' '#1080' '#1073#1083#1102#1076#1072
        TabOrder = 1
        DesignSize = (
          618
          54)
        object ButtonAddFood: TSpeedButton
          Left = 561
          Top = 33
          Width = 25
          Height = 25
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' (Enter)'
          Anchors = [akTop, akRight]
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          Spacing = 2
          OnClick = ButtonAddFoodClick
        end
        object EditFoodMass: TEditNumb
          Tag = 1
          Left = 451
          Top = 16
          Width = 57
          Height = 24
          Hint = #1052#1072#1089#1089#1072' ('#1075')'
          Anchors = [akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'EditFoodMass'
          OnEnter = ResetTabStop
          OnKeyDown = EditMassKeyDown
          Decimal = ','
          WarningShow = True
        end
        object ComboFood: TACComboBox
          Tag = 1
          Left = 49
          Top = 26
          Width = 363
          Height = 24
          UserHint = #1053#1072#1095#1085#1080#1090#1077' '#1074#1074#1086#1076#1080#1090#1100' '#1085#1072#1079#1074#1072#1085#1080#1077
          ShowUserHint = True
          Style = csSimple
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 14
          ItemHeight = 16
          TabOrder = 0
          OnCloseUp = ComboFoodCloseUp
          OnDrawItem = ComboFoodDrawItem
          OnKeyDown = ComboKeyDown
        end
      end
    end
  end
  object GroupBoxName: TGroupBox
    Left = 0
    Top = 0
    Width = 638
    Height = 65
    Align = alTop
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077
    TabOrder = 1
    DesignSize = (
      638
      65)
    object EditName: TEdit
      Left = 16
      Top = 24
      Width = 605
      Height = 24
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'EditName'
      OnChange = EditNameChange
      OnEnter = ResetTabStop
      OnKeyDown = EditNameKeyDown
      OnKeyPress = EditNameKeyPress
    end
    object CheckFixedMass_: TCheckBox
      Left = 19
      Top = 65
      Width = 603
      Height = 16
      Cursor = crHelp
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      Caption = #1059#1082#1072#1079#1072#1090#1100' '#1084#1072#1089#1089#1091' '#1075#1086#1090#1086#1074#1086#1075#1086' '#1073#1083#1102#1076#1072
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = CheckFixedMass_Click
      OnEnter = ResetTabStop
    end
    object EditResultMass_: TEdit
      Left = 81
      Top = 73
      Width = 56
      Height = 24
      TabStop = False
      TabOrder = 2
      Text = 'EditResultMass_'
      OnChange = EditResultMass_Change
      OnClick = EditResultMass_Click
      OnEnter = ResetTabStop
      OnKeyDown = EditResultMass_KeyDown
      OnKeyPress = ValueEditKeyPress
    end
  end
  object GroupBoxMass: TGroupBox
    Left = 0
    Top = 73
    Width = 638
    Height = 64
    Align = alTop
    Caption = #1052#1072#1089#1089#1072
    TabOrder = 3
    DesignSize = (
      638
      64)
    object ButtonRealMass: TSpeedButton
      Left = 16
      Top = 24
      Width = 579
      Height = 25
      AllowAllUp = True
      Anchors = [akLeft, akTop, akRight]
      GroupIndex = 1
      Caption = #1043#1041':   512'
      ParentShowHint = False
      ShowHint = True
      OnClick = ButtonRealMassClick
    end
    object ButtonRunCalc: TSpeedButton
      Left = 212
      Top = 32
      Width = 25
      Height = 25
      Hint = #1050#1072#1083#1100#1082#1091#1083#1103#1090#1086#1088
      Anchors = [akTop, akRight]
      Flat = True
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      Visible = False
      OnClick = ButtonRunCalcClick
    end
    object ButtonSimpleMass: TSpeedButton
      Left = 601
      Top = 24
      Width = 26
      Height = 25
      AllowAllUp = True
      Anchors = [akTop, akRight]
      Flat = True
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = ButtonSimpleMassClick
    end
  end
  object MenuHidden: TMainMenu
    Left = 26
    Top = 203
    object HiddenCuts1: TMenuItem
      Caption = 'HiddenCuts'
      Visible = False
      object Shortcut_ShowDishMass: TMenuItem
        Caption = 'ShowDishMass'
        ShortCut = 32839
        OnClick = Shortcut_ShowDishMassClick
      end
      object Item_Save: TMenuItem
        Caption = 'Save'
        ShortCut = 32851
        OnClick = Item_SaveClick
      end
    end
  end
  object PopupContent: TPopupActionBarEx
    Images = DataInterface.Images_Menu
    Left = 58
    Top = 203
    object Item_ChangeMass: TMenuItem
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1084#1072#1089#1089#1091'...'
      ImageIndex = 13
      OnClick = Item_ChangeMassClick
    end
    object Item_Remove: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100
      ImageIndex = 14
      OnClick = Item_RemoveClick
    end
  end
end

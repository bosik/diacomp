object FormFood: TFormFood
  Left = 412
  Top = 289
  Width = 303
  Height = 627
  BorderIcons = [biSystemMenu]
  BorderWidth = 8
  Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1087#1088#1086#1076#1091#1082#1090#1086#1074
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
    000000000000000000000000000000000000000000000022222220000000002A
    AAAA22000000002AAAAA2A200000002AAAAA2AA20000002AAAAA2AA20000002A
    AAAA2AA20000002222222AA200000002AAAAA2A2000000002AAAAA2200000000
    022222220000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000C07F0000C03F0000C01F0000C00F0000C00F0000C00F
    0000C00F0000E00F0000F00F0000F80F0000FFFF0000FFFF0000FFFF0000}
  Menu = MenuHidden
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Shape1: TShape
    Left = 0
    Top = 57
    Width = 279
    Height = 8
    Align = alTop
    Brush.Style = bsClear
    Pen.Style = psClear
  end
  object Shape2: TShape
    Left = 0
    Top = 450
    Width = 279
    Height = 9
    Align = alBottom
    Brush.Style = bsClear
    Pen.Style = psClear
  end
  object GroupBoxName: TGroupBox
    Left = 0
    Top = 0
    Width = 279
    Height = 57
    Align = alTop
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077
    TabOrder = 0
    DesignSize = (
      279
      57)
    object EditName: TEdit
      Left = 9
      Top = 25
      Width = 246
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'EditName'
      OnKeyDown = EditNameKeyDown
      OnKeyPress = EditNameKeyPress
    end
  end
  object GroupBoxContent: TGroupBox
    Left = 0
    Top = 65
    Width = 279
    Height = 385
    Align = alClient
    Caption = #1057#1086#1089#1090#1072#1074' ('#1085#1072' 100'#1075')'
    TabOrder = 1
    object Panel2: TPanel
      Left = 2
      Top = 18
      Width = 275
      Height = 365
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 8
      TabOrder = 0
      object FoodEditor: TValueListEditor
        Left = 8
        Top = 8
        Width = 259
        Height = 349
        Align = alClient
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
        TabOrder = 0
        TitleCaptions.Strings = (
          #1055#1072#1088#1072#1084#1077#1090#1088
          #1047#1085#1072#1095#1077#1085#1080#1077)
        OnKeyDown = FoodEditorKeyDown
        OnKeyPress = FoodEditorKeyPress
        ColWidths = (
          150
          103)
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 532
    Width = 279
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      279
      47)
    object ButtonCancel: TBitBtn
      Left = 153
      Top = 12
      Width = 120
      Height = 34
      Hint = 'Esc'
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&'#1054#1090#1084#1077#1085#1072
      ModalResult = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = ButtonCancelClick
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
      Width = 121
      Height = 34
      Anchors = [akLeft, akBottom]
      Caption = '&'#1057#1086#1093#1088#1072#1085#1080#1090#1100
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      OnClick = ButtonSaveClick
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
  object GroupBoxSource: TRadioGroup
    Left = 0
    Top = 459
    Width = 279
    Height = 73
    Align = alBottom
    Caption = #1048#1089#1090#1086#1095#1085#1080#1082' '#1076#1072#1085#1085#1099#1093
    Items.Strings = (
      #1069#1090#1080#1082#1077#1090#1082#1072
      #1058#1072#1073#1083#1080#1094#1072)
    TabOrder = 3
  end
  object MenuHidden: TMainMenu
    Left = 26
    Top = 179
    object HiddenCuts1: TMenuItem
      Caption = 'HiddenCuts'
      Visible = False
      object Item_Save: TMenuItem
        Caption = 'Save'
        ShortCut = 32851
        OnClick = Item_SaveClick
      end
    end
  end
end

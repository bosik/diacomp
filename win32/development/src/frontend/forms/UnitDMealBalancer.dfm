object DMealBalancer: TDMealBalancer
  Left = 170
  Top = 237
  Width = 474
  Height = 465
  BorderIcons = [biSystemMenu, biHelp]
  BorderWidth = 5
  Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1087#1088#1080#1077#1084#1072' '#1087#1080#1097#1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBoxList: TGroupBox
    Left = 0
    Top = 89
    Width = 456
    Height = 285
    Align = alClient
    Caption = #1052#1077#1085#1102
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 0
    object Shape4: TShape
      Left = 2
      Top = 240
      Width = 452
      Height = 5
      Align = alBottom
      Brush.Style = bsClear
      Pen.Style = psClear
    end
    object Shape5: TShape
      Left = 2
      Top = 278
      Width = 452
      Height = 5
      Align = alBottom
      Brush.Style = bsClear
      Pen.Style = psClear
    end
    object ListMeal: TListView
      Left = 2
      Top = 18
      Width = 452
      Height = 222
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
          MinWidth = 120
        end
        item
          Caption = #1052#1072#1089#1089#1072
          MinWidth = 60
          Width = 60
        end
        item
          Caption = #1041
          MinWidth = 40
          Width = 40
        end
        item
          Caption = #1046
          MinWidth = 40
          Width = 40
        end
        item
          Caption = #1059
          MinWidth = 40
          Width = 40
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      GridLines = True
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      TabOrder = 0
      TabStop = False
      ViewStyle = vsReport
      OnColumnRightClick = ListMealColumnRightClick
    end
    object PanelTools: TPanel
      Left = 2
      Top = 245
      Width = 452
      Height = 33
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 1
      object ButtonSelectAll: TSpeedButton
        Tag = 1
        Left = 8
        Top = 0
        Width = 33
        Height = 33
        Hint = #1054#1090#1084#1077#1090#1080#1090#1100' '#1074#1089#1077
        Flat = True
        ParentShowHint = False
        ShowHint = True
        OnClick = ButtonChangeSelectClick
      end
      object ButtonDiselectAll: TSpeedButton
        Left = 48
        Top = 0
        Width = 33
        Height = 33
        Hint = #1059#1073#1088#1072#1090#1100' '#1086#1090#1084#1077#1090#1082#1080
        Flat = True
        ParentShowHint = False
        ShowHint = True
        OnClick = ButtonChangeSelectClick
      end
      object ButtonCalc: TSpeedButton
        Left = 88
        Top = 0
        Width = 33
        Height = 33
        Hint = #1055#1086#1076#1086#1073#1088#1072#1090#1100' '#1084#1072#1089#1089#1099
        Flat = True
        ParentShowHint = False
        ShowHint = True
      end
      object ButtonRemove: TSpeedButton
        Left = 408
        Top = 0
        Width = 33
        Height = 33
        Hint = #1059#1076#1072#1083#1080#1090#1100' (Delete)'
        Enabled = False
        Flat = True
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
      end
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 374
    Width = 456
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      456
      41)
    object ButtonSave: TBitBtn
      Left = 8
      Top = 8
      Width = 120
      Height = 33
      Caption = '&'#1057#1086#1093#1088#1072#1085#1080#1090#1100
      ModalResult = 1
      TabOrder = 0
      TabStop = False
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
    object ButtonCancel: TBitBtn
      Left = 325
      Top = 8
      Width = 120
      Height = 33
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&'#1054#1090#1084#1077#1085#1072
      ModalResult = 7
      TabOrder = 1
      TabStop = False
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
  end
  object GroupBoxInfo: TGroupBox
    Left = 0
    Top = 0
    Width = 456
    Height = 89
    Align = alTop
    Caption = #1047#1072' '#1074#1077#1089#1100' '#1087#1088#1080#1077#1084' '#1087#1080#1097#1080
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentBackground = False
    ParentColor = False
    ParentFont = False
    TabOrder = 2
    object PanelInfo: TPanel
      Left = 2
      Top = 18
      Width = 452
      Height = 69
      Align = alClient
      BevelOuter = bvLowered
      Color = clInfoBk
      ParentBackground = False
      TabOrder = 0
      object LabelProts: TLabel
        Left = 16
        Top = 8
        Width = 40
        Height = 16
        Caption = #1041#1077#1083#1082#1080
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelFats: TLabel
        Left = 16
        Top = 24
        Width = 38
        Height = 16
        Caption = #1046#1080#1088#1099
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelCarbs: TLabel
        Left = 16
        Top = 40
        Width = 26
        Height = 16
        Caption = #1059#1075#1083'.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
    end
  end
  object PopupColSettings: TPopupMenu
    Left = 8
    Top = 134
    object N0: TMenuItem
      Caption = #1041#1077#1083#1082#1080
      Checked = True
    end
    object N1: TMenuItem
      Caption = #1046#1080#1088#1099
      Checked = True
    end
    object N2: TMenuItem
      Caption = #1059#1075#1083#1077#1074#1086#1076#1099
      Checked = True
    end
    object N3: TMenuItem
      Caption = #1050#1072#1083#1086#1088#1080#1081#1085#1086#1089#1090#1100
      Checked = True
    end
    object N4: TMenuItem
      Caption = #1042#1083#1080#1103#1085#1080#1077' '#1085#1072' '#1057#1050
    end
  end
end

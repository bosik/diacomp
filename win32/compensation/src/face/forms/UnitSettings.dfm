object FormSettings: TFormSettings
  Left = 292
  Top = 204
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  BorderWidth = 8
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 593
  ClientWidth = 472
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
    00000000000000000077700000000007707F70770000007F77FFF77F70000077
    FFFFFFF770000007FF777FF70000077FF7FFF7FF770007FFF7FFF7FFF700077F
    F7FFF7FF77000007FF777FF700000077FFFFFFF77000007F77FFF77F70000007
    707F70770000000000777000000000000000000000000000000000000000FFFF
    0000FC7F0000E44F0000C0070000C0070000E00F000080030000800300008003
    0000E00F0000C0070000C0070000E44F0000FC7F0000FFFF0000FFFF0000}
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object PageSettings: TPageControl
    Left = 107
    Top = 0
    Width = 365
    Height = 546
    ActivePage = TabAccount
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    TabWidth = 70
    object TabPrivate: TTabSheet
      BorderWidth = 8
      Caption = 'TabPrivate'
      object Shape1: TShape
        Left = 0
        Top = 265
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object Shape11: TShape
        Left = 0
        Top = 161
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object GroupBS: TGroupBox
        Left = 0
        Top = 0
        Width = 341
        Height = 161
        Align = alTop
        Caption = #1057#1072#1093#1072#1088' '#1082#1088#1086#1074#1080' ('#1084#1084#1086#1083#1100'/'#1083')'
        TabOrder = 0
        DesignSize = (
          341
          161)
        object Label3: TLabel
          Left = 16
          Top = 24
          Width = 77
          Height = 16
          Caption = #1062#1077#1083#1077#1074#1086#1081' '#1057#1050
        end
        object Label6: TLabel
          Left = 16
          Top = 56
          Width = 159
          Height = 16
          Caption = #1052#1080#1085#1080#1084#1072#1083#1100#1085#1099#1081' '#1057#1050' '#1076#1086' '#1077#1076#1099
        end
        object Label7: TLabel
          Left = 16
          Top = 88
          Width = 165
          Height = 16
          Caption = #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1099#1081' '#1057#1050' '#1076#1086' '#1077#1076#1099
        end
        object Label8: TLabel
          Left = 16
          Top = 120
          Width = 188
          Height = 16
          Caption = #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1099#1081' '#1057#1050' '#1087#1086#1089#1083#1077' '#1077#1076#1099
        end
        object EditTargetBS: TEditNumb
          Left = 232
          Top = 24
          Width = 40
          Height = 24
          Anchors = [akTop, akRight]
          TabOrder = 0
          Text = 'EditTargetBS'
          AcceptNegative = False
          Decimal = ','
          WarningShow = True
        end
        object EditBS1: TEditNumb
          Left = 240
          Top = 56
          Width = 40
          Height = 24
          Anchors = [akTop, akRight]
          TabOrder = 1
          Text = 'EditNumb1'
          AcceptNegative = False
          Decimal = ','
          WarningShow = True
        end
        object EditBS2: TEditNumb
          Left = 232
          Top = 88
          Width = 40
          Height = 24
          Anchors = [akTop, akRight]
          TabOrder = 2
          Text = 'EditNumb1'
          AcceptNegative = False
          Decimal = ','
          WarningShow = True
        end
        object EditBS3: TEditNumb
          Left = 232
          Top = 120
          Width = 40
          Height = 24
          Anchors = [akTop, akRight]
          TabOrder = 3
          Text = 'EditNumb1'
          AcceptNegative = False
          Decimal = ','
          WarningShow = True
        end
      end
      object GroupCalc: TGroupBox
        Left = 0
        Top = 273
        Width = 341
        Height = 64
        Align = alTop
        Caption = #1055#1086#1076#1073#1086#1088' '#1091#1075#1083#1077#1074#1086#1076#1086#1074
        TabOrder = 1
        DesignSize = (
          341
          64)
        object Label5: TLabel
          Left = 16
          Top = 24
          Width = 116
          Height = 16
          Caption = #1053#1072#1080#1073#1086#1083#1100#1096#1072#1103' '#1076#1086#1079#1072
        end
        object SpinSettingsMaxDose: TSpinEdit
          Left = 270
          Top = 24
          Width = 60
          Height = 26
          Anchors = [akTop, akRight]
          MaxValue = 100
          MinValue = 2
          TabOrder = 0
          Value = 100
        end
      end
      object GroupPostPeriods: TGroupBox
        Left = 0
        Top = 169
        Width = 341
        Height = 96
        Align = alTop
        Caption = #1055#1077#1088#1080#1086#1076#1099' ('#1084#1080#1085')'
        TabOrder = 2
        DesignSize = (
          341
          96)
        object LabelPPTime: TLabel
          Left = 16
          Top = 24
          Width = 185
          Height = 16
          Caption = #1057#1090#1072#1085#1076#1072#1088#1090#1085#1099#1081' '#1087#1086#1089#1090#1087#1088#1072#1085#1076#1080#1072#1083
        end
        object LabelSPTime: TLabel
          Left = 16
          Top = 56
          Width = 187
          Height = 16
          Caption = #1057#1086#1082#1088#1072#1097#1105#1085#1085#1099#1081' '#1087#1086#1089#1090#1087#1088#1072#1085#1076#1080#1072#1083
        end
        object EditPPTime: TSpinEdit
          Left = 270
          Top = 24
          Width = 60
          Height = 26
          Anchors = [akTop, akRight]
          MaxValue = 300
          MinValue = 60
          TabOrder = 0
          Value = 210
        end
        object EditSPTime: TSpinEdit
          Left = 270
          Top = 54
          Width = 60
          Height = 26
          Anchors = [akTop, akRight]
          MaxValue = 120
          MinValue = 0
          TabOrder = 1
          Value = 120
        end
      end
    end
    object TabAnalyze: TTabSheet
      BorderWidth = 8
      Caption = 'TabAnalyze'
      ImageIndex = 1
      object Shape7: TShape
        Left = 0
        Top = 193
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object Shape8: TShape
        Left = 0
        Top = 129
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object GroupAnGeneral: TGroupBox
        Left = 0
        Top = 0
        Width = 341
        Height = 129
        Align = alTop
        Caption = #1054#1089#1085#1086#1074#1085#1099#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1099
        TabOrder = 0
        DesignSize = (
          341
          129)
        object LabelDaysProcess: TLabel
          Left = 16
          Top = 24
          Width = 152
          Height = 16
          Caption = #1055#1077#1088#1080#1086#1076' '#1072#1085#1072#1083#1080#1079#1072' ('#1076#1085#1077#1081')'
        end
        object LabelAdSpeed: TLabel
          Left = 16
          Top = 88
          Width = 145
          Height = 16
          Caption = #1059#1089#1082#1086#1088#1077#1085#1080#1077' '#1072#1076#1072#1087#1090#1072#1094#1080#1080
        end
        object LabelCompression: TLabel
          Left = 16
          Top = 56
          Width = 78
          Height = 16
          Caption = #1050#1086#1084#1087#1088#1077#1089#1089#1080#1103
          Enabled = False
        end
        object EditAnPeriod: TSpinEdit
          Left = 270
          Top = 24
          Width = 60
          Height = 26
          Anchors = [akTop, akRight]
          MaxValue = 365
          MinValue = 7
          TabOrder = 0
          Value = 70
        end
        object EditAdSpeed: TSpinEdit
          Left = 270
          Top = 88
          Width = 60
          Height = 26
          Anchors = [akTop, akRight]
          MaxValue = 99
          MinValue = 10
          TabOrder = 1
          Value = 75
        end
        object EditComp: TSpinEdit
          Left = 270
          Top = 56
          Width = 60
          Height = 26
          Anchors = [akTop, akRight]
          Color = cl3DLight
          Enabled = False
          MaxValue = 99
          MinValue = 10
          TabOrder = 2
          Value = 70
        end
      end
      object GroupKoofUpdating: TGroupBox
        Left = 0
        Top = 201
        Width = 341
        Height = 81
        Align = alTop
        Caption = #1054#1073#1085#1086#1074#1083#1077#1085#1080#1077' '#1082#1086#1101#1092#1092#1080#1094#1080#1077#1085#1090#1086#1074
        TabOrder = 1
        object RadioUpdateKOnChange: TRadioButton
          Left = 16
          Top = 24
          Width = 313
          Height = 17
          Caption = #1055#1088#1080' '#1079#1072#1087#1091#1089#1082#1077', '#1087#1088#1080' '#1080#1079#1084#1077#1085#1077#1085#1080#1080' '#1076#1085#1077#1074#1085#1080#1082#1072
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioUpdateKOnStartup: TRadioButton
          Left = 16
          Top = 48
          Width = 313
          Height = 17
          Caption = #1058#1086#1083#1100#1082#1086' '#1087#1088#1080' '#1079#1072#1087#1091#1089#1082#1077
          TabOrder = 1
        end
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 137
        Width = 341
        Height = 56
        Align = alTop
        Caption = #1054#1090#1086#1073#1088#1072#1078#1077#1085#1080#1077
        TabOrder = 2
        object CheckShowPoints: TCheckBox
          Left = 16
          Top = 24
          Width = 281
          Height = 17
          Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1090#1086#1095#1082#1080' '#1076#1072#1085#1085#1099#1093
          TabOrder = 0
        end
      end
    end
    object TabInterface: TTabSheet
      BorderWidth = 8
      Caption = 'TabInterface'
      ImageIndex = 4
      object Shape4: TShape
        Left = 0
        Top = 65
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object GroupSorting: TGroupBox
        Left = 0
        Top = 0
        Width = 341
        Height = 65
        Align = alTop
        Caption = #1040#1085#1072#1083#1080#1079' '#1095#1072#1089#1090#1086#1090#1099' '#1080#1089#1087#1086#1083#1100#1079#1086#1074#1072#1085#1080#1103
        TabOrder = 0
        DesignSize = (
          341
          65)
        object LabelAnPeriod: TLabel
          Left = 16
          Top = 24
          Width = 96
          Height = 16
          Caption = #1055#1077#1088#1080#1086#1076' ('#1076#1085#1077#1081'):'
        end
        object EditOptPeriod: TSpinEdit
          Left = 268
          Top = 24
          Width = 59
          Height = 26
          Anchors = [akTop, akRight]
          MaxValue = 90
          MinValue = 7
          TabOrder = 0
          Value = 90
        end
      end
      object GroupMisc: TGroupBox
        Left = 0
        Top = 73
        Width = 341
        Height = 104
        Align = alTop
        Caption = #1056#1072#1079#1085#1086#1077
        TabOrder = 1
        object CheckMealNotify: TCheckBox
          Left = 16
          Top = 48
          Width = 313
          Height = 17
          Caption = #1053#1072#1087#1086#1084#1080#1085#1072#1090#1100' '#1086' '#1077#1076#1077
          Enabled = False
          TabOrder = 0
        end
        object CheckUpdates: TCheckBox
          Left = 16
          Top = 72
          Width = 313
          Height = 17
          Caption = #1055#1088#1086#1074#1077#1088#1103#1090#1100' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103
          Enabled = False
          TabOrder = 1
        end
        object CheckCarbsInfo: TCheckBox
          Left = 16
          Top = 24
          Width = 313
          Height = 17
          Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086#1073' '#1091#1075#1083#1077#1074#1086#1076#1085#1086#1089#1090#1080
          TabOrder = 2
        end
      end
    end
    object TabView: TTabSheet
      BorderWidth = 8
      Caption = 'TabView'
      ImageIndex = 2
      object Shape2: TShape
        Left = 0
        Top = 329
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object Shape3: TShape
        Left = 0
        Top = 116
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object GroupFont: TGroupBox
        Left = 0
        Top = 0
        Width = 341
        Height = 116
        Align = alTop
        Caption = #1064#1088#1080#1092#1090
        TabOrder = 0
        DesignSize = (
          341
          116)
        object MemoDemo: TMemo
          Left = 8
          Top = 24
          Width = 321
          Height = 48
          Cursor = crArrow
          Hint = #1054#1073#1088#1072#1079#1077#1094
          Anchors = [akLeft, akTop, akRight, akBottom]
          Color = clInfoBk
          Lines.Strings = (
            #1055#1077#1095#1077#1085#1100#1077' '#1086#1074#1089#1103#1085#1086#1077' (25)')
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 0
        end
        object ButtonFont: TButton
          Left = 200
          Top = 83
          Width = 129
          Height = 25
          Caption = #1048#1079#1084#1077#1085#1080#1090#1100'...'
          TabOrder = 1
          OnClick = ButtonFontClick
        end
      end
      object GroupInterfaceAdditional: TGroupBox
        Left = 0
        Top = 337
        Width = 341
        Height = 128
        Align = alTop
        Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1086
        TabOrder = 1
        DesignSize = (
          341
          128)
        object CheckDoubleBuffered: TCheckBox
          Left = 16
          Top = 72
          Width = 313
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = #1044#1074#1086#1081#1085#1072#1103' '#1073#1091#1092#1077#1088#1080#1079#1072#1094#1080#1103
          TabOrder = 0
          OnClick = CheckDoubleBufferedClick
        end
        object CheckShadow: TCheckBox
          Left = 16
          Top = 24
          Width = 313
          Height = 17
          Caption = #1047#1072#1090#1077#1085#1103#1090#1100' '#1087#1088#1080' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1080' '#1088#1077#1076#1072#1082#1090#1086#1088#1072
          TabOrder = 1
        end
        object CheckAnimate: TCheckBox
          Left = 64
          Top = 96
          Width = 233
          Height = 17
          Caption = #1040#1085#1080#1084#1080#1088#1086#1074#1072#1085#1085#1099#1077' '#1087#1072#1085#1077#1083#1080
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object CheckColoredEditors: TCheckBox
          Left = 16
          Top = 48
          Width = 313
          Height = 17
          Caption = #1062#1074#1077#1090#1085#1099#1077' '#1088#1077#1076#1072#1082#1090#1086#1088#1099
          TabOrder = 3
        end
      end
      object GroupColorScheme: TGroupBox
        Left = 0
        Top = 124
        Width = 341
        Height = 205
        Align = alTop
        Caption = #1062#1074#1077#1090#1086#1074#1072#1103' '#1089#1093#1077#1084#1072
        TabOrder = 2
        DesignSize = (
          341
          205)
        object ImageColors: TImage
          Left = 8
          Top = 24
          Width = 321
          Height = 137
          Cursor = crHandPoint
          OnMouseDown = ImageColorsMouseDown
        end
        object ButtonBackToDefaultColors: TButton
          Left = 8
          Top = 168
          Width = 321
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Caption = #1042#1077#1088#1085#1091#1090#1100' '#1089#1090#1072#1085#1076#1072#1088#1090#1085#1091#1102' '#1089#1093#1077#1084#1091
          TabOrder = 0
          OnClick = ButtonBackToDefaultColorsClick
        end
      end
    end
    object TabNorms: TTabSheet
      BorderWidth = 8
      Caption = 'TabNorms'
      ImageIndex = 3
      object LabelNormsDiscription: TLabel
        Left = 0
        Top = 0
        Width = 341
        Height = 56
        Align = alTop
        AutoSize = False
        Caption = 
          #1045#1089#1083#1080' '#1042#1099' '#1093#1086#1090#1080#1090#1077' '#1089#1073#1072#1083#1072#1085#1089#1080#1088#1086#1074#1072#1090#1100' '#1089#1074#1086#1081' '#1088#1072#1094#1080#1086#1085', '#1091#1089#1090#1072#1085#1086#1074#1080#1090#1077' '#1089#1091#1090#1086#1095#1085#1099#1077' '#1085 +
          #1086#1088#1084#1099' '#1076#1083#1103' '#1073#1077#1083#1082#1086#1074', '#1078#1080#1088#1086#1074' '#1080' '#1091#1075#1083#1077#1074#1086#1076#1086#1074'.'
        WordWrap = True
      end
      object Shape6: TShape
        Left = 0
        Top = 237
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object ListNorms: TValueListEditor
        Left = 0
        Top = 93
        Width = 341
        Height = 144
        Align = alTop
        TabOrder = 0
        TitleCaptions.Strings = (
          #1055#1072#1088#1072#1084#1077#1090#1088
          #1052#1072#1089#1089#1072', '#1075)
        ColWidths = (
          150
          165)
      end
      object GroupShowProcents: TGroupBox
        Left = 0
        Top = 245
        Width = 341
        Height = 105
        Align = alTop
        Caption = #1055#1088#1086#1094#1077#1085#1090#1099
        TabOrder = 1
        DesignSize = (
          341
          105)
        object RadioProcMass: TRadioButton
          Left = 16
          Top = 24
          Width = 307
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = #1055#1088#1086#1094#1077#1085#1090' '#1086#1090' '#1086#1073#1097#1077#1081' '#1084#1072#1089#1089#1099
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioProcCal: TRadioButton
          Left = 16
          Top = 48
          Width = 307
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = #1055#1088#1086#1094#1077#1085#1090' '#1086#1090' '#1086#1073#1097#1077#1075#1086' '#1082#1072#1083#1086#1088#1072#1078#1072
          TabOrder = 1
        end
        object RadioProcNorm: TRadioButton
          Left = 16
          Top = 72
          Width = 307
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = #1055#1088#1086#1094#1077#1085#1090' '#1086#1090' '#1085#1086#1088#1084#1099
          TabOrder = 2
          OnClick = RadioProcNormClick
        end
      end
      object PanelCheckBalance: TPanel
        Left = 0
        Top = 56
        Width = 341
        Height = 37
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object CheckUseNorms: TCheckBox
          Left = 9
          Top = 8
          Width = 264
          Height = 17
          Caption = #1041#1072#1083#1072#1085#1089#1080#1088#1086#1074#1072#1090#1100' '#1087#1080#1090#1072#1085#1080#1077
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CheckUseNormsClick
        end
      end
    end
    object TabAccount: TTabSheet
      BorderWidth = 8
      Caption = 'TabAccount'
      ImageIndex = 5
      object Shape9: TShape
        Left = 0
        Top = 177
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object Shape5: TShape
        Left = 0
        Top = 290
        Width = 341
        Height = 8
        Align = alTop
        Brush.Style = bsClear
        Pen.Style = psClear
      end
      object GroupAccountCommon: TGroupBox
        Left = 0
        Top = 0
        Width = 341
        Height = 177
        Align = alTop
        Caption = #1059#1095#1105#1090#1085#1072#1103' '#1079#1072#1087#1080#1089#1100
        TabOrder = 0
        DesignSize = (
          341
          177)
        object LabelLogin: TLabel
          Left = 16
          Top = 72
          Width = 90
          Height = 16
          Caption = #1051#1086#1075#1080#1085' (e-mail):'
        end
        object LabelPassword: TLabel
          Left = 16
          Top = 120
          Width = 52
          Height = 16
          Caption = #1055#1072#1088#1086#1083#1100':'
        end
        object LabelServer: TLabel
          Left = 16
          Top = 24
          Width = 101
          Height = 16
          Caption = #1040#1076#1088#1077#1089' '#1089#1077#1088#1074#1077#1088#1072':'
        end
        object EditLogin: TEdit
          Left = 16
          Top = 88
          Width = 313
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Text = 'EditLogin'
        end
        object EditPassword: TEdit
          Left = 16
          Top = 136
          Width = 313
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PasswordChar = '*'
          TabOrder = 2
          Text = 'EditPassword'
        end
        object EditServerURL: TEdit
          Left = 16
          Top = 40
          Width = 313
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Text = 'EditServerURL'
        end
      end
      object GroupServer: TGroupBox
        Left = 0
        Top = 298
        Width = 341
        Height = 95
        Align = alTop
        Caption = #1057#1080#1085#1093#1088#1086#1085#1080#1079#1072#1094#1080#1103
        TabOrder = 1
        DesignSize = (
          341
          95)
        object LabelAutosaveInterval: TLabel
          Left = 16
          Top = 56
          Width = 104
          Height = 16
          Caption = #1048#1085#1090#1077#1088#1074#1072#1083' ('#1084#1080#1085'):'
        end
        object CheckAutoSync: TCheckBox
          Left = 16
          Top = 23
          Width = 305
          Height = 17
          Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1089#1080#1085#1093#1088#1086#1085#1080#1079#1080#1088#1086#1074#1072#1090#1100
          TabOrder = 0
        end
        object EditAutosaveInterval: TSpinEdit
          Left = 268
          Top = 56
          Width = 59
          Height = 26
          Anchors = [akTop, akRight]
          MaxValue = 30
          MinValue = 1
          TabOrder = 1
          Value = 30
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 185
        Width = 341
        Height = 105
        Align = alTop
        Caption = #1057#1086#1079#1076#1072#1085#1080#1077' '#1085#1086#1074#1086#1081' '#1091#1095#1105#1090#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
        TabOrder = 2
        object Label1: TLabel
          Left = 16
          Top = 24
          Width = 261
          Height = 32
          Caption = 
            #1045#1089#1083#1080' '#1091' '#1074#1072#1089' '#1077#1097#1105' '#1085#1077#1090' '#1091#1095#1105#1090#1085#1086#1081' '#1079#1072#1087#1080#1089#1080', '#1079#1072#1088#1077#1075#1080#1089#1090#1088#1080#1088#1091#1081#1090#1077#1089#1100', '#1085#1072#1078#1072#1074' '#1082#1085#1086#1087 +
            #1082#1091' '#1085#1080#1078#1077
          WordWrap = True
        end
        object ButtonRegister: TButton
          Left = 8
          Top = 64
          Width = 313
          Height = 25
          Caption = #1056#1077#1075#1080#1089#1090#1088#1072#1094#1080#1103
          TabOrder = 0
          OnClick = ButtonRegisterClick
        end
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 546
    Width = 472
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      472
      47)
    object ButtonSave: TBitBtn
      Left = 199
      Top = 11
      Width = 130
      Height = 34
      Anchors = [akTop, akRight]
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Default = True
      TabOrder = 0
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
    object ButtonCancel: TBitBtn
      Left = 344
      Top = 11
      Width = 125
      Height = 34
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
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
  object PanelTabs: TPanel
    Left = 0
    Top = 0
    Width = 107
    Height = 546
    Align = alLeft
    BevelOuter = bvLowered
    ParentBackground = False
    TabOrder = 2
    object ButtonTabPrivate: TSpeedButton
      Tag = 1
      Left = 8
      Top = 8
      Width = 90
      Height = 68
      GroupIndex = 1
      Down = True
      Caption = #1051#1080#1095#1085#1099#1077
      Flat = True
      Layout = blGlyphTop
      OnClick = ButtonTabClick
    end
    object ButtonTabAnalyze: TSpeedButton
      Tag = 2
      Left = 8
      Top = 82
      Width = 90
      Height = 68
      GroupIndex = 1
      Caption = #1040#1085#1072#1083#1080#1079
      Flat = True
      Layout = blGlyphTop
      Transparent = False
      OnClick = ButtonTabClick
    end
    object ButtonTabInterface: TSpeedButton
      Tag = 3
      Left = 8
      Top = 156
      Width = 90
      Height = 68
      GroupIndex = 1
      Caption = #1048#1085#1090#1077#1088#1092#1077#1081#1089
      Flat = True
      Layout = blGlyphTop
      OnClick = ButtonTabClick
    end
    object ButtonTabView: TSpeedButton
      Tag = 4
      Left = 8
      Top = 230
      Width = 90
      Height = 68
      GroupIndex = 1
      Caption = #1042#1080#1076
      Flat = True
      Layout = blGlyphTop
      OnClick = ButtonTabClick
    end
    object ButtonTabNorms: TSpeedButton
      Tag = 5
      Left = 24
      Top = 328
      Width = 90
      Height = 68
      GroupIndex = 1
      Caption = #1053#1086#1088#1084#1099
      Flat = True
      Layout = blGlyphTop
      OnClick = ButtonTabClick
    end
    object ButtonTabInternet: TSpeedButton
      Tag = 6
      Left = 24
      Top = 416
      Width = 90
      Height = 68
      GroupIndex = 1
      Caption = #1048#1085#1090#1077#1088#1085#1077#1090
      Flat = True
      Layout = blGlyphTop
      OnClick = ButtonTabClick
    end
  end
  object ColorDialog: TColorDialog
    Color = clLime
    Options = [cdFullOpen, cdAnyColor]
    Left = 23
    Top = 557
  end
  object FontDialog: TFontDialog
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'ChinaCyr'
    Font.Style = [fsBold]
    MaxFontSize = 30
    Options = [fdAnsiOnly, fdNoFaceSel, fdNoStyleSel, fdLimitSize, fdScalableOnly]
    Left = 60
    Top = 555
  end
end

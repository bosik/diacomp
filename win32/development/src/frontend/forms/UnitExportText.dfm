object FormExportText: TFormExportText
  Left = 509
  Top = 183
  Width = 472
  Height = 620
  BorderIcons = [biSystemMenu, biMaximize]
  BorderWidth = 8
  Caption = #1069#1082#1089#1087#1086#1088#1090' '#1076#1085#1077#1074#1085#1080#1082#1072
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
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000777
    77777777770007FFFFFFFFFFF70007FFFFFFFFF8880007FFFFFFFFF8F88007FF
    FFF88888FF8807FFFFF8FFFFFFF807FFFFF88888FF8807FFFFFFFFF8F88007FF
    FFFFFFF8870007FFFFFFFFFFF700007999999999700000799999999970000089
    8977798980000089898889898000009181999181900000087800087800008003
    0000800300008003000080010000800000008000000080000000800100008003
    000080030000C0070000C0070000C0070000C0070000C0070000E38F0000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Shape1: TShape
    Left = 0
    Top = 89
    Width = 448
    Height = 8
    Align = alTop
    Brush.Style = bsClear
    Pen.Style = psClear
  end
  object Shape3: TShape
    Left = 0
    Top = 321
    Width = 448
    Height = 8
    Align = alTop
    Brush.Style = bsClear
    Pen.Style = psClear
  end
  object GroupPeriod: TGroupBox
    Left = 0
    Top = 0
    Width = 448
    Height = 89
    Align = alTop
    Caption = #1055#1077#1088#1080#1086#1076
    TabOrder = 0
    DesignSize = (
      448
      89)
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 50
      Height = 16
      Caption = #1053#1072#1095#1072#1083#1086
    end
    object Label2: TLabel
      Left = 16
      Top = 56
      Width = 40
      Height = 16
      Caption = #1050#1086#1085#1077#1094
    end
    object Picker1: TDateTimePicker
      Left = 80
      Top = 24
      Width = 300
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Date = 40383.448972430550000000
      Time = 40383.448972430550000000
      TabOrder = 0
      OnChange = CreateLog
    end
    object Picker2: TDateTimePicker
      Left = 80
      Top = 56
      Width = 300
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Date = 40383.449173171300000000
      Time = 40383.449173171300000000
      TabOrder = 1
      OnChange = CreateLog
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 534
    Width = 448
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      448
      38)
    object ButtonSave: TSpeedButton
      Left = 8
      Top = 8
      Width = 130
      Height = 28
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100'...'
      OnClick = ButtonSaveClick
    end
    object ButtonClose: TBitBtn
      Left = 272
      Top = 8
      Width = 130
      Height = 28
      Anchors = [akTop, akRight]
      Caption = #1047#1072#1082#1088#1099#1090#1100
      TabOrder = 0
      Kind = bkNo
    end
  end
  object MemoOut: TRichEdit
    Left = 0
    Top = 329
    Width = 448
    Height = 205
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'MemoOut')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object PanelSettings: TPanel
    Left = 0
    Top = 97
    Width = 448
    Height = 224
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Splitter1: TSplitter
      Left = 253
      Top = 0
      Height = 224
      Align = alRight
    end
    object Shape2: TShape
      Left = 249
      Top = 0
      Width = 4
      Height = 224
      Align = alRight
      Brush.Style = bsClear
      Pen.Style = psClear
      Shape = stCircle
    end
    object GroupContent: TGroupBox
      Left = 0
      Top = 0
      Width = 249
      Height = 224
      Align = alClient
      Caption = #1044#1072#1085#1085#1099#1077
      TabOrder = 0
      DesignSize = (
        249
        224)
      object CheckAddBS: TCheckBox
        Left = 24
        Top = 24
        Width = 216
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1047#1072#1084#1077#1088#1099' '#1057#1050
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CreateLog
      end
      object CheckAddIns: TCheckBox
        Left = 24
        Top = 48
        Width = 216
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1048#1085#1098#1077#1082#1094#1080#1080
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = CreateLog
      end
      object CheckAddMeal: TCheckBox
        Left = 24
        Top = 72
        Width = 216
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1055#1088#1080#1105#1084#1099' '#1087#1080#1097#1080
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CheckAddMealClick
      end
      object RadioMealCarbs: TRadioButton
        Left = 64
        Top = 120
        Width = 181
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1059#1075#1083#1077#1074#1086#1076#1099
        Checked = True
        TabOrder = 3
        TabStop = True
        OnClick = CreateLog
      end
      object RadioMealCarbsProts: TRadioButton
        Left = 64
        Top = 144
        Width = 181
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1059#1075#1083#1077#1074#1086#1076#1099' '#1080' '#1073#1077#1083#1082#1080
        TabOrder = 4
        OnClick = CreateLog
      end
      object RadioMealBU: TRadioButton
        Left = 64
        Top = 168
        Width = 181
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1061#1045' (12'#1075' '#1091#1075#1083')'
        TabOrder = 5
        OnClick = CreateLog
      end
      object CheckMealContent: TCheckBox
        Left = 64
        Top = 96
        Width = 181
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1057#1087#1080#1089#1086#1082' '#1073#1083#1102#1076
        TabOrder = 6
        OnClick = CreateLog
      end
      object CheckAddNotes: TCheckBox
        Left = 24
        Top = 192
        Width = 216
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1047#1072#1084#1077#1090#1082#1080
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = CreateLog
      end
    end
    object GroupFormat: TGroupBox
      Left = 256
      Top = 0
      Width = 192
      Height = 224
      Align = alRight
      Caption = #1060#1086#1088#1084#1072#1090
      TabOrder = 1
      DesignSize = (
        192
        224)
      object CheckBreaks: TCheckBox
        Left = 16
        Top = 24
        Width = 161
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1056#1072#1079#1088#1099#1074#1099' (3 '#1095#1072#1089#1072')'
        TabOrder = 0
        OnClick = CreateLog
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      #1058#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083' (*.txt)|*.txt|'#1058#1077#1082#1089#1090' '#1089' '#1092#1086#1088#1084#1072#1090#1080#1088#1086#1074#1072#1085#1080#1077#1084' (*.rtf)|*.r' +
      'tf'
    Title = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    Left = 88
    Top = 496
  end
end

object FormMisc: TFormMisc
  Left = 465
  Top = 241
  Width = 621
  Height = 675
  Caption = #1054#1090#1083#1072#1076#1082#1072
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 224
    Top = 216
    Width = 41
    Height = 16
    Caption = 'Label1'
  end
  object ButtonExportXml: TButton
    Left = 8
    Top = 8
    Width = 201
    Height = 25
    Caption = 'Export diary into XML'
    Enabled = False
    TabOrder = 0
    OnClick = ButtonExportXmlClick
  end
  object ButtonExportJson: TButton
    Left = 8
    Top = 40
    Width = 201
    Height = 25
    Caption = 'Export diary into JSON'
    TabOrder = 1
    OnClick = ButtonExportJsonClick
  end
  object ButtonBruteforce: TButton
    Left = 8
    Top = 504
    Width = 201
    Height = 25
    Caption = #1041#1088#1091#1090#1092#1086#1088#1089
    Enabled = False
    TabOrder = 2
    OnClick = ButtonBruteforceClick
  end
  object Button5: TButton
    Left = 8
    Top = 536
    Width = 201
    Height = 25
    Caption = #1043#1088#1072#1076#1080#1077#1085#1090
    TabOrder = 3
    Visible = False
    OnClick = Button5Click
  end
  object Button4: TButton
    Left = 8
    Top = 214
    Width = 201
    Height = 25
    Caption = 'Cloud'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button3: TButton
    Left = 8
    Top = 248
    Width = 201
    Height = 25
    Caption = #1057#1073#1072#1083#1072#1085#1089#1080#1088#1086#1074#1072#1085#1085#1086#1089#1090#1100' '#1041#1046#1059
    TabOrder = 5
    OnClick = Button3Click
  end
  object ButtonAverageFoodMass: TButton
    Left = 8
    Top = 281
    Width = 201
    Height = 25
    Caption = #1057#1088#1077#1076#1085#1103#1103' '#1084#1072#1089#1089#1072' '#1087#1088#1086#1076#1091#1082#1090#1072
    TabOrder = 6
    OnClick = ButtonAverageFoodMassClick
  end
  object Button8: TButton
    Left = 8
    Top = 312
    Width = 201
    Height = 25
    Caption = 'Test Smth'
    TabOrder = 7
    Visible = False
    OnClick = Button8Click
  end
  object Edit1: TEdit
    Left = 224
    Top = 184
    Width = 377
    Height = 24
    TabOrder = 8
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Memo1: TMemo
    Left = 224
    Top = 8
    Width = 377
    Height = 177
    Lines.Strings = (
      'Memo1')
    TabOrder = 9
  end
  object ButtonCovariance: TButton
    Left = 8
    Top = 344
    Width = 201
    Height = 25
    Caption = #1050#1086#1088#1088#1077#1083#1103#1094#1080#1103
    TabOrder = 10
    OnClick = ButtonCovarianceClick
  end
  object ButtonExportRaw: TButton
    Left = 8
    Top = 104
    Width = 201
    Height = 25
    Caption = 'Export AnList'
    TabOrder = 11
    OnClick = ButtonExportRawClick
  end
  object Button1: TButton
    Left = 8
    Top = 376
    Width = 201
    Height = 25
    Caption = 'Average Daily BS'
    TabOrder = 12
    OnClick = Button1Click
  end
  object ButtonFoodBSCorrelation: TButton
    Left = 8
    Top = 408
    Width = 201
    Height = 25
    Caption = 'ButtonFoodBSCorrelation'
    TabOrder = 13
    OnClick = ButtonFoodBSCorrelationClick
  end
  object ButtonVerifyLinear: TButton
    Left = 8
    Top = 440
    Width = 201
    Height = 25
    Caption = 'ButtonVerifyLinear'
    TabOrder = 14
    OnClick = ButtonVerifyLinearClick
  end
  object ButtonExportFoodBase: TButton
    Left = 8
    Top = 72
    Width = 201
    Height = 25
    Caption = 'Export food base into JSON'
    TabOrder = 15
    OnClick = ButtonExportFoodBaseClick
  end
  object ButtonFoodCompare: TButton
    Left = 8
    Top = 472
    Width = 201
    Height = 25
    Caption = 'Compare food lists'
    TabOrder = 16
    OnClick = ButtonFoodCompareClick
  end
  object ButtonTestAnalyzer: TButton
    Left = 8
    Top = 568
    Width = 201
    Height = 25
    Caption = 'Test analyzer'
    TabOrder = 17
    OnClick = ButtonTestAnalyzerClick
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 316
    Top = 222
  end
end

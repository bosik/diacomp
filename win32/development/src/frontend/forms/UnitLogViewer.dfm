object FormLogViewer: TFormLogViewer
  Left = 319
  Top = 158
  Width = 1305
  Height = 675
  Caption = #1051#1086#1075#1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object MemoLog: TRichEdit
    Left = 249
    Top = 0
    Width = 1048
    Height = 643
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'MemoLog')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnKeyDown = FormKeyDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 643
    Align = alLeft
    TabOrder = 1
    DesignSize = (
      249
      643)
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 144
      Height = 16
      Caption = #1059#1088#1086#1074#1077#1085#1100' '#1083#1086#1075#1080#1088#1086#1074#1072#1085#1080#1103
    end
    object ComboBoxLogLevel: TComboBox
      Left = 8
      Top = 32
      Width = 233
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      TabOrder = 0
      OnChange = ComboBoxLogLevelChange
      Items.Strings = (
        'ERROR'
        'WARNING'
        'INFO'
        'DEBUG'
        'VERBOUS')
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 16
    Top = 64
  end
end

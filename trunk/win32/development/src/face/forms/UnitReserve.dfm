object FormSync: TFormSync
  Left = 405
  Top = 313
  Anchors = [akTop, akRight]
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = #1057#1080#1085#1093#1088#1086#1085#1080#1079#1072#1094#1080#1103
  ClientHeight = 377
  ClientWidth = 396
  Color = clBtnFace
  Constraints.MaxWidth = 520
  Constraints.MinHeight = 280
  Constraints.MinWidth = 330
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    396
    377)
  PixelsPerInch = 120
  TextHeight = 16
  object Shape1: TShape
    Left = 0
    Top = 65
    Width = 396
    Height = 8
    Align = alTop
    Brush.Style = bsClear
    Pen.Style = psClear
  end
  object ButtonOK: TBitBtn
    Left = 270
    Top = 300
    Width = 120
    Height = 28
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = #1047#1072#1082#1088#1099#1090#1100
    ModalResult = 1
    TabOrder = 0
    OnClick = ButtonOKClick
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
  object GroupSyncSource: TGroupBox
    Left = 0
    Top = 0
    Width = 396
    Height = 65
    Align = alTop
    Caption = #1048#1089#1090#1086#1095#1085#1080#1082
    TabOrder = 1
    DesignSize = (
      396
      65)
    object ComboPath: TComboBox
      Left = 16
      Top = 24
      Width = 327
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      TabOrder = 0
      Text = 'ComboPath'
      OnChange = ComboPathChange
    end
    object ButtonBrowse: TButton
      Left = 357
      Top = 24
      Width = 29
      Height = 25
      Hint = #1054#1073#1079#1086#1088
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = ButtonBrowseClick
    end
  end
  object GroupOperations: TGroupBox
    Left = 0
    Top = 73
    Width = 396
    Height = 160
    Align = alTop
    Caption = #1054#1087#1077#1088#1072#1094#1080#1080
    TabOrder = 2
    DesignSize = (
      396
      160)
    object ImageState: TImage
      Left = 16
      Top = 24
      Width = 150
      Height = 110
      AutoSize = True
    end
    object ButtonTest: TSpeedButton
      Left = 193
      Top = 24
      Width = 193
      Height = 28
      Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100' '#1087#1088#1086#1074#1077#1088#1082#1091
      OnClick = ButtonTestClick
    end
    object ButtonSync: TSpeedButton
      Left = 193
      Top = 58
      Width = 193
      Height = 28
      Caption = #1057#1080#1085#1093#1088#1086#1085#1080#1079#1080#1088#1086#1074#1072#1090#1100
      OnClick = ButtonSyncClick
    end
    object LabelState: TLabel
      Left = 192
      Top = 96
      Width = 191
      Height = 41
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'LabelState'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
  end
end

object DBloodEditor: TDBloodEditor
  Left = 388
  Top = 407
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = #1047#1072#1084#1077#1088' '#1057#1050
  ClientHeight = 164
  ClientWidth = 262
  Color = 16311512
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    262
    164)
  PixelsPerInch = 120
  TextHeight = 16
  object Image: TImage
    Left = 16
    Top = 16
    Width = 80
    Height = 80
    Center = True
    Transparent = True
  end
  object LabelTime: TLabel
    Left = 112
    Top = 8
    Width = 44
    Height = 16
    Caption = #1042#1088#1077#1084#1103':'
  end
  object LabelValue: TLabel
    Left = 112
    Top = 56
    Width = 68
    Height = 16
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077':'
  end
  object Bevel: TBevel
    Left = 8
    Top = 121
    Width = 245
    Height = 2
  end
  object EditTime: TMaskEdit
    Tag = 1
    Left = 112
    Top = 24
    Width = 138
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditMask = '!99.99;1;_'
    MaxLength = 5
    TabOrder = 0
    Text = '  .  '
    OnKeyDown = EditKeyDown
  end
  object ButtonSave: TBitBtn
    Left = 8
    Top = 132
    Width = 120
    Height = 28
    Caption = #1054#1050
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
  object ButtonCancel: TBitBtn
    Left = 142
    Top = 132
    Width = 110
    Height = 28
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 2
    OnClick = ButtonCancelClick
    Kind = bkNo
  end
  object EditValue: TEditNumb
    Tag = 2
    Left = 112
    Top = 72
    Width = 121
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'EditValue'
    OnKeyDown = EditKeyDown
    Decimal = ','
  end
end

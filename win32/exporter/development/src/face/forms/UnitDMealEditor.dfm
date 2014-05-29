object DMealEditor: TDMealEditor
  Left = 608
  Top = 30
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = #1055#1088#1080#1105#1084' '#1087#1080#1097#1080
  ClientHeight = 121
  ClientWidth = 261
  Color = 14811135
  Constraints.MinHeight = 130
  Constraints.MinWidth = 220
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    261
    121)
  PixelsPerInch = 120
  TextHeight = 16
  object Image: TImage
    Left = 32
    Top = 24
    Width = 39
    Height = 42
    AutoSize = True
    Stretch = True
    Transparent = True
  end
  object LabelTime: TLabel
    Left = 97
    Top = 1
    Width = 44
    Height = 16
    Caption = #1042#1088#1077#1084#1103':'
  end
  object Bevel1: TBevel
    Left = 9
    Top = 74
    Width = 241
    Height = 2
    Anchors = [akLeft, akRight, akBottom]
  end
  object EditTime: TMaskEdit
    Left = 65
    Top = 41
    Width = 112
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditMask = '!99.99;1;_'
    MaxLength = 5
    TabOrder = 0
    Text = '  .  '
    OnKeyDown = EditTimeKeyDown
  end
  object ButtonSave: TBitBtn
    Left = 9
    Top = 82
    Width = 119
    Height = 28
    Anchors = [akLeft, akBottom]
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
    Left = 139
    Top = 82
    Width = 111
    Height = 28
    Anchors = [akRight, akBottom]
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 2
    OnClick = ButtonCancelClick
    Kind = bkNo
  end
end

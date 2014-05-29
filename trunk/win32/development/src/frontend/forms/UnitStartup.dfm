object FormProcess: TFormProcess
  Left = 311
  Top = 233
  BorderStyle = bsNone
  Caption = #1047#1072#1075#1088#1091#1079#1082#1072
  ClientHeight = 80
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 80
    Align = alClient
    TabOrder = 0
    DesignSize = (
      345
      80)
    object LabelHint: TLabel
      Left = 12
      Top = 8
      Width = 57
      Height = 16
      Caption = 'LabelHint'
    end
    object ProgressBar1: TProgressBar
      Left = 12
      Top = 32
      Width = 289
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Max = 14
      Step = 1
      TabOrder = 0
    end
  end
end

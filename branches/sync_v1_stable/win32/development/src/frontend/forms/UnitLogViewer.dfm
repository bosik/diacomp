object FormLogViewer: TFormLogViewer
  Left = 193
  Top = 120
  Width = 1305
  Height = 675
  Caption = 'FormLogViewer'
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
    Left = 0
    Top = 0
    Width = 1297
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
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 16
    Top = 24
  end
end

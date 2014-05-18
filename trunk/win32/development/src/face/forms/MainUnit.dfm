object Form1: TForm1
  Left = 31
  Top = 31
  Width = 1249
  Height = 959
  Caption = #1053#1072#1079#1074#1072#1085#1080#1077
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MenuHidden
  OldCreateOrder = False
  OnClose = FormClose
  OnResize = FormResize
  PixelsPerInch = 120
  TextHeight = 16
  object BevelTop: TBevel
    Left = 0
    Top = 30
    Width = 1241
    Height = 2
    Align = alTop
  end
  object ShapeTop: TShape
    Left = 0
    Top = 32
    Width = 1241
    Height = 5
    Align = alTop
    Brush.Style = bsClear
    Pen.Style = psClear
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 908
    Width = 1241
    Height = 19
    Panels = <
      item
        Text = #1057#1090#1072#1090#1091#1089' '#1079#1072#1075#1088#1091#1079#1082#1080
        Width = 220
      end
      item
        Text = #1057#1090#1072#1090#1091#1089' '#1076#1085#1077#1074#1085#1080#1082#1072
        Width = 240
      end
      item
        Text = #1054#1092#1092#1083#1072#1081#1085
        Width = 220
      end
      item
        Text = #1057#1077#1088#1074#1077#1088
        Width = 50
      end>
  end
  object MainMenu: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 1241
    Height = 30
    ActionManager = ActionManager
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    HorzMargin = 2
    Spacing = 4
    VertMargin = 2
  end
  object PanelMain: TPanel
    Left = 0
    Top = 37
    Width = 1241
    Height = 871
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 1241
      Height = 871
      ActivePage = TabDiary
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      HotTrack = True
      Images = DataInterface.Images_Main_Tabs
      ParentFont = False
      Style = tsFlatButtons
      TabOrder = 0
      TabStop = False
      TabWidth = 100
      OnChange = PageControl1Change
      OnExit = ResetTabStop
      object TabDiary: TTabSheet
        Caption = #1044#1085#1077#1074#1085#1080#1082
        ParentShowHint = False
        ShowHint = False
        object Splitter4: TSplitter
          Left = 965
          Top = 0
          Height = 837
          Align = alRight
          Color = clSkyBlue
          ParentColor = False
          OnMoved = Splitter4Moved
        end
        object PanelDiaryTop: TPanel
          Left = 222
          Top = 0
          Width = 743
          Height = 837
          Align = alClient
          BevelOuter = bvNone
          ParentBackground = True
          TabOrder = 0
          object ScrollBoxDiary: TScrollBox
            Left = 0
            Top = 0
            Width = 743
            Height = 756
            VertScrollBar.Increment = 40
            VertScrollBar.Tracking = True
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            PopupMenu = DataInterface.PopupDiary
            TabOrder = 0
            OnMouseDown = ScrollBoxDiaryMouseDown
            object DiaryView: TDiaryView
              Left = 0
              Top = 0
              Width = 739
              Height = 117
              Align = alTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -17
              Font.Name = 'Arial'
              Font.Style = []
              PopupBlood = DataInterface.PopupDiaryBlood
              PopupIns = DataInterface.PopupDiaryIns
              PopupMeal = DataInterface.PopupDiaryMeal
              PopupFood = DataInterface.PopupDiaryDish
              PopupNote = DataInterface.PopupDiaryNote
              TextEmptyPage = #1057#1090#1088#1072#1085#1080#1094#1072' '#1087#1091#1089#1090#1072
              OnChange = DiaryViewChange
              OnDoubleClickBlood = DiaryViewDoubleClickBlood
              OnDoubleClickIns = DiaryViewDoubleClickIns
              OnDoubleClickMeal = DiaryViewDoubleClickMeal
              OnDoubleClickNote = DiaryViewDoubleClickNote
              OnFoodShow = DiaryViewFoodShow
              OnMouseDown = DiaryViewMouseDown
              OnPage = DiaryViewPage
            end
          end
          object PanelDiaryBottom: TPanel
            Left = 0
            Top = 756
            Width = 743
            Height = 81
            Align = alBottom
            BevelOuter = bvNone
            ParentBackground = False
            TabOrder = 1
            object BevelRecRight: TBevel
              Left = 289
              Top = 0
              Width = 2
              Height = 81
              Align = alLeft
            end
            object PanelAdd: TPanel
              Left = 0
              Top = 0
              Width = 289
              Height = 81
              Align = alLeft
              BevelOuter = bvNone
              ParentBackground = True
              TabOrder = 0
              object ButtonAddBlood: TSpeedButton
                Tag = 1
                Left = 10
                Top = 8
                Width = 65
                Height = 65
                Hint = #1047#1072#1084#1077#1088' '#1057#1050
                ParentShowHint = False
                ShowHint = True
                OnClick = ActionAddRecordExecute
              end
              object ButtonAddIns: TSpeedButton
                Tag = 2
                Left = 88
                Top = 8
                Width = 65
                Height = 65
                Hint = #1048#1098#1077#1082#1094#1080#1103
                ParentShowHint = False
                ShowHint = True
                OnClick = ActionAddRecordExecute
              end
              object ButtonAddMeal: TSpeedButton
                Tag = 3
                Left = 172
                Top = 8
                Width = 65
                Height = 65
                Hint = #1055#1088#1080#1105#1084' '#1087#1080#1097#1080
                ParentShowHint = False
                ShowHint = True
                OnClick = ActionAddRecordExecute
              end
            end
            object GroupBoxAdd: TGroupBox
              Left = 291
              Top = 0
              Width = 452
              Height = 81
              Align = alClient
              Caption = #1055#1088#1086#1076#1091#1082#1090#1099' '#1080' '#1073#1083#1102#1076#1072
              Color = clBtnFace
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clBlue
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentBackground = False
              ParentColor = False
              ParentFont = False
              TabOrder = 1
              DesignSize = (
                452
                81)
              object ButtonDiaryNewAddFood: TSpeedButton
                Left = 388
                Top = 25
                Width = 25
                Height = 25
                Hint = #1044#1086#1073#1072#1074#1080#1090#1100' (Enter)'
                Anchors = [akTop, akRight]
                NumGlyphs = 2
                ParentShowHint = False
                ShowHint = True
                OnClick = ButtonDiaryNewAddClick
              end
              object ComboDiaryNew: TACComboBox
                Tag = 1
                Left = 56
                Top = 24
                Width = 251
                Height = 24
                UserHint = #1053#1072#1095#1085#1080#1090#1077' '#1074#1074#1086#1076#1080#1090#1100' '#1085#1072#1079#1074#1072#1085#1080#1077
                ShowUserHint = True
                Style = csSimple
                Anchors = [akLeft, akTop, akRight]
                DropDownCount = 14
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -15
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ItemHeight = 16
                ParentFont = False
                TabOrder = 1
                OnCloseUp = ComboDiaryNewCloseUp
                OnDrawItem = ComboDiaryNewDrawItem
                OnKeyDown = ComboDiaryNewKeyDown
              end
              object EditDiaryNewMass: TEditNumb
                Tag = 1
                Left = 327
                Top = 25
                Width = 56
                Height = 25
                Hint = #1052#1072#1089#1089#1072' ('#1075')'
                TabStop = False
                Anchors = [akTop, akRight]
                Enabled = False
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -15
                Font.Name = 'Courier New'
                Font.Style = [fsBold]
                ParentFont = False
                ParentShowHint = False
                ShowHint = True
                TabOrder = 0
                OnEnter = ResetTabStop
                OnKeyPress = EditDiaryNewMassKeyPress
                Decimal = ','
                WarningShow = True
              end
            end
          end
        end
        object PanelDiaryTopLeft: TPanel
          Left = 0
          Top = 0
          Width = 222
          Height = 837
          Align = alLeft
          BevelOuter = bvLowered
          BorderWidth = 8
          Color = clWhite
          DockSite = True
          ParentBackground = False
          TabOrder = 1
          OnDblClick = PanelDiaryTopLeftDblClick
          object ShapeLeft1: TShape
            Left = 9
            Top = 192
            Width = 204
            Height = 10
            Align = alTop
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object ShapeLeft2: TShape
            Left = 9
            Top = 346
            Width = 204
            Height = 10
            Align = alTop
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object ShapeDivImageBS: TShape
            Left = 9
            Top = 515
            Width = 204
            Height = 10
            Align = alTop
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object ImageNote: TImage
            Left = 64
            Top = 704
            Width = 81
            Height = 65
            Visible = False
          end
          object GroupBoxStatistic: TGroupBox
            Left = 9
            Top = 202
            Width = 204
            Height = 144
            Align = alTop
            Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
            Color = clWhite
            Ctl3D = True
            DragKind = dkDock
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentBackground = False
            ParentColor = False
            ParentCtl3D = False
            ParentFont = False
            PopupMenu = DataInterface.PopupConfigNorms
            TabOrder = 0
            DesignSize = (
              204
              144)
            object LabelDayMass: TLabel
              Left = 16
              Top = 96
              Width = 92
              Height = 16
              Caption = 'LabelDayMass'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
            object LabelDayValue: TLabel
              Left = 16
              Top = 80
              Width = 94
              Height = 16
              Caption = 'LabelDayValue'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
            object LabelDayCarbs_: TLabel
              Left = 73
              Top = 57
              Width = 102
              Height = 16
              Caption = 'LabelDayCarbs_'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
            object LabelDayFats_: TLabel
              Left = 73
              Top = 41
              Width = 92
              Height = 16
              Caption = 'LabelDayFats_'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
            object LabelDayProts_: TLabel
              Left = 73
              Top = 25
              Width = 97
              Height = 16
              Caption = 'LabelDayProts_'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
            object ImageMinMaxStat: TImage
              Tag = 1
              Left = 170
              Top = 0
              Width = 23
              Height = 16
              Hint = #1057#1074#1077#1088#1085#1091#1090#1100
              Anchors = [akTop, akRight]
              AutoSize = True
              ParentShowHint = False
              ShowHint = True
              Transparent = True
              OnMouseDown = ImageMinMaxStatMouseDown
            end
            object LabelDayIns: TLabel
              Left = 16
              Top = 121
              Width = 76
              Height = 16
              Caption = 'LabelDayIns'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
            object StatProgProts: TStatProgress
              Left = 16
              Top = 25
              Width = 50
              Height = 14
              ColorFill = 13369035
              ColorOver = clRed
              Progress = 100
              Rotation = 0
              Volume = 50
            end
            object StatProgFats: TStatProgress
              Left = 16
              Top = 41
              Width = 50
              Height = 14
              ColorFill = 9371109
              ColorOver = clRed
              Progress = 100
              Rotation = 0
              Volume = 50
            end
            object StatProgCarbs: TStatProgress
              Left = 16
              Top = 57
              Width = 50
              Height = 14
              ColorFill = 16684798
              ColorOver = clRed
              Progress = 100
              Rotation = 0
              Volume = 50
            end
            object LabelDayProtsVal: TLabel
              Left = 177
              Top = 25
              Width = 16
              Height = 16
              Anchors = [akTop, akRight]
              Caption = 'XX'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
            object LabelDayFatsVal: TLabel
              Left = 177
              Top = 41
              Width = 16
              Height = 16
              Anchors = [akTop, akRight]
              Caption = 'XX'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
            object LabelDayCarbsVal: TLabel
              Left = 177
              Top = 57
              Width = 16
              Height = 16
              Anchors = [akTop, akRight]
              Caption = 'XX'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              PopupMenu = DataInterface.PopupConfigNorms
            end
          end
          object GroupBoxGraph: TGroupBox
            Left = 9
            Top = 356
            Width = 204
            Height = 159
            Align = alTop
            Caption = #1043#1088#1072#1092#1080#1082' '#1057#1050
            Color = clWhite
            DragKind = dkDock
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentBackground = False
            ParentColor = False
            ParentFont = False
            TabOrder = 1
            DesignSize = (
              204
              159)
            object ImageMinMaxGraph: TImage
              Tag = 1
              Left = 167
              Top = 0
              Width = 25
              Height = 16
              Hint = #1057#1074#1077#1088#1085#1091#1090#1100
              Anchors = [akTop, akRight]
              AutoSize = True
              ParentShowHint = False
              ShowHint = True
              Transparent = True
              OnMouseDown = ImageMinMaxGraphMouseDown
            end
            object ImagePreview: TImage
              Left = 2
              Top = 18
              Width = 200
              Height = 139
              Hint = #1050#1083#1080#1082#1085#1080#1090#1077' '#1076#1074#1072' '#1088#1072#1079#1072' '#1076#1083#1103' '#1087#1077#1088#1077#1093#1086#1076#1072' '#1082' '#1073#1086#1083#1100#1096#1086#1084#1091' '#1075#1088#1072#1092#1080#1082#1091
              Align = alClient
              ParentShowHint = False
              ShowHint = True
              OnDblClick = ImagePreviewDblClick
            end
          end
          object CalendarDiary: TMonthCalendar
            Left = 9
            Top = 9
            Width = 204
            Height = 183
            Align = alTop
            Date = 40251.689881273150000000
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = CalendarDiaryChange
          end
          object GroupBoxTimeLeft: TGroupBox
            Left = 9
            Top = 525
            Width = 204
            Height = 96
            Align = alTop
            Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1086
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentBackground = False
            ParentColor = False
            ParentFont = False
            TabOrder = 3
            DesignSize = (
              204
              96)
            object LabelDiaryTimeLeftInsVal: TLabel
              Left = 165
              Top = 25
              Width = 37
              Height = 16
              Align = alCustom
              Alignment = taRightJustify
              Anchors = [akTop, akRight]
              BiDiMode = bdLeftToRight
              Caption = '00:00'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentBiDiMode = False
              ParentFont = False
            end
            object LabelDiaryTimeLeftMealVal: TLabel
              Left = 165
              Top = 40
              Width = 37
              Height = 16
              Align = alCustom
              Alignment = taRightJustify
              Anchors = [akTop, akRight]
              BiDiMode = bdLeftToRight
              Caption = '00:00'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentBiDiMode = False
              ParentFont = False
            end
            object LabelDiaryTimeLeftMeal: TLabel
              Left = 2
              Top = 41
              Width = 148
              Height = 16
              Align = alCustom
              BiDiMode = bdLeftToRight
              Caption = 'LabelDiaryTimeLeftMeal'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentBiDiMode = False
              ParentFont = False
            end
            object LabelDiaryTimeLeftIns: TLabel
              Left = 2
              Top = 24
              Width = 135
              Height = 16
              Align = alCustom
              BiDiMode = bdLeftToRight
              Caption = 'LabelDiaryTimeLeftIns'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentBiDiMode = False
              ParentFont = False
            end
            object LabelDiaryFinger: TLabel
              Left = 0
              Top = 57
              Width = 104
              Height = 16
              Caption = 'LabelDiaryFinger'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LabelDiaryFingerVal: TLabel
              Left = 189
              Top = 56
              Width = 19
              Height = 16
              Cursor = crHelp
              Alignment = taRightJustify
              Anchors = [akTop, akRight]
              Caption = 'XX'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
            end
            object ImageMinMaxTimes: TImage
              Tag = 1
              Left = 167
              Top = 0
              Width = 25
              Height = 16
              Hint = #1057#1074#1077#1088#1085#1091#1090#1100
              Anchors = [akTop, akRight]
              AutoSize = True
              ParentShowHint = False
              ShowHint = True
              Transparent = True
              OnMouseDown = ImageMinMaxTimesMouseDown
            end
          end
          object PanelDevelopment: TPanel
            Left = 16
            Top = 656
            Width = 193
            Height = 73
            Caption = 'DEV'
            Color = clBlack
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -27
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 4
            OnClick = PanelDevelopmentClick
          end
        end
        object PanelDiaryDish: TPanel
          Left = 968
          Top = 0
          Width = 265
          Height = 837
          Align = alRight
          BevelOuter = bvLowered
          BorderWidth = 10
          Color = clWhite
          ParentBackground = False
          TabOrder = 2
          object ShapeRight3: TShape
            Left = 11
            Top = 233
            Width = 243
            Height = 10
            Align = alTop
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object ShapeRight2: TShape
            Left = 11
            Top = 140
            Width = 243
            Height = 10
            Align = alTop
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object GroupBoxMeal: TGroupBox
            Left = 11
            Top = 11
            Width = 243
            Height = 129
            Align = alTop
            Caption = #1055#1088#1080#1105#1084' '#1087#1080#1097#1080
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            TabOrder = 0
            object LabelDiaryMealProts: TLabel
              Left = 16
              Top = 25
              Width = 127
              Height = 16
              Caption = 'LabelDiaryMealProts'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LabelDiaryMealCarbs: TLabel
              Left = 16
              Top = 57
              Width = 132
              Height = 16
              Caption = 'LabelDiaryMealCarbs'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LabelDiaryMealFats: TLabel
              Left = 16
              Top = 41
              Width = 122
              Height = 16
              Caption = 'LabelDiaryMealFats'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LabelDiaryMealValue: TLabel
              Left = 16
              Top = 80
              Width = 131
              Height = 16
              Caption = 'LabelDiaryMealValue'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LabelDiaryMealMass: TLabel
              Left = 16
              Top = 96
              Width = 129
              Height = 16
              Caption = 'LabelDiaryMealMass'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
          end
          object CorrectCarbs: TGroupBox
            Left = 11
            Top = 243
            Width = 243
            Height = 583
            Align = alClient
            Caption = #1055#1086#1076#1073#1086#1088' '#1091#1075#1083#1077#1074#1086#1076#1086#1074
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            TabOrder = 1
            object PanelCorrect: TPanel
              Left = 2
              Top = 18
              Width = 239
              Height = 563
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 8
              Color = clWhite
              TabOrder = 0
              object LabelCorrectionEmpty: TLabel
                Left = 8
                Top = 8
                Width = 223
                Height = 16
                Align = alTop
                Caption = 'LabelCorrectionEmpty'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -15
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
              end
              object ListCorrectCarbs: TListView
                Left = 8
                Top = 24
                Width = 223
                Height = 531
                Align = alClient
                BorderStyle = bsNone
                Columns = <
                  item
                    Caption = #1044#1086#1079#1072
                    MinWidth = 50
                    Width = 62
                  end
                  item
                    AutoSize = True
                    Caption = #1050#1086#1088#1088#1077#1082#1094#1080#1103
                    MinWidth = 70
                  end
                  item
                    Caption = #1048#1090#1086#1075#1086
                    Width = 55
                  end>
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -15
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                HideSelection = False
                ReadOnly = True
                RowSelect = True
                ParentFont = False
                TabOrder = 0
                TabStop = False
                ViewStyle = vsReport
                OnExit = ResetTabStop
              end
            end
          end
          object GroupBoxDose: TGroupBox
            Left = 11
            Top = 150
            Width = 243
            Height = 83
            Align = alTop
            Caption = #1048#1085#1089#1091#1083#1080#1085
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            TabOrder = 2
            object LabelDiaryMealDose: TLabel
              Left = 16
              Top = 25
              Width = 148
              Height = 16
              Caption = 'LabelDiaryMealDose'
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object LabelDiaryCorrection: TLabel
              Left = 16
              Top = 41
              Width = 127
              Height = 16
              Hint = #1053#1077' '#1088#1077#1082#1086#1084#1077#1085#1076#1091#1077#1090#1089#1103' '#1080#1079#1084#1077#1085#1103#1090#1100' '#1057#1050' '#1090#1072#1082' '#1088#1077#1079#1082#1086
              Caption = 'LabelDiaryCorrection'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGray
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LabelDiaryMealExpectedBS: TLabel
              Left = 16
              Top = 57
              Width = 171
              Height = 16
              Caption = 'LabelDiaryMealExpectedBS'
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
          end
        end
      end
      object TabBase: TTabSheet
        Hint = #1041#1072#1079#1072' (Ctrl+B)'
        Caption = #1041#1072#1079#1099
        ImageIndex = 1
        ParentShowHint = False
        ShowHint = False
        object SplitterBase: TSplitter
          Left = 553
          Top = 0
          Height = 837
          Color = clSkyBlue
          ParentColor = False
          OnCanResize = SplitterBaseCanResize
          OnMoved = SplitterBaseMoved
        end
        object PanelBaseFood: TPanel
          Left = 0
          Top = 0
          Width = 553
          Height = 837
          Align = alLeft
          BevelOuter = bvLowered
          BorderWidth = 8
          Color = clWhite
          DragKind = dkDock
          ParentBackground = False
          TabOrder = 0
          object LabelFoodBase: TLabel
            Left = 9
            Top = 9
            Width = 121
            Height = 16
            Align = alTop
            Caption = #1041#1072#1079#1072' '#1087#1088#1086#1076#1091#1082#1090#1086#1074
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
          end
          object PanelFoodButtons: TPanel
            Left = 9
            Top = 770
            Width = 535
            Height = 58
            Align = alBottom
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 2
            DesignSize = (
              535
              58)
            object ButtonCreateFood: TSpeedButton
              Left = 0
              Top = 12
              Width = 160
              Height = 40
              Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1088#1086#1076#1091#1082#1090
              Caption = #1053#1086#1074#1099#1081' '#1087#1088#1086#1076#1091#1082#1090
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              OnClick = ButtonCreateFoodClick
            end
            object ButtonDeleteFood: TSpeedButton
              Left = 368
              Top = 12
              Width = 160
              Height = 41
              Hint = #1059#1076#1072#1083#1080#1090#1100' '#1087#1088#1086#1076#1091#1082#1090' (Delete)'
              Anchors = [akTop, akRight]
              Caption = #1059#1076#1072#1083#1080#1090#1100
              Enabled = False
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              NumGlyphs = 2
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              OnClick = ButtonDeleteFoodClick
            end
          end
          object ListFood: TListView
            Tag = 1
            Left = 9
            Top = 63
            Width = 535
            Height = 707
            Align = alClient
            Columns = <
              item
                AutoSize = True
                Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
                MinWidth = 150
              end
              item
                Caption = #1041
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end
              item
                Caption = #1046
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end
              item
                Caption = #1059
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end
              item
                Caption = #1082#1082#1072#1083
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end
              item
                Caption = #1043#1048
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end>
            GridLines = True
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            SmallImages = DataInterface.Images_BaseContent
            TabOrder = 0
            ViewStyle = vsReport
            OnColumnRightClick = ListBaseColumnRightClick
            OnData = ListFoodData
            OnDblClick = ListFoodDblClick
            OnKeyDown = TableFoodBaseKeyDown
            OnKeyPress = ListFoodKeyPress
            OnMouseDown = ListBaseMouseDown
          end
          object PanelSearchFoodBase: TPanel
            Left = 9
            Top = 25
            Width = 535
            Height = 38
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 1
            DesignSize = (
              535
              38)
            object ButtonResetFilterFoodBase: TSpeedButton
              Left = 506
              Top = 4
              Width = 28
              Height = 28
              Hint = '123'
              Anchors = [akTop, akRight]
              Enabled = False
              Flat = True
              Glyph.Data = {
                B60D0000424DB60D000000000000360000002800000030000000180000000100
                180000000000800D000074120000741200000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA
                FAFAE3E3E3CECECEC0C0C0BABABABABABABEBEBECBCBCBDEDEDEF7F7F7FFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFAFAFAE3E3E3CECECEC0C0C0BABABABABABABEBEBECBCBCBDE
                DEDEF7F7F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFDFDFDDFDFDFB1B7C3718FD6396DEB1558F9034DFF034D
                FF0E54FB3068EE6185DBA6AFC4D7D7D7FBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFDDFDFDFB9B9B99D9D9D868686
                777777707070707070747474828282969696B3B3B3D7D7D7FBFBFBFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFAC6C8CC688ADA11
                58FB0651FF135EFF206BFF2771FF2771FF226CFF1761FF0651FF0A53FD527EE2
                B8BDC7F5F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFA
                FAC9C9C99999997777777272727B7B7B8383838888888888888484847D7D7D72
                7272737373919191BFBFBFF5F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFCFCFCB9BECA386FEE0D56FF2469FF245EFF2052FF1D4CF81C49EF1C49
                EF1D4BF51F51FF235BFF296DFF0D56FF2564F5A7B1C9F5F5F5FFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFCFCFCC0C0C08787877676768484848080807B7B7B
                7676767171717171717474747A7A7A7F7F7F8787877676767F7F7FB6B6B6F5F5
                F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C9D1306BF31259FF275FFF20
                4EF71F4CF11F4CF11F4CF11F4CF11F4CF11F4CF11F4CF11F4CF11F4DF3255BFF
                145AFF2062F9ACB6CDFCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCBCBCB8585
                8579797982828277777774747474747474747474747474747474747474747474
                74747575758080807A7A7A7E7E7EBABABAFCFCFCFFFFFFFFFFFFFFFFFFFFFFFF
                EBEBEB497BEA175BFF2A5EFF285AFF295DFF285CFF2758FF2451F82451F82451
                F82451F82759FF295CFF295DFF2758FF2759FF195AFF336EF4D5D8E0FFFFFFFF
                FFFFFFFFFFFFFFFFEBEBEB8F8F8F7B7B7B8282828080808282828181817F7F7F
                7A7A7A7A7A7A7A7A7A7A7A7A8080808181818282827F7F7F8080807B7B7B8787
                87DADADAFFFFFFFFFFFFFFFFFFFFFFFF95AAD71E63FF2A59FF2B5CFF99B3FFFF
                FFFFFFFFFF2F64FF2C5DFF2855FA2855FA2D5FFF2F65FFFFFFFFFFFFFF99B2FF
                2B5AFF2856FD1C5DFF7193DDFAFAFAFFFFFFFFFFFFFFFFFFB2B2B28080808181
                81828282C4C4C4FFFFFFFFFFFF8686868383837D7D7D7D7D7D848484868686FF
                FFFFFFFFFFC3C3C38181817E7E7E7D7D7DA0A0A0FAFAFAFFFFFFFFFFFFF0F0F0
                3F7AF32355FF2B57F72D5BFFFFFFFFFFFFFFFFFFFFFFFFFF3469FF3366FF3266
                FF3367FFFFFFFFFFFFFFFFFFFFFFFFFF2D5CFF2B57F72957FF2C6DFCD6DBE5FF
                FFFFFFFFFFF0F0F08F8F8F7D7D7D7E7E7E828282FFFFFFFFFFFFFFFFFFFFFFFF
                898989888888888888888888FFFFFFFFFFFFFFFFFFFFFFFF8383837E7E7E8080
                80878787DDDDDDFFFFFFFFFFFFBAC8E62661FF305BF9305BF9315CFDFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF3D74FF3C71FFEEF2FFFFFFFFFFFFFFFFFFFFFFFFFF
                305CFB305BF9305BF92358FF90ACE4FFFFFFFFFFFFCDCDCD8282828181818181
                81838383FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9090908F8F8FF5F5F5FFFFFFFF
                FFFFFFFFFFFFFFFF8282828181818181817E7E7EB5B5B5FFFFFFFFFFFF8AAAE7
                2251F83460FB3561FE3562FF3562FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFF3562FF3562FF3460FC3460FB2754F26491ECFF
                FFFFFFFFFFB4B4B4797979858585878787878787878787FFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8787878787878585858585
                857A7A7AA0A0A0FFFFFFFFFFFF6D94ED2A55EE3965FF3A66FF3A66FF3A66FF3A
                66FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3A66FF3A66FF
                3A66FF3A65FF3964FD325DF64A78F4FFFFFFFFFFFFA5A5A57A7A7A8A8A8A8A8A
                8A8A8A8A8A8A8A8A8A8AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFF8A8A8A8A8A8A8A8A8A8A8A8A898989828282929292FFFFFFFFFFFF6288F1
                325DF43F6BFF3F6BFF3F6BFF3F6BFF3F6BFF3F6BFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFF3F6BFF3F6BFF3F6BFF3F6BFF3F6BFF3F6AFF3964FA416DF8FF
                FFFFFFFFFF9E9E9E8181818E8E8E8E8E8E8E8E8E8E8E8E8E8E8E8E8E8EFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8E8E8E8E8E8E8E8E8E8E8E8E8E8E8E8D8D
                8D8888888D8D8DFFFFFFFFFFFF698BF23762F9446FFF4570FF446FFF446FFF44
                6FFF4570FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4774FF4774FF446FFF
                446FFF4570FF446FFF3F6AFF456EF8FFFFFFFFFFFFA2A2A28686869191919191
                91919191919191919191919191FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF93
                93939393939191919191919191919191918D8D8D8E8E8EFFFFFFFFFFFF84A1F5
                3864FB4975FF4975FF4A77FF5081FF5585FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFF5386FF4F80FF4975FF4975FF4976FF3F6BFF5F82F6FF
                FFFFFFFFFFB3B3B38888889494949494949595959B9B9B9E9E9EFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9D9D9D9A9A9A9494949494949595
                958E8E8E9D9D9DFFFFFFFFFFFFBACBFC3B66FD4F7BFF4E79FF5584FF6E98FFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5A8AFF
                5483FF4E7AFF4F7BFF3C67FE92AAF8FFFFFFFFFFFFD6D6D68A8A8A9898989797
                979D9D9DACACACFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFA1A1A19D9D9D9898989898988B8B8BBCBCBCFFFFFFFFFFFFF2F6FF
                4470FE5380FF537FFF5784FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF527CFF527C
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5988FF5480FF5582FF406DFFD6E1FFFF
                FFFFFFFFFFF8F8F89191919B9B9B9B9B9B9E9E9EFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF9A9A9A9A9A9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA0A0A09C9C9C9D9D
                9D8F8F8FE7E7E7FFFFFFFFFFFFFFFFFF8CA6FC4673FF5A85FF5883FFFFFFFFFF
                FFFFFFFFFFFFFFFF5781FF5781FF5781FF5781FFFFFFFFFFFFFFFFFFFFFFFFFF
                5B87FF5B86FF4D7AFF698CFCFFFFFFFFFFFFFFFFFFFFFFFFBABABA9393939F9F
                9F9E9E9EFFFFFFFFFFFFFFFFFFFFFFFF9D9D9D9D9D9D9D9D9D9D9D9DFFFFFFFF
                FFFFFFFFFFFFFFFFA0A0A0A0A0A0979797A6A6A6FFFFFFFFFFFFFFFFFFFFFFFF
                ECF1FF5580FE5581FF5E8AFFAEC3FFFFFFFFFFFFFF5B85FF5B85FF5B85FF5B85
                FF5B85FF5B85FFFFFFFFFFFFFFB0C5FF5F8BFF5D8AFF4D7AFFD5DFFFFFFFFFFF
                FFFFFFFFFFFFFFFFF4F4F49C9C9C9C9C9CA2A2A2D0D0D0FFFFFFFFFFFFA0A0A0
                A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0FFFFFFFFFFFFD1D1D1A3A3A3A2A2A29797
                97E6E6E6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCAD8FF5280FF608DFF638FFF61
                8BFF608AFF5F89FF5F89FF5F89FF5F89FF5F89FF5F89FF608AFF628DFF6491FF
                6795FF5381FFAAC1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE0E0E09B9B
                9BA4A4A4A6A6A6A4A4A4A3A3A3A2A2A2A2A2A2A2A2A2A2A2A2A2A2A2A2A2A2A3
                A3A3A5A5A5A7A7A7A9A9A99C9C9CCECECEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFB6CBFF5987FF5E8CFF6995FF6692FF648FFF648FFF648EFF648F
                FF648FFF6590FF6793FF6A97FF6694FF5887FF99B6FFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFD5D5D5A0A0A0A3A3A3AAAAAAA8A8A8A6A6A6
                A6A6A6A6A6A6A6A6A6A6A6A6A7A7A7A8A8A8ABABABA8A8A89F9F9FC5C5C5FFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD1DFFF6895FF5B
                8AFF6C9BFF6E9BFF6C99FF6C99FF6C99FF6D9BFF6F9DFF709FFF5D8DFF6191FF
                B8CDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFE5E5E5A9A9A9A1A1A1ADADADADADADACACACACACACACACACADADADAEAEAEAF
                AFAFA3A3A3A6A6A6D7D7D7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFF6F9FFABC5FF6C99FF6291FF6190FF6392FF6594
                FF6191FF6292FF6796FF9EBCFFEDF2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFAD0D0D0ACACACA6A6A6
                A5A5A5A7A7A7A8A8A8A6A6A6A6A6A6A9A9A9C8C8C8F5F5F5FFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFBFDFFD6E3FFC2D6FFA9C5FFA4C2FFC2D6FFD1E0FFF4F8FFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFDFDFDE8E8E8DDDDDDCFCFCFCCCCCCDDDDDDE5E5E5F9
                F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              NumGlyphs = 2
              ParentShowHint = False
              ShowHint = True
              Transparent = False
              OnClick = ButtonResetFilterFoodBaseClick
            end
            object EditBaseFoodSearch: TEdit
              Left = 0
              Top = 8
              Width = 500
              Height = 24
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              OnChange = EditBaseFoodSearchChange
              OnKeyDown = EditBaseFoodSearchKeyDown
              OnKeyPress = EditBaseFoodSearchKeyPress
            end
          end
        end
        object PanelBaseDish: TPanel
          Left = 556
          Top = 0
          Width = 677
          Height = 837
          Align = alClient
          BevelOuter = bvLowered
          BorderWidth = 8
          Color = 16710908
          ParentBackground = False
          TabOrder = 1
          object Shape11: TShape
            Left = 9
            Top = 25
            Width = 659
            Height = 7
            Align = alTop
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object PanelDishHeader: TPanel
            Left = 9
            Top = 9
            Width = 659
            Height = 16
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            object LabelDishBase: TLabel
              Left = 0
              Top = 0
              Width = 80
              Height = 16
              Caption = #1041#1072#1079#1072' '#1073#1083#1102#1076
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = False
            end
          end
          object PanelDishButtons: TPanel
            Left = 9
            Top = 770
            Width = 659
            Height = 58
            Align = alBottom
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 1
            DesignSize = (
              659
              58)
            object ButtonCreateDish: TSpeedButton
              Left = 0
              Top = 12
              Width = 160
              Height = 42
              Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1086#1077' '#1073#1083#1102#1076#1086
              Caption = #1053#1086#1074#1086#1077' '#1073#1083#1102#1076#1086
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              OnClick = ButtonCreateDishClick
            end
            object ButtonDeleteDish: TSpeedButton
              Left = 963
              Top = 12
              Width = 160
              Height = 42
              Hint = #1059#1076#1072#1083#1080#1090#1100' '#1073#1083#1102#1076#1086' (Delete)'
              Anchors = [akTop, akRight]
              Caption = #1059#1076#1072#1083#1080#1090#1100
              Enabled = False
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              NumGlyphs = 2
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              OnClick = ButtonDeleteDishClick
            end
          end
          object ListDish: TListView
            Tag = 2
            Left = 9
            Top = 32
            Width = 659
            Height = 738
            Align = alClient
            Columns = <
              item
                AutoSize = True
                Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
                MinWidth = 120
              end
              item
                Caption = #1052#1072#1089#1089#1072
                MaxWidth = 100
                MinWidth = 60
                Width = 74
              end
              item
                Caption = #1041
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end
              item
                Caption = #1046
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end
              item
                Caption = #1059
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end
              item
                Caption = #1082#1082#1072#1083
                MaxWidth = 100
                MinWidth = 50
                Width = 62
              end
              item
                Caption = #1044#1072#1090#1072
              end>
            GridLines = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            SmallImages = DataInterface.Images_BaseContent
            TabOrder = 2
            ViewStyle = vsReport
            OnColumnRightClick = ListBaseColumnRightClick
            OnDblClick = ListDishDblClick
            OnKeyDown = ListDishKeyDown
            OnKeyPress = ListFoodKeyPress
            OnMouseDown = ListBaseMouseDown
          end
        end
      end
      object TabAnalyze: TTabSheet
        Hint = #1040#1085#1072#1083#1080#1079' (Ctrl+G)'
        Caption = #1040#1085#1072#1083#1080#1079
        ImageIndex = 2
        ParentShowHint = False
        ShowHint = False
        object PanelAnManagment: TPanel
          Left = 0
          Top = 0
          Width = 249
          Height = 837
          Align = alLeft
          BevelOuter = bvLowered
          BorderWidth = 8
          Color = clWhite
          ParentBackground = False
          TabOrder = 0
          object ShapeAn1: TShape
            Left = 9
            Top = 369
            Width = 231
            Height = 7
            Align = alTop
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object Shape1: TShape
            Left = 9
            Top = 265
            Width = 231
            Height = 7
            Align = alTop
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object TheKoofsGroupBox: TGroupBox
            Left = 9
            Top = 272
            Width = 231
            Height = 97
            Align = alTop
            Caption = #1044#1077#1081#1089#1090#1074#1080#1103
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            DesignSize = (
              231
              97)
            object LabelAnCompValue: TLabel
              Left = 9
              Top = 41
              Width = 39
              Height = 16
              Caption = '             '
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object ButtonConfigKoof: TButton
              Left = 9
              Top = 56
              Width = 208
              Height = 24
              Action = ActionSettingsAnalyze
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
            end
            object ButtonUpdateKoof: TButton
              Left = 9
              Top = 24
              Width = 208
              Height = 24
              Anchors = [akLeft, akTop, akRight]
              Caption = #1055#1077#1088#1077#1089#1095#1080#1090#1072#1090#1100
              TabOrder = 1
              OnClick = ButtonUpdateKoofClick
            end
          end
          object GroupBoxInfo: TGroupBox
            Left = 9
            Top = 9
            Width = 231
            Height = 256
            Align = alTop
            Caption = #1055#1088#1086#1089#1084#1086#1090#1088
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 1
            DesignSize = (
              231
              256)
            object LabelKoofDiscription: TLabel
              Left = 9
              Top = 177
              Width = 214
              Height = 64
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1082#1086#1101#1092#1092#1080#1094#1080#1077#1085#1090
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGrayText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object Label2: TLabel
              Left = 8
              Top = 24
              Width = 101
              Height = 16
              Caption = #1052#1077#1090#1086#1076' '#1072#1085#1072#1083#1080#1079#1072
              Color = clWindow
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentColor = False
              ParentFont = False
            end
            object Label3: TLabel
              Left = 8
              Top = 128
              Width = 93
              Height = 16
              Caption = #1050#1086#1101#1092#1092#1080#1094#1080#1077#1085#1090
              Color = clWindow
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentColor = False
              ParentFont = False
            end
            object LabelCalcTime: TLabel
              Left = 8
              Top = 72
              Width = 92
              Height = 16
              Caption = 'LabelCalcTime'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGrayText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LabelAvgDeviation: TLabel
              Left = 8
              Top = 88
              Width = 115
              Height = 16
              Caption = 'LabelAvgDeviation'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGrayText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LabelWeight: TLabel
              Left = 8
              Top = 104
              Width = 76
              Height = 16
              Caption = 'LabelWeight'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGrayText
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object ComboAnalyzers: TComboBox
              Left = 9
              Top = 41
              Width = 206
              Height = 22
              Style = csOwnerDrawFixed
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ItemHeight = 16
              ParentFont = False
              TabOrder = 0
              OnChange = ComboKoofChange
              Items.Strings = (
                #1050#1086#1084#1073#1080#1085#1080#1088#1086#1074#1072#1085#1085#1099#1081)
            end
            object ComboKoof: TComboBox
              Left = 9
              Top = 145
              Width = 206
              Height = 22
              Style = csOwnerDrawFixed
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ItemHeight = 16
              ParentFont = False
              TabOrder = 1
              OnChange = ComboKoofChange
              Items.Strings = (
                #1059#1075#1083#1077#1074#1086#1076#1085#1099#1081' '#1082#1086#1101#1092#1092#1080#1094#1080#1077#1085#1090
                #1062#1045#1048
                #1041#1077#1083#1082#1086#1074#1099#1081' '#1082#1086#1101#1092#1092#1080#1094#1080#1077#1085#1090
                #1054#1050#1050)
            end
          end
        end
        object PanelImageLarge: TPanel
          Left = 249
          Top = 0
          Width = 984
          Height = 837
          Align = alClient
          BevelOuter = bvLowered
          Caption = #1053#1077#1090' '#1075#1088#1072#1092#1080#1082#1072
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object ImageLarge: TImage
            Left = 1
            Top = 1
            Width = 982
            Height = 835
            Align = alClient
          end
        end
      end
      object TabStat: TTabSheet
        Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ImageIndex = 5
        ParentFont = False
        object GroupBoxBSHistory: TGroupBox
          Left = 0
          Top = 0
          Width = 225
          Height = 837
          Align = alLeft
          Caption = #1056#1077#1082#1086#1088#1076#1099' '#1057#1050
          TabOrder = 0
          DesignSize = (
            225
            837)
          object ButtonBSHistory: TButton
            Left = 9
            Top = 25
            Width = 208
            Height = 24
            Caption = #1055#1086#1089#1090#1088#1086#1080#1090#1100' '#1089#1087#1080#1089#1086#1082
            TabOrder = 0
            OnClick = ButtonBSHistoryClick
          end
          object ButtonBSList: TButton
            Left = 9
            Top = 57
            Width = 208
            Height = 24
            Caption = #1055#1086#1089#1090#1088#1086#1080#1090#1100' '#1088#1072#1089#1087#1088#1077#1076#1077#1083#1077#1085#1080#1077
            TabOrder = 1
            OnClick = ButtonBSListClick
          end
          object ListBS: TListBox
            Left = 2
            Top = 296
            Width = 221
            Height = 539
            Align = alBottom
            Anchors = [akLeft, akTop, akRight, akBottom]
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Courier New'
            Font.Style = []
            ItemHeight = 17
            ParentFont = False
            TabOrder = 2
            OnDblClick = ListBSDblClick
          end
          object ButtonAvgBSDynamic: TButton
            Left = 9
            Top = 89
            Width = 208
            Height = 24
            Anchors = [akLeft, akTop, akRight]
            Caption = 'ButtonAvgBSDynamic'
            TabOrder = 3
            OnClick = ButtonAvgBSDynamicClick
          end
          object ButtonInsList: TButton
            Left = 8
            Top = 136
            Width = 209
            Height = 25
            Caption = #1048#1085#1089#1091#1083#1080#1085#1086#1087#1086#1090#1088#1077#1073#1085#1086#1089#1090#1100
            TabOrder = 4
            OnClick = ButtonInsListClick
          end
          object ButtonAnList: TButton
            Left = 8
            Top = 168
            Width = 209
            Height = 25
            Caption = 'AnList'
            TabOrder = 5
            OnClick = ButtonAnListClick
          end
          object ButtonAvgBS: TBitBtn
            Left = 8
            Top = 198
            Width = 209
            Height = 24
            Caption = #1057#1088#1077#1076#1085#1080#1081' '#1057#1050' ('#1084#1077#1089#1103#1094')'
            TabOrder = 6
            TabStop = False
            OnClick = ButtonAvgBSClick
            OnExit = ResetTabStop
          end
          object ButtonInsulinCalc: TButton
            Left = 8
            Top = 232
            Width = 209
            Height = 25
            Caption = #1056#1072#1089#1093#1086#1076' '#1080#1085#1089#1091#1083#1080#1085#1072
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 7
            OnClick = ButtonInsulinCalcClick
          end
        end
        object GroupBoxCarbsHistory: TGroupBox
          Left = 225
          Top = 0
          Width = 225
          Height = 837
          Align = alLeft
          Caption = #1056#1077#1082#1086#1088#1076#1099' '#1041#1046#1059
          TabOrder = 1
          DesignSize = (
            225
            837)
          object ListCB: TListBox
            Left = 2
            Top = 56
            Width = 221
            Height = 779
            Hint = #1050#1083#1080#1082#1085#1080#1090#1077' '#1076#1074#1072' '#1088#1072#1079#1072' '#1076#1083#1103' '#1087#1077#1088#1077#1093#1086#1076#1072
            Style = lbOwnerDrawFixed
            Align = alBottom
            Anchors = [akLeft, akTop, akRight, akBottom]
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Courier New'
            Font.Style = []
            ItemHeight = 17
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnDblClick = ListBSDblClick
          end
          object ComboValue: TComboBox
            Left = 16
            Top = 25
            Width = 193
            Height = 24
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 16
            TabOrder = 1
            OnChange = ComboValueChange
            Items.Strings = (
              #1041#1077#1083#1082#1080
              #1046#1080#1088#1099
              #1059#1075#1083#1077#1074#1086#1076#1099
              #1050#1072#1083#1086#1088#1080#1081#1085#1086#1089#1090#1100
              #1052#1072#1089#1089#1072)
          end
        end
      end
    end
  end
  object TimerTimeLeft: TTimer
    Enabled = False
    OnTimer = TimerTimeLeftTimer
    Left = 324
    Top = 171
  end
  object MenuHidden: TMainMenu
    Left = 148
    object MenuHiddenCuts: TMenuItem
      Caption = 'HiddenCuts'
      Visible = False
      object MenuItem_ShowDiaryPage: TMenuItem
        Caption = 'Show Diary Page'
        ShortCut = 16452
        Visible = False
        OnClick = MenuItem_ShowDiaryPageClick
      end
      object MenuItem_ShowBasePage: TMenuItem
        Caption = 'Show Base Page'
        ShortCut = 16450
        Visible = False
        OnClick = MenuItem_ShowBasePageClick
      end
      object MenuItem_ShowGraphicPage: TMenuItem
        Caption = 'Show Graphic Page'
        ShortCut = 16455
        Visible = False
        OnClick = MenuItem_ShowGraphicPageClick
      end
      object MenuItem_ShowTodolistPage: TMenuItem
        Caption = 'Show Todolist Page'
        ShortCut = 16468
        Visible = False
      end
      object MenuItem_SaveToDoList: TMenuItem
        Caption = 'Save ToDoList'
        ShortCut = 16467
      end
    end
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Items = <
                  item
                    Action = ActionAddBlood
                    ImageIndex = 9
                    ShortCut = 32817
                  end
                  item
                    Action = ActionAddIns
                    ImageIndex = 10
                    ShortCut = 32818
                  end
                  item
                    Action = ActionAddMeal
                    ImageIndex = 11
                    ShortCut = 32819
                  end
                  item
                    Action = ActionAddNote
                    ImageIndex = 12
                    ShortCut = 32820
                  end>
                Caption = #1053#1086#1074#1072#1103' '#1079#1072#1087#1080#1089#1100
                UsageCount = 1
              end
              item
                Caption = '-'
              end
              item
                Items = <
                  item
                    Action = ActionExportDiary
                    ImageIndex = 0
                    ShortCut = 16453
                  end
                  item
                    Visible = False
                    Action = ActionExportFood
                  end
                  item
                    Action = ActionExportKoofs
                  end>
                Caption = #1069#1082#1089#1087#1086#1088#1090
                UsageCount = 1
              end
              item
                Visible = False
                Action = ActionPrint
                ImageIndex = 1
                ShortCut = 16464
              end
              item
                Action = ActionSync
                ImageIndex = 2
              end
              item
                Action = ActionSettings
                ImageIndex = 3
                ShortCut = 121
              end
              item
                Caption = '-'
              end
              item
                Action = ActionExit
                ImageIndex = 4
                ShortCut = 32883
              end>
            Caption = #1060#1072#1081#1083
          end
          item
            Items = <
              item
                Action = ActionHelp
                ImageIndex = 6
                ShortCut = 112
              end
              item
                Caption = '-'
              end
              item
                Action = ActionCheckUpdate
                ImageIndex = 8
              end
              item
                Action = ActionAbout
                ImageIndex = 7
                ShortCut = 113
              end>
            Caption = #1057#1087#1088#1072#1074#1082#1072
          end>
        ActionBar = MainMenu
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = ActionAddBlood
            ImageIndex = 8
            ShortCut = 32817
          end
          item
            Action = ActionAddIns
            ImageIndex = 9
            ShortCut = 32818
          end
          item
            Action = ActionAddMeal
            ImageIndex = 10
            ShortCut = 32819
          end
          item
            Action = ActionAddNote
            ImageIndex = 11
            ShortCut = 32820
          end
          item
            Caption = '-'
          end
          item
            Action = ActionSync
            ImageIndex = 2
          end
          item
            Caption = '-'
          end
          item
            Action = ActionSettings
            ImageIndex = 3
            ShortCut = 121
          end>
      end>
    Images = DataInterface.Images_Menu
    Left = 239
    Top = 83
    StyleName = 'XP Style'
    object ActionSync: TAction
      Category = #1060#1072#1081#1083
      Caption = #1057#1080#1085#1093#1088#1086#1085#1080#1079#1080#1088#1086#1074#1072#1090#1100
      ImageIndex = 2
      OnExecute = ActionSyncExecute
    end
    object ActionSettings: TAction
      Category = #1060#1072#1081#1083
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      ImageIndex = 3
      ShortCut = 121
      OnExecute = ActionSettingsExecute
    end
    object ActionAddBlood: TAction
      Tag = 1
      Category = #1053#1086#1074#1072#1103' '#1079#1072#1087#1080#1089#1100
      Caption = #1047#1072#1084#1077#1088' '#1057#1050'...'
      ImageIndex = 9
      ShortCut = 32817
      OnExecute = ActionAddRecordExecute
    end
    object ActionAddIns: TAction
      Tag = 2
      Category = #1053#1086#1074#1072#1103' '#1079#1072#1087#1080#1089#1100
      Caption = #1048#1085#1098#1077#1082#1094#1080#1103'...'
      ImageIndex = 10
      ShortCut = 32818
      OnExecute = ActionAddRecordExecute
    end
    object ActionAddMeal: TAction
      Tag = 3
      Category = #1053#1086#1074#1072#1103' '#1079#1072#1087#1080#1089#1100
      Caption = #1055#1088#1080#1105#1084' '#1087#1080#1097#1080'...'
      ImageIndex = 11
      ShortCut = 32819
      OnExecute = ActionAddRecordExecute
    end
    object ActionAddNote: TAction
      Tag = 4
      Category = #1053#1086#1074#1072#1103' '#1079#1072#1087#1080#1089#1100
      Caption = #1047#1072#1084#1077#1090#1082#1072'...'
      ImageIndex = 12
      ShortCut = 32820
      OnExecute = ActionAddRecordExecute
    end
    object ActionExportDiary: TAction
      Category = #1069#1082#1089#1087#1086#1088#1090
      Caption = #1044#1085#1077#1074#1085#1080#1082'...'
      ImageIndex = 0
      ShortCut = 16453
      OnExecute = ActionExportDiaryExecute
    end
    object ActionPrint: TAction
      Category = #1060#1072#1081#1083
      Caption = #1055#1077#1095#1072#1090#1100'...'
      ImageIndex = 1
      ShortCut = 16464
    end
    object ActionExit: TAction
      Category = #1060#1072#1081#1083
      Caption = #1042#1099#1093#1086#1076
      ImageIndex = 4
      ShortCut = 32883
      OnExecute = ActionExitExecute
    end
    object ActionHelp: TAction
      Category = #1057#1087#1088#1072#1074#1082#1072
      Caption = #1055#1086#1084#1086#1097#1100
      ImageIndex = 6
      ShortCut = 112
    end
    object ActionAbout: TAction
      Category = #1057#1087#1088#1072#1074#1082#1072
      Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077'...'
      ImageIndex = 7
      ShortCut = 113
      OnExecute = ActionAboutExecute
    end
    object ActionExportFood: TAction
      Category = #1069#1082#1089#1087#1086#1088#1090
      Caption = #1041#1072#1079#1072' '#1087#1088#1086#1076#1091#1082#1090#1086#1074
      OnExecute = ActionExportFoodExecute
    end
    object ActionExportKoofs: TAction
      Category = #1069#1082#1089#1087#1086#1088#1090
      Caption = #1050#1086#1101#1092#1092#1080#1094#1080#1077#1085#1090#1099
      OnExecute = ActionExportKoofsExecute
    end
    object ActionBalanceOnOff: TAction
      Category = 'Popup: '#1089#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Caption = #1041#1072#1083#1072#1085#1089#1080#1088#1086#1074#1072#1090#1100
      OnExecute = ActionBalanceOnOffExecute
    end
    object ActionSettingsStat: TAction
      Category = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' ('#1085#1086#1088#1084#1099')...'
      ImageIndex = 3
      OnExecute = ActionSettingsStatExecute
    end
    object ActionEditBlood: TAction
      Category = 'Popup: '#1087#1072#1085#1077#1083#1080
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1079#1072#1084#1077#1088' '#1057#1050'...'
      ImageIndex = 13
      OnExecute = ActionEditBloodExecute
    end
    object ActionEditIns: TAction
      Category = 'Popup: '#1087#1072#1085#1077#1083#1080
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1080#1085#1098#1077#1082#1094#1080#1102'...'
      ImageIndex = 13
      OnExecute = ActionEditInsExecute
    end
    object ActionEditMeal: TAction
      Category = 'Popup: '#1087#1072#1085#1077#1083#1080
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080'...'
      ImageIndex = 13
      OnExecute = ActionEditMealExecute
    end
    object ActionEditNote: TAction
      Category = 'Popup: '#1087#1072#1085#1077#1083#1080
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1079#1072#1084#1077#1090#1082#1091'...'
      ImageIndex = 13
      OnExecute = ActionEditNoteExecute
    end
    object ActionRemovePanel: TAction
      Category = 'Popup: '#1087#1072#1085#1077#1083#1080
      Caption = #1059#1076#1072#1083#1080#1090#1100
      ImageIndex = 14
      OnExecute = ActionRemovePanelExecute
    end
    object ActionBalanceMeal: TAction
      Category = 'Popup: '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080
      Caption = #1055#1086#1076#1086#1073#1088#1072#1090#1100' '#1084#1072#1089#1089#1099
      Visible = False
      OnExecute = ActionBalanceMealExecute
    end
    object ActionEditFood: TAction
      Category = 'Popup: '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1084#1072#1089#1089#1091'...'
      ImageIndex = 13
      OnExecute = ActionEditFoodExecute
    end
    object ActionDeltaMass: TAction
      Category = 'Popup: '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080
      Caption = #1055#1088#1080#1073#1072#1074#1080#1090#1100'/'#1074#1099#1095#1077#1089#1090#1100'...'
      ImageIndex = 15
      OnExecute = ActionDeltaMassExecute
    end
    object ActionCalcMass: TAction
      Category = 'Popup: '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080
      Caption = #1055#1086#1076#1086#1073#1088#1072#1090#1100' '#1076#1086' '#1074#1074#1077#1076#1105#1085#1085#1086#1081' '#1076#1086#1079#1099
      ImageIndex = 16
      OnExecute = ActionCalcMassExecute
    end
    object ActionRemoveFood: TAction
      Category = 'Popup: '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080
      Caption = #1059#1076#1072#1083#1080#1090#1100
      ImageIndex = 14
      OnExecute = ActionRemoveFoodExecute
    end
    object ActionShowWindow: TAction
      Category = 'Popup: '#1090#1088#1077#1081
      Caption = #1054#1090#1082#1088#1099#1090#1100' '#1050#1086#1084#1087#1077#1085#1089#1072#1094#1080#1102
      OnExecute = ActionShowWindowExecute
    end
    object ActionHideWindow: TAction
      Category = 'Popup: '#1090#1088#1077#1081
      Caption = #1057#1074#1077#1088#1085#1091#1090#1100' '#1050#1086#1084#1087#1077#1085#1089#1072#1094#1080#1102
      OnExecute = ActionHideWindowExecute
    end
    object ActionIsMinToTray: TAction
      Category = 'Popup: '#1090#1088#1077#1081
      Caption = #1057#1074#1086#1088#1072#1095#1080#1074#1072#1090#1100' '#1074' '#1090#1088#1077#1081
      OnExecute = ActionIsMinToTrayExecute
    end
    object ActionCheckUpdate: TAction
      Category = #1057#1087#1088#1072#1074#1082#1072
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103
      Enabled = False
      ImageIndex = 8
      OnExecute = ActionCheckUpdateExecute
    end
    object ActionSettingsImage: TAction
      Category = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' ('#1074#1080#1076')...'
      ImageIndex = 3
      OnExecute = ActionSettingsImageExecute
    end
    object ActionSettingsAnalyze: TAction
      Category = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' ('#1072#1085#1072#1083#1080#1079')...'
      ImageIndex = 3
      OnExecute = ActionSettingsAnalyzeExecute
    end
    object ActionShortMeal: TAction
      Category = 'Popup: '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080
      AutoCheck = True
      Caption = #1057#1086#1082#1088#1072#1097#1105#1085#1085#1099#1081' '#1087#1086#1089#1090#1087#1088#1072#1085#1076#1080#1072#1083
      OnExecute = ActionShortMealExecute
    end
    object ActionMoveMealBack: TAction
      Category = 'Popup: '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080
      Caption = '<< '#1053#1072' '#1076#1077#1085#1100' '#1085#1072#1079#1072#1076
      OnExecute = ActionMoveMealBackExecute
    end
    object ActionMoveMealForward: TAction
      Category = 'Popup: '#1087#1088#1080#1105#1084' '#1087#1080#1097#1080
      Caption = #1053#1072' '#1076#1077#1085#1100' '#1074#1087#1077#1088#1105#1076' >>'
      OnExecute = ActionMoveMealForwardExecute
    end
  end
  object TrayIcon: TCoolTrayIcon
    CycleInterval = 0
    Hint = #1050#1086#1084#1087#1077#1085#1089#1072#1094#1080#1103
    Icon.Data = {
      0000010001004040000000000000281600001600000028000000400000008000
      000001000800000000000012000000000000000000000001000000000000FFFF
      FF00B0B0B0003060A000F8F8F800C0C0C000F0F0F00030A0D0003060A8000000
      0000D0D0D00030A0D800D8D8D8003060B000E0E0E000A8A8A800305090003058
      98003098D0004060B000A0A0A00040B0E00038A8E00038A8D8002850880038A0
      D8003098C8003090C8003090C00030A0E00030508800B8B8B800E8E8E800C8C8
      C8003088C000284880003050800090C0FF00909090003860B00030A8D8003890
      C800285890003860A8003080C0003088C8003090D0003070A80040B0E80040A8
      E0003898D00020407800385898003070B0003068A80030A8E0003080B8003060
      980028488800284878002040700038B0E00040B8E8003078B80038A0D00030B0
      E00040A0E0002898D0003888C8003078B0003858A0004068B800284070003048
      780040C0F0003098D8004090D00028689800305890003868B000205080002038
      70009898A0003898D8003058A0004070C0002858980020488000285080002858
      880038A8E800286090004098D00038A0E00040A0D8009898980040B0F0003088
      B80028588000285090003068A0006888C8003868A0004060A800204068003890
      D0003080B0002870A0003868A8004870B8003860A00030507800284870004060
      8800406080004080C0003878B80038B0E8002868A00040B8F0004088C8002860
      980040507000203878003868B8004068B0002050880020588800285078002040
      800070B0FF009090A0003880C0003870B0004098D8002870A8002860A0002840
      600020487000204060004070B800304888004870C00028407800204878003048
      80003050700090909800506888002898D8003878B00030609000306898004870
      B000304060004868B00038588000385888005070B800184870005078C8008888
      880088889000587088002878B000788098003070A0002890C8002890D0003888
      C0002878A8003880B0004070B0002850700090A0C800788090007080A0003058
      8800485868007088B80040609000284068004878C8005078C000304070003860
      9800203860004868B800203868003870B800D0D8E00048689000283878006880
      A00068A8F8004878B8008890A000507098003880B8009098A0004080B8005070
      90002090D0006078980018507800808088004068880048688800707888003898
      C80088909800486078004870A000486898002068980080889000406898008890
      B000808898003858780028A0D8005070B0002888C80058789800206090006878
      90005870900040587800A8B8E0005078B80038B8E8002868A8003070B8007090
      C80098B0D0003878C000A0A0B000285878001038680098A8D800708090008098
      C800405880007080B000204888005080D8003058780078A8F000385098005060
      70006088C000B0C0E000283858001830680070A0F0006090E000203070000808
      08080865B8C00808080808080808080808080808080808080808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      0808C4656BC2B3B2080808080808080808080808080808080808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      0808AB774B774E0CC264CA080808080808080808080808080808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      08087741065B5B772B26AB719308080808080808080808080808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      08084315420A15521A2B3729719B930808080808080808080808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      080827A60A40360A14182D37735765DA93080808080808080808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      08081C16191C0616142F141518C54402579CCD08080808080808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      08083D06060A1A14743D110A3C150A1121C5C796C4A208080808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      0808163014142D1C403640A7271C400A1C191B2B73ABB3E10808080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      08083F420A143C421111145C1940DB3614412111282B7265B3C8080808080808
      0808080808080808080808080808080808080808080808080808080808080808
      0808C744370A3C0A0A0A36110A270615152F28315C2D2843EA651DBEE0080808
      0808080808080808080808080808080808080808080808080808080808080808
      08087034342B2106142D1C1C1449111C0AE549C9C91A11062B43430C10C8C008
      0808080808080808080808080808080808080808080808080808080808080808
      0808D4667C72352121372C11143D2F3F18302F1C360A741106061A288395D7C4
      CA08080808080808080808080808080808080808080808080808080808080808
      0808970F846B2E34957E4C37280A1530422727152706163C142F521128837235
      B8D3DE0808080808080808080808080808080808080808080808080808080808
      0808E4738B170273727DED9E2E1B061614163031300A062C143D3D140A112C37
      44356B7093080808080808080808080808080808080808080808080808080808
      08085D314B4373E7BC6D8FFC7D294437113D2F181A111159063C5C40270A4A11
      192B372E659BCACF080808080808080808080808080808080808080808080808
      080885273F3068A3352E3932478ACBD5A94A52591819361C4A364A4A27360A0A
      1111D04B2134656DCEA208080808080808080808080808080808080808080808
      08084B76180A15412C2B755533BB807E9E4C2E2111111C0A1506060A36941641
      5F2C16402D062C3E2ECDBEDE0808080808080808080808080808080808080808
      08084B313D1518522D2C2C3E750C7D328E4D176963A321111606060A3F065915
      491642119436593F11373465CEA2CF0808080808080808080808080808080808
      08085B190616143006194118854B212B022A02330F2339341A284B142F2F5241
      5F590A0611191530161B2B1B2E02719308080808080808080808080808080808
      080802111606160A30151C142F5B314343832E07298F4D6F2984A8143F0A1911
      0A151606272719151430184AD0602ED7F171CF08080808080808080808080808
      0808613716593C3C153C1C2F3C421614161A1B34BC4E651D2210383E31060616
      06061606143F4A141441181C2F310643AA387093A20808080808080808080808
      08086E6A11143606165F94161C1C5F940A15064B212D728B0F320F0F4C6A213F
      49150614764906315C5F2706411441061B1B443570D208080808080808080808
      0808B4296A19A6DD06361C150A1C76590A1C154131212B2B2163557E29384C69
      1B605B16161640364011273627144914151818283E96E2080808080808080808
      0808481D10022E3E0627063D275F2F5F3C2730063F0668285D28777334021058
      615A75AA5B1A4A0A0A060A270A1576141830144168849C080808080808080808
      08086F231D221D78342B06410A1C741C1C151619063D30301419064A5B437235
      397FAC5A3869060A060A0A111815490659593D2F14284C080808080808080808
      0808E1D4330F8C570F78A32B37210A360A160A1A0A0A0A061515060A740A1B44
      3463293A6157752B180611111511110A273C3049185D84D30808080808080808
      080808DE3312104D56560F63D54C691A423C27420A0652180A0606402F150A19
      2B342E664FB0F386694B19190A110615493D3116066835700808080808080808
      08080808DC29550C0232390F29584F2E1B115D164918185C855D062F745F1518
      062D2B952E6361783578752128191C1630272F3D161BA5D70808080808080808
      08080808089A07024E2A7B2A0F6289577E3486683D76161A52183142151C1C5C
      151A11311A604438556278962E44213106062D302F314CDA0808080808080808
      0808080808DC2A0202530207070CF745B932DFA9111140DB1B40191906401616
      42190A2D2D2D2D2CE6387E0F5A356A211B1A1142142835230808080808080808
      0808080808086C4502020202020C7B451D1757565A86A8282D0A0AA7DD210616
      160A3C1C181806682C37354C5A56025A2E601B1B739502700808080808080808
      080808080808089A07070202020207297B10327A175834A544601A18191B1A11
      1614492F1515415211061A376997DFEC6158964C222323DE0808080808080808
      08080808080808B6020202020202028702265407100F338C4F623E1B28282886
      2B313C1411212D18522831061B2B34A57D239E973B8923E0A408080808080808
      08080808080808087C07028707070707020202077B55550F329C6F5884835D83
      2CA61A060A4A1927112C190A3F183E34353802296E7F71E1E008080808080808
      0808080808080808084E0C070C0C070707070202022907264E0F1D17105A3E43
      2D113F422B2C060606062C2C06064A111B6021372E2E5AE27908080808080808
      080808080808080808E42A0C2610260702070202020C87020C546D3A7A8C0F5A
      6A3E1B3D191A28060606281B212C1B305C2C2D182C3761FB9908080808080808
      080808080808080808086C070C072A070207020202020202026B4EE729557D17
      10E6442E605B5D311A28284319211A185D7452E5A7196A918808080808080808
      0808080808080808080808AB020C020202020C02870C02020202073535124602
      0FB01D3A3978AA68A93EA84B5B191A113F1506112C1A4CAC9908080808080808
      08080808080808080808086C260C07070207020702070C260C260707070C4EEA
      7B7C0F617FAC4F9644601B2D061A060A5C1B851B85433E888808080808080808
      080808080808080808080808460C0C26070702070726262A07260707070C0702
      386422102958227F56753E28421A2D181A1A3E34213F3EEC9908080808080808
      080808080808080808080808DC12531002028702020C3326020C0C02070C38ED
      38DC33EDED220F8E23EC2E864B060606110A3F1B3E376A6E8808080808080808
      080808080808080808080808089A121266070707020207020C02070C0C2607BB
      3BB98F32903289480F89550278977743190A2F761A6944619108080808080808
      08080808080808080808080808F2660202020707070C0C070202070C2612124D
      4D1702072A5617237A32392217B04C63342B195C15694C6EF108080808080808
      0808080808080808080808080808980202100C10530C0C0C0C0C0C1212124663
      0246F4FEB554583B38F3223A2380B717356A215D1B44F59188CC080808080808
      08080808080808080808080808080898025353020C0C0C0C121233F78D7C1022
      4F2A8BB5F4FEF610500F6F3250323B8F3217176B38176FF56EB1080808080808
      0808080808080808080808080808089D12530C070C101212B329FC239A505039
      0F027C124EB59FFE33476F233B503B5089673BB43A6E328A6E79080808080808
      080808080808080808080808080808F92A100F4E8B124512128EBB8E504F1066
      5410021226100246983A3AB9473B7A503B3B678A476723B4B9F8080808080808
      080808080808080808080808080808CA450F17620F532A2602FC8C98622AB59F
      8D544E070C262A2A10101017176239327A50678A67484748FB79080808080808
      080808080808080808080808080808C01D91FC7A530C6D1212FF8CBA9FFEFDFD
      BA5454540712121212070F02070C0C33334F804FFF50FC474899080808080808
      080808080808080808080808080808082347B79A47222233DC3A1D66122BB5FE
      64B6B69A6C663307070707070C0C0C530C100C2917BFBF229BB3080808080808
      08080808080808080808080808080808231D174647B9FCFC9B471D334612027B
      9D64F664F0B264649A4510104D4D101010296217391D481DCA08080808080808
      08080808080808080808080808080808486C322A121DB7678A0F170226541207
      029FF6242424248181C12AB93B804F100C0F1748170F12330808080808080808
      08080808080808080808080808080808CA48225033E4BA0F62458D5538071202
      382946FEFEFEFE6C2A22B947391007468D46532917B808080808080808080808
      08080808080808080808080808080808081D453A228C8D6610268D8D6B101212
      0F0F90391D1D1D223B47488C1002128D8D8B454D0F0808080808080808080808
      080808080808080808080808080808080833F0F9BB0F0F120245267C546D3312
      1266100F48220F900F0210101212544612662323C40808080808080808080808
      08080808080808080808080808080808086BFE9D398E100F2955070CBC4B6358
      291D0F1055100F0F171002072A2A072A1022579C080808080808080808080808
      0808080808080808080808080808080808F91226455690100202100C7BFEF429
      7E2232BF7D0F170F4F58580226451017564FBE08080808080808080808080808
      0808080808080808080808080808080808081212466D4D0F0F10025717451D23
      233B6767321735101756222239177E384D080808080808080808080808080808
      0808080808080808080808080808080808089A9D6CF9640F390F0F57228F3A3A
      473A2239172A4E120F0F4D0F5657700808080808080808080808080808080808
      080808080808080808080808080808080808089F9F72FE73806E7F3A3A4F1D7F
      0F100F0F026D331D9E2223177008080808080808080808080808080808080808
      080808080808080808080808080808080808080866106558506FB93B3A80481D
      0F0F0F0F1D1D33D3CA0808080808080808080808080808080808080808080808
      08080808080808080808080808080808080808080893223B3B6767232348236F
      2290481DC0080808080808080808080808080808080808080808080808080808
      0808080808080808080808080808080808080808080808C0E171717123232323
      917008080808080808080808080808080808080808080808080808080808F8FF
      FFFFFFFFFFFFF03FFFFFFFFFFFFFF007FFFFFFFFFFFFF001FFFFFFFFFFFFF000
      7FFFFFFFFFFFF0001FFFFFFFFFFFF00007FFFFFFFFFFF00000FFFFFFFFFFF000
      003FFFFFFFFFF000000FFFFFFFFFF0000001FFFFFFFFF00000007FFFFFFFF000
      00001FFFFFFFF000000007FFFFFFF000000001FFFFFFF0000000003FFFFFF000
      0000000FFFFFF00000000003FFFFF000000000007FFFF000000000003FFFF000
      0000000007FFF0000000000001FFF0000000000000FFF00000000000007FF000
      00000000007FF00000000000007FF00000000000003FF80000000000003FFC00
      00000000003FFE0000000000003FFE0000000000003FFF0000000000003FFF80
      00000000003FFF8000000000001FFFC000000000001FFFE000000000001FFFE0
      00000000001FFFF000000000001FFFF800000000001FFFF800000000001FFFFC
      00000000001FFFFC00000000001FFFFE00000000001FFFFE00000000001FFFFF
      00000000000FFFFF80000000000FFFFF80000000000FFFFF80000000000FFFFF
      80000000000FFFFF80000000000FFFFFC0000000000FFFFFC0000000001FFFFF
      C0000000003FFFFFC000000000FFFFFFE000000001FFFFFFE000000001FFFFFF
      E000000003FFFFFFE000000007FFFFFFF00000001FFFFFFFF00000007FFFFFFF
      F8000001FFFFFFFFFC00001FFFFFFFFFFE0001FFFFFFFFFFFF800FFFFFFF}
    IconVisible = True
    IconIndex = 0
    PopupMenu = DataInterface.PopupTray
    MinimizeToTray = True
    OnClick = ActionShowWindowExecute
    OnBalloonHintClick = TrayIconBalloonHintClick
    Left = 586
    Top = 404
  end
  object PopupFoodBase: TPopupActionBarEx
    Images = DataInterface.Images_Menu
    Left = 258
    Top = 443
    object Item_EditFood: TMenuItem
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100'...'
      ImageIndex = 13
      OnClick = ListFoodDblClick
    end
    object ItemCopyFood: TMenuItem
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1082#1086#1087#1080#1102
      OnClick = ItemCopyFoodClick
    end
    object Item_SepFood: TMenuItem
      Caption = '-'
    end
    object item_RemoveFood: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100
      ImageIndex = 14
      OnClick = ButtonDeleteFoodClick
    end
  end
  object PopupDishBase: TPopupActionBarEx
    Images = DataInterface.Images_Menu
    Left = 292
    Top = 443
    object Item_EditDish: TMenuItem
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100'...'
      ImageIndex = 13
      OnClick = ListDishDblClick
    end
    object ItemCopyDish: TMenuItem
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1082#1086#1087#1080#1102
      OnClick = ItemCopyDishClick
    end
    object Item_SepDish: TMenuItem
      Caption = '-'
    end
    object Item_RemoveDish: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100
      ImageIndex = 14
      OnClick = ButtonDeleteDishClick
    end
  end
  object PopupFoodCol: TPopupActionBarEx
    Left = 260
    Top = 475
    object Item_FoodP: TMenuItem
      Tag = 1
      Caption = #1041#1077#1083#1082#1080
      OnClick = Item_BaseColClick
    end
    object Item_FoodF: TMenuItem
      Tag = 2
      Caption = #1046#1080#1088#1099
      OnClick = Item_BaseColClick
    end
    object Item_FoodC: TMenuItem
      Tag = 3
      Caption = #1059#1075#1083#1077#1074#1086#1076#1099
      OnClick = Item_BaseColClick
    end
    object Item_FoodV: TMenuItem
      Tag = 4
      Caption = #1050#1072#1083#1086#1088#1080#1081#1085#1086#1089#1090#1100
      OnClick = Item_BaseColClick
    end
  end
  object PopupDishCol: TPopupActionBarEx
    Left = 292
    Top = 475
    object Item_DishM: TMenuItem
      Tag = -1
      Caption = #1052#1072#1089#1089#1072
      OnClick = Item_BaseColClick
    end
    object Item_DishP: TMenuItem
      Tag = -2
      Caption = #1041#1077#1083#1082#1080
      OnClick = Item_BaseColClick
    end
    object Item_DishF: TMenuItem
      Tag = -3
      Caption = #1046#1080#1088#1099
      OnClick = Item_BaseColClick
    end
    object Item_DishC: TMenuItem
      Tag = -4
      Caption = #1059#1075#1083#1077#1074#1086#1076#1099
      OnClick = Item_BaseColClick
    end
    object Item_DishV: TMenuItem
      Tag = -5
      Caption = #1050#1072#1083#1086#1088#1080#1081#1085#1086#1089#1090#1100
      OnClick = Item_BaseColClick
    end
    object Item_DishD: TMenuItem
      Tag = -6
      Caption = #1044#1072#1090#1072
      OnClick = Item_BaseColClick
    end
  end
  object TimerAutosave: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = TimerAutosaveTimer
    Left = 357
    Top = 171
  end
  object ThreadExec_: TThreadExecutor
    OnDone = ThreadExec_Done
    OnTimeOut = ThreadExec_TimeOut
    Left = 296
    Top = 80
  end
end

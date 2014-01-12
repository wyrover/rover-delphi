object MainForm: TMainForm
  Left = 595
  Top = 318
  Width = 839
  Height = 551
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object g: TGauge2
    Left = 56
    Top = 32
    Width = 297
    Height = 113
    Kind = gkPie
    Progress = 0
  end
  object g21: TGauge2
    Left = 360
    Top = 48
    Width = 369
    Height = 97
    Kind = gkText
    Progress = 0
  end
  object g22: TGauge2
    Left = 24
    Top = 168
    Width = 369
    Height = 97
    Kind = gkNeedle
    Progress = 0
  end
  object g23: TGauge2
    Left = 400
    Top = 168
    Width = 369
    Height = 97
    Kind = gkVerticalBar
    Progress = 0
  end
  object g24: TGauge2
    Left = 0
    Top = 416
    Width = 823
    Height = 97
    Align = alBottom
    Color = clAqua
    ForeColor = clGreen
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Progress = 0
  end
  object btn1: TButton
    Left = 216
    Top = 344
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object tmr1: TTimer
    Interval = 200
    OnTimer = tmr1Timer
    Left = 680
    Top = 352
  end
  object tooltip1: TToolTipManager
    ToolTips = <>
    Left = 96
    Top = 328
  end
end

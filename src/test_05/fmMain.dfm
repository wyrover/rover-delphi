object MainForm: TMainForm
  Left = 594
  Top = 231
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
  object btn1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 30
    OnTimer = tmr1Timer
    Left = 680
    Top = 352
  end
  object tmr2: TTimer
    Enabled = False
    Interval = 30
    OnTimer = tmr2Timer
    Left = 144
    Top = 232
  end
end

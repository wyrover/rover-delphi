object MainForm: TMainForm
  Left = 192
  Top = 130
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
  object tmr1: TTimer
    Interval = 30
    OnTimer = tmr1Timer
    Left = 680
    Top = 352
  end
end

object MainForm: TMainForm
  Left = 662
  Top = 280
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
  object img1: TRotateImage
    Left = 96
    Top = 64
    Width = 449
    Height = 273
  end
  object tmr1: TTimer
    Interval = 200
    OnTimer = tmr1Timer
    Left = 680
    Top = 352
  end
end

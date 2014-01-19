object Form1: TForm1
  Left = 528
  Top = 398
  Width = 824
  Height = 186
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 17
  object tmr1: TTimer
    Interval = 1500
    OnTimer = tmr1Timer
    Left = 120
    Top = 32
  end
  object pm1: TPopupMenu
    Left = 192
    Top = 24
    object mni_topMost: TMenuItem
      AutoCheck = True
      Caption = #24635#22312#26368#21069
      OnClick = mni_topMostClick
    end
    object mni_transparent: TMenuItem
      AutoCheck = True
      Caption = #32972#26223#31359#36879
      OnClick = mni_transparentClick
    end
    object mni_exit: TMenuItem
      AutoCheck = True
      Caption = #36864#20986
      OnClick = mni_exitClick
    end
  end
end

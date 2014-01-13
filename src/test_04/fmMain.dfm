object MainForm: TMainForm
  Left = 192
  Top = 130
  Caption = 'Form1'
  ClientHeight = 513
  ClientWidth = 823
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
  object btn360styl1: TBtn360Style
    Left = 152
    Top = 104
    Width = 353
    Height = 241
    Caption = 'btn360styl1'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    PngFileName = 'png-0030.png'
    BkgColor = clBlue
    EdgColor = clSkyBlue
  end
  object tmr1: TTimer
    Interval = 200
    Left = 680
    Top = 352
  end
end

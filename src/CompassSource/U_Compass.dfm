object Form1: TForm1
  Left = 384
  Top = 191
  AutoScroll = False
  AutoSize = True
  Caption = 'Compass Drawing Demo'
  ClientHeight = 643
  ClientWidth = 888
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 120
  TextHeight = 16
  object StaticText1: TStaticText
    Left = 0
    Top = 620
    Width = 888
    Height = 23
    Cursor = crHandPoint
    Align = alBottom
    Alignment = taCenter
    Caption = 'Copyright '#169' 2010, Gary Darby,  www.DelphiForFun.org'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    TabOrder = 0
    OnClick = StaticText1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 888
    Height = 620
    Align = alClient
    TabOrder = 1
    object Compass: TPaintBox
      Left = 432
      Top = 128
      Width = 433
      Height = 457
      OnPaint = CompassPaint
    end
    object Label1: TLabel
      Left = 680
      Top = 56
      Width = 162
      Height = 16
      Caption = 'Heading angle in degrees'
    end
    object Memo1: TMemo
      Left = 25
      Top = 30
      Width = 360
      Height = 467
      Color = 14548991
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        'Here'#39's a sample Compass drawing'
        'demonstration which will show how to rotate the '
        'dial or the pointer in Delphi code.    The angle '
        'is specified in the "Heading" SpinEdit control;'
        ''
        'It uses a TPaintbox, "Compass", to repaint the '
        'compass when needed.  Compass is '
        'completely redrawn for each call so its owner '
        '(Panel1) has its "Doublebuffered" property set '
        'True to prevent flashing when the space is '
        'cleared before redrawing.'
        ''
        'It uses the TLogRec data structure and the'
        'CreateFontIndirect procedure to draw letters'
        'and numbers at right angles to the direction'
        'radial..'
        ''
        'The pointer is two back-to-back triangles drawn'
        'with the Polygon method and filled with different'
        'Brush property colors of the Paintbox'#39's Canvas'
        'property.')
      ParentFont = False
      TabOrder = 0
    end
    object Heading: TSpinEdit
      Left = 680
      Top = 80
      Width = 65
      Height = 34
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Arial'
      Font.Style = []
      MaxValue = 3600
      MinValue = -3600
      ParentFont = False
      TabOrder = 1
      Value = 45
      OnChange = ForceRepaint
    end
    object TypeGrp: TRadioGroup
      Left = 488
      Top = 48
      Width = 161
      Height = 65
      Caption = 'Compass type'
      ItemIndex = 0
      Items.Strings = (
        'Rotate dial'
        'Rotate pointer')
      TabOrder = 2
      OnClick = ForceRepaint
    end
  end
end

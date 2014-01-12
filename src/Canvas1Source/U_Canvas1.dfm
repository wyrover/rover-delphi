object Form1: TForm1
  Left = 1
  Top = 47
  Width = 646
  Height = 419
  Caption = 'Canvas Drawing/Saving Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object Image1: TImage
    Left = 216
    Top = 16
    Width = 377
    Height = 329
    OnMouseDown = Image1MouseDown
    OnMouseMove = Image1MouseMove
    OnMouseUp = Image1MouseUp
  end
  object Label2: TLabel
    Left = 24
    Top = 16
    Width = 116
    Height = 32
    Caption = 'Left button down on canvas to scribble'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 24
    Top = 224
    Width = 144
    Height = 16
    Caption = 'Click list item to restore it'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object DrawBtn: TButton
    Left = 24
    Top = 88
    Width = 137
    Height = 25
    Caption = 'Draw some ellipses'
    TabOrder = 0
    OnClick = DrawBtnClick
  end
  object SaveBtn: TButton
    Left = 24
    Top = 152
    Width = 137
    Height = 25
    Caption = 'Save canvas in list'
    TabOrder = 1
    OnClick = SaveBtnClick
  end
  object ClearBtn: TButton
    Left = 24
    Top = 120
    Width = 137
    Height = 25
    Caption = 'Clear it'
    TabOrder = 2
    OnClick = ClearBtnClick
  end
  object ListBox1: TListBox
    Left = 24
    Top = 248
    Width = 121
    Height = 97
    ItemHeight = 16
    TabOrder = 3
    OnClick = ListBox1Click
  end
  object Button1: TButton
    Left = 24
    Top = 184
    Width = 137
    Height = 25
    Caption = 'Save canvas to file'
    TabOrder = 4
    OnClick = Button1Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'bmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 600
    Top = 24
  end
end

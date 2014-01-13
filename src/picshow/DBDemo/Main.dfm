object MainForm: TMainForm
  Left = 188
  Top = 74
  Width = 712
  Height = 536
  Caption = 'TDBPicShow Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBPicShow: TDBPicShow
    Left = 0
    Top = 27
    Width = 696
    Height = 454
    Hint = 'Double click to update/insert the image.'
    DataField = 'Pic'
    DataSource = DataSource
    Align = alClient
    Center = True
    Color = clBlack
    ParentColor = False
    Proportional = True
    Style = 119
    TabOrder = 0
    OnBeforeLoadPicture = DBPicShowBeforeLoadPicture
    OnCustomDraw = DBPicShowCustomDraw
    OnDblClick = DBPicShowDblClick
  end
  object Toolbar: TPanel
    Left = 0
    Top = 0
    Width = 696
    Height = 27
    Align = alTop
    AutoSize = True
    BorderWidth = 2
    TabOrder = 1
    DesignSize = (
      696
      27)
    object StylesLabel: TLabel
      Left = 6
      Top = 6
      Width = 75
      Height = 13
      Caption = 'Transition Style:'
    end
    object DBNavigator: TDBNavigator
      Left = 448
      Top = 3
      Width = 240
      Height = 21
      DataSource = DataSource
      Anchors = [akTop, akRight]
      Flat = True
      TabOrder = 0
    end
    object Styles: TComboBox
      Left = 84
      Top = 3
      Width = 309
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 481
    Width = 696
    Height = 19
    AutoHint = True
    Panels = <
      item
        Width = 400
      end
      item
        Width = 50
      end>
  end
  object DataSource: TDataSource
    DataSet = PicturesTable
    OnDataChange = DataSourceDataChange
    Left = 40
    Top = 56
  end
  object OpenPictureDialog: TOpenPictureDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 144
    Top = 56
  end
  object PicturesTable: TTable
    FieldDefs = <
      item
        Name = 'Pic'
        Attributes = [faRequired]
        DataType = ftBlob
      end>
    StoreDefs = True
    TableName = 'Pictures'
    Left = 40
    Top = 128
    object PicturesTablePic: TBlobField
      FieldName = 'Pic'
      Required = True
      GraphicHeader = False
    end
  end
end

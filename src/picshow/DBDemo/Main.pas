{------------------------------------------------------------------------------}
{                                                                              }
{  DBPicShow Demonstration                                                     }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit Main;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Messages, Classes, Graphics, SysUtils, Controls, Forms, Dialogs,
  ExtDlgs, StdCtrls, ExtCtrls, ComCtrls, DBCtrls, DB, DBTables, PicShow,
  {$IFDEF COMPILER2009_UP} PngImage, {$ENDIF} Jpeg;

type
  TMainForm = class(TForm)
    DBPicShow: TDBPicShow;
    Toolbar: TPanel;
    DBNavigator: TDBNavigator;
    Styles: TComboBox;
    StylesLabel: TLabel;
    DataSource: TDataSource;
    OpenPictureDialog: TOpenPictureDialog;
    PicturesTable: TTable;
    PicturesTablePic: TBlobField;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure DBPicShowBeforeLoadPicture(Sender: TObject);
    procedure DBPicShowCustomDraw(Sender: TObject; Picture, Screen: TBitmap);
    procedure DBPicShowDblClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DBPicShow.GetStyleNames(Styles.Items);
  Styles.ItemIndex := Styles.Items.IndexOf(DBPicShow.StyleName);
  PicturesTable.DatabaseName := ExtractFilePath(ParamStr(0)) + 'Database';
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Update;
  PicturesTable.Open;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  PicturesTable.Close;
end;

procedure TMainForm.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  if DataSource.State = dsInsert then
    StatusBar.Panels[1].Text := Format('Picture * of %d',
      [PicturesTable.RecordCount])
  else
    StatusBar.Panels[1].Text := Format('Picture %d of %d',
      [PicturesTable.RecNo, PicturesTable.RecordCount]);
end;

procedure TMainForm.DBPicShowBeforeLoadPicture(Sender: TObject);
begin
  DBPicShow.StyleName := Styles.Text;
end;

// This procedure will be called when DBPicShow.Style is 0
// Picture: This is the image.
// Screen: This is what we should draw on it.
procedure TMainForm.DBPicShowCustomDraw(Sender: TObject;
  Picture, Screen: TBitmap);
var
  Text: String;
begin
  Text := Format('CUSTOM: PROGRESS = %d%%', [DBPicShow.Progress]);
  Screen.Canvas.Draw(0, 0, Picture);
  SetTextAlign(Screen.Canvas.Handle, TA_CENTER or TA_BASELINE);
  Screen.Canvas.TextOut(Screen.Width div 2, Screen.Height div 2, Text);
end;

procedure TMainForm.DBPicShowDblClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    if not (DataSource.State in [dsEdit, dsInsert]) then
      DataSource.Edit;
    DBPicShow.Picture.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

end.

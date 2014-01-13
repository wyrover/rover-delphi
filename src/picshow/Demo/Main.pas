{------------------------------------------------------------------------------}
{                                                                              }
{  PicShow Demonstration                                                       }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit Main;

{$I DELPHIAREA.INC}

{$IFDEF COMPILER6_UP}
  {$WARN UNIT_PLATFORM OFF}  // No warning for FileCtrl unit
{$ENDIF}

{.$DEFINE CAPTURE}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ExtDlgs, PicShow,
  {$IFDEF COMPILER2009_UP} PngImage, {$ENDIF} jpeg;

{$IFDEF CAPTURE}
const
  CaptureFile = 'C:\PS%6.6u.bmp';
{$ENDIF}

type
  TMainForm = class(TForm)
    PicShow: TPicShow;
    Timer: TTimer;
    StatusBar: TStatusBar;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    lblStyle: TLabel;
    cbStyle: TComboBox;
    rgStyleControl: TRadioGroup;
    gbProgressControl: TGroupBox;
    rbProgressAuto: TRadioButton;
    rbProgressManual: TRadioButton;
    lblStyleNo: TLabel;
    tbProgress: TTrackBar;
    lblProgressStep: TLabel;
    lblProgressDelay: TLabel;
    edtProgressStep: TEdit;
    udProgressStep: TUpDown;
    edtProgressDelay: TEdit;
    udProgressDelay: TUpDown;
    ckExactTiming: TCheckBox;
    ckThreaded: TCheckBox;
    ckOverDraw: TCheckBox;
    lblDisplayInterval: TLabel;
    tbDisplayInterval: TTrackBar;
    gbBackground: TGroupBox;
    lblBackgroundMode: TLabel;
    cbBackgroundMode: TComboBox;
    btnChangeBackground: TButton;
    gbImagePlacement: TGroupBox;
    ckCenter: TCheckBox;
    ckProportional: TCheckBox;
    ckStretch: TCheckBox;
    OpenPictureDialog: TOpenPictureDialog;
    lblDisplayIntervalValue: TLabel;
    gbFrame: TGroupBox;
    lblFrameWidth: TLabel;
    edtFrameWidth: TEdit;
    udFrameWidth: TUpDown;
    btnChangeFrameColor: TButton;
    ColorDialog: TColorDialog;
    btnChangePath: TButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PicShowStart(Sender: TObject; Picture, Screen: TBitmap);
    procedure PicShowStop(Sender: TObject);
    procedure PicShowProgress(Sender: TObject);
    procedure PicShowDblClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure cbStyleChange(Sender: TObject);
    procedure rbProgressAutoClick(Sender: TObject);
    procedure rbProgressManualClick(Sender: TObject);
    procedure edtProgressStepChange(Sender: TObject);
    procedure edtProgressDelayChange(Sender: TObject);
    procedure ckExactTimingClick(Sender: TObject);
    procedure ckThreadedClick(Sender: TObject);
    procedure tbProgressChange(Sender: TObject);
    procedure btnChangePathClick(Sender: TObject);
    procedure tbDisplayIntervalChange(Sender: TObject);
    procedure ckOverDrawClick(Sender: TObject);
    procedure ckCenterClick(Sender: TObject);
    procedure ckStretchClick(Sender: TObject);
    procedure ckProportionalClick(Sender: TObject);
    procedure cbBackgroundModeChange(Sender: TObject);
    procedure btnChangeBackgroundClick(Sender: TObject);
    procedure edtFrameWidthChange(Sender: TObject);
    procedure btnChangeFrameColorClick(Sender: TObject);
  private
    Pictures: TStringList;
    PicturesPath: String;
    ShowingImage: String;
    LoadedImage: String;
    {$IFDEF CAPTURE}
    CaptureSequence: Integer;
    procedure CaptureScreen;
    {$ENDIF}
    procedure CheckTimer;
    procedure ShowNextImage;
    procedure LoadNextImage;
    procedure CreateImageList(const Path: String);
    procedure SetFullScreen(Active: Boolean);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  FileCtrl {$IFDEF COMPILER7_UP}, XPMan {$ENDIF};

{$IFDEF CAPTURE}
procedure TMainForm.CaptureScreen;
var
  Bitmap: TBitmap;
  ScrDC: HDC;
begin
  Update;
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Brush.Color := clFuchsia;
    Bitmap.Width := Width;
    Bitmap.Height := Height;
    Bitmap.HandleType := bmDIB;
    ScrDC := GetDC(0);
    try
      BitBlt(Bitmap.Canvas.Handle, 0, 0, Width, Height, ScrDC, Left, Top, SRCCOPY);
    finally
      ReleaseDC(0, ScrDC);
    end;
    Bitmap.SaveToFile(Format(CaptureFile, [CaptureSequence]));
  finally
    Bitmap.Free;
  end;
  Inc(CaptureSequence);
end;
{$ENDIF}

// Activate or deactvates the full screen mode
procedure TMainForm.SetFullScreen(Active: Boolean);
begin
  if Active and (PicShow.Align = alClient) then
  begin
    PicShow.SetFocus;
    PicShow.Align := alNone;
    PicShow.BgMode := bmNone;
    PicShow.FrameWidth := 0;
    PicShow.ShowHint := False;
    Windows.SetParent(PicShow.Handle, 0);
    PicShow.SetBounds(0, 0, Screen.Width, Screen.Height);
    SetWindowPos(PicShow.Handle, HWND_TOPMOST, 0, 0, Screen.Width, Screen.Height, SWP_SHOWWINDOW);
    ShowCursor(False);
  end
  else if not Active and (PicShow.Align = alNone) then
  begin
    Windows.SetParent(PicShow.Handle, Self.Handle);
    PicShow.Align := alClient;
    PicShow.BgMode := TBackgroundMode(cbBackgroundMode.ItemIndex);
    PicShow.FrameWidth := udFrameWidth.Position;
    PicShow.ShowHint := True;
    ShowCursor(True);
  end;
end;

// Toggles timer based on state of controls
procedure TMainForm.CheckTimer;
begin
  Timer.Enabled := not PicShow.Busy and rbProgressAuto.Checked and (Pictures.Count > 0);
end;

// Begins transition of the currently loaded image
procedure TMainForm.ShowNextImage;
begin
  Timer.Enabled := False;
  // if there is no picture in the list, exit
  if Pictures.Count = 0 then Exit;
  // if PicShow is playing, stops it
  if PicShow.Busy then PicShow.Stop;
  // Sets the transition style according to the user's choice
  case rgStyleControl.ItemIndex of
    0: cbStyle.ItemIndex := (cbStyle.ItemIndex + 1) mod cbStyle.Items.Count;
    1: cbStyle.ItemIndex := Random(cbStyle.Items.Count);
  end;
  cbStyleChange(nil);
  // Updates image name status
  ShowingImage := LoadedImage;
  StatusBar.Panels[0].Text := 'Showing: ' + ShowingImage;
  // Begins the transition
  PicShow.Execute;
end;

// Selects randomly an image from the list and loads it in to PicShow
procedure TMainForm.LoadNextImage;
var
  Index: Integer;
begin
  LoadedImage := '';
  if Pictures.Count > 0 then
  begin
    repeat
      Index := Random(Pictures.Count);
    until (Pictures.Count <= 1) or (ShowingImage <> Pictures[Index]);
    LoadedImage := Pictures[Index];
    PicShow.Picture.LoadFromFile(PicturesPath + '\' + LoadedImage);
  end;
  StatusBar.Panels[1].Text := 'Next: ' + LoadedImage;
end;

// Creates a list of image filenames found in the path
procedure TMainForm.CreateImageList(const Path: String);
const
  SNoImage = 'The specified folder does not contain any supported image file.';
var
  FileList: TFileListBox;
begin
  if Path <> PicturesPath then
  begin
    FileList := TFileListBox.Create(nil);
    try
      FileList.Visible := False;
      FileList.Parent := Self;
      FileList.Mask := GraphicFileMask(TGraphic);
      FileList.Directory := Path;
      if FileList.Items.Count > 0 then
      begin
        Pictures.Assign(FileList.Items);
        PicturesPath := Path;
        if (Length(Path) > 0) and (PicturesPath[Length(Path)] = '\') then
          Delete(PicturesPath, Length(Path), 1);
        StatusBar.Panels[2].Text := IntToStr(Pictures.Count) + ' Image(s)';
        StatusBar.Panels[3].Text := 'Folder: ' + Path;
        LoadNextImage;
      end
      else
        MessageDlg(Path + #13#10 + SNoImage, mtWarning, [mbCancel], 0);
    finally
      FileList.Free;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;
  {$IFDEF CAPTURE}
  PicShow.ShowHint := False;
  PicShow.Step := 5;
  PicShow.ExactTiming := False;
  rgStyleControl.ItemIndex := 0; // Random Style
  {$ENDIF}
  // Creates a string list for storing list of image files
  Pictures := TStringList.Create;
  // Updates controls by PicShow's properties
  PicShow.GetStyleNames(cbStyle.Items);
  cbStyle.ItemIndex := PicShow.Style - 1;
  rbProgressAuto.Checked := not PicShow.Manual;
  rbProgressManual.Checked := PicShow.Manual;
  udProgressStep.Position := PicShow.Step;
  udProgressDelay.Position := PicShow.Delay;
  ckExactTiming.Checked := PicShow.ExactTiming;
  ckThreaded.Checked := PicShow.Threaded;
  tbProgress.Position := PicShow.Progress;
  ckOverDraw.Checked := PicShow.OverDraw;
  ckCenter.Checked := PicShow.Center;
  ckStretch.Checked := PicShow.Stretch;
  ckProportional.Checked := PicShow.Proportional;
  cbBackgroundMode.ItemIndex :=  Ord(PicShow.BgMode);
  udFrameWidth.Position := PicShow.FrameWidth;
  tbDisplayInterval.Position := Timer.Interval;
  // you may want to extend range of TPercent type!
  tbProgress.Min := Low(TPercent);
  tbProgress.Max := High(TPercent);
  tbProgress.Frequency := (High(TPercent) - Low(TPercent)) div 10;
  // prepare list by images found in the specified path or the program's path
  if ParamCount > 0 then
    CreateImageList(ParamStr(1))
  else
    CreateImageList(ExtractFilePath(Application.ExeName) + 'Photos');
  // Checkes state of photo changer timer
  CheckTimer;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Pictures.Free;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (PicShow.Align = alNone) then
  begin
    SetFullScreen(False);
    Key := 0;
  end;
end;

procedure TMainForm.PicShowStart(Sender: TObject; Picture, Screen: TBitmap);
begin
  CheckTimer;
  // When PicShow begins transaction, we can load the next image into the
  // control. This is possible because PicShow converts the image to Bitmap
  // and use this copy during its process.
  LoadNextImage;
end;

procedure TMainForm.PicShowStop(Sender: TObject);
begin
  CheckTimer;
end;

procedure TMainForm.PicShowProgress(Sender: TObject);
begin
  tbProgress.Position := PicShow.Progress;
  {$IFDEF CAPTURE}
  CaptureScreen;
  {$ENDIF}
end;

procedure TMainForm.PicShowDblClick(Sender: TObject);
begin
  SetFullScreen(PicShow.Align <> alNone);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  ShowNextImage;
end;

procedure TMainForm.cbStyleChange(Sender: TObject);
begin
  if PicShow.Style <> cbStyle.ItemIndex + 1 then
  begin
    PicShow.Style := cbStyle.ItemIndex + 1;
    lblStyleNo.Caption := Format('[ #%d ]', [PicShow.Style]);
    lblStyleNo.Update;
    cbStyle.Hint := PicShow.StyleName;
    if PtInRect(cbStyle.BoundsRect, cbStyle.Parent.ScreenToClient(Mouse.CursorPos)) then
      Application.CancelHint;
  end;
end;

procedure TMainForm.rbProgressAutoClick(Sender: TObject);
begin
  PicShow.Manual := False;
  lblProgressStep.Enabled := not PicShow.Manual;
  edtProgressStep.Enabled := not PicShow.Manual;
  udProgressStep.Enabled := not PicShow.Manual;
  lblProgressDelay.Enabled := not PicShow.Manual;
  edtProgressDelay.Enabled := not PicShow.Manual;
  udProgressDelay.Enabled := not PicShow.Manual;
  ckExactTiming.Enabled := not PicShow.Manual;
  ckThreaded.Enabled := not PicShow.Manual;
  tbProgress.Enabled := PicShow.Manual;
  CheckTimer;
end;

procedure TMainForm.rbProgressManualClick(Sender: TObject);
begin
  PicShow.Manual := True;
  lblProgressStep.Enabled := not PicShow.Manual;
  edtProgressStep.Enabled := not PicShow.Manual;
  udProgressStep.Enabled := not PicShow.Manual;
  lblProgressDelay.Enabled := not PicShow.Manual;
  edtProgressDelay.Enabled := not PicShow.Manual;
  udProgressDelay.Enabled := not PicShow.Manual;
  ckExactTiming.Enabled := not PicShow.Manual;
  ckThreaded.Enabled := not PicShow.Manual;
  tbProgress.Enabled := PicShow.Manual;
  tbProgress.PageSize := PicShow.Step;
  tbProgress.Position := PicShow.Progress;
  CheckTimer;
  // When PicShow is in manual mode, first we must call the Execute method.
  // Then, we can change the Progress property. If PicShow is already busy,
  // calling the Execute method is not necessary.
  if not (PicShow.Busy or PicShow.Empty) then
  begin
    Update;
    PicShow.Execute;
  end;
end;

procedure TMainForm.edtProgressStepChange(Sender: TObject);
begin
  PicShow.Step := udProgressStep.Position;
end;

procedure TMainForm.edtProgressDelayChange(Sender: TObject);
begin
  PicShow.Delay := udProgressDelay.Position;
end;

procedure TMainForm.ckExactTimingClick(Sender: TObject);
begin
  PicShow.ExactTiming := ckExactTiming.Checked;
end;

procedure TMainForm.ckThreadedClick(Sender: TObject);
begin
  PicShow.Threaded := ckThreaded.Checked;
end;

procedure TMainForm.tbProgressChange(Sender: TObject);
begin
  if PicShow.Manual then
    PicShow.Progress := tbProgress.Position;
end;

procedure TMainForm.btnChangePathClick(Sender: TObject);
var
  Path: String;
begin
  Path := PicturesPath;
  if SelectDirectory('Select folder of images for slide show:', '', Path) then
    CreateImageList(Path);
end;

procedure TMainForm.tbDisplayIntervalChange(Sender: TObject);
begin
  Timer.Interval := tbDisplayInterval.Position;
  lblDisplayIntervalValue.Caption := Format('[ %.1f Seconds ]', [Timer.Interval / 1000]);
end;

procedure TMainForm.ckOverDrawClick(Sender: TObject);
begin
  PicShow.OverDraw := ckOverDraw.Checked;
end;

procedure TMainForm.ckCenterClick(Sender: TObject);
begin
  PicShow.Center := ckCenter.Checked;
end;

procedure TMainForm.ckStretchClick(Sender: TObject);
begin
  PicShow.Stretch := ckStretch.Checked;
end;

procedure TMainForm.ckProportionalClick(Sender: TObject);
begin
  PicShow.Proportional := ckProportional.Checked;
end;

procedure TMainForm.cbBackgroundModeChange(Sender: TObject);
begin
  PicShow.BgMode := TBackgroundMode(cbBackgroundMode.ItemIndex);
end;

procedure TMainForm.btnChangeBackgroundClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    PicShow.BgPicture.LoadFromFile(OpenPictureDialog.FileName);
end;

procedure TMainForm.edtFrameWidthChange(Sender: TObject);
begin
  PicShow.FrameWidth := udFrameWidth.Position;
end;

procedure TMainForm.btnChangeFrameColorClick(Sender: TObject);
begin
  ColorDialog.Color := PicShow.FrameColor;
  if ColorDialog.Execute then
    PicShow.FrameColor := ColorDialog.Color;
end;

end.


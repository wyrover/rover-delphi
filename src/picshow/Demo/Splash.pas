unit Splash;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PicShow, ExtCtrls, jpeg;

type
  TSplashForm = class(TForm)
    PicShow: TPicShow;
    procedure PicShowProgress(Sender: TObject);
  private
    procedure CreateBackground;
  public
    class function Execute: TSplashForm;
  end;

implementation

{$R *.DFM}



procedure TSplashForm.CreateBackground;
var
  Background: TBitmap;
  DC: HDC;
begin
  
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  // We create a bitmap object for storing the screen behind the form.
  Background := TBitmap.Create;
  try
    Background.Width := Width;
    Background.Height := Height;



    // 复制桌面位图
    DC := GetDC(0);
    try
      BitBlt(Background.Canvas.Handle, 0, 0, Width, Height, DC, Left, Top, SRCCOPY);
    finally
      ReleaseDC(0, DC);
    end;

    // We  setBackgrund property of PicShow to captured screen image.
    // By this trick the form seems as transparent.
    PicShow.BgPicture.Assign(Background);
  finally
    // We don't need the bitmap object, then we free it.
    Background.Free;
  end;
end;

class function TSplashForm.Execute: TSplashForm;
begin
  Result := TSplashForm.Create(nil);
  if ParamCount = 0 then
    with Result do
    begin

      // 获取桌面背景，相当于获取父窗口背景，透明
      CreateBackground;

      Show;
      // To prevent flickering, update the form immediately.
      Update;
      // Select randomly a transition effect.
      Randomize;
      PicShow.Style := TShowStyle(Random(High(TShowStyle))+1);
      // Start image transition.
      // For splash forms don't use PicShow as Threaded. When threaded is true,
      // transition will start after activation of main form.
      PicShow.Execute;
      // Wait a bit before continuing the rest of the application.
      // Consider that we don't use threaded mode, otherwise the following
      // line has no effect.
      Sleep(400);
    end;
end;

procedure TSplashForm.PicShowProgress(Sender: TObject);
begin
  if (PicShow.Progress = 100) and not PicShow.Reverse then
  begin
    // we select another transition effect randomly,
    PicShow.Style := TShowStyle(Random(High(TShowStyle))+1);
    // and continue the transaction to its initial state.
    PicShow.Reverse := True;
    // we wait two seconds before hiding the image
    Sleep(1500);
  end;
end;

end.

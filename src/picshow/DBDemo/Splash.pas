{------------------------------------------------------------------------------}
{                                                                              }
{  DBPicShow Demonstration                                                     }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}
{                                                                              }
{  If forms of your application appear fast on the screen, the splash of the   }
{  other demo more suitable.                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit Splash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PicShow, ExtCtrls, jpeg;

type
  TSplashForm = class(TForm)
    PicShow: TPicShow;
    procedure PicShowProgress(Sender: TObject);
  private
    procedure Init;
  public
    class procedure ShowSplash;
    class procedure HideSplash;
  end;

implementation

{$R *.DFM}

var
  SplashForm: TSplashForm;

{$IFNDEF COMPILER4_UP}
// I've realized the Random function on Delphi 3 does not work correctly. It
// sometimes returns a negative value and sometimes a value larger than the
// Range parameter. By the way, I have to mention that I have not installed
// any service pack.
function Random(Range: Integer): Integer;
begin
  Result := System.Random(Range);
  if Result < 0 then Result := -Result;
  Result := Result mod Range;
end;
{$ENDIF}

function CaptureScreen(Left, Top, Width, Height: Integer): TBitmap;
var
  DC: HDC;
begin
  // We create a bitmap object for storing the screen behind the form.
  Result := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
  // Then, we get device context of the screen and copy the screen behind the
  // form to the created bitmap.
  DC := GetDC(0);
  try
    BitBlt(Result.Canvas.Handle, 0, 0, Width, Height, DC, Left, Top, SRCCOPY);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TSplashForm.Init;
var
  Scr: TBitmap;
begin
  // First we set position of the form on the center of desktop.
  // We set Position property of the form to poDesigned because we
  // need the form's position before showing it.
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  // Then to give a transparent look to picshow, we capture the screen image
  Scr := CaptureScreen(Left, Top, Width, Height);
  try
    // and assign it to PicShow's background
    PicShow.BgPicture.Assign(Scr);
    // To reduce chance of flickering (only when PicShow is used as non-windowed
    // control we may sometime have flickers) we set background color of the
    // form to color of upper left pixel of the captured screen.
    Color := Scr.Canvas.Pixels[0,0];
  finally
    // We don't need the bitmap object, so we free it.
    Scr.Free;
  end;
  // Select randomly a transition effect.
  Randomize;
  PicShow.Style := Random(High(TShowStyle)) + 1;
end;

class procedure TSplashForm.ShowSplash;
begin
  if SplashForm = nil then
  begin
    SplashForm := TSplashForm.Create(nil);
    SplashForm.Init;
    SplashForm.Show;
    SplashForm.Update;
    SplashForm.PicShow.Execute;
  end;
end;

class procedure TSplashForm.HideSplash;
begin
  if SplashForm <> nil then
  begin
    SplashForm.PicShow.Reverse := True;
    SplashForm.PicShow.Manual := False;
    SplashForm.Free;
    SplashForm := nil;
    Sleep(500);
  end;
end;

procedure TSplashForm.PicShowProgress(Sender: TObject);
begin
  if PicShow.Progress = 100 then
  begin
    PicShow.Style := Random(High(TShowStyle)) + 1;
    PicShow.Manual := True;
  end;
end;

end.

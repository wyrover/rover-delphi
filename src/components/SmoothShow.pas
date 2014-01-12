{------------------------------------------------------------------------------}
{                                                                              }
{  TSmoothShow v2.04                                                           }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit SmoothShow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type

  TMinLoc = (mlCenter, mlTopLeft, mlTopRight, mlBottomLeft, mlBottomRight, mlCustom);
  TMaxLoc = (xlDefault, xlCustom);

  TSmoothShow = class(TComponent)
  private
    FDelay: Word;
    FColor: TColor;
    FBorderWidth: TBorderWidth;
    FEnabled: Boolean;
    FReverse: Boolean;
    FMinLocation: TMinLoc;
    FMinLeft: Integer;
    FMinTop: Integer;
    FMinWidth: Word;
    FMinHeight: Word;
    FMaxLocation: TMaxLoc;
    FMaxTop: Integer;
    FMaxLeft: Integer;
    FMaxHeight: Word;
    FMaxWidth: Word;
    FControl: TWinControl;
    FBusy: Boolean;
    FOnFinishing: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    Timer: TTimer;
    Shadow: TForm;
    ThisStep: Integer;
    StartRect: TRect;
    StopRect: TRect;
    DiffRect: TRect;
    OrignalWndProc: TWndMethod;
    function GetMinCustomBounds: TRect;
    procedure SetMinCustomBounds(Value: TRect);
    function GetMaxCustomBounds: TRect;
    procedure SetMaxCustomBounds(Value: TRect);
    procedure SetControl(Value: TWinControl);
    function MinimizedRect: TRect;
    function MaximizedRect: TRect;
    function CurrentRect(ThisStep: Word): TRect;
    procedure TimerFired(Sender: TObject);
    function IsControlStored: Boolean;
    procedure NoShowWndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Execute;
    property Busy: Boolean read FBusy;
    property MinCustomBounds: TRect read GetMinCustomBounds write SetMinCustomBounds;
    property MaxCustomBounds: TRect read GetMaxCustomBounds write SetMaxCustomBounds;
  published
    property Color: TColor read FColor write FColor default clBlack;
    property BorderWidth: TBorderWidth read FBorderWidth write FBorderWidth default 1;
    property Delay: Word read FDelay write FDelay default 50;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property MinLocation: TMinLoc read FMinLocation write FMinLocation default mlCenter;
    property MinLeft: Integer read FMinLeft write FMinLeft default 0;
    property MinTop: Integer read FMinTop write FMinTop default 0;
    property MinWidth: Word read FMinWidth write FMinWidth default 27;
    property MinHeight: Word read FMinHeight write FMinHeight default 27;
    property MaxLocation: TMaxLoc read FMaxLocation write FMaxLocation default xlDefault;
    property MaxLeft: Integer read FMaxLeft write FMaxLeft default 100;
    property MaxTop: Integer read FMaxTop write FMaxTop default 100;
    property MaxWidth: Word read FMaxWidth write FMaxWidth default 127;
    property MaxHeight: Word read FMaxHeight write FMaxHeight default 127;
    property Reverse: Boolean read FReverse write FReverse default False;
    property Control: TWinControl read FControl write SetControl Stored IsControlStored;
    property OnFinishing: TNotifyEvent read FOnFinishing write FOnFinishing;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

implementation

{ TSmoothShow }

constructor TSmoothShow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDelay := 50;
  FColor := clBlack;
  FEnabled := True;
  FReverse := False;
  FMinLocation := mlCenter;
  FMaxLocation := xlDefault;
  FBorderWidth := 1;
  if AOwner is TWinControl then
    FControl := (AOwner as TWinControl)
  else
    FControl := nil;
  MinCustomBounds := Rect(0, 0, 27, 27);
  MaxCustomBounds := Rect(100, 100, 227, 227);
  FBusy := False;
end;

procedure TSmoothShow.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Control then
      FControl := nil;
end;

function TSmoothShow.IsControlStored: Boolean;
begin
  Result := (Control <> Owner);
end;

function TSmoothShow.GetMinCustomBounds: TRect;
begin
  SetRect(Result, MinLeft, MinTop, MinLeft + MinWidth, MinTop + MinHeight);
end;

procedure TSmoothShow.SetMinCustomBounds(Value: TRect);
begin
  With Value do
  begin
    FMinLeft := Left;
    FMinTop := Top;
    FMinWidth := Right - Left;
    FMinHeight := Bottom - Top;
  end;
end;

function TSmoothShow.GetMaxCustomBounds: TRect;
begin
  SetRect(Result, MaxLeft, MaxTop, MaxLeft + MaxWidth, MaxTop + MinHeight);
end;

procedure TSmoothShow.SetMaxCustomBounds(Value: TRect);
begin
  With Value do
  begin
    FMaxLeft := Left;
    FMaxTop := Top;
    FMaxWidth := Right - Left;
    FMaxHeight := Bottom - Top;
  end;
end;

procedure TSmoothShow.SetControl(Value: TWinControl);
begin
  if not Busy and (Control <> Value) then
  begin
    FControl := Value;
    if Control <> nil then
      Control.FreeNotification(Self);
  end;
end;

function TSmoothShow.MinimizedRect: TRect;
begin
  case MinLocation of
    {$IFDEF COMPILER4_UP}
    mlCenter:
      SetRect(Result, Screen.DesktopLeft + Screen.DesktopWidth div 2,
        Screen.DesktopTop + Screen.DesktopHeight div 2, 0, 0);
    mlTopLeft:
      SetRect(Result, Screen.DesktopLeft, Screen.DesktopTop, 0, 0);
    mlTopRight:
      SetRect(Result, Screen.DesktopLeft + Screen.DesktopWidth,
        Screen.DesktopTop, 0, 0);
    mlBottomLeft:
      SetRect(Result, Screen.DesktopLeft, Screen.DesktopTop +
        Screen.DesktopHeight, 0, 0);
    mlBottomRight:
      SetRect(Result, Screen.DesktopLeft + Screen.DesktopWidth,
        Screen.DesktopTop + Screen.DesktopHeight, 0, 0);
    {$ELSE}
    mlCenter:
      SetRect(Result, Screen.Width div 2, Screen.Height div 2, 0, 0);
    mlTopLeft:
      SetRect(Result, 0, 0, 0, 0);
    mlTopRight:
      SetRect(Result, Screen.Width, 0, 0, 0);
    mlBottomLeft:
      SetRect(Result, 0, Screen.Height, 0, 0);
    mlBottomRight:
      SetRect(Result, Screen.Width, Screen.Height, 0, 0);
    {$ENDIF}
  else
    SetRect(Result, MinLeft, MinTop, MinWidth, MinHeight);
  end;
end;

function TSmoothShow.MaximizedRect: TRect;
begin
  if (MaxLocation = xlDefault) and Assigned(Control) then
    with Control.ClientToScreen(Point(0,0)) do
      SetRect(Result, X, Y, Control.Width, Control.Height)
  else
    SetRect(Result, MaxLeft, MaxTop, MaxWidth, MaxHeight);
end;

function TSmoothShow.CurrentRect(ThisStep: Word): TRect;
begin
  Result.Left := StartRect.Left + (ThisStep * DiffRect.Left) div 100;
  Result.Top := StartRect.Top + (ThisStep * DiffRect.Top) div 100;
  Result.Right := StartRect.Right + (ThisStep * DiffRect.Right) div 100;
  Result.Bottom := StartRect.Bottom + (ThisStep * DiffRect.Bottom) div 100;
end;

procedure TSmoothShow.NoShowWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_WINDOWPOSCHANGING then
    with PWindowPos(Message.lParam)^ do
      flags := flags or SWP_HIDEWINDOW and not SWP_SHOWWINDOW;
  OrignalWndProc(Message);
end;

procedure TSmoothShow.Execute;
begin
  if not Busy and Assigned(Control) and
    (Enabled or (csDesigning in ComponentState)) then
  begin
    FBusy := True;
    if Reverse then
    begin
      StartRect := MaximizedRect;
      StopRect := MinimizedRect;
    end
    else
    begin
      StartRect := MinimizedRect;
      StopRect := MaximizedRect;
    end;
    DiffRect.Left := StopRect.Left - StartRect.Left;
    DiffRect.Top := StopRect.Top - StartRect.Top;
    DiffRect.Right := StopRect.Right - StartRect.Right;
    DiffRect.Bottom := StopRect.Bottom - StartRect.Bottom;
    if not (csDesigning in ComponentState) then
    begin
      if IsWindowVisible(Control.Handle) then
        SetWindowPos(Control.Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or
          SWP_NOZORDER or SWP_NOSIZE or SWP_NOMOVE or SWP_NOSENDCHANGING);
      OrignalWndProc := Control.WindowProc;
      Control.WindowProc := NoShowWndProc;
    end;
    Shadow := TForm.Create(Application);
    Shadow.BorderStyle := bsNone;
    Shadow.Color := Color;
    ThisStep := 0;
    Timer := TTimer.Create(Self);
    Timer.Interval := Delay;
    Timer.OnTimer := TimerFired;
    TimerFired(Self);
  end;
end;

procedure TSmoothShow.TimerFired(Sender: TObject);
var
  Rgn1, Rgn2: HRgn;
begin
  with CurrentRect(ThisStep) do
    Shadow.SetBounds(Left, Top, Right, Bottom);
  Rgn1 := CreateRectRgn(0, 0, Shadow.Width, Shadow.Height);
  Rgn2 := CreateRectRgn(BorderWidth, BorderWidth,
    Shadow.Width-BorderWidth, Shadow.Height-BorderWidth);
  CombineRgn(Rgn1, Rgn1, Rgn2, RGN_DIFF);
  DeleteObject(Rgn2);
  SetWindowRgn(Shadow.Handle, Rgn1, True);
  if not Shadow.Visible then
  begin
    SetWindowPos(Shadow.Handle, HWND_TOP, 0, 0, 0, 0,
      SWP_SHOWWINDOW or SWP_NOACTIVATE);
    Shadow.Visible := True;
  end;
  Shadow.Update;
  if ThisStep >= 100 then
  begin
    Timer.Free;
    if not (csDesigning in ComponentState) and Assigned(OnFinishing) then
      OnFinishing(Self);
    Shadow.Free;
    if not (csDesigning in ComponentState) then
    begin
      Control.WindowProc := OrignalWndProc;
      if Control.Visible and not IsWindowVisible(Control.Handle) then
        SetWindowPos(Control.Handle, 0, 0, 0, 0, 0, SWP_SHOWWINDOW or
          SWP_NOZORDER or SWP_NOSIZE or SWP_NOMOVE or SWP_NOSENDCHANGING);
    end;
    FBusy := False;
    if not (csDesigning in ComponentState) and Assigned(OnFinish) then
      OnFinish(Self);
  end;
  ThisStep := (3 * ThisStep div 2) + 2;
  if ThisStep > 100 then ThisStep := 100;
end;

end.

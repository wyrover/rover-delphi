{------------------------------------------------------------------------------}
{                                                                              }
{  ToolTipManager v1.3                                                         }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit ToolTipManager;

interface

uses
  WIndows, Messages, Classes, Graphics, Controls;

type

  TToolTipItem = class;
  TToolTipItems = class;
  TToolTipManager = class;

  TToolTipItemClass = class of TToolTipItem;

  TToolTipIcon = (ttiNone, ttiInformation, ttiWarning, ttiError,
    ttiInformationLarge, ttiWarningLarge, ttiErrorLarge, ttiCustom);

  TToolTipEvent = procedure(Sender: TObject; ToolTip: TToolTipItem) of object;

  // TToolTip Item
  TToolTipItem = class(TCollectionItem)
  private
    fControl: TWinControl;
    fIconType: TToolTipIcon;
    fCustomIcon: TIcon;
    fTitle: String;
    fDescription: String;
    fColor: TColor;
    fTextColor: TColor;
    procedure SetControl(Value: TWinControl);
    procedure SetCustomIcon(Value: TIcon);
    function GetIcon: HIcon;
  protected
    function GetDisplayName: String; override;
    function GetManager: TToolTipManager;
    property Icon: HIcon read GetIcon;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Refresh;
  published
    property Control: TWinControl read fControl write SetControl;
    property CustomIcon: TIcon read fCustomIcon write SetCustomIcon;
    property IconType: TToolTipIcon read fIconType write fIconType default ttiNone;
    property Title: String read fTitle write fTitle;
    property Description: String read fDescription write fDescription;
    property Color: TColor read fColor write fColor default clDefault;
    property TextColor: TColor read fTextColor write fTextColor default clDefault;
  end;

  // TToolTipItems
  TToolTipItems = class(TCollection)
  private
    fOwner: TPersistent;
    function GetItem(Index: Integer): TToolTipItem;
    procedure SetItem(Index: Integer; Value: TToolTipItem);
    function GetByHandle(Handle: THandle): TToolTipItem;
  protected
    function GetOwner: TPersistent; override;
  public
    {$IFNDEF COMPILER4_UP}
    constructor Create(AOwner: TPersistent; ItemClass: TToolTipItemClass); virtual;
    {$ELSE}
    constructor Create(AOwner: TPersistent; ItemClass: TToolTipItemClass); reintroduce; virtual;
    {$ENDIF}
    function Add: TToolTipItem;
    {$IFDEF COMPILER4_UP}
    function Insert(Index: Integer): TToolTipItem;
    {$ENDIF}
    property ByHandle[Handle: THandle]: TToolTipItem read GetByHandle;
    property Items[Index: Integer]: TToolTipItem read GetItem write SetItem; default;
  end;

  TToolTipManager = class(TComponent)
  private
    fEnabled: Boolean;
    fToolTips: TToolTipItems;
    fDefaultColor: TColor;
    fDefaultTextColor: TColor;
    fDefaultCustomIcon: TIcon;
    fCenterTip: Boolean;
    fBalloonTip: Boolean;
    fShowPause: DWord;
    fHidePause: DWord;
    fReshowPause: DWord;
    fOwnerWnd: THandle;
    fToolTipWnd: THandle;
    fVisibleToolTip: TToolTipItem;
    fOnBeforeShow: TToolTipEvent;
    fOnShow: TToolTipEvent;
    fOnHide: TToolTipEvent;
    procedure SetEnabled(Value: Boolean);
    procedure SetToolTips(Value: TToolTipItems);
    procedure SetBalloonTip(Value: Boolean);
    procedure SetCenterTip(Value: Boolean);
    procedure SetShowPause(Value: DWord);
    procedure SetHidePause(Value: DWord);
    procedure SetReshowPause(Value: DWord);
    procedure SetDefaultCustomIcon(Value: TIcon);
    procedure Callback(var Message: TMessage);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure AddToolTip(ToolTip: TToolTipItem); virtual;
    procedure UpdateToolTip(ToolTip: TToolTipItem); virtual;
    procedure RemoveToolTip(ToolTip: TToolTipItem); virtual;
    procedure ApplyToolTipParams(ToolTip: TToolTipItem); virtual;
    property OwnerWnd: THandle read fOwnerWnd;
    property ToolTipWnd: THandle read fToolTipWnd;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RebuildToolTips; virtual;
    procedure RemoveToolTips; virtual;
    property VisibleToolTip: TToolTipItem read fVisibleToolTip;
  published
    property BalloonTip: Boolean read fBalloonTip write SetBalloonTip default True;
    property CenterTip: Boolean read fCenterTip write SetCenterTip default False;
    property DefaultColor: TColor read fDefaultColor write fDefaultColor default clInfoBk;
    property DefaultTextColor: TColor read fDefaultTextColor write fDefaultTextColor default clInfoText;
    property DefaultCustomIcon: TIcon read fDefaultCustomIcon write SetDefaultCustomIcon;
    property Enabled: Boolean read fEnabled write SetEnabled default True;
    property ShowPause: DWord read fShowPause write SetShowPause default 500;
    property HidePause: DWord read fHidePause write SetHidePause default 5000;
    property ReshowPause: DWord read fReshowPause write SetReshowPause default 100;
    property ToolTips: TToolTipItems read fToolTips write SetToolTips;
    property OnBeforeShow: TToolTipEvent read fOnBeforeShow write fOnBeforeShow;
    property OnShow: TToolTipEvent read fOnShow write fOnShow;
    property OnHide: TToolTipEvent read fOnHide write fOnHide;
  end;



implementation

uses
  CommCtrl, Forms;


const
  TTS_BALLOON  = $40;
  {$IFDEF UNICODE}
  TTM_SETTITLE = WM_USER + 33;
  {$ELSE}
  TTM_SETTITLE = WM_USER + 32;
  {$ENDIF}

{ TToolTipItem }

constructor TToolTipItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fCustomIcon := TIcon.Create;
  fColor := clDefault;
  fTextColor := clDefault;
end;

destructor TToolTipItem.Destroy;
begin
  Control := nil;
  fCustomIcon.Free;
  inherited Destroy;
end;

procedure TToolTipItem.Assign(Source: TPersistent);
begin
  if Source is TToolTipItem then
    with TToolTipItem(Source) do
    begin
      Self.Control := Control;
      Self.IconType := IconType;
      Self.CustomIcon := CustomIcon;
      Self.Title := Title;
      Self.Description := Description;
      Self.Color := Color;
      Self.TextColor := TextColor;
    end
  else
    inherited Assign(Source);
end;

function TToolTipItem.GetDisplayName: String;
begin
  if Title <> '' then
    if Description <> '' then
      Result := '<' + Title + '> ' + Description
    else
      Result := '<' + Title + '>'
  else if Description <> '' then
    Result := Description
  else
    Result := inherited GetDisplayName;
end;

function TToolTipItem.GetManager: TToolTipManager;
begin
  Result := TToolTipManager(TToolTipItems(Collection).GetOwner);
end;

procedure TToolTipItem.SetControl(Value: TWinControl);
begin
  if Control <> Value then
  begin
    if Assigned(Control) and not (csDestroying in Control.ComponentState) then
    begin
      {$IFDEF COMPILER5_UP}
      Control.RemoveFreeNotification(GetManager);
      {$ENDIF}
      GetManager.RemoveToolTip(Self);
    end;
    fControl := Value;
    if Assigned(Control) then
    begin
      Control.FreeNotification(GetManager);
      if Title = '' then
        Title := GetShortHint(Control.Hint);
      if Description = '' then
        Description := GetLongHint(Control.Hint);
      GetManager.AddToolTip(Self);
    end;
  end;
end;

procedure TToolTipItem.SetCustomIcon(Value: TIcon);
begin
  CustomIcon.Assign(Value);
end;

function TToolTipItem.GetIcon: HIcon;
begin
  if IconType = ttiCustom then
  begin
    if not CustomIcon.Empty then
      Result := CustomIcon.Handle
    else if not GetManager.DefaultCustomIcon.Empty then
      Result := GetManager.DefaultCustomIcon.Handle
    else
      Result := Application.Icon.Handle;
  end
  else
    Result := Ord(IconType);
end;

procedure TToolTipItem.Refresh;
begin
  if Control <> nil then
    GetManager.UpdateToolTip(Self);
end;

{ TToolTipItems }

constructor TToolTipItems.Create(AOwner: TPersistent; ItemClass: TToolTipItemClass);
begin
  inherited Create(ItemClass);
  fOwner := AOwner;
end;

function TToolTipItems.GetOwner: TPersistent;
begin
  Result := fOwner;
end;

function TToolTipItems.Add: TToolTipItem;
begin
  Result := TToolTipItem(inherited Add);
end;

{$IFDEF COMPILER4_UP}
function TToolTipItems.Insert(Index: Integer): TToolTipItem;
begin
  Result := TToolTipItem(inherited Insert(Index));
end;
{$ENDIF}

function TToolTipItems.GetByHandle(Handle: THandle): TToolTipItem;
var
  I: Integer;
  Nearest: TToolTipItem;
begin
  Nearest := nil;
  for I := Count - 1 downto 0 do
  begin
    Result := Items[I];
    if Assigned(Result.Control) then
    begin
      if Result.Control.Handle = Handle then
        Exit
      else if not Assigned(Nearest) and IsChild(Result.Control.Handle, Handle) then
        Nearest := Result;
    end;
  end;
  Result := Nearest;
end;

function TToolTipItems.GetItem(Index: Integer): TToolTipItem;
begin
  Result := TToolTipItem(inherited Items[Index]);
end;

procedure TToolTipItems.SetItem(Index: Integer; Value: TToolTipItem);
begin
  inherited Items[Index] := Value;
end;

{ TToolTipManager }

constructor TToolTipManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOwnerWnd := {$IFDEF COMPILER6_UP}Classes.{$ENDIF}AllocateHWnd(Callback);
  fToolTips := TToolTipItems.Create(Self, TToolTipItem);
  fDefaultCustomIcon := TIcon.Create;
  fDefaultColor := clInfoBk;
  fDefaultTextColor := clInfoText;
  fShowPause := 500;
  fHidePause := 5000;
  fReshowPause := 100;
  fBalloonTip := True;
  fEnabled := True;
end;

destructor TToolTipManager.Destroy;
begin
  fToolTips.Free;
  fDefaultCustomIcon.Free;
  RemoveToolTips;
  {$IFDEF COMPILER6_UP}Classes.{$ENDIF}DeallocateHWnd(fOwnerWnd);
  inherited Destroy;
end;

procedure TToolTipManager.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    for I := 0 to ToolTips.Count - 1 do
      if ToolTips[I].Control = AComponent then
        ToolTips[I].Control := nil;
end;

procedure TToolTipManager.Loaded;
begin
  inherited Loaded;
  RebuildToolTips;
end;

procedure TToolTipManager.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then
  begin
    fEnabled := Value;
    if fEnabled then
      RebuildToolTips
    else
      RemoveToolTips;
  end;
end;

procedure TToolTipManager.SetBalloonTip(Value: Boolean);
begin
  if BalloonTip <> Value then
  begin
    fBalloonTip := Value;
    RebuildToolTips;
  end;
end;

procedure TToolTipManager.SetCenterTip(Value: Boolean);
begin
  if CenterTip <> Value then
  begin
    fCenterTip := Value;
    RebuildToolTips;
  end;
end;

procedure TToolTipManager.SetShowPause(Value: DWord);
begin
  if ShowPause <> Value then
  begin
    fShowPause := Value;
    if ToolTipWnd <> 0 then
      SendMessage(ToolTipWnd, TTM_SETDELAYTIME, TTDT_INITIAL, MakeLong(fShowPause, 0));
  end;
end;

procedure TToolTipManager.SetHidePause(Value: DWord);
begin
  if HidePause <> Value then
  begin
    fHidePause := Value;
    if ToolTipWnd <> 0 then
      SendMessage(ToolTipWnd, TTM_SETDELAYTIME, TTDT_AUTOPOP, MakeLong(fHidePause, 0));
  end;
end;

procedure TToolTipManager.SetReshowPause(Value: DWord);
begin
  if ReshowPause <> Value then
  begin
    fReshowPause := Value;
    if ToolTipWnd <> 0 then
      SendMessage(ToolTipWnd, TTM_SETDELAYTIME, TTDT_RESHOW, MakeLong(fReshowPause, 0));
  end;
end;

procedure TToolTipManager.SetDefaultCustomIcon(Value: TIcon);
begin
  fDefaultCustomIcon.Assign(Value);
end;

procedure TToolTipManager.SetToolTips(Value: TToolTipItems);
begin
  ToolTips.Assign(Value);
end;

procedure TToolTipManager.RebuildToolTips;
var
  I: Integer;
  Style: DWord;
begin
  if not (csLoading in ComponentState) and
     not (csDesigning in ComponentState) and
     not (csDestroying in ComponentState) and
     fEnabled then
  begin
    RemoveToolTips;
    Style := TTS_NOPREFIX or TTS_ALWAYSTIP;
    if BalloonTip then
      Style := Style or TTS_BALLOON;
    fToolTipWnd := CreateWindow(TOOLTIPS_CLASS, nil, Style, 0, 0, 0, 0,
      fOwnerWnd, 0, HInstance, nil);
    if fToolTipWnd <> 0 then
    begin
      SendMessage(ToolTipWnd, TTM_SETMAXTIPWIDTH, 0, Screen.Width div 3);
      SendMessage(ToolTipWnd, TTM_SETDELAYTIME, TTDT_INITIAL, MakeLong(fShowPause, 0));
      SendMessage(ToolTipWnd, TTM_SETDELAYTIME, TTDT_AUTOPOP, MakeLong(fHidePause, 0));
      SendMessage(ToolTipWnd, TTM_SETDELAYTIME, TTDT_RESHOW, MakeLong(fReshowPause, 0));
      for I := 0 to ToolTips.Count - 1 do
        AddToolTip(ToolTips[I]);
    end;
  end;
end;

procedure TToolTipManager.RemoveToolTips;
begin
  if fToolTipWnd <> 0 then
  begin
    DestroyWindow(fToolTipWnd);
    fToolTipWnd := 0;
  end;
  fVisibleToolTip := nil;
end;

procedure TToolTipManager.AddToolTip(ToolTip: TToolTipItem);
begin
  UpdateToolTip(ToolTip);
end;

procedure TToolTipManager.UpdateToolTip(ToolTip: TToolTipItem);
var
  ToolInfo: TToolInfo;
begin
  if (ToolTipWnd <> 0) and Assigned(ToolTip.Control) then
  begin
    FillChar(ToolInfo, SizeOf(ToolInfo), 0);
    ToolInfo.cbSize := SizeOf(ToolInfo);
    ToolInfo.hwnd := OwnerWnd;
    ToolInfo.uId := ToolTip.Control.Handle;
    ToolInfo.uFlags := TTF_TRANSPARENT or TTF_SUBCLASS or TTF_IDISHWND;
    if CenterTip then
      ToolInfo.uFlags := ToolInfo.uFlags or TTF_CENTERTIP;
    ToolInfo.lpszText := LPSTR_TEXTCALLBACK;
    SendMessage(ToolTipWnd, TTM_ADDTOOL, 0, Integer(@ToolInfo));
    if not (csAcceptsControls in ToolTip.Control.ControlStyle) then
    begin
      ToolInfo.uId := GetWindow(ToolTip.Control.Handle, GW_CHILD);
      while ToolInfo.uId <> 0 do
      begin
        SendMessage(ToolTipWnd, TTM_ADDTOOL, 0, Integer(@ToolInfo));
        ToolInfo.uId := GetWindow(ToolInfo.uId, GW_HWNDNEXT);
      end;
    end;
  end;
end;

procedure TToolTipManager.RemoveToolTip(ToolTip: TToolTipItem);
var
  ToolInfo: TToolInfo;
begin
  if (ToolTipWnd <> 0) and Assigned(ToolTip.Control) then
  begin
    FillChar(ToolInfo, SizeOf(ToolInfo), 0);
    ToolInfo.cbSize := SizeOf(ToolInfo);
    ToolInfo.hwnd := OwnerWnd;
    ToolInfo.uId := ToolTip.Control.Handle;
    SendMessage(ToolTipWnd, TTM_DELTOOL, 0, Integer(@ToolInfo));
  end;
end;

procedure TToolTipManager.ApplyToolTipParams(ToolTip: TToolTipItem);
begin
  if ToolTipWnd <> 0 then
  begin
    if ToolTip.Color = clDefault then
      SendMessage(ToolTipWnd, TTM_SETTIPBKCOLOR, ColorToRGB(DefaultColor), 0)
    else
      SendMessage(ToolTipWnd, TTM_SETTIPBKCOLOR, ColorToRGB(ToolTip.Color), 0);
    if ToolTip.TextColor = clDefault then
      SendMessage(ToolTipWnd, TTM_SETTIPTEXTCOLOR, ColorToRGB(DefaultTextColor), 0)
    else
      SendMessage(ToolTipWnd, TTM_SETTIPTEXTCOLOR, ColorToRGB(ToolTip.TextColor), 0);
    SendMessage(ToolTipWnd, TTM_SETTITLE, ToolTip.Icon, Integer(PChar(ToolTip.Title)));
  end;
end;

procedure TToolTipManager.Callback(var Message: TMessage);
var
  NotifyMsg: TWMNotify absolute Message;
  ToolTip: TToolTipItem;
begin
  if Message.Msg = WM_NOTIFY then
  begin
    ToolTip := ToolTips.ByHandle[NotifyMsg.NMHdr^.idFrom];
    if Assigned(ToolTip) then
      case NotifyMsg.NMHdr^.code of
        TTN_NEEDTEXT:
        begin
          if Assigned(OnBeforeShow) then
            OnBeforeShow(Self, ToolTip);
          PNMTTDispInfo(NotifyMsg.NMHdr)^.lpszText := PChar(ToolTip.Description);
          ApplyToolTipParams(ToolTip);
        end;
        TTN_SHOW:
        begin
          if Assigned(OnShow) then
            OnShow(Self, ToolTip);
          fVisibleToolTip := ToolTip;
        end;
        TTN_POP:
        begin
          fVisibleToolTip := nil;
          if Assigned(OnHide) then
            OnHide(Self, ToolTip);
        end;
      end;
  end
  else
    with Message do Result := DefWindowProc(OwnerWnd, Msg, WParam, LParam);
end;

end.

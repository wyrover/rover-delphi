unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Gauges, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    tmr1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    function Linear(t,b,c,d: Single): Single;

  private
    x: Integer;
    y: Integer;
    targetX: Integer;
    targetY: Integer;
    easing: Single;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  nScreenX, nScreenY: integer;
  nLeft, nTop: integer;
begin
  Self.Caption := ExtractFileName(Application.ExeName);

  self.Width := 200;
  Self.Height := 200;

  nScreenX := GetSystemMetrics(SM_CXSCREEN);
  nScreenY := GetSystemMetrics(SM_CYSCREEN);

  Self.x := 0;
  self.y := 0;

  targetX := trunc((nScreenX - Self.Width) / 2);
  targetY := trunc((nScreenY - Self.Height) / 2);



  easing := 0.2;

  MoveWindow(Self.Handle, 0, 0, 200, 200, False);
end;

function TMainForm.Linear(t,b,c,d: Single): Single;
begin
  Result := c*t/d + b;
end;

procedure TMainForm.tmr1Timer(Sender: TObject);
var
  vx, vy: Integer;
begin
  vx := trunc((targetX - Self.x) * easing);
  vy := trunc((targetY - Self.y) * easing);
  self.x := Self.x + vx;
  self.y := Self.y + vy;

  MoveWindow(Self.Handle, Self.x, Self.y, 200, 200, False);

  if (Abs(Self.targetX-self.x) < 5) then
  begin
    Self.x := Self.targetX;
    tmr1.Enabled := false;
  end;
end;



end.

unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Gauges, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    tmr1: TTimer;
    tmr2: TTimer;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    function Linear(t,b,c,d: integer): Single;
    function Quad_easeIn(t, b, c, d: Integer): Single;
    function Quart_easeIn(t, b , c, d: integer): Single;

    procedure StartMove;
    procedure tmr2Timer(Sender: TObject);
  private
    x: Integer;
    y: Integer;
    targetX: Integer;
    targetY: Integer;
    easing: Single;

    t: integer;
    b: integer;
    c: integer;
    d: integer;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btn1Click(Sender: TObject);
begin
  StartMove;
end;



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

function TMainForm.Linear(t,b,c,d: integer): Single;
begin
  Result := c*t/d + b;
end;

function TMainForm.Quad_easeIn(t, b, c, d: Integer): Single;
var
  t2: Single;
begin
  t2 := t / d;
  Result := c * t2 * t2 + b;
end;

function TMainForm.Quart_easeIn(t, b , c, d: integer): Single;
var
  t2: Single;
begin
  t2 := t / d;
  Result := c* t2 *t2*t2*t2 + b;
end;





procedure TMainForm.StartMove;
begin
  t := 0;

  d := 24;

  tmr2.Interval := 30;
  tmr2.Enabled := True;
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

procedure TMainForm.tmr2Timer(Sender: TObject);
begin
  if (t <= d) then
  begin
    // xÖá´Ó300ÒÆ¶¯µ½500
    MoveWindow(Self.Handle, trunc(Quad_easeIn(t, 300, 500, d)), trunc(Quart_easeIn(t, 300, 500, d)), 200, 200, False);
    Inc(t);
  end else
  begin
    tmr2.Enabled := false;
  end;
end;



end.

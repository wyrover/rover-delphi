unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SmoothShow;

type
  TMainForm = class(TForm)
    SmoothShow1: TSmoothShow;
    SmoothShow2: TSmoothShow;
    Button1: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SmoothShow1Finishing(Sender: TObject);
    procedure SmoothShow1Finish(Sender: TObject);
    procedure SmoothShow2Finishing(Sender: TObject);
    procedure SmoothShow2Finish(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Button1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses ShellAPI;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SmoothShow1.Reverse := False;
  SmoothShow1.Execute;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not SmoothShow1.Reverse then
  begin
    SmoothShow1.Reverse := True;
    SmoothShow1.Execute;
    CanClose := False;
  end;
end;

procedure TMainForm.SmoothShow1Finishing(Sender: TObject);
begin
  if SmoothShow1.Reverse then
    Hide;
end;

procedure TMainForm.SmoothShow1Finish(Sender: TObject);
begin
  if SmoothShow1.Reverse then
    Close;
end;

procedure TMainForm.SmoothShow2Finishing(Sender: TObject);
begin
  Panel1.Visible := not SmoothShow2.Reverse;
end;

procedure TMainForm.SmoothShow2Finish(Sender: TObject);
begin
  SmoothShow2.Reverse := not SmoothShow2.Reverse;
  if SmoothShow2.Reverse then
    Button1.Caption := 'Hide About'
  else
    Button1.Caption := 'Show About';
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  R: TRect;
begin
  R := Button1.BoundsRect;
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);
  SmoothShow2.MinCustomBounds := R;
  SmoothShow2.Execute;
end;

procedure TMainForm.Label6Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(TControl(Sender).Hint), nil, nil, SW_NORMAL);
end;

procedure TMainForm.Button1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  R := Button1.BoundsRect;
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);
  SmoothShow2.MinCustomBounds := R;
  SmoothShow2.Execute;
end;

end.

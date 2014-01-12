unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Gauges, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    g: TGauge2;
    tmr1: TTimer;
    g21: TGauge2;
    g22: TGauge2;
    g23: TGauge2;
    g24: TGauge2;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  self.Width := 800;
  Self.Height := 600;
  Self.Caption := ExtractFileName(Application.ExeName);
end;

procedure TMainForm.btn1Click(Sender: TObject);
begin
  g.AddProgress(1);
end;

procedure TMainForm.tmr1Timer(Sender: TObject);
begin
  g.AddProgress(1);
  g21.AddProgress(1);
  g22.AddProgress(1);
  g23.AddProgress(1);
  g24.AddProgress(1)
end;

end.

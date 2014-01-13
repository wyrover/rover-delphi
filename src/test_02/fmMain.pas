
unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Gauges, StdCtrls, ExtCtrls, RotImg, pngimage, GIFImg;

type
  TMainForm = class(TForm)
    tmr1: TTimer;
    img1: TRotateImage;
    procedure FormCreate(Sender: TObject);
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

  Self.DoubleBuffered := True;
end;

procedure TMainForm.tmr1Timer(Sender: TObject);
begin
   img1.Angle := Frac((img1.Angle + 1.0) / 360.0) * 360.0;
end;

end.

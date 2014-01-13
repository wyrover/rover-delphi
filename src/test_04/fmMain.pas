unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Gauges, StdCtrls, ExtCtrls, u360StyleButton;

type
  TMainForm = class(TForm)
    tmr1: TTimer;
    btn360styl1: TBtn360Style;
    procedure FormCreate(Sender: TObject);
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

end.

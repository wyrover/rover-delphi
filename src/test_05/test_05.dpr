program test_05;

uses
  Forms,
  fmMain in 'fmMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

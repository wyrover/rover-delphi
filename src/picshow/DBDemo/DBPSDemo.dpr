program DBPSDemo;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Splash in 'Splash.pas' {SplashForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TDBPicShow Demo';
  TSplashForm.ShowSplash;
  try
    Application.CreateForm(TMainForm, MainForm);
  finally
    TSplashForm.HideSplash;
  end;
  Application.Run;
end.

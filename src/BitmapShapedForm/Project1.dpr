program Project1;

uses
  Forms,
  UForm1 in 'UForm1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

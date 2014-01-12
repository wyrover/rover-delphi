program Canvas1;

uses
  Forms,
  U_Canvas1 in 'U_Canvas1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

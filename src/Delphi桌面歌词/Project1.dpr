program Project1;

uses
  //CnMemProf,
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  //mmPopupMsgDlg := True;
  //mmShowObjectInfo := True;
  //mmUseObjectList := True;
  //mmSaveToLogFile := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

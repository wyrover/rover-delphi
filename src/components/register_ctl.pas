unit register_ctl;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls,
  DesignIntf, DesignEditors;


procedure Register;

implementation

uses
  TypInfo, rover_ctl_mycontrol, rover_gauges, ToolTipManager, RotImg;

procedure Register;
begin
  RegisterComponents('ROVER', [TMyControl]);
  RegisterComponents('ROVER', [TGauge2]);
  RegisterComponents('ROVER', [TToolTipManager]);
  RegisterComponents('ROVER', [TRotateImage]);
  //RegisterComponentEditor(TRotateImage, TRotateImageEditor);
end;

end.
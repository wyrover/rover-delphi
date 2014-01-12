unit register_ctl;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls;


procedure Register;

implementation

uses
  rover_ctl_mycontrol, Gauges, ToolTipManager;

procedure Register;
begin
  RegisterComponents('ROVER', [TMyControl]);
  RegisterComponents('ROVER', [TGauge2]);
  RegisterComponents('ROVER', [TToolTipManager]);

end;

end.
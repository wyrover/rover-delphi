{------------------------------------------------------------------------------}
{                                                                              }
{  TSmoothShow v2.04                                                           }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit SSReg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF COMPILER6_UP} DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF};

type
  TSmoothShowEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses
  SmoothShow;

procedure TSmoothShowEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    (Component as TSmoothShow).Execute
  else
    inherited ExecuteVerb(Index);
end;

function TSmoothShowEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show the Effect';
  else
    Result := '';
  end;
end;

function TSmoothShowEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TSmoothShowEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure Register;
begin
  RegisterComponents('ROVER', [TSmoothShow]);
  RegisterComponentEditor(TSmoothShow, TSmoothShowEditor);
end;

end.

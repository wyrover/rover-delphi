{------------------------------------------------------------------------------}
{                                                                              }
{  TRotateImage v1.54                                                          }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit RotImgReg;

interface

uses
  Windows, Classes,
  {$IFDEF COMPILER6_UP} DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF};

type
  TRotateImageEditor = class(TDefaultEditor)
  private
    {$IFNDEF COMPILER6_UP}
    procedure PictureEditor(Prop: TPropertyEditor);
    {$ENDIF}
  public
    {$IFDEF COMPILER6_UP}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
    {$ELSE}
    procedure Edit; override;
    {$ENDIF}
  end;

procedure Register;

implementation

uses
  RotImg, TypInfo;

{$IFDEF COMPILER6_UP}

procedure TRotateImageEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if Prop.GetName = 'Picture' then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;

{$ELSE}

procedure TRotateImageEditor.PictureEditor(Prop: TPropertyEditor);
begin
  if Prop.GetName = 'Picture' then
    Prop.Edit;
end;

procedure TRotateImageEditor.Edit;
var
  {$IFDEF COMPILER5_UP}
  List: TDesignerSelectionList;
  {$ELSE}
  List: TComponentList;
  {$ENDIF}
begin
  {$IFDEF COMPILER5_UP}
  List := TDesignerSelectionList.Create;
  {$ELSE}
  List := TComponentList.Create;
  {$ENDIF}
  try
    List.Add(Component);
    GetComponentProperties(List, [tkClass], Designer, PictureEditor);
  finally
    List.Free;
  end;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('ROVER', [TRotateImage]);
  RegisterComponentEditor(TRotateImage, TRotateImageEditor);
end;

end.

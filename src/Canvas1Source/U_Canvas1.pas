unit U_Canvas1;
{Example of saving and restoring canvas contents using a bitmap}
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    DrawBtn: TButton;
    SaveBtn: TButton;
    Image1: TImage;
    ClearBtn: TButton;
    ListBox1: TListBox;
    Label2: TLabel;
    Label1: TLabel;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    procedure DrawBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    drawing:boolean;  {user is scribbling on the canvas}
  end;

var
  Form1: TForm1;

implementation
{$R *.DFM}

{********************* DrawBtnClick ******************}
procedure TForm1.DrawBtnClick(Sender: TObject);
{Draw some ellipses on the canvas - random size and color}
var
  i:integer;
  cx,cy:integer;
begin
  with image1, canvas do
  begin
    for i:= 1 to 10 do
    begin
      cx:=random(width);
      cy:=random(height);
      brush.color:=random(256*256*256);
      ellipse(cx,cy,cx+random(100), cy+random(100));
    end;
  end;
end;

{******************* ClearBtnClick ****************}
procedure TForm1.ClearBtnClick(Sender: TObject);
{Erase the canvas by drawing a big rectangle}
begin
  with image1, canvas do
  begin
    brush.color:=clwhite;
    canvas.rectangle(clientrect);
  end;
end;

{***************** SaveBtnClick *******************}
procedure TForm1.SaveBtnClick(Sender: TObject);
{Make a bitmap, copy canavsa to it and save it in a listbox}
var
  b:TBitmap;
begin
  b:=TBitmap.Create;
  b.height:=image1.height;
  b.width:=image1.width;
  b.canvas.copyrect(rect(0,0,b.width,b.height),image1.canvas,image1.clientrect);
  listbox1.items.addobject('Image #'+inttostr(listbox1.items.count+1),b);
end;

{******************* ListBox1Click *****************}
procedure TForm1.ListBox1Click(Sender: TObject);
{Retrieve bitmap from listbox and copy it to the canvas}
var
  b:TBitmap;
begin
  if listbox1.itemindex>=0 then
  with listbox1 do
  begin
    b:=TBitmap(items.objects[itemindex]);
    image1.Canvas.CopyRect(image1.clientrect,b.canvas,rect(0,0,b.width,b.height));
  end;
end;

{******************** Image1MouseDown **************}
procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with image1.canvas do
  begin
    pen.width:=3;
    pen.color:=clblack;
    drawing:=true;
    moveto(x,y);
  end;
end;

{*************** Image1MouseMove **************}
procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  If drawing
  then with image1 do
  begin
    cursor:=crNone; {to keep cursor redraw from erasing part of our line}
    canvas.lineto(x,y);
    cursor:=crdefault;
  end;
end;

{***************** Image1MouseUp ****************}
procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  drawing:=false;
end;

{******************* Formcreate *************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  clearbtnclick(sender); {draw initial rectangle to clear image canvas}
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  b:TBitMap;
begin
  If savedialog1.execute
  then image1.picture.savetofile(savedialog1.filename);
end;

end.

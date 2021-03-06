unit U_Sprites2;
{Copyright 2002, Gary Darby, Intellitech Systems Inc., www.DelphiForFun.org

 This program may be used or modified for any non-commercial purpose
 so long as this original notice remains in place.
 All other rights are reserved
 }
 {The 2nd of 4 sprite drawing demo programs - this one is intermediate in speed,
  it draws on a TImage control and only redraws what needs it by copying
  background image rectangles over each sprite in its old location and them
  redrawing all sprites in their new locations.}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, ImgList;

type
  TSpriteRec=record
    w,h: integer; {width & height of sprite rectangle}
    theta:single;  {angle to move}
    speed:single;  {pixels to move for each frame}
    x,y:single; {use floating x,y values to allow fractional pixel moves}
    prevrect:Trect;
    Sprite:TBitmap;
  end;

  TManspriteRec=record
    index:integer; {index of current image being displayed}
    w,h:integer;
    ispeed:integer;{this guy moves slowly so integer speed and coordinates are OK}
    ix,iy:integer;
    prevrect:Trect;
    manpics: array[0..5] of TBitmap;
  end;

  TForm1 = class(TForm)
    StopBtn: TButton;
    DrawBtn: TButton;
    TimeLbl: TLabel;
    Spritecount: TSpinEdit;
    Label3: TLabel;
    Speedbar: TTrackBar;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    Image1: TImage;
    Label2: TLabel;
    ManSpeedBar: TTrackBar;
    DoubleBufBox: TCheckBox;
    MemLbl: TLabel;
    procedure StopBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpritePaint(Sender: TObject);
    procedure DrawBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SpeedbarChange(Sender: TObject);
    procedure SpritecountChange(Sender: TObject);
    procedure ManSpeedBarChange(Sender: TObject);
    procedure DoubleBufBoxClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
public
    { Public declarations }
    spriterec:array[1..20] of TSpriterec;
    mansprite:TManSpriterec;
    bg:TBitmap;  {Bitmap to hold background image}
    perffreq:int64;
    loopcount:integer;
    manrest:integer;
    startcount:int64;  {1st counter value frame rate calculation}
    procedure setupsprite(n:integer);
    procedure setupmansprites;
    procedure MoveSprite(N:integer);
    procedure Movemansprite;
    procedure animate;
  end;

var  Form1: TForm1;

implementation

{$R *.DFM}
{$R Sprites.Res}


{******************** FormCreate ***************}
procedure TForm1.FormCreate(Sender: TObject);
var
  Context:HDC;
  Bitsperpixel:integer;
  pixelformat:TPixelFormat;
begin
  Context:=getdc(application.handle);
  BitsPerPixel:=GetDeviceCaps(Context,BitsPixel);
  case BitsPerPixel of
    8: pixelformat:=pf8bit;
    16:pixelformat:=pf16bit;
    24:pixelformat:=pf24bit;
    else pixelformat:=pf32bit;
  end;

  releaseDC(application.handle,context);
  bg:=TBitmap.create; {holds background}
  QueryPerformanceFrequency(perffreq);
  bg.handle:=loadBitmap(Hinstance, 'cloudbg8'); {load background picture}
  bg.pixelformat:=pixelformat;
  {bg.loadfromfile('cloudbg8.bmp');}
  stopbtn.bringtofront;
  doublebuffered:=false;
end;

{**********************  FormActivate ************}
procedure TForm1.FormActivate(Sender: TObject);
begin    windowstate:=wsmaximized;  end;

{****************** FormCloseQuesry ************}
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  StopBtnClick(sender);
  canclose:=true;
end;

{*********** FormResize ************}
procedure TForm1.FormResize(Sender: TObject);
begin
  tag:=1; {stop if running}
  with image1 do
  begin
    width:=form1.clientwidth-2*left;
    height:=stopbtn.top-top-10;
    picture.bitmap.width:=width;
    picture.bitmap.height:=height;
    canvas.stretchdraw(rect(0,0,width,height),bg);
  end;
  setupmansprites; {only need to do this once (or on resize)}
end;

{******************** StopBtnClick **************}
procedure TForm1.StopBtnClick(Sender: TObject);
begin   tag:=1; end;

{***************** DrawPartBtnClick ************}
procedure TForm1.DrawBtnClick(Sender: TObject);
begin   animate; end;

{*************** SpeedBarChange *************}
procedure TForm1.SpeedbarChange(Sender: TObject);
begin
   queryperformancecounter(startcount);
   loopcount:=0;
end;

{*************** SpriteCountChange ************}
procedure TForm1.SpritecountChange(Sender: TObject);
begin
  tag:=1; {stop it if running}
  {animate; }{and start it up again}
end;

{**************** ManSpeedBarChange ****************}
procedure TForm1.ManSpeedBarChange(Sender: TObject);
begin  manrest:=manspeedbar.max-manspeedbar.position+1;  end;

{******************* DoubleBufBoxClick **************}
procedure TForm1.DoubleBufBoxClick(Sender: TObject);
begin doublebuffered:=doublebufbox.checked; end;

{****************** SetupSprite *****************}
procedure TForm1.setupsprite(n:integer);
{Make an ellipse (or load a letter image) with random location, color size
 and initial angle of movement}
begin
  with spriterec[n] do
  begin
    theta:=0.1+random*2*pi; {initial direction-  make sure it never hits in a corner}
    speed:=2+random(6);
    if not assigned(sprite) then  sprite:=TBitmap.create;
    {make sprite image}
    with sprite, canvas  do
    begin
      transparent:=true;
      transparentcolor:=clblack;
      transparent:=true;
      case n of
        1,2,4,6,8,10,12,14:  {ellipses}
          begin
            width:=20+random(21); {from 20 to 40 pixels wide}
            height:=20+random(21); {from 20 to 40 pixels high}
            brush.color:=clblack;
            rectangle(0,0,width,height);
            { set sprite color - make sure that it's not black (0,0,0) since
              that's  our transparent color}
            brush.color:=rgb(1+random(254),1+random(254),1+random(254));
            ellipse(0,0,width,height);

          end;
          {letters}
          3: Sprite.handle:=loadbitmap(Hinstance,'LETTERD'); {loadfromfile('D.bmp');}
          5: Sprite.handle:=loadbitmap(Hinstance,'LETTERE');
          7: Sprite.handle:=loadbitmap(Hinstance,'LETTERL');
          9: Sprite.handle:=loadbitmap(Hinstance,'LETTERP');
          11:Sprite.handle:=loadbitmap(Hinstance,'LETTERH');
          13:Sprite.handle:=loadbitmap(Hinstance,'LETTERI');
      end;
      w:=width;
      h:=height;
      x:=w+random(Image1.width-3*w);  {initial position}
      y:=h+random(Image1.height-3*h);
    end;
  end;
end;


{**************  setupmansprites **********}
procedure TForm1.setupmansprites;
var
  i:integer;
begin
  with mansprite do
  begin

    for i:=0 to 5 do {load "walking man" images}
    begin
      If  not assigned(manpics[i]) then manpics[i]:=TBitmap.create;
      with manpics[i] do
      begin
        handle:=loadbitmap(Hinstance,pchar('ManSprite'+inttostr(i)));
        transparent:=true;
        transparentcolor:=clblack;
      end;
    end;

    w:=manpics[0].width;
    h:=manpics[0].height;

    index:=0;
    ispeed:=8;
    ix:=0;
    iy:=Image1.height-h;
  end;
end;

{******************* MoveSprite ***********}
procedure TForm1.MoveSprite(n:integer);
{Set next coordinates for Nth sprite }
var
  trycount:integer;
  tryx,tryy:single;
begin
  with spriterec[n] do
  begin  {random bounce}
    if (x<0) or (x+w>Image1.width) or (y<0) or (y+h>Image1.height) then
    begin  {If we're off the edge, get a new random direction that keeps us in sight}
      trycount:=0;
      repeat
        theta:=2*pi*random;
        tryx:=x+speed*cos(theta);
        tryy:=y+speed*sin(theta);
        inc(trycount)
      until (trycount>100) or
      ((tryx>=0) and (tryx+w<=Image1.width) and (tryy>=0)
                 and (tryy+h<=Image1.height));
    end
    else
    begin
      tryx:=x+speed*cos(theta);
      tryy:=y+speed*sin(theta);
    end;
    x:=tryx;
    y:=tryy;
  end;
end;

{*************** MovemanSprite *********}
procedure TForm1.MoveManSprite;
{Set next man sprite coordinates}
begin
  with mansprite do
  begin
    if  (ix>Image1.width) then ix:=0;
    ix:=ix+ispeed;
  end;
end;

{********************** Animate *********************}
procedure TForm1.animate;
{Loop moving all sprites until user clicks  Stop button}
var i:integer;
    stopcount:int64;
    avgrate:single;
begin
  stopbtn.visible:=true; {Stop button covers Start button}
  loopcount:=0;
  manrest:=manspeedbar.max-manspeedbar.position+1;
  for i:=1 to spritecount.value do setupsprite(i); {make some sprites}
  tag:=0;
  {copy background image bg image to work canvas}
  with Image1 do canvas.stretchdraw(rect(0,0,width,height),bg);

  {now copy it back to background - just to make sure they are the same size
   we'll be copying rectangles from the bg to the work canvas with each frame}
  with bg do
  begin
    width:=Image1.width;
    height:=Image1.height;
    canvas.draw(0,0,image1.picture.bitmap);
  end;
  QueryPerformanceCounter(startcount); {get start timer value}
  while tag=0 do
  begin
    for i:=1 to spritecount.value do MoveSprite(i); {move the sprites}
    if loopcount mod manrest = 0 then movemansprite; {"rest" the guy between steps}
    SpritePaint(self);  {repaint}
    inc(loopcount);
    if loopcount mod 64=0 then
    begin
      queryperformancecounter(stopcount);
      avgrate:=loopcount*perffreq/(stopcount-startcount);
      TimeLbl.caption:=format('Avg. frames/sec %6.0f',[avgrate]);
      {memlbl.caption:=inttostr(allocmemsize);}
    end;
    application.processmessages;
    sleep(11-speedbar.position); {control speed by waiting a few milliseconds}
  end;
  stopbtn.visible:=false;
end;

{******************* SpritePaint **************}
procedure TForm1.SpritePaint(Sender: TObject);
{Redraw background and all of the sprites}
var
  i:integer;
  ix,iy:integer;
begin
  if not assigned(spriterec[1].sprite) then exit;
 {Erase the old sprites directly on the image canvas by copying background
  rectangles}

  for i:=1 to spritecount.value do
     with spriterec[i] do  image1.canvas.copyrect(prevrect,bg.canvas,prevrect);

  with mansprite do  image1.canvas.copyrect(prevrect,bg.canvas,prevrect);

 {Now add the sprites back to the image}
  for i:=1 to spritecount.value do
  with spriterec[i], prevrect do
  begin
    {save cuurrent coordinates in prevrect so we can erase it later}
    ix:=trunc(x); iy:=trunc(y);
    left:=ix;
    top:=iy;
    right:=ix+w;
    bottom:=iy+h;
    image1.canvas.draw(ix,iy,sprite); {draw the new sprite}
  end;

  {Same for man sprite}
  with mansprite, prevrect do
  begin
    left:=ix;
    top:=iy;
    right:=ix+w;
    bottom:=iy+h;
    i:=(loopcount div manrest) mod 6; {need to select which man image to use, we're
                                  resting the guy 13 loops bewtween steps, so
                                  select image based on how many of these rests
                                  have gone by }
    {draw the man}
    image1.canvas.draw(ix,iy,manpics[i]);
  end;

end;


end.

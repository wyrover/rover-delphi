unit U_Sprites4;
{Copyright 2002, Gary Darby, Intellitech Systems Inc., www.DelphiForFun.org

 This program may be used or modified for any non-commercial purpose
 so long as this original notice remains in place.
 All other rights are reserved
 }

{Sprites4 switches the drawing surface from TImage to a TPaintBox control.
 Now double buffering is definitely required. We will accomplish this by
 creating a Work TBitmap to hold the image as it being built. Once built we can
 use it to replace the currently displayed image without visible flicker.    In
 this version, I used the simplest strategy described above - copy the
 background to the work image, draw the new sprites on the work canvas, copy
 the work canvas to the paintbox canvas.}
                                             
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
  end;

  TManspriteRec=record
    index:integer; {index of current image being displayed}
    w,h:integer;
    ispeed:integer;
    ix,iy:integer;
    prevrect:Trect;
  end;

  TForm1 = class(TForm)
    Paint1: TPaintBox;
    StopBtn: TButton;
    DrawBtn: TButton;
    TimeLbl: TLabel;
    Spritecount: TSpinEdit;
    Label3: TLabel;
    Speedbar: TTrackBar;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    Label2: TLabel;
    ManSpeedBar: TTrackBar;
    SpriteList: TImageList;
    ManList: TImageList;
    procedure StopBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpritePaint(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DrawBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SpeedbarChange(Sender: TObject);
    procedure SpritecountChange(Sender: TObject);
    procedure ManSpeedBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
public
    { Public declarations }
    spriterec:array[1..20] of TSpriterec;
    mansprite:TManSpriterec;
    bg:TBitmap;  {Bitmap to hold background image}
    work:TBitmap;  {bitmap to combine background and sprite}
    perffreq:int64;  {frequency of the performance counter}
    loopcount:integer; {animation loops}
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
{$R Sprites.res}
{uses math;}

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
    32:pixelformat:=pf32bit;
  end;
  releaseDC(application.handle,context);
  bg:=TBitmap.create; {holds background}
  QueryPerformanceFrequency(perffreq);
  bg.handle:=loadBitmap(Hinstance,'cloudbg8'); {load background picture}
  bg.pixelformat:=pixelformat;
  stopbtn.bringtofront;
  work:=tBitmap.create; {memory copy of image used in paint routine to build image }
  
end;

{**********************  FormActivate ************}
procedure TForm1.FormActivate(Sender: TObject);
begin
  manrest:=1;  {next statement wil force resize which will force repaint which
                divides by manrest --- so let's just give an initial value here}
  windowstate:=wsmaximized;
end;

{****************** FormPaint **************}
procedure TForm1.FormPaint(Sender: TObject);
  {redraw background which will in turn force repaint of sprites}
begin
  canvas.stretchdraw(rect(0,0,paint1.width,paint1.height),bg);
end;

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
  with paint1 do
  begin
    width:=form1.clientwidth-2*left;
    height:=stopbtn.top-top-10;
  end;
  setupmansprites; {only need to do this once (or on resize)}
end;


{******************** StopBtnClick **************}
procedure TForm1.StopBtnClick(Sender: TObject);
begin   tag:=1; end;

{***************** DrawPartBtnClick ************}
procedure TForm1.DrawBtnClick(Sender: TObject);
begin
  animate;
end;

{*************** SpeedBarChange *************}
procedure TForm1.SpeedbarChange(Sender: TObject);
begin
   queryperformancecounter(startcount);
   loopcount:=0;
end;

procedure TForm1.SpritecountChange(Sender: TObject);
begin
  tag:=1; {stop it if running}
  animate; {start it up again}
end;

{***************** ManSpeedBarChange ************}
procedure TForm1.ManSpeedBarChange(Sender: TObject);
begin
   manrest:=manspeedbar.max-manspeedbar.position+1;
end;

{****************** SetupSprite *****************}
procedure TForm1.setupsprite(n:integer);
{Make an ellipse (or load a letter image) with random location, color size
 and initial angle of movement}
var
  b:TBitmap;
  ww,hh:integer;
begin
  b:=TBitmap.create;
  if n=1 then spritelist.Clear;
  with spriterec[n] do
  begin
    theta:=0.1+random*2*pi; {initial direction-  make sure it never hits in a corner}
    speed:=2+random(6);
    {make sprite image}
    with b, canvas  do
    begin
      width:=spritelist.width;
      height:=spritelist.height;
      transparentcolor:=clblack;
      transparent:=true;
      case n of
        1,2,4,6,8,10,12,14:  {ellipses}
          begin
            { set sprite color - make sure that it's not black (0,0,0) since
              that's  our transparent color}
            ww:=(20+random(21)) div 2; {from 20 to 40 pixels wide}
            hh:=(20+random(21)) div 2; {from 20 to 40 pixels high}
            brush.color:=clblack;
            rectangle(0,0,width,height);
            brush.color:=rgb(1+random(254),1+random(254),1+random(254));
            ellipse(width div 2-ww,height div 2-hh,width div 2+ww,height div 2+hh);

          end;
          {letters}
          3: b.handle:=loadbitmap(Hinstance,'LETTERD'); {loadfromfile('D.bmp');}
          5: b.handle:=loadbitmap(Hinstance,'LETTERE');
          7: b.handle:=loadbitmap(Hinstance,'LETTERL');
          9: b.handle:=loadbitmap(Hinstance,'LETTERP');
          11:b.handle:=loadbitmap(Hinstance,'LETTERH');
          13:b.handle:=loadbitmap(Hinstance,'LETTERI');
      end;
      b.pixelformat:=bg.pixelformat;
      spriteList.addmasked(b,clblack);
      w:=width;
      h:=height;
      x:=w+random(paint1.width-3*w);  {initial position}
      y:=h+random(Paint1.height-3*h);
    end;
  end;
  b.free;
end;


{**************  setupmansprites **********}
procedure TForm1.setupmansprites;
var  i:integer;
     b:TBitmap;

begin
  b:=TBitmap.create;
  manlist.clear;
  with mansprite do
  begin
    for i:=0 to 5 do
    begin
      b.height:=manlist.height;
      b.width:=manlist.width;
      b.handle:=loadbitmap(Hinstance,Pchar('ManSprite'+inttostr(i)));
      b.pixelformat:=bg.pixelformat;
      manlist.addmasked(b,clblack);
    end;
    h:=manlist.height;
    w:=manlist.width;
    index:=0;
    ispeed:=8;
    ix:=0;
    iy:=paint1.height-h;
  end;
  b.free;
end;

{******************* MoveSprite ***********}
procedure TForm1.MoveSprite(n:integer);
{Set next sprite coordinates}
var
  trycount:integer;
  tryx,tryy:single;
begin
  with spriterec[n] do
  begin  {random bounce}
    if (x<0) or (x+w>paint1.width) or (y<0) or (y+h>paint1.height) then
    begin  {If we're off the edge, get a new random direction that keeps us in sight}
      trycount:=0;
      repeat
        theta:=2*pi*random;
        tryx:=x+speed*cos(theta);
        tryy:=y+speed*sin(theta);
        inc(trycount)
      until (trycount>100) or
      ((tryx>=0) and (tryx+w<=paint1.width) and (tryy>=0)
                 and (tryy+h<=paint1.height));
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
{Set next sprite coordinates}
begin
  with mansprite do
  begin
    if  (ix>paint1.width) then ix:=0;
    ix:=ix+ispeed;
  end;
end;

{********************** Animate *********************}
procedure TForm1.animate;
{Loop moving all sprites until user clicks  Stop button}
var i:integer;
    r:Trect;
    stopcount:int64;
    avgrate:single;
begin
  stopbtn.visible:=true;
  loopcount:=0;
  manrest:=manspeedbar.max-manspeedbar.position+1;
  for i:=1 to spritecount.value do setupsprite(i); {make some sprites}
  tag:=0;
  {copy background image bg image to work canvas}
  work.width:=paint1.width;
  work.height:=paint1.height;
  r:=rect(0,0,work.width,work.height);
  work.canvas.stretchdraw(r,bg);
  paint1.canvas.draw(0,0,work);
  update;

  {now copy it back to background - just to make sure they are the same size
   we'll be copying rectangles from the bg to the work canvas with each frame}
  bg.width:=paint1.width;
  bg.height:=paint1.height;
  bg.canvas.copyrect(r,work.canvas,r);
  QueryPerformanceCounter(startcount);
  while tag=0 do
  begin
    for i:=1 to spritecount.value do MoveSprite(i); {move the sprites}
    if loopcount mod manrest = 0 then movemansprite;
    SpritePaint(self);  {repaint}
    inc(loopcount);
    if loopcount mod 64=0 then
    begin
      queryperformancecounter(stopcount);
      avgrate:=loopcount*perffreq/(stopcount-startcount);
      TimeLbl.caption:=format('Avg. frames/sec %6.0f',[avgrate]);
    end;
    application.processmessages;
    sleep(10-speedbar.position); {control speed by waiting a few milliseconds}
  end;
  stopbtn.visible:=false;
end;


{******************* SpritePaint **************}
procedure TForm1.SpritePaint(Sender: TObject);
{Repaint background and all of the sprites}
var
  i:integer;
  ix,iy, ispeed:integer;
  r:TRect;
begin
  if spritelist.count=0 then exit;
  {Copy changed background rectangles to work bitmap}

  for i:=1 to spritecount.value do {erase old sprint images}
     with spriterec[i] do  work.canvas.copyrect(prevrect,bg.canvas,prevrect);
  with mansprite do work.canvas.copyrect(prevrect,bg.canvas,prevrect);

  {add the sprites to the work bitmap}
  for i:=1 to spritecount.value do
  with spriterec[i], prevrect do
  begin
    ix:=trunc(x); iy:=trunc(y); ispeed:=trunc(speed+1);
    left:=ix;
    top:=iy;
    right:=ix+w;
    bottom:=iy+h;
    spritelist.draw(work.canvas,ix,iy,i-1,true);
  end;

  with mansprite, prevrect do
  begin
    left:=ix; {max(ix-ispeed,0);}
    top:=iy; {max(iy-ispeed,0);}
    right:=ix+w; {ix+w+ispeed;}
    bottom:=iy+h; {iy+h+ispeed;}
    i:=(loopcount div manrest) mod 6;
    {put the sprite in the rectangle}
    manlist.draw(work.canvas,ix,iy,i);
  end;
  with work do  r:=rect(0,0,width,height);
  if tag=0 {image may have been resized during this loop - this test will
            prevent writing a incorrectly sized image}
  then paint1.canvas.copyrect(r,work.canvas,r);
end;

end.

unit U_Sprites5;
{Copyright 2002, Gary Darby, Intellitech Systems Inc., www.DelphiForFun.org

 This program may be used or modified for any non-commercial purpose
 so long as this original notice remains in place.
 All other rights are reserved
 }

{Sprites5 is the fastest and takes us one step further.  Now we'll replace only
 the rectangles that contain the old sprite images with corresponding background
 rectangles, then draw the sprites in their new location on the Work bitmap.
 Now, since we cleverly saved the old image rectangle and the new image
 rectangle, we make define a new super rectangle that just encloses both
 Prevrect and NewRect and use that to copy pieces of the work canvas to
 the Paintbox canvas.  Especially when the number of sprite is small, this is
 very fast.  }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, ImgList;

type

  TSpriteRec=record
    w,h: integer; {width & height of sprite rectangle}
    spritecolor:TColor;
    theta:single;  {angle to move}
    speed:single;  {pixels to move for each frame}
    x,y:single; {use floating x,y values to allow fractional pixel moves}
    prevrect, newrect:Trect;
    Sprite:TBitmap;
  end;

  TManspriteRec=record
    index:integer; {index of current image being displayed}
    w,h:integer;
    ispeed:integer;
    ix,iy:integer;
    prevrect, newrect:Trect;
    manpics: array[0..5] of TBitmap;
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
{$R Sprites.res}
uses math;

{******************** FormCreate ***************}
procedure TForm1.FormCreate(Sender: TObject);
var
  Context:HDC;
  Bitsperpixel:integer;
  pixelformat:TPixelFormat;
begin
  work:=tBitmap.create; {memory copy of image used in paint routine to build image }
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
begin  animate; end;

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
            { set sprite color - make sure that it's not black (0,0,0) since
              that's  our transparent color}
            spritecolor:=rgb(1+random(254),1+random(254),1+random(254));
            width:=20+random(20); {from 10 to 30 pixels wide}
            height:=20+random(20); {from 10 to 50 pixels high}
            brush.color:=clblack;
            rectangle(0,0,width,height);
            brush.color:=spritecolor;
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
      x:=w+random(paint1.width-3*w);  {initial position}
      y:=h+random(paint1.height-3*h);
    end;
  end;
end;


{**************  setupmansprites **********}
procedure TForm1.setupmansprites;
var i:integer;
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
    iy:=paint1.height-h;
  end;
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

{******************* SpritePaint **************}
procedure TForm1.SpritePaint(Sender: TObject);
{Repaint background and all of the sprites}

function combinerect(r1,r2:TRect):Trect;
begin
  with result do
  begin
    left:=min(r1.left, r2.left);
    top:=min(r1.top,r2.top);
    right:=max(r1.right, r2.right);
    bottom:=max(r1.bottom,r2.bottom);
  end;
end;

var
  i:integer;
  ix,iy, ispeed:integer;
  r:Trect;
begin
  if not assigned(spriterec[1].sprite) then exit;
 {Copy background rectangles to work bitmap}
  for i:=1 to spritecount.value do {erase old sprint images}
     with spriterec[i] do  work.canvas.copyrect(prevrect,bg.canvas,prevrect);

  with mansprite do  work.canvas.copyrect(prevrect,bg.canvas,prevrect);

  {add the sprites to the work bitmap}
  for i:=1 to spritecount.value do
  with spriterec[i], newrect do
  begin
    ix:=trunc(x); iy:=trunc(y);
    left:=ix;
    top:=iy;
    right:=ix+w;
    bottom:=iy+h;
    work.canvas.draw(ix,iy,sprite); {put the sprite in the rectangle}
  end;

  with mansprite, newrect do
  begin
    left:=ix;
    top:=iy;
    right:=ix+w;
    bottom:=iy+h;
    i:=(loopcount div manrest) mod 6;
    work.canvas.draw(ix,iy,manpics[i]); {put the sprite in the rectangle}
  end;


  {If not stopping, then repaint the rectangles on the paintbox canvas might
   have changed}
  if tag=0 then
  begin
    for i:= 1 to spritecount.value do
    with spriterec[i] do
    begin
      r:=combinerect(prevrect, newrect);
      prevrect:=newrect;
      paint1.canvas.copyrect(r,work.canvas,r);
    end;
    with mansprite do
    begin
      r:=combinerect(prevrect, newrect);
      prevrect:=newrect;
      if tag=0 then paint1.canvas.copyrect(r,work.canvas,r);
    end;
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

end.

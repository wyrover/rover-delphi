unit U_TChartDemo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TeEngine, Series, ExtCtrls, TeeProcs, Chart;

type
  TForm1 = class(TForm)
    GenPointsBtn: TButton;
    PlotPointsBtn: TButton;
    ListBox1: TListBox;
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    procedure GenPointsBtnClick(Sender: TObject);
    procedure PlotPointsBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    x,y1,y2:array[1..100] of extended;
    //Series1,Series2:TLineseries; {currently defined a designtime}
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

(*  {trig functions commented out here}
{*********** GatVal1 **********}
function getval1(x:extended):extended;
{return Sin(x)}
begin  result:=sin(x); end;

{************ GetVal2 ***********88}
function getval2(x:extended):extended;
{return Cos(2X)}
begin  result:=cos(2*x); end;
*)

{*********** GatVal1 **********}
function getval1(x:extended):extended;
{return x*x}
begin  result:=x*x; end;

{************ GetVal2 ***********88}
function getval2(x:extended):extended;
{return 10*x)}
begin  result:=10*x end;

{************* GenPointsBtnClick *************}
procedure TForm1.GenPointsBtnClick(Sender: TObject);
{Generate  100 points }
var i:integer;
    xincr:extended;
begin
  listbox1.clear;
  (*  {used for trig function plots}
  x[1]:=-2*pi; {start value}
  incr:=(4*pi)/100;  {make the range from -2Pi to +2Pi}
   *)

  x[1]:=-10; {start value}
  xincr:=20/100;  {make the range from -10 to +10 divided into 100 parts}
  for i:=1 to 100 do
  begin
    if i>1 then x[i]:=x[i-1]+xincr;
    y1[i]:=getval1(x[i]); {1st y value}
    y2[i]:=getval2(x[i]); {2nd y value}
    listbox1.items.add(format('(X, Y1, Y2) (%4.1f, %4.1f, %4.1f)',[x[i],y1[i],y2[i]]));
  end;
end;

{*************** PlotPointsBtnClick ************}
procedure TForm1.PlotPointsBtnClick(Sender: TObject);
var i:integer;
begin
  {add points to series}
  for i:= 1 to 100 do
  begin
    series1.addxy(x[i],y1[i]);
    series2.addxy(x[i],y2[i]);
  end;
  {Following code illustrates how to modify chart features at run time}
  {All of this could also be entered at design time by using the TChart edit (right click)}
  Chart1.title.text.clear;
  Chart1.Title.text.add('Sample plot of a couple of functions');
  //Series1.title:='Plot of Y = Sin(x)';
  //Series2.title:='Plot of Y = Cos(2x)';
  Series1.title:='Plot of Y = X*X';
  Series2.title:='Plot of Y = 10*X';
  Chart1.LeftAxis.title.caption:='Y=F(X)';
  Chart1.LeftAxis.title.font.size:=10;
  Chart1.LeftAxis.title.font.style:=[fsbold];
  Chart1.BottomAxis.title.caption:='X';
  Chart1. BottomAxis.title.font.size:=10;
  Chart1.BottomAxis.title.font.style:=[fsbold];
  
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  (*  {series can be defined here or at design time by right clicking on the TChart}
  Series1:=TLineSeries.create(self);
  series1.parentchart:=Chart1;
  Series2:=TLineSeries.create(self);
  Series2.parentChart:=Chart1;
  *)
end;

end.

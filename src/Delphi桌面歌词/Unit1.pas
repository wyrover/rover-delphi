{*******************************************************}
{                                                       }
{       环境：Delphi 7 + IGDIPlus                       }
{                                                       }
{       作者：无幻 http://blog.csdn.net/akof1314        }
{                                                       }
{*******************************************************}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IGDIPlus, ExtCtrls, Menus;

type
  TForm1 = class(TForm)
    tmr1: TTimer;
    pm1: TPopupMenu;
    mni_topMost: TMenuItem;
    mni_transparent: TMenuItem;
    mni_exit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tmr1Timer(Sender: TObject);
    procedure mni_topMostClick(Sender: TObject);
    procedure MouseLeave(var Msg: TMessage);message WM_MOUSELEAVE;
    procedure mni_transparentClick(Sender: TObject);
    procedure mni_exitClick(Sender: TObject);
  private
    m_Kind: Integer;        //当前第几行字符串
    m_bBack: Boolean;       //是否显示背景
    m_pszbuf: array[0..5] of WideString;   //要绘制的字符串数组
    function UpdateDisplay(pszbuf: WideString;bBack: Boolean = False;
        Transparent: Integer = 100):Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{-------------------------------------------------------------------------------
  过程名:    TForm1.UpdateDisplay
  功能:      绘制桌面歌词
  参数:      pszbuf: WideString;     绘制的字符串
             bBack: Boolean;         是否绘制背景
             Transparent: Integer    透明程度
  返回值:    Boolean
-------------------------------------------------------------------------------}
function TForm1.UpdateDisplay(pszbuf: WideString;bBack: Boolean;Transparent: Integer):Boolean;
var
  hdcTemp,hdcScreen,m_hdcMemory: HDC;
  hBitMap: Windows.HBITMAP;
  blend: BLENDFUNCTION;      //这种结构的混合控制通过指定源和目标位图的混合功能
  rct: TRect;
  ptWinPos,ptSrc: TPoint;
  graphics: IGPGraphics;     //封装一个 GDI+ 绘图图面
  fontFamily: IGPFontFamily; //定义有着相似的基本设计但在形式上有某些差异的一组字样
  path: IGPGraphicsPath;     //表示一系列相互连接的直线和曲线
  strFormat: IGPStringFormat;//封装文本布局信息，显示操作
  pen,pen1,pen2: IGPPen;     //定义用于绘制直线和曲线的对象
  linGrBrush,linGrBrushW: IGPLinearGradientBrush;  //使用线性渐变封装 Brush
  brush: IGPSolidBrush;      //定义单色画笔，画笔用于填充图形形状
  image: TGPImage;           //使用这个类来创建和操作GDI+图像
  i: Integer;
  sizeWindow: SIZE;
begin
  //---------------------开始：初始化操作--------------------------------------
  hdcTemp := GetDC(Self.Handle);
  m_hdcMemory := CreateCompatibleDC(hdcTemp);
  hBitMap := CreateCompatibleBitmap(hdcTemp,755,350);
  SelectObject(m_hdcMemory,hBitMap);
  if (Transparent < 0) or (Transparent > 100) then
    Transparent := 100;
  with blend do
  begin
    BlendOp := AC_SRC_OVER;     //把源图片覆盖到目标之上
    BlendFlags := 0;
    AlphaFormat := AC_SRC_ALPHA;//每个像素有各自的alpha通道
    SourceConstantAlpha :=Trunc(Transparent * 2.55);  //源图片的透明度
  end;
  hdcScreen := GetDC(Self.Handle);
  GetWindowRect(Self.Handle,rct);
  ptWinPos := Point(rct.Left,rct.Top);
  graphics := TGPGraphics.Create(m_hdcMemory);
  graphics.SetSmoothingMode(SmoothingModeAntiAlias); //指定平滑（抗锯齿）
  graphics.SetInterpolationMode(InterpolationModeHighQualityBicubic);//指定的高品质，双三次插值
  fontFamily := TGPFontFamily.Create('微软雅黑'); //△字体，效果图为'微软雅黑'字体
  strFormat := TGPStringFormat.Create();
  path := TGPGraphicsPath.Create();
  //---------------------结束：初始化操作--------------------------------------
  path.AddString(pszbuf,          //要添加的 String
                fontFamily,       //表示绘制文本所用字体的名称
                0,                //指定应用到文本的字形信息,这里为普通文本
                38,               //限定字符的 Em（字体大小）方框的高度
                MakePoint(10,10), //一个 Point，它表示文本从其起始的点
                strFormat);       //指定文本格式设置信息
  pen := TGPPen.Create(MakeColor(155,215,215,215),3);  //颜色、宽度
  graphics.DrawPath(pen,path);    //初步绘制GraphicsPath
  linGrBrush := TGPLinearGradientBrush.Create(MakePoint(0,0),    //线性渐变起始点
                                                MakePoint(0,90), //线性渐变终结点
                                                MakeColor(255,255,255,255), //线性渐变起始色
                                                MakeColor(255,30,120,195)); //线性渐变结束色
  linGrBrushW := TGPLinearGradientBrush.Create(MakePoint(0,10),
                                                MakePoint(0,60),
                                                MakeColor(255,255,255,255),
                                                MakeColor(15,1,1,1));
  //---------------------开始：画字符串阴影--------------------------------------
  for i := 1 to 8 do
  begin
    pen.SetWidth(i);
    pen.SetColor(MakeColor(62, 0, 2, 2));
    pen.SetLineJoin(LineJoinRound); //指定圆形联接。这将在两条线之间产生平滑的圆弧。
    graphics.DrawPath(pen,path);
  end;
  //---------------------开始：画背景框和背景图----------------------------------
  if bBack then
  begin
    brush := TGPSolidBrush.Create(MakeColor(25,228,228,228));
    pen1 := TGPPen.Create(MakeColor(155,223,223,223));
    pen2 := TGPPen.Create(MakeColor(55,223,223,223));
    image := TGPImage.Create('back.png');             //背景图片
    graphics.FillRectangle(brush,3,5,750,90);         //填充背景框色
    graphics.DrawRectangle(pen1,2,6,751,91);          //内层背景框
    graphics.DrawRectangle(pen2,1,5,753,93);          //外层背景框
    graphics.DrawImage(image,600,25);
  end;
  //---------------------开始：以渐变色笔刷填充GraphicsPath内部-----------------
  graphics.FillPath(linGrBrush,path);
  graphics.FillPath(linGrBrushW,path);
  sizeWindow.cx := 755;
  sizeWindow.cy := 350;
  ptSrc := Point(0,0);
  //---------------------开始：更新一个分层的窗口的位置，大小，形状，内容和半透明度---
  Result := UpdateLayeredWindow(Self.Handle,   //分层窗口的句柄
                                hdcScreen,     //屏幕的DC句柄
                                @ptWinPos,     //分层窗口新的屏幕坐标
                                @sizeWindow,   //分层窗口新的大小
                                m_hdcMemory,   //用来定义分层窗口的表面DC句柄
                                @ptSrc,        //分层窗口在设备上下文的位置
                                0,             //合成分层窗口时使用指定颜色键值
                                @blend,        //在分层窗口进行组合时的透明度值
                                ULW_ALPHA);    //使用pblend为混合功能
  //---------------------开始：释放和删除--------------------------------------
  ReleaseDC(Self.Handle,hdcScreen);
  ReleaseDC(Self.Handle,hdcTemp);
  DeleteObject(hBitMap);
  DeleteDC(m_hdcMemory);
end;
{-------------------------------------------------------------------------------
  功能:      窗体创建初始化
-------------------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin
  //设置窗体属性
  SetWindowLong(Application.Handle,
                GWL_EXSTYLE,
                GetWindowLong(Application.Handle,GWL_EXSTYLE)
                or WS_EX_TOOLWINDOW);   //不在任务栏出现
  SetWindowLong(Self.Handle,
                GWL_EXSTYLE,
                GetWindowLong(Self.Handle,GWL_EXSTYLE)
                or WS_EX_LAYERED       //层次窗口
                or WS_EX_TOOLWINDOW);  //不在alt+tab中出现
  //初始化变量等等
  m_kind := 0;
  m_bBack := False;
  PopupMenu := pm1;
  Self.Cursor := crHandPoint;
  mni_topMost.Checked := True;
  mni_topMostClick(mni_topMost);

  m_pszbuf[0] := '你好 Everyone!';
  m_pszbuf[1] := '这是GDI+绘制的桌面歌词!';
  m_pszbuf[2] := '欢迎光临无幻博客!';
  m_pszbuf[3] := 'http://blog.csdn.net/akof1314!';
  m_pszbuf[4] := '源码根据需要进行修改!';
  UpdateDisplay(m_pszbuf[m_kind],m_bBack);
end;
{-------------------------------------------------------------------------------
  功能:      鼠标按下移动窗体
-------------------------------------------------------------------------------}
procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Self.Handle,WM_SYSCOMMAND,SC_MOVE or HTCAPTION,0);
end;
{-------------------------------------------------------------------------------
  功能:      鼠标移过窗体
-------------------------------------------------------------------------------}
procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  xh: TTrackMouseEvent;
begin
  m_bBack := True;
  UpdateDisplay(m_pszbuf[m_kind],m_bBack);
  with xh do
  begin
    cbSize := SizeOf(xh);
    dwFlags := TME_LEAVE;
    hwndTrack := Self.Handle;
    dwHoverTime := 0;
  end;
  TrackMouseEvent(xh);
end;
{-------------------------------------------------------------------------------
  功能:      鼠标移出窗体时，去掉背景
-------------------------------------------------------------------------------}
procedure TForm1.MouseLeave(var Msg: TMessage);
begin
  m_bBack := False;
  UpdateDisplay(m_pszbuf[m_kind],m_bBack);
  Msg.Result := 0;
end;
{-------------------------------------------------------------------------------
  功能:      定时器切换字符串
-------------------------------------------------------------------------------}
procedure TForm1.tmr1Timer(Sender: TObject);
begin
  Inc(m_Kind);
  if m_kind > 4 then
    m_kind := 0;
  UpdateDisplay(m_pszbuf[m_kind],m_bBack);
end;
{-------------------------------------------------------------------------------
  功能:      窗体置顶
-------------------------------------------------------------------------------}
procedure TForm1.mni_topMostClick(Sender: TObject);
begin
  if mni_topMost.Checked then
  SetWindowPos(Self.Handle,
               HWND_TOPMOST,
               0,0,0,0,
               SWP_NOSIZE or SWP_NOMOVE)    //窗口置顶
  else
  SetWindowPos(Self.Handle,
               HWND_NOTOPMOST,
               0,0,0,0,
               SWP_NOSIZE or SWP_NOMOVE);    //窗口置顶
end;
{-------------------------------------------------------------------------------
  功能:      背景穿透 （相当于锁定桌面）
-------------------------------------------------------------------------------}
procedure TForm1.mni_transparentClick(Sender: TObject);
begin
  SetWindowLong(Self.Handle,
                GWL_EXSTYLE,
                GetWindowLong(Self.Handle,GWL_EXSTYLE) or WS_EX_TRANSPARENT);
  Application.MessageBox('你已关闭不了程序了，请从任务管理器里关闭',
                        '提示',MB_OK or MB_ICONINFORMATION);
end;
{-------------------------------------------------------------------------------
  功能:      退出
-------------------------------------------------------------------------------}
procedure TForm1.mni_exitClick(Sender: TObject);
begin
  Self.Close;
end;

end.

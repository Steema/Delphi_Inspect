{**********************************************}
{   Delphi Inspect FireMonkey app              }
{   davidberneda@gmail.com david@steema.com    }
{**********************************************}
unit Unit_Info platform;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Media, FMX.Gestures, FMX.Types3D,
  {$IF FireMonkeyVersion>18}
  FMX.Graphics,
  {$ENDIF}
  FMX.Dialogs, FMX.Layouts, FMX.Ani,
  FMX.Memo, FMX.ExtCtrls, FMX.TabControl, FMX.Objects, FMX.Styles,

  FMXTee.Procs,

  {$IF Declared(TParallelProc)}
  FMXTee.About,
  {$ENDIF}

  FMX.Viewport3D, FMX.Platform, FMX.StdCtrls,
  FMX.TextLayout, InspectDelphi, System.Sensors,
  System.Sensors.Components, FMXTee.Engine, FMXTee.Chart,
  System.IOUtils, FMXTee.Constants, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Tether.AppProfile, FMXTee.Series,
  FMX.Controls.Presentation, FMX.Edit, FMX.ComboEdit, FMX.ComboTrackBar,
  FMX.ScrollBox;

{$IF Declared(TParallelProc)}
{$DEFINE TEEPRO}
{$ENDIF}

type
  TSystemInfoForm = class(TForm)
    TabMain: TTabControl;
    TabItem1: TTabItem;
    TabMemory: TTabItem;
    Image1: TImage;
    TabAbout: TTabItem;
    ToolBar1: TToolBar;
    Button1: TButton;
    TabFireMonkey: TTabItem;
    ToolBar2: TToolBar;
    Button2: TButton;
    ToolBar3: TToolBar;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    LocationSensor1: TLocationSensor;
    MotionSensor1: TMotionSensor;
    OrientationSensor1: TOrientationSensor;
    TabDelphi: TTabControl;
    TabSystem: TTabItem;
    MemoSystem: TMemo;
    TabItem5: TTabItem;
    MemoMath: TMemo;
    TabPaths: TTabItem;
    MemoPaths: TMemo;
    TabLocale: TTabItem;
    MemoLocale: TMemo;
    TabOS: TTabItem;
    MemoOS: TMemo;
    TabHard: TTabItem;
    MemoHard: TMemo;
    ToolBarAbout: TToolBar;
    Gyroscope: TCheckBox;
    Timer1: TTimer;
    ButtonDownload: TButton;
    TabBenchmarks: TTabItem;
    TabControlBench: TTabControl;
    TabCanvasBench: TTabItem;
    ToolBar5: TToolBar;
    Button4: TButton;
    Chart1: TChart;
    Button5: TButton;
    CheckBox3D: TCheckBox;
    ButtonShareChart: TButton;
    TabBenchRTL: TTabItem;
    ToolBar4: TToolBar;
    Button3: TButton;
    CheckBox3DSystem: TCheckBox;
    ButtonShareChartSystem: TButton;
    Chart2: TChart;
    TabControlAbout: TTabControl;
    TabAboutBox: TTabItem;
    Viewport3D1: TViewport3D;
    TabLocation: TTabItem;
    ChartLocation: TChart;
    TabDatabase: TTabItem;
    MemoDatabase: TMemo;
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    TetheringManager2: TTetheringManager;
    TabControlFMX: TTabControl;
    TabFMXInfo: TTabItem;
    TabGesture: TTabItem;
    MemoFireMonkey: TMemo;
    ChartGesture: TChart;
    TabThreading: TTabItem;
    ToolBar6: TToolBar;
    Button6: TButton;
    CheckBox3DThreading: TCheckBox;
    ButtonShareChartThreading: TButton;
    Chart3: TChart;
    Layout1: TLayout;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TabMainChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure GyroscopeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OrientationSensor1StateChanged(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBox3DChange(Sender: TObject);
    procedure ButtonShareChartClick(Sender: TObject);
    procedure Button4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ButtonShareChartSystemClick(Sender: TObject);
    procedure CheckBox3DSystemChange(Sender: TObject);
    procedure TabControlAboutChange(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
    procedure ChartGestureTap(Sender: TObject; const Point: TPointF);
    procedure FormTouch(Sender: TObject; const Touches: TTouches;
      const Action: TTouchAction);
    procedure ChartGestureAfterDraw(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ButtonShareChartThreadingClick(Sender: TObject);
    procedure CheckBox3DThreadingChange(Sender: TObject);
    procedure Viewport3D1Resize(Sender: TObject);
    procedure ChartLocationClickSeries(Sender: TCustomChart;
      Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button7Click(Sender: TObject);
    procedure ChartLocationDblClick(Sender: TObject);
  private
    { Private declarations }

    DebugBench : Boolean;

    Point : TPointSeries;

    ITouches : TTouches;
    IAction : TTouchAction;

    {$IFDEF TEEPRO}
    IAbout : TFormAbout;
    //Navigator : TMouse3D;

    procedure PrepareAbout;
    {$ENDIF}

    procedure RefreshMemory;
  public
    { Public declarations }
  end;

var
  SystemInfoForm: TSystemInfoForm;

implementation

{$R *.fmx}

uses
  FMXTee.Series.Map, FMXTee.Series.World, FMXTee.Tools, FMXTee.Canvas,
  FMXTee.Editor.Chart,
  Unit_Utils, Unit_Benchmark_Canvas;

procedure TSystemInfoForm.Button1Click(Sender: TObject);
begin
  // Alternative usage, if you want to load all info into a single TStringList:
  // MyStrings:=TStringList.Create; TInspectDelphi.AddAll(MyStrings);

  MemoSystem.Lines.Clear;
  TInspectDelphi.AddSystem(MemoSystem.Lines);

  MemoOS.Lines.Clear;
  TInspectDelphi.AddOS(MemoOS.Lines);

  MemoLocale.Lines.Clear;
  TInspectDelphi.AddLocale(MemoLocale.Lines);

  MemoPaths.Lines.Clear;
  TInspectDelphi.AddPaths(MemoPaths.Lines);

  MemoHard.Lines.Clear;
  TInspectDelphi.AddHardware(MemoHard.Lines);

  MemoMath.Lines.Clear;
  TInspectDelphi.AddMath(MemoMath.Lines);

  MemoDatabase.Lines.Clear;
  TInspectDelphi.AddDB(MemoDatabase.Lines);
end;

procedure TSystemInfoForm.Button2Click(Sender: TObject);
begin
  MemoFireMonkey.Lines.Clear;
  TInspectDelphi.AddFireMonkey(MemoFireMonkey.Lines);
end;

procedure TSystemInfoForm.Button3Click(Sender: TObject);
begin
  TSystemBenchmark.Run(Chart2,Width>Height);

  ButtonShareChartSystem.Visible:=True;
  CheckBox3DSystem.Visible:=True;
end;

procedure TSystemInfoForm.ButtonDownloadClick(Sender: TObject);
begin
  VisitHomepage;
end;

procedure TSystemInfoForm.ButtonShareChartClick(Sender: TObject);
var s : TStrings;
begin
  s:=SeriesData(Chart1[0]);
  try
    Share(s);
  finally
    s.Free;
  end;
end;

procedure TSystemInfoForm.Button4Click(Sender: TObject);
begin
  TCanvasBenchmark.Run(Chart1,Width>Height);

  ButtonShareChart.Visible:=True;
  CheckBox3D.Visible:=True;
end;

procedure TSystemInfoForm.Button4MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  // Debug trick:
  DebugBench:=Button=TMouseButton.mbRight;

  if DebugBench then
     Chart1.Invalidate;
end;

procedure TSystemInfoForm.Button5Click(Sender: TObject);
var s : TStrings;
begin
  s:=TStringList.Create;
  try
    TInspectDelphi.AddAll(s);
    Share(s);
  finally
    s.Free;
  end;
end;

procedure TSystemInfoForm.Button6Click(Sender: TObject);
begin
  TThreadingBenchmark.Run(Chart3,Width>Height);

  ButtonShareChartThreading.Visible:=True;
  CheckBox3DThreading.Visible:=True;
end;

procedure TSystemInfoForm.Button7Click(Sender: TObject);
begin
  ChartLocation.Axes.Left.SetMinMax(-60,85);
  ChartLocation.Axes.Right.SetMinMax(-60,85);
  ChartLocation.Axes.Top.SetMinMax(-180,180);
  ChartLocation.Axes.Bottom.SetMinMax(-180,180);
end;

procedure TSystemInfoForm.ButtonShareChartThreadingClick(Sender: TObject);
var s : TStrings;
begin
  s:=SeriesData(Chart3[0]);
  try
    Share(s);
  finally
    s.Free;
  end;
end;

procedure TSystemInfoForm.ButtonShareChartSystemClick(Sender: TObject);
var s : TStrings;
begin
  s:=SeriesData(Chart2[0]);
  try
    Share(s);
  finally
    s.Free;
  end;
end;

procedure TSystemInfoForm.Chart1AfterDraw(Sender: TObject);
var R : TRectF;
begin
  if DebugBench then
  begin
    if Chart1.Canvas.ReferenceCanvas.BeginScene then
    try
      R:=RectF(0,0,BitmapOffScreen.Width,BitmapOffScreen.Height);
      Chart1.Canvas.ReferenceCanvas.DrawBitmap(BitmapOffScreen,R,R,1);
    finally
      Chart1.Canvas.ReferenceCanvas.EndScene;
    end;
  end;
end;

function PointFToString(const P:TPointF):String;
begin
  result:=FormatFloat('#.##',P.X)+', '+FormatFloat('#.##',P.Y);
end;

procedure TSystemInfoForm.ChartGestureAfterDraw(Sender: TObject);
var t : Integer;
    c : TAlphaColor;
    p : TPointF;
begin
  ChartGesture.Canvas.Pen.Style:=TPenStyle.psSolid;
  ChartGesture.Canvas.Pen.Color:=TAlphaColors.Chocolate;

  ChartGesture.Canvas.Brush.Style:=TBrushKind.Solid;

  case IAction of
    TTouchAction.None: c:=TAlphaColors.White;
    TTouchAction.Up: c:=TAlphaColors.Green;
    TTouchAction.Down: c:=TAlphaColors.Red;
    TTouchAction.Move: c:=TAlphaColors.Yellow;
  else
   // TTouchAction.Cancel:
    c:=TAlphaColors.Blue;
  end;

  ChartGesture.Canvas.Brush.Color:=c;

  for t := Low(ITouches) to High(ITouches) do
  begin
    p:=ITouches[t].Location;
    p:=ChartGesture.AbsoluteToLocal(p);
    ChartGesture.Canvas.Ellipse(TRectF.Create(p,10,10));
  end;

  ChartGesture.Foot.Caption:='Touches: '+IntToStr(Length(ITouches));
end;

procedure TSystemInfoForm.ChartGestureTap(Sender: TObject; const Point: TPointF);
begin
  ChartGesture.Title.Caption:='Tap: '+PointFToString(Point);
end;

procedure TSystemInfoForm.ChartLocationClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  function PointsToPath(const P:TPointArray):TStrings;
  var t : Integer;
  begin
    result:=TStringList.Create;

    for t:=Low(P) to High(P) do
        result.Add(FormatFloat('0.###',P[t].X)+' '+FormatFloat('0.###',P[t].Y));
  end;

var World : TWorldSeries;
    s     : TStrings;
begin
  if ValueIndex=25 then
  begin
    World:=ChartLocation[0] as TWorldSeries;

    s:=PointsToPath(World.Shapes[ValueIndex].Points.Polygon.GetPoints);
    try
      Share(s);
    finally
      s.Free;
    end;
  end;
end;

procedure TSystemInfoForm.ChartLocationDblClick(Sender: TObject);
begin
  TChartEditForm.Edit(Self,ChartLocation);
end;

procedure TSystemInfoForm.CheckBox1Change(Sender: TObject);
begin
  RefreshMemory;
end;

procedure TSystemInfoForm.CheckBox2Change(Sender: TObject);
begin
  RefreshMemory;
end;

procedure TSystemInfoForm.CheckBox3DThreadingChange(Sender: TObject);
begin
  Chart3.View3D:=CheckBox3DThreading.IsChecked;
end;

procedure TSystemInfoForm.CheckBox3DChange(Sender: TObject);
begin
  Chart1.View3D:=CheckBox3D.IsChecked;
end;

procedure TSystemInfoForm.CheckBox3DSystemChange(Sender: TObject);
begin
  Chart2.View3D:=CheckBox3DSystem.IsChecked;
end;

procedure TSystemInfoForm.FormCreate(Sender: TObject);
begin
  // For Mac OSX Screenshots only:
  //Width:=1280-6;
  //Height:=800-2;

  {$IFNDEF MSWINDOWS}
  TabMemory.Visible:=False;
  {$ENDIF}

  TabMain.ActiveTab:=TabItem1;
  TabDelphi.ActiveTab:=TabSystem;
  TabControlBench.ActiveTab:=TabCanvasBench;
  TabControlAbout.ActiveTab:=TabAboutBox;
  TabControlFMX.ActiveTab:=TabFMXInfo;

  Chart3.Foot.Caption:=IntToStr(TThread.ProcessorCount)+' CPUs';
end;

procedure TSystemInfoForm.FormDestroy(Sender: TObject);
begin
  //Navigator.Free;
end;

{$IFDEF TEEPRO}
procedure TSystemInfoForm.PrepareAbout;
begin
  if Assigned(IAbout) then
  begin
    IAbout.LVersion.Visible:=Width>460;
    IAbout.Image3D1.Visible:=Width>500;

    if IAbout.Image3D1.Visible then
       IAbout.Chart3D1.Position.X:=0.43
    else
       IAbout.Chart3D1.Position.X:=0;
  end;
end;
{$ENDIF}

procedure TSystemInfoForm.FormResize(Sender: TObject);
begin
  if TabMain.ActiveTab=TabBenchmarks then
  begin
    if TabControlBench.ActiveTab=TabCanvasBench then
       TCanvasBenchmark.ResizeChart(Chart1,Width>Height)
    else
    if TabControlBench.ActiveTab=TabBenchRTL then
       TSystemBenchmark.ResizeChart(Chart2,Width>Height)
    else
       TThreadingBenchmark.ResizeChart(Chart3,Width>Height);
  end
  {$IFDEF TEEPRO}
  else
  if TabMain.ActiveTab=TabAbout then
     PrepareAbout;
  {$ENDIF}
end;

procedure TSystemInfoForm.FormShow(Sender: TObject);
begin
  Button1Click(Self);
end;

procedure TSystemInfoForm.FormTouch(Sender: TObject; const Touches: TTouches;
  const Action: TTouchAction);
begin
  if TabMain.ActiveTab=TabFireMonkey then
     if TabControlFMX.ActiveTab=TabGesture then
     begin
       ITouches:=Touches;
       IAction:=Action;

       ChartGesture.Invalidate;
     end;
end;

procedure TSystemInfoForm.GyroscopeChange(Sender: TObject);
begin
  OrientationSensor1.Active:=Gyroscope.IsChecked;
end;

procedure TSystemInfoForm.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);

  function PointSeries:TPointSeries;
  begin
    if Point=nil then
    begin
      Point:=ChartLocation.AddSeries(TPointSeries) as TPointSeries;

      Point.Transparency:=50;

      Point.Pointer.Style:=psCircle;
      Point.Pointer.Size:=14;
      Point.Brush.Color:=TAlphaColors.Red;

      Point.Pen.Width:=3;
      Point.Pen.Color:=TAlphaColors.Darkred;
    end;

    result:=Point;
  end;

var X,Y : Single;
begin
  if ChartLocation.SeriesCount>0 then
  begin
    PointSeries.Clear;

    X:=NewLocation.Longitude;
    Y:=NewLocation.Latitude;

    PointSeries.AddXY(X,Y);

    // Zoom world map around the current location
    PointSeries.GetVertAxis.SetMinMax(Y-10,Y+10);
    PointSeries.GetHorizAxis.SetMinMax(X-20,X+20);
  end;

  LocationSensor1.Active:=False;
end;

procedure TSystemInfoForm.OrientationSensor1StateChanged(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  Timer1.Enabled:=OrientationSensor1.Active;

  if Assigned(IAbout) then
     IAbout.FloatAnimation2.Enabled:=not Gyroscope.IsChecked;
  {$ENDIF}
end;

procedure TSystemInfoForm.RefreshMemory;
begin
  {$IFDEF MSWINDOWS}
  Image1.Bitmap:=MemoryMap(CheckBox1.IsChecked,CheckBox2.IsChecked);
  {$ENDIF}
end;

procedure TSystemInfoForm.TabControlAboutChange(Sender: TObject);
var World : TWorldSeries;
    Marks : TMarksTipTool;
begin
  if TabControlAbout.ActiveTab=TabLocation then
  begin
    if ChartLocation.SeriesCount=0 then
    begin
      ChartLocation.Zoom.MouseButton:=TMouseButton.mbRight;
      ChartLocation.ScrollMouseButton:=TMouseButton.mbLeft;
      ChartLocation.ZoomWheel:=pmwNormal;
      ChartLocation.Zoom.KeepAspectRatio:=True;

      World:=TWorldSeries.Create(Self);

      World.Entities.Antarctica.Visible:=False;
      World.Pen.Color:=TAlphaColorRec.Darkgreen;

      World.FillSampleValues;

      {$IFDEF NEXTGEN}
      //World.Brush.Style:=TBrushKind.None;
      {$ENDIF}

      {$IFDEF MSWINDOWS} // GPU Canvas concave polygons
      World.Transparency:=50;
      {$ENDIF}

      World.HorizAxis:=aBothHorizAxis;
      World.VertAxis:=aBothVertAxis;

      ChartLocation.Axes.Left.Grid.Color:=TAlphaColors.Lightgrey;
      ChartLocation.Axes.Bottom.Grid.Color:=TAlphaColors.Lightgrey;

      ChartLocation.Legend.Hide;

      if TeeVCLBuildVersionInteger>=160906 then
         World.Layers.Cities.Visible:=True;

      ChartLocation.AddSeries(World);

      ChartLocation.Axes.Left.SetMinMax(-60,85);
      ChartLocation.Axes.Right.SetMinMax(-60,85);
      ChartLocation.Axes.Top.SetMinMax(-180,180);
      ChartLocation.Axes.Bottom.SetMinMax(-180,180);

      ChartLocation.Axes.Right.Grid.Hide;
      ChartLocation.Axes.Top.Grid.Hide;

      ChartLocation.Axes.Left.Texts.Font.Size:=10;
      ChartLocation.Axes.Top.Texts.Font.Size:=10;
      ChartLocation.Axes.Right.Texts.Font.Size:=10;
      ChartLocation.Axes.Bottom.Texts.Font.Size:=10;

      Marks:=TMarksTipTool.Create(Self);
      ChartLocation.Tools.Add(Marks);

      Timer1.Enabled:=True;
    end;
  end;
end;

procedure TSystemInfoForm.TabMainChange(Sender: TObject);
begin
  if TabMain.ActiveTab=TabFireMonkey then
  begin
    if MemoFireMonkey.Lines.Count=0 then
       Button2Click(Self);
  end
  else
  if TabMain.ActiveTab=TabMemory then
     RefreshMemory
  else
  if TabMain.ActiveTab=TabAbout then
  begin
    {$IFDEF TEEPRO}
    if not Assigned(IAbout) then
    begin
      IAbout:=CreateAboutBox(Self,Viewport3D1);
      PrepareAbout;
    end;
    {$ENDIF}
  end;
end;

procedure TSystemInfoForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  LocationSensor1.Active:=True;
end;

procedure TSystemInfoForm.Viewport3D1Resize(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  PrepareAbout;
  {$ENDIF}
end;

end.

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

  FMXTee.Procs,

  {$IF Declared(TParallelProc)}
  FMXTee.About,
  {$ENDIF}

  FMX.Dialogs, FMX.Layouts, FMX.Ani,
  FMX.Memo, FMX.ExtCtrls, FMX.TabControl, FMX.Objects, FMX.Styles,
  FMX.Viewport3D, FMX.Platform, FMX.StdCtrls,
  FMX.TextLayout, InspectDelphi, System.Sensors,
  System.Sensors.Components, FMXTee.Engine, FMXTee.Chart,
  System.IOUtils, IPPeerClient, IPPeerServer, System.Tether.Manager,
  System.Tether.AppProfile;

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
    TabFiremonkey: TTabItem;
    ToolBar2: TToolBar;
    Button2: TButton;
    MemoFireMonkey: TMemo;
    ToolBar3: TToolBar;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
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
    ToolBar4: TToolBar;
    Gyroscope: TCheckBox;
    Button3: TButton;
    TabBenchmarks: TTabItem;
    TabControlBench: TTabControl;
    TabCanvasBench: TTabItem;
    ToolBar5: TToolBar;
    Button4: TButton;
    Button5: TButton;
    Chart1: TChart;
    CheckBox3D: TCheckBox;
    ButtonShareChart: TButton;
    TabBenchRTL: TTabItem;
    ToolBar6: TToolBar;
    Button6: TButton;
    CheckBox3: TCheckBox;
    ButtonShareChartSystem: TButton;
    Chart2: TChart;
    TabDB: TTabItem;
    MemoDB: TMemo;
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    LocationSensor1: TLocationSensor;
    MotionSensor1: TMotionSensor;
    OrientationSensor1: TOrientationSensor;
    TabControlAbout: TTabControl;
    TabItem2: TTabItem;
    TabLocation: TTabItem;
    Viewport3D1: TViewport3D;
    ChartLocation: TChart;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TabMainChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure GyroscopeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure OrientationSensor1StateChanged(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBox3DChange(Sender: TObject);
    procedure ButtonShareChartClick(Sender: TObject);
    procedure Button4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure ButtonShareChartSystemClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure TabControlAboutChange(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
  private
    { Private declarations }

    DebugBench : Boolean;

    {$IFDEF TEEPRO}
    IAbout : TFormAbout;

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
  FMXTee.Series.World, FMXTee.Tools,

  Unit_Utils, FMXTee.Series, Unit_Benchmark_Canvas, Unit_Splash;

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

  MemoDB.Lines.Clear;
  TInspectDelphi.AddDB(MemoDB.Lines);
end;

procedure TSystemInfoForm.Button2Click(Sender: TObject);
begin
  MemoFireMonkey.Lines.Clear;
  TInspectDelphi.AddFireMonkey(MemoFireMonkey.Lines);
end;

procedure TSystemInfoForm.Button3Click(Sender: TObject);
begin
  VisitHomepage;
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
  TSystemBenchmark.Run(Chart2,Width>Height);

  ButtonShareChartSystem.Visible:=True;
  CheckBox3.Visible:=True;
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

procedure TSystemInfoForm.Chart1AfterDraw(Sender: TObject);
var R : TRectF;
    b : TBitmap;
begin
  if DebugBench then
  begin
    Chart1.Canvas.ReferenceCanvas.BeginScene;
    try
      b:=BitmapOffScreen;

      R:=RectF(0,0,b.Width,b.Height);

      Chart1.Canvas.ReferenceCanvas.DrawBitmap(b,R,R,1);
    finally
      Chart1.Canvas.ReferenceCanvas.EndScene;
    end;
  end;
end;

procedure TSystemInfoForm.CheckBox1Change(Sender: TObject);
begin
  RefreshMemory;
end;

procedure TSystemInfoForm.CheckBox2Change(Sender: TObject);
begin
  RefreshMemory;
end;

procedure TSystemInfoForm.CheckBox3Change(Sender: TObject);
begin
  Chart2.View3D:=CheckBox3.IsChecked;
end;

procedure TSystemInfoForm.CheckBox3DChange(Sender: TObject);
begin
  Chart1.View3D:=CheckBox3D.IsChecked;
end;

procedure TSystemInfoForm.FormCreate(Sender: TObject);
begin
  {$IFNDEF MSWINDOWS}
  TabMemory.Visible:=False;
  {$ENDIF}

  TabMain.ActiveTab:=TabItem1;
  TabDelphi.ActiveTab:=TabSystem;
  TabControlBench.ActiveTab:=TabCanvasBench;

  {$IFNDEF IOS}
  Button1Click(Self);
  {$ENDIF}
end;

procedure TSystemInfoForm.FormResize(Sender: TObject);
begin
  if TabMain.ActiveTab=TabBenchmarks then
  begin
     if TabControlBench.ActiveTab=TabCanvasBench then
        TCanvasBenchmark.ResizeChart(Chart1,Width>Height)
     else
     if TabControlBench.ActiveTab=TabBenchRTL then
        TSystemBenchmark.ResizeChart(Chart2,Width>Height);
  end
  {$IFDEF TEEPRO}
  else
  if TabMain.ActiveTab=TabAbout then
     PrepareAbout;
  {$ENDIF}
end;

procedure TSystemInfoForm.FormShow(Sender: TObject);
begin
//  FormSplash.Close;
//  FormSplash.Free;
end;

{$IFDEF TEEPRO}
procedure TSystemInfoForm.PrepareAbout;
begin
  if Assigned(IAbout) then
  begin
    IAbout.LVersion.Visible:=Width>(460*Canvas.Scale);
    IAbout.Image3D1.Visible:=Width>(500*Canvas.Scale);
  end;
end;
{$ENDIF}

procedure TSystemInfoForm.GyroscopeChange(Sender: TObject);
begin
  //OrientationSensor1.Active:=Gyroscope.IsChecked;
end;

procedure TSystemInfoForm.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);

  function PointSeries:TPointSeries;
  var Point : TPointSeries;
  begin
    if ChartLocation.SeriesCount>1 then
       Point:=ChartLocation[1] as TPointSeries
    else
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

    // Zoom around the current location

    PointSeries.GetVertAxis.SetMinMax(Y-10,Y+10);

    PointSeries.GetHorizAxis.SetMinMax(X-20,X+20);
  end;

  LocationSensor1.Active:=False;
end;

procedure TSystemInfoForm.OrientationSensor1StateChanged(Sender: TObject);
begin
  {$IFDEF TEEPRO}
  //Timer1.Enabled:=OrientationSensor1.Active;

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

      //World.FillSampleValues;

      World.Brush.Style:=TBrushKind.None;

      World.Transparency:=50;

      ChartLocation.Axes.Left.Grid.Color:=TAlphaColors.Lightgrey;
      ChartLocation.Axes.Bottom.Grid.Color:=TAlphaColors.Lightgrey;

      ChartLocation.Legend.Hide;

      ChartLocation.AddSeries(World);

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

end.

{**********************************************}
{   Delphi FireMonkey Canvas Benchmark         }
{   davidberneda@gmail.com david@steema.com    }
{**********************************************}
unit Unit_Benchmark_Canvas;
{$I InspectDefines.inc}

{$EXCESSPRECISION OFF}

{$IFDEF D18}
{$DEFINE FMX}
{$ENDIF}

interface

uses
  Classes, Types
  {$IFDEF FMX}
  ,FMX.Types,
  FMXTee.Engine, FMX.Graphics,
  FMXTee.Chart, FMXTee.Series
  {$ENDIF}
  ;

var
  TestSeconds:Single=0.2; // Time in seconds to spend for each test.

type
  TTestProc=reference to procedure;

  TBenchmark=class
  private
    FTimerService : IFMXTimerService;
    FChart : TChart;
    FSeries : TChartSeries;

    procedure FinishChart(const ATitle:String);
    procedure InitChart(IsLandscape:Boolean);
    class procedure PrepareChart(const AChart:TChart);
  protected
    function DoRun(const AChart:TChart; IsLandscape:Boolean):Single; virtual;

    procedure RunItem(const TestCaption:String; const Test:TTestProc;
                                  const Prepare:TTestProc=nil;
                                  const Multiplier:Single=1);
  public
    class procedure ResizeChart(const AChart:TChart; IsLandscape:Boolean);
    class function Run(const AChart:TChart; IsLandscape:Boolean):Single;
  end;

  {$IFDEF FMX}
  TCanvasBenchmark=class(TBenchmark)
  protected
    function DoRun(const AChart:TChart; IsLandscape:Boolean):Single; override;
  end;
  {$ENDIF}

  TSystemBenchmark=class(TBenchmark)
  protected
    function DoRun(const AChart:TChart; IsLandscape:Boolean):Single; override;
  end;

  TThreadingBenchmark=class(TBenchmark)
  protected
    function DoRun(const AChart:TChart; IsLandscape:Boolean):Single; override;
  end;

// This is not a class variable to allow compiling in old Delphi versions
var
  BitmapOffScreen : TBitmap=nil;

// Return ASeries data texts and values as TStrings
function SeriesData(const ASeries:TChartSeries):TStrings;

implementation

uses
  System.Math, System.Math.Vectors, System.SysUtils,

  {$IF CompilerVersion>=28}
  System.Threading,
  {$ENDIF}

  System.Generics.Collections, System.IOUtils,
  {$IFDEF D18}
  FMX.TextLayout,
  {$ELSE}
  FMX.Text,
  {$ENDIF}
  FMX.Forms, FMX.Platform, System.UITypes, FMXTee.Procs;

procedure TBenchmark.RunItem(const TestCaption:String; const Test:TTestProc;
                                  const Prepare:TTestProc=nil;
                                  const Multiplier:Single=1);
var t : Integer;
    t1 : Extended;
begin
  if Assigned(Prepare) then
     Prepare;

  t1:=FTimerService.GetTick;

  t:=0;
  repeat
    Test;
    Inc(t);
  until (FTimerService.GetTick-t1)>TestSeconds;

  // Times per Second
  FSeries.Add(Round(Multiplier*t/TestSeconds),TestCaption);
end;

procedure TBenchmark.InitChart(IsLandscape:Boolean);

  function CreateSeries:TChartSeries;
  begin
    if IsLandscape then
       result:=TBarSeries.Create(FChart.Owner)
    else
       result:=THorizBarSeries.Create(FChart.Owner);

    result.ColorEachPoint:=True;
    result.Marks.Transparent:=True;
    result.Marks.Style:=smsValue;

    FChart.Axes.Bottom.GridCentered:=IsLandscape;
    FChart.Axes.Left.GridCentered:=not IsLandscape;
  end;

begin
  FChart.Visible:=False;

  //FChart.BufferedDisplay:=True;

  {$IFDEF ANDROID}
  FChart.ColorPaletteIndex:=19;
  {$ELSE}
  {$IFDEF IOS}
  FChart.ColorPaletteIndex:=18;
  {$ELSE}
  {$IFDEF MACOSX}
  FChart.ColorPaletteIndex:=10;
  {$ELSE}
  FChart.ColorPaletteIndex:=13; //OperaPalette;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  FChart.AllowZoom:=False;
  FChart.ScrollMouseButton:=TMouseButton.mbLeft;

  FChart.FreeAllSeries;

  FSeries:=CreateSeries;

  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, IInterface(FTimerService)) then
     raise EUnsupportedPlatformService.Create('IFMXTimerService');
end;

{$IFDEF FMX}
function TCanvasBenchmark.DoRun(const AChart:TChart; IsLandscape:Boolean):Single;
var TestCanvas : TCanvas;
    TestLayout : TTextLayout;

  procedure RunCanvasItem(const TestCaption:String; const Test:TTestProc;
                                    const Prepare:TTestProc=nil;
                                    const Multiplier:Single=1);
  begin
    if TestCanvas.BeginScene then
    try
      RunItem(TestCaption,Test,Prepare,Multiplier);
    finally
      TestCanvas.EndScene;
    end;
  end;

var Res : TResourceStream;
    b : TBitmap;
    Start,
    x,y,
    tmpSize : Single;
    S : String;
    R : TRectF;
    P1,P2 : TPointF;
    P : TPolygon;
    Path : TPathData;
    t : Integer;

begin
  inherited;

  res:=TResourceStream.Create(HInstance,'CLOUDS',RT_RCDATA);
  try
    b:=TBitmap.CreateFromStream(res);
  finally
    res.Free;
  end;

  if not Assigned(BitmapOffScreen) then
     BitmapOffScreen:=TBitmap.Create(600,800);

  TestCanvas:=BitmapOffScreen.Canvas;

  Start:=FTimerService.GetTick;

  TestCanvas.Stroke.Color:=TAlphaColors.Red;

  x:=10;
  y:=30;

  R:=RectF(x,y,x+100,y+50);
  S:='Hello World';

  RunCanvasItem('Text',procedure begin
      TestCanvas.FillText(R,S,False,1,[],
               TTextAlign.{$IFDEF D20}Leading{$ELSE}taLeading{$ENDIF},
               TTextAlign.{$IFDEF D20}Center{$ELSE}taCenter{$ENDIF});
     end,
     procedure begin TestCanvas.Fill.Color:=TAlphaColors.Black; end);

  R.Offset(0,R.Height+10);

  RunCanvasItem('TextLayout x10',procedure begin
        TestLayout.BeginUpdate;
        TestLayout.TopLeft := R.TopLeft;
        TestLayout.MaxSize := PointF(10000,10000);
        TestLayout.Text := S;
        TestLayout.EndUpdate;
        TestLayout.RenderLayout(TestCanvas);
     end,
     procedure begin
       TestCanvas.Fill.Color:=TAlphaColors.Black;

       if TCanvasManager.DefaultCanvas = TestCanvas.ClassType then
          TestLayout := TTextLayoutManager.DefaultTextLayout.Create(TestCanvas)
       else
          TestLayout := TTextLayoutManager.TextLayoutByCanvas(TestCanvas.ClassType).Create(TestCanvas);

       TestLayout.WordWrap := False;
       TestLayout.HorizontalAlign := TTextAlign.{$IFDEF D20}Leading{$ELSE}taLeading{$ENDIF};
       TestLayout.VerticalAlign := TTextAlign.{$IFDEF D20}Leading{$ELSE}taLeading{$ENDIF};
       TestLayout.RightToLeft := False;

       TestLayout.Font := TestCanvas.Font;
       TestLayout.Color := TestCanvas.Fill.Color;
       TestLayout.Opacity := 1;
     end,
     0.1);

  R.Offset(0,R.Height+10);

  RunCanvasItem('Fill Rectangle x10',procedure begin TestCanvas.FillRect(R,0,0,AllCorners,1); end,nil,0.1);
  RunCanvasItem('Draw Rectangle x2',procedure begin TestCanvas.DrawRect(R,0,0,AllCorners,1); end,nil,0.5);

  P1:=PointF(R.Left,R.Bottom+10);
  P2:=PointF(R.Right,P1.Y);

  S:='Solid Line';

  S:=S+' x10';

  RunCanvasItem(S,procedure begin TestCanvas.DrawLine(P1,P2,1); end,nil,0.1);

  P1.Offset(0,5);
  P2.Offset(0,5);

  RunCanvasItem('Dotted Line',procedure begin
      TestCanvas.DrawLine(P1,P2,1);
     end,
     procedure begin
            TestCanvas.Stroke.Dash:=TStrokeDash.{$IFDEF D20}Dot{$ELSE}sdDot{$ENDIF};
          end);

  R.Offset(0,R.Height+20);

  RunCanvasItem('Fill Ellipse',procedure begin TestCanvas.FillEllipse(R,1); end);
  RunCanvasItem('Draw Ellipse',procedure begin TestCanvas.DrawEllipse(R,1); end);

  P1.X:=R.CenterPoint.X;
  P1.Y:=R.Bottom+10;

  SetLength(P,10);
  P[0]:=P1;
  P[1]:=PointF(P1.X+5,P1.Y+15);
  P[2]:=PointF(P1.X+10,P1.Y+5);
  P[3]:=PointF(P1.X+5,P1.Y+25);
  P[4]:=PointF(P1.X,P1.Y+30);
  P[5]:=PointF(P1.X-5,P1.Y+40);
  P[6]:=PointF(P1.X-15,P1.Y+25);
  P[7]:=PointF(P1.X-25,P1.Y+5);
  P[8]:=PointF(P1.X-15,P1.Y+10);
  P[9]:=PointF(P1.X-5,P1.Y+5);

  RunCanvasItem('Fill Polygon',procedure begin TestCanvas.FillPolygon(P,1); end);
  RunCanvasItem('Draw Polygon',procedure begin TestCanvas.DrawPolygon(P,1); end);

  Path:=TPathData.Create;
  Path.MoveTo(P[0]);
  for t := 1 to 9 do
      Path.LineTo(P[t]);
  Path.ClosePath;

  RunCanvasItem('Fill Path',procedure begin TestCanvas.FillPath(Path,1); end);
  RunCanvasItem('Draw Path',procedure begin TestCanvas.DrawPath(Path,1); end);

  Path.Free;

  R.Offset(0,130);

  S:='Fill Gradient';

  {$IFDEF D21}
  S:=S+' x10';
  {$ENDIF}

  RunCanvasItem(S,procedure begin
      TestCanvas.FillRect(R,0,0,AllCorners,1);
     end,
     procedure
     begin
       TestCanvas.Fill.Kind:=TBrushKind.{$IFDEF D20}Gradient{$ELSE}bkGradient{$ENDIF};
       TestCanvas.Fill.Gradient.Style:=TGradientStyle.{$IFDEF D20}Linear{$ELSE}gsLinear{$ENDIF};
       TestCanvas.Fill.Gradient.Color:=TAlphaColorRec.Brown;
       TestCanvas.Fill.Gradient.Color1:=TAlphaColorRec.Orange;
     end,
     {$IFDEF D21}0.1{$ELSE}1{$ENDIF});

  x:=150;
  R:=RectF(x,y,x+100,y+50+10);

  RunCanvasItem('Fill RoundRect',procedure begin TestCanvas.FillRect(R,8,8,AllCorners,1); end);
  RunCanvasItem('Draw RoundRect',procedure begin TestCanvas.DrawRect(R,8,8,AllCorners,1); end);

  R.Offset(0,R.Height+10);

  R.Width:=b.Width;
  R.Height:=b.Height;

  RunCanvasItem('Brush Bitmap x10',procedure begin
      TestCanvas.FillRect(R,0,0,AllCorners,1);
     end,
     procedure begin
        TestCanvas.Fill.Kind:=TBrushKind.{$IFDEF D20}Bitmap{$ELSE}bkBitmap{$ENDIF};
        TestCanvas.Fill.Bitmap.Bitmap:=b;
     end,
     0.1
  );

  R.Offset(0,R.Height+10);

  RunCanvasItem('Radial Gradient',procedure begin
      TestCanvas.FillRect(R,0,0,AllCorners,1);
     end,
     procedure
     begin
       TestCanvas.Fill.Kind:=TBrushKind.{$IFDEF D20}Gradient{$ELSE}bkGradient{$ENDIF};
       TestCanvas.Fill.Gradient.Style:=TGradientStyle.{$IFDEF D20}Radial{$ELSE}gsRadial{$ENDIF};
       TestCanvas.Fill.Gradient.Color:=TAlphaColorRec.Brown;
       TestCanvas.Fill.Gradient.Color1:=TAlphaColorRec.Orange;
     end);

  RunCanvasItem('Text Width',procedure begin tmpSize:=TestCanvas.TextWidth(S); end);
  RunCanvasItem('Text Height',procedure begin tmpSize:=TestCanvas.TextHeight(S); end);

  result:=FTimerService.GetTick-Start;

  b.Free;

  TestLayout.Free;

  FinishChart('TCanvas Benchmark (FireMonkey '+FloatToStr(FireMonkeyVersion)+')');
end;
{$ENDIF}

{ TSystemBenchmark }

type
  TEnum=(a,b,c,d,e);

  TData=record
    A : Integer;
    B : String;
    C : Double;
    D : TEnum;
    P : Pointer;
  end;

  TObject1=class
  private
    Data : TData;

    class var ClassData : TData;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    class procedure ClassMethod;
    class procedure ClassMethodStatic; static;
    procedure DynamicMethod; dynamic;
    procedure Method;
    procedure VirtualMethod; virtual;
  end;

  TObject2=class(TObject1)
  public
    procedure DynamicMethod; override;
    procedure VirtualMethod; override;
  end;

function TSystemBenchmark.DoRun(const AChart:TChart; IsLandscape:Boolean):Single;
var
  bo : Boolean;
  b : Byte;
  i : Integer;
  i64 : Int64;
  s : Single;
  d : Double;
  e : Extended;
  S1 : String;

  fo : TForm;

var
  Start : Single;
begin
  inherited;

  Start:=FTimerService.GetTick;

  RunItem('String',procedure
    const
      ConstString='!"·%&/()=?¿´ç^*[]\ªº@|#~€,.-;:_<>ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';

    var S1,S2,S3,S4,S5 : String;
        b1 : Boolean;
    begin
      S1:=ConstString;
      S2:=S1;
      S3:=S1+S2;
      S2:=S3.Substring(10);
      S4:=BoolToStr(S2.Contains('ABC'));
      S2:=String.Create('T',255);
      S1:=String.Parse(12345);
      S1:=String.Parse(12345.6789);
      S1:=String.Parse(False);
      //S1:=String.Parse(2*Int64(MaxInt));
      b1:=S1.ToBoolean;
      i:=S1.ToInteger;
      d:=S1.ToDouble;
      e:=S1.ToExtended;
      i:=String.CompareText(S1,S2);
      S2:=String.LowerCase(ConstString);
      S3:=String.UpperCase(ConstString);
      S4:=String.Copy(S1);
      S5:=String.Join('/',[S1,S2,S3,S4]);
      S5:=String.Join('/',[S1,S2,S3,S4,b,i,d,e]);
      bo:=S5.IsNullOrEmpty(S1) or S5.IsNullOrWhiteSpace(S2);
      S1:='    '+S1;
      S2:=S2+'    ';
      S4:='   '+S4+'   ';
      S5:=S4.Trim+S1.TrimLeft+S2.TrimRight;

      bo:=b1;
     end,
     procedure begin end);

  RunItem('Math',procedure

  var ba,bb,bc,bd : Byte;
  var ia,ib,ic,id : Integer;
  var i64a,i64b,i64c,i64d : Int64;
  var sa,sb,sc,sd : Single;
  var da,db,dc,dd : Double;

  // Do not test Extended in 64bit (it is emulated, not native)
  var ea,eb,ec,ed : {$IFDEF CPUX64}Double{$ELSE}Extended{$ENDIF};

  begin

    //TestByte;
    ba:=23;
    bb:=ba*2;
    bc:=bb+ba;
    bd:=bc div ba;
    ba:=bc-bb-bd;
    bb:=ba mod bc;
    bd:=bb xor bc;

    b:=ba+bb+bd;

    // TestInteger;
    ia:=123456789;
    ib:=ia*2;
    ic:=ib+ia;
    id:=ic div ia;
    ia:=ib-ic-id;
    ib:=ia mod ic;
    id:=ib xor ic;

    i:=ia+id+ib;

  // TestInt64;
    i64a:=123456789;
    i64b:=i64a*2;
    i64c:=i64b+i64a;
    i64d:=i64c div i64a;
    i64a:=i64b-i64c-i64d;
    i64b:=i64a mod i64c;
    i64d:=i64b xor i64c;

    i64:=i64a+i64d+i64b;

  // TestSingle;
    sa:=5.2;
    sb:=1.3;
    sc:=Power(sa,sb);

    sa:=123456789.987654321;
    sb:=sa*2;
    sc:=sc+sb+sa;
    sb:=sc-sa;
    sd:=sc/sa;
    sa:=Abs(sb-sc-sd);

    sb:=Log10(sa);
    sc:=Hypot(0.001*sb,0.001*sa);
    sd:=Exp(sb);
    sa:=Sin(sd);
    sb:=Cos(sd);
    sc:=Tan(sd+sc);

    s:=sa+sb+sc;

  // TestDouble;
    da:=5.2;
    db:=1.3;
    dc:=Power(da,db);

    da:=123456789.987654321;
    db:=da*2;
    dc:=dc+db+da;
    db:=dc-da;
    dd:=dc/da;
    da:=Abs(db-dc-dd);

    db:=Log10(da);
    dc:=Hypot(0.001*db,0.001*da);
    dd:=Exp(db);
    da:=Sin(dd);
    db:=Cos(dd);
    dc:=Tan(dd+dc);

    d:=da+db+dc;

  // TestExtended;  (double in x64)

    ea:=5.2;
    eb:=1.3;
    ec:=Power(ea,eb);

    ea:=123456789.987654321;
    eb:=ea*2;
    ec:=ec+eb+ea;

    eb:=ec-ea;
    ed:=ec/ea;
    ea:=Abs(eb-ec-ed);

    eb:=Log10(ea);
    ec:=Hypot(0.001*eb,0.001*ea);
    ed:=Exp(eb);
    ea:=Sin(ed);
    eb:=Cos(ed);
    ec:=Tan(ed+ec);

    e:=ea+eb+ec;

     end,
     procedure begin end);

  RunItem('TObject',procedure
     var o1 : TObject1;
         o2 : TObject2;
         l  : TObjectList<TObject>;
         c1,
         c2  : TComponent;
     begin
       o1:=TObject1.Create;
       o2:=TObject2.Create;
       o1.ClassMethod;
       o1.Method;
       o1.VirtualMethod;
       o2.VirtualMethod;

       l:=TObjectList<TObject>.Create(False);
       l.Add(o1);
       l.Add(o2);
       l.Remove(o1);
       l.Remove(o2);

       l.Free;

       c1:=TComponent.Create(nil);
       c2:=TComponent.Create(nil);

       c1.InsertComponent(c2);

       c2.Free;
       c1.Free;

       o2.Free;
       o1.Free;
     end,
     procedure begin end);

  fo:=TForm.CreateNew(nil);

  try
    RunItem('I/O x0.01',procedure
     var f : TFileStream;
         s : String;
     begin
       s:=TPath.GetTempFileName;

       f:=TFile.Create(s);
       try
         f.WriteComponent(fo);
       finally
         f.Free;
       end;

       if TFile.Exists(s) then
          TFile.Delete(s);
     end,
     procedure begin end,
     100);

  finally
    fo.Free;
  end;

    RunItem('Arrays',procedure
     var a : Array of TData;
         ta : TArray<TData>;
         d1 : TData;
     begin
       SetLength(a,100);
       a[0]:=d1;

       SetLength(ta,100);
       ta[0]:=d1;

       ta:=nil;
       a:=nil;
     end,
     procedure begin end);

  result:=FTimerService.GetTick-Start;

  // This is to avoid the compiler to skip above tests (unused results)
  S1:=BoolToStr(bo)+IntToStr(b)+IntToStr(i)+IntToStr(i64)+FloatToStr(s)+FloatToStr(d)+FloatToStr(e);
  S1:='';

  FinishChart('System Benchmark (RTL '+FloatToStr(RTLVersion)+')'+S1);
end;

{ TBenchmark }

class procedure TBenchmark.PrepareChart(const AChart:TChart);
var tmpHoriz : Boolean;
begin
  if AChart.SeriesCount>0 then
  begin
    tmpHoriz:=AChart[0] is THorizBarSeries;

    AChart.Legend.Visible:=not tmpHoriz;

    if tmpHoriz then
    begin
      AChart.Axes.Bottom.Title.Text:='Times per second';
      AChart.Axes.Left.Title.Text:='';
      AChart.AllowPanning:=TPanningMode.pmVertical;
    end
    else
    begin
      AChart.Axes.Left.Title.Text:='Times per second';
      AChart.Axes.Bottom.Title.Text:='';
      AChart.AllowPanning:=TPanningMode.pmHorizontal;
    end;

    AChart.Axes.Left.GridCentered:=tmpHoriz;
    AChart.Axes.Bottom.GridCentered:=not tmpHoriz;
  end;

  AChart.Axes.Reset;
end;

function TBenchmark.DoRun(const AChart: TChart; IsLandscape: Boolean): Single;
begin
  FChart:=AChart;
  InitChart(IsLandscape);
  result:=0;
end;

class procedure TBenchmark.ResizeChart(const AChart: TChart; IsLandscape:Boolean);
var tmp : TChartSeriesClass;
begin
  if AChart.SeriesCount>0 then
  begin
    if IsLandscape then
       tmp:=TBarSeries
    else
       tmp:=THorizBarSeries;

    if tmp.ClassName<>AChart[0].ClassName then
    begin
      ChangeAllSeriesType(AChart,tmp);
      PrepareChart(AChart);
    end;
  end;
end;

class function TBenchmark.Run(const AChart: TChart; IsLandscape: Boolean): Single;
begin
  with Self.Create do
  try
    result:=DoRun(AChart,IsLandscape);
  finally
    Free;
  end;
end;

procedure TBenchmark.FinishChart(const ATitle:String);
begin
  FChart.AddSeries(FSeries);
  FChart.Title.Caption:=ATitle;

  PrepareChart(FChart);

  FChart.Visible:=True;
end;

function SeriesData(const ASeries:TChartSeries):TStrings;
var t : Integer;
begin
  result:=TStringList.Create;

  result.Add((ASeries.ParentChart as TChart).Title.Caption);
  result.Add('');

  for t := 0 to ASeries.Count-1 do
      result.Add(ASeries.XLabel[t]+','+FloatToStr(ASeries.MandatoryValueList[t]));
end;

{ TObject1 }

class procedure TObject1.ClassMethod;
begin
  ClassData.A:=123;
end;

class procedure TObject1.ClassMethodStatic;
begin
  ClassData.A:=456;
end;

constructor TObject1.Create;
begin
end;

destructor TObject1.Destroy;
begin
  inherited;
end;

procedure TObject1.DynamicMethod;
begin
end;

procedure TObject1.Method;
begin
  Data.A:=789;
  Data.P:=ClassInfo;
end;

procedure TObject1.VirtualMethod;
begin
  Data.A:=InstanceSize;
  Data.D:=TEnum.e;
  Data.B:=Self.ClassName;
end;

{ TObject2 }

procedure TObject2.DynamicMethod;
begin
  inherited;
end;

procedure TObject2.VirtualMethod;
begin
  inherited;
  Data.B:=Self.ClassName+Self.ClassParent.ClassName;
end;

{ TThreadingBenchmark }

  // Example of a function that takes time to run:
  procedure RunTest(Index:Integer);
  var t,tt : Integer;
      x,y,z : Single;
  begin
    for t := 1 to 100 do
    begin
      x:=44343+t;

      for tt := 1 to 100 do
      begin
        y:=x*123.456;
        z:=y/789.321;
        x:=z*0.5;
      end;
    end;
  end;


function TThreadingBenchmark.DoRun(const AChart: TChart;
  IsLandscape: Boolean): Single;
var
  Start : Single;
begin
  inherited;

  Start:=FTimerService.GetTick;

  TestSeconds:=1;

  RunItem('Linear',procedure
  var t : Integer;
  begin
    for t := 0 to 99 do
       RunTest(0);
  end);

  {$IF CompilerVersion>=28}
  RunItem('Parallel',procedure
  begin
    TParallel.For(1,0,99,RunTest);
  end);
  {$ENDIF}

  result:=FTimerService.GetTick-Start;

  TestSeconds:=0.1;

  FinishChart('Threading Benchmark (RTL '+FloatToStr(RTLVersion)+')');
end;

initialization
finalization
  BitmapOffScreen.Free;
end.

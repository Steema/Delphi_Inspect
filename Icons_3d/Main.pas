unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Viewport3D;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Viewport3D1: TViewport3D;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses Unit8;

procedure TForm1.Button1Click(Sender: TObject);
const P='C:\Root\TeeChartVCL\Tests\FMX\Delphi_Inspect\Icons_3d\';

var b: TBitmap;
    b2: TBitmap;

  procedure SaveResized(const AWidth:Integer; AHeight:Integer=0);
  var bb : TBitmap;
  begin
    if AHeight=0 then
       AHeight:=AWidth;

    bb:=TBitmap.Create;
    try
      bb.SetSize(AWidth,AHeight);
      bb.Clear(Viewport3D1.Color);
      bb.Canvas.BeginScene;
      bb.Canvas.DrawBitmap(b2,RectF(0,0,b.Width-1,b.Height-1),RectF(0,0,AWidth-1,AHeight-1),1);
      bb.Canvas.EndScene;
      bb.SaveToFile(P+'Icon_'+IntToStr(AWidth)+'x'+IntToStr(AHeight)+'.png');
    finally
      bb.Free;
    end;
  end;

var data : TBitmapData;
    c:TAlphaColor;
begin
  b:=Viewport3D1.MakeScreenshot;
  try
    if b.Map(TMapAccess.Read,data) then
    begin
      c:=Data.GetPixel(0,0);
      Caption:=IntToStr(TAlphaColorRec(c).A);
      b.Unmap(data);
    end;

    b2:=TBitmap.Create;
    b2.SetSize(b.Width,b.Height);
    b2.Clear(TAlphaColorRec.Null);
    b2.Canvas.BeginScene;

    b2.Canvas.Stroke.Kind:=TBrushKind.None;
    b2.Canvas.Fill.Kind:=TBrushKind.Gradient;
    b2.Canvas.Fill.Gradient.Color:=TAlphaColorRec.Black;
    b2.Canvas.Fill.Gradient.Color1:=TAlphaColorRec.DkGray;

    b2.Canvas.FillRect(RectF(0,0,b2.Width,b2.Height),48,48,AllCorners,1);
    b2.Canvas.DrawBitmap(b,RectF(0,0,b.Width-1,b.Height-1),RectF(0,0,b.Width-1,b.Height-1),1);
    b2.Canvas.EndScene;

    b2.SaveToFile(P+'Big.png');

    // Icns:
    SaveResized(16);

    SaveResized(32); // and: 48, 96, 512


    SaveResized(64);
    SaveResized(128);
    SaveResized(256);
    SaveResized(1024); // <-- Should be renamed to icon_512x512@2x.png

    // SEE HERE FOR Mac OSX Icns nightmare:
    // https://developer.apple.com/library/mac/documentation/GraphicsAnimation/Conceptual/HighResolutionOSX/Optimizing/Optimizing.html#//apple_ref/doc/uid/TP40012302-CH7-SW3

    // Android and Icns:
    SaveResized(36);
    SaveResized(48);
    SaveResized(72);
    SaveResized(96);
    SaveResized(144);
    SaveResized(512);

    // Android splash
    SaveResized(426,320);
    SaveResized(470,320);
    SaveResized(640,480);
    SaveResized(960,720);

    // iOS
    SaveResized(57);
    SaveResized(60);
    SaveResized(114);
    SaveResized(120);

    // iOS splash
    SaveResized(320,480);
    SaveResized(640,960);
    SaveResized(640,1136);

    // iOS spotlight
    SaveResized(29);
    SaveResized(40);
    SaveResized(58);
    SaveResized(80);

    // iPad
    SaveResized(50);
    SaveResized(76);
    SaveResized(100);
    SaveResized(152);
    SaveResized(768,1004);
    SaveResized(768,1024);
    SaveResized(1024,748);
    SaveResized(1024,768);
    SaveResized(1536,2008);
    SaveResized(1536,2048);
    SaveResized(2048,1496);
    SaveResized(2048,1536);

  finally
    b.Free;
    b2.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var f: TForm8;
begin
  f:=TForm8.Create(Self);

  Viewport3D1.Color:=f.Color;

  while f.ChildrenCount>0 do
     f.Children[0].Parent:=Self.Viewport3D1;

  Width:=600;
  Height:=600+Round(Panel1.Height)+23;
end;

end.

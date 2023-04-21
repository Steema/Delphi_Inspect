unit Unit_Utils;
{$I InspectDefines.inc}

interface

uses
  System.Classes,
  FMX.Types,
  {$IFDEF MSWINDOWS}
  System.UITypes,
  {$IF FireMonkeyVersion>18}
  FMX.Graphics,
  {$ENDIF}
  {$ENDIF}
  FMXTee.Procs,
  {$IF Declared(TParallelProc)}
  FMXTee.About,
  {$ENDIF}
  FMX.Types3D, FMX.Viewport3D;

{$IF Declared(TParallelProc)}
function CreateAboutBox(const AOwner:TComponent; const AViewport: TViewport3D):TFormAbout;
{$ENDIF}

{$IFDEF MSWINDOWS}
function MemoryMap(const ShowUnallocated,ShowSysReserved:Boolean):TBitmap;
{$ENDIF}

procedure VisitHomepage;

procedure Share(const S:TStrings);

implementation

uses
  {$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  FMX.Helpers.Android,
  AndroidApi.Helpers,

  {$IFDEF D24}
  Androidapi.JNI.App,
  {$ENDIF}

  {$ENDIF}

  // iOS
  FMX.Platform,
  FMX.MediaLibrary,

  System.IOUtils;

{$IF Declared(TParallelProc)}
function CreateAboutBox(const AOwner:TComponent; const AViewport: TViewport3D):TFormAbout;
var IAbout : TFormAbout;
var tmp : TFMXObject;
begin
 IAbout:=TFormAbout.Create(AOwner);

 IAbout.TextOffer.Text:='www.steema.com';
 IAbout.TextOffer.Visible:=True;
 IAbout.TextOffer.Stretch:=False;
 IAbout.TextOffer.Width:=8;

 IAbout.Image3D1.Position.X:=-5;
 IAbout.FloatAnimation4.Loop:=True;
 IAbout.FloatAnimation4.Duration:=25;
 IAbout.FloatAnimation4.Delay:=1;

 while IAbout.ChildrenCount>0 do
 begin
   tmp:=IAbout.Children[0];
   tmp.Parent:=AViewport;
 end;

 IAbout.Series1.DepthPercent:=50;

 //Navigator:=TMouse3D.Create(IAbout.Chart3D1);
 //IAbout.Chart3D1.Chart.Tools.Add(TRotateTool.Create(IAbout));

 result:=IAbout;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function MemoryMap(const ShowUnallocated,ShowSysReserved:Boolean):TBitmap;
var Data : TBitmapData;
  Y: Integer;
  t,
  tmpCount,
  X: Integer;
  tmp : TAlphaColor;
  AMap : TMemoryMap;
begin
  result:=TBitmap.Create{$IF FireMonkeyVersion<=18}(0,0){$ENDIF};

  GetMemoryMap(AMap);

  if ShowUnallocated and ShowSysReserved then
     tmpCount:=256
  else
  begin
    tmpCount:=0;

    for t := 0 to 65535 do
       if (ShowUnallocated or (AMap[t]<>csUnallocated)) and
          (ShowSysReserved or (AMap[t]<>csSysReserved)) then
            Inc(tmpCount);

    tmpCount:=Round(Sqrt(tmpCount));
  end;

  result.SetSize(tmpCount,tmpCount);

  if result.Map(TMapAccess.{$IF FireMonkeyVersion>18}Write{$ELSE}maWrite{$ENDIF},Data) then
  begin
    Y:=0;
    X:=0;

    tmp:=TAlphaColorRec.Null;

    for t:=0 to 65535 do
    begin

      case AMap[t] of
        csUnallocated: if ShowUnallocated then
                          tmp:=TAlphaColorRec.White
                       else
                          Continue;

        csAllocated: tmp:=TAlphaColorRec.Yellow;
        csReserved: tmp:=TAlphaColorRec.Red;
        csSysAllocated: tmp:=TAlphaColorRec.Lightgreen;
      else
        if ShowSysReserved then
           tmp:=TAlphaColorRec.Black
        else
           Continue;
      end;

      Data.SetPixel(X,Y,tmp);

      Inc(X);

      if X>=tmpCount then
      begin
        X:=0;
        Inc(Y);
      end;
    end;

    result.Unmap(Data);
  end;
end;
{$ENDIF}

procedure VisitHomepage;
const
  HomePage='http://steema.com/wp/blog/2014/07/31/delphi-inspect-simple-free-tool-to-view-firemonkey-and-rtl-system-parameters/';
{$IFDEF ANDROID}
var Intent : JIntent;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  Intent:=TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(HomePage));
  SharedActivity.startActivity(Intent);
  {$ELSE}
  TeeGotoURL(0 {NativeUInt(Handle)},HomePage);
  {$ENDIF}
end;

procedure Share(const S:TStrings);
var
  {$IFDEF ANDROID}
  Intent: JIntent;
  {$ELSE}
  {$IFDEF IOS}
  ShareAction: IFMXShareSheetActionsService;
  {$ELSE}
  tmp : String;
  {$ENDIF}
  {$ENDIF}
begin
  {$IFDEF ANDROID}
  Intent:=TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_SEND);
  Intent.setType(StringToJString('text/plain'));
  Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(S.Text));
  SharedActivity.startActivity(TJIntent.JavaClass.createChooser(Intent,StrToJCharSequence('Share')));

  {$ELSE}

  {$IFDEF IOS}

  if TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetActionsService, IInterface(ShareAction)) then
     ShareAction.Share(nil,S.Text,nil);

  {$ELSE}
  tmp:=TPath.ChangeExtension(TPath.GetTempFileName,'.txt');
  S.SaveToFile(tmp);
  TeeGotoURL(0,tmp);
  {$ENDIF}
  {$ENDIF}
end;

end.

{**********************************************}
{   Delphi Inspect FireMonkey app              }
{   davidberneda@gmail.com david@steema.com    }
{**********************************************}
unit InspectDelphi platform;
{$I InspectDefines.inc}

interface

uses
  System.Classes, System.Types, System.UITypes, System.Generics.Collections,
  FMX.Types;

type
  TInspectDelphi=class
  private
    IStrings : TStrings;

    procedure Add(const Text:String; const Value:Integer); overload;
    procedure Add(const Text:String; const Value:Int64; const AFormat:String=''); overload;
    procedure Add(const Text:String; const Value:UInt64; const AFormat:String=''); overload;
    procedure Add(const Text:String=''); overload;
    procedure Add(const Text,Value:String); overload;
    procedure Add(const Text:String; const Value:Boolean); overload;
    procedure Add(const Text:String; const Value:Pointer); overload;
    procedure Add(const Text:String; const Value:Single); overload;
    procedure Add(const Text:String; const Value:Double); overload;
    procedure Add(const Text:String; const Value:Extended); overload;

    procedure Add(const Text: String; const Value: TRectF); overload;
    procedure Add(const Text: String; const Value: TSizeF); overload;

    // Not an Add overload because clashes with Add(.. Value:Single)
    procedure AddDate(const Text:String; const Value:TDate); //overload;

    {$IF FireMonkeyVersion>20}
    procedure Add(const Text:String; const Value:TRect); overload;
    {$ENDIF}

    procedure Add(const Text:String; const ASize:TSize); overload;

    procedure Add(const Text:String; const AStrings:Array of String); overload;
    procedure Add(const Text:String; const AStrings:TArray<String>); overload;

    procedure AddDBXDrivers;
    procedure AddHeader(const Text:String);
    function BidiMode:String;

    procedure DoDatabase;
    procedure DoEncoding;
    procedure DoFireMonkey;
    procedure DoHardware;
    procedure DoLocale;
    procedure DoMath;
    procedure DoOS;
    procedure DoPaths;
    procedure DoSystem;
  public
    class procedure AddAll(const Dest:TStrings);

    class procedure AddDB(const Dest:TStrings);
    class procedure AddFireMonkey(const Dest:TStrings);
    class procedure AddHardware(const Dest:TStrings);
    class procedure AddLocale(const Dest:TStrings);
    class procedure AddMath(const Dest:TStrings);
    class procedure AddOS(const Dest:TStrings);
    class procedure AddPaths(const Dest:TStrings);
    class procedure AddSystem(const Dest:TStrings);
  end;

implementation

{$IFDEF ANDROID}
{$IF CompilerVersion>=28} // XE7 (not yet !)
//  {$DEFINE HASPRINTER}
{$ENDIF}
{$ELSE}
{$DEFINE HASPRINTER}
{$ENDIF}


uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}

  Data.DB,
  Data.DBXCommon,

  System.SysUtils, System.Diagnostics,
  System.IOUtils, System.Math, System.Sensors,
  System.TimeSpan, System.DateUtils,
  FMX.Platform, FMX.Forms, FMX.Controls, FMX.Graphics,
  {$IF FireMonkeyVersion>18}
  FMX.BehaviorManager,
  {$ENDIF}
  {$IF FireMonkeyVersion>20}
  System.Devices,
  {$ENDIF}

  {$IFDEF ANDROID}
  FMX.Helpers.Android,
  Androidapi.Helpers,
  Androidapi.JNI.Os,

  {$IF FireMonkeyVersion>=21}
  Androidapi.JNI.App,
  {$ENDIF}

  {$IFDEF D24}
  Androidapi.JNI.JavaTypes,
  {$ENDIF}

  {$ENDIF}

  {$IFDEF IOS}
  FMX.Helpers.iOS,
  iOSapi.UIKit,
  {$ENDIF}

  {$IFDEF MACOSX}
  FMX.Helpers.Mac,
  {$ENDIF}

  FMX.TextLayout, FMX.Media, FMX.Types3D, FMX.Gestures, FMX.Ani, FMX.Filter,
  FMX.Filter.Custom, FMX.Filter.Standard,
  {$IFDEF HASPRINTER}
  FMX.Printer,
  {$ENDIF}

  REST.Client,
  FireDAC.Stan.Consts,

  {$IFDEF D28} // 11.3
  {$IFNDEF CPUX64}
  {$DEFINE NO_INTERBASE}
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF NO_INTERBASE}
  IBX.IBXConst,
  {$ENDIF}

  System.ZLib, System.Threading,
  System.Tether.Manager,
  System.Sensors.Components,

  System.Bluetooth, System.Bluetooth.Components,

  FMX.Styles, Unit_Utils, FMXTee.Constants;

function ReadableString(const S:String):String;
var C : Char;
begin
  result:='';

  for c in S do
     if Ord(C)<32 then
        result:=result+'$'+IntToStr(Ord(C))
     else
        result:=result+C;
end;

{ TInspectDelphi }

class procedure TInspectDelphi.AddAll(const Dest: TStrings);
begin
  Dest.BeginUpdate;
  try
    AddSystem(Dest);
    AddFireMonkey(Dest);
    AddHardware(Dest);
    AddLocale(Dest);
    AddMath(Dest);
    AddOS(Dest);
    AddPaths(Dest);
    AddDB(Dest);
  finally
    Dest.EndUpdate;
  end;
end;

class procedure TInspectDelphi.AddDB(const Dest: TStrings);
var i : TInspectDelphi;
begin
  i:=TInspectDelphi.Create;
  try
    i.IStrings:=Dest;
    Dest.BeginUpdate;
    i.DoDatabase;
  finally
    Dest.EndUpdate;
    i.Free;
  end;
end;

class procedure TInspectDelphi.AddSystem(const Dest: TStrings);
var i : TInspectDelphi;
begin
  i:=TInspectDelphi.Create;
  try
    i.IStrings:=Dest;
    Dest.BeginUpdate;
    i.DoSystem;
  finally
    Dest.EndUpdate;
    i.Free;
  end;
end;

class procedure TInspectDelphi.AddFireMonkey(const Dest: TStrings);
var i : TInspectDelphi;
begin
  i:=TInspectDelphi.Create;
  try
    i.IStrings:=Dest;
    Dest.BeginUpdate;
    i.DoFireMonkey;
  finally
    Dest.EndUpdate;
    i.Free;
  end;
end;

class procedure TInspectDelphi.AddHardware(const Dest: TStrings);
var i : TInspectDelphi;
begin
  i:=TInspectDelphi.Create;
  try
    i.IStrings:=Dest;
    Dest.BeginUpdate;
    i.DoHardware;
  finally
    Dest.EndUpdate;
    i.Free;
  end;
end;

class procedure TInspectDelphi.AddLocale(const Dest: TStrings);
var i : TInspectDelphi;
begin
  i:=TInspectDelphi.Create;
  try
    i.IStrings:=Dest;
    Dest.BeginUpdate;
    i.DoLocale;
  finally
    Dest.EndUpdate;
    i.Free;
  end;
end;

class procedure TInspectDelphi.AddMath(const Dest: TStrings);
var i : TInspectDelphi;
begin
  i:=TInspectDelphi.Create;
  try
    i.IStrings:=Dest;
    Dest.BeginUpdate;
    i.DoMath;
  finally
    Dest.EndUpdate;
    i.Free;
  end;
end;

class procedure TInspectDelphi.AddOS(const Dest: TStrings);
var i : TInspectDelphi;
begin
  i:=TInspectDelphi.Create;
  try
    i.IStrings:=Dest;
    Dest.BeginUpdate;
    i.DoOS;
  finally
    Dest.EndUpdate;
    i.Free;
  end;
end;

class procedure TInspectDelphi.AddPaths(const Dest: TStrings);
var i : TInspectDelphi;
begin
  i:=TInspectDelphi.Create;
  try
    i.IStrings:=Dest;
    Dest.BeginUpdate;
    i.DoPaths;
  finally
    Dest.EndUpdate;
    i.Free;
  end;
end;

procedure TInspectDelphi.DoEncoding;

  procedure AddEncoding(const AName:String; const AEncoding:TEncoding);
  begin
    AddHeader('TEncoding '+AName);
    Add('CodePage',AEncoding.CodePage);
    Add('EncodingName',AEncoding.EncodingName);
    Add('IsSingleByte',AEncoding.IsSingleByte);
  end;

var mbcs : TMBCSEncoding;
  {$IFDEF POSIX}
  cs: TCharSetEncoding;
  {$ENDIF}
begin
  AddEncoding('Default',TEncoding.Default);
  AddEncoding('ANSI',TEncoding.ANSI);
  AddEncoding('ASCII',TEncoding.ASCII);
  AddEncoding('BigEndianUnicode',TEncoding.BigEndianUnicode);
  AddEncoding('Unicode',TEncoding.Unicode);
  AddEncoding('UTF7',TEncoding.UTF7);
  AddEncoding('UTF8',TEncoding.UTF8);

  mbcs:=TMBCSEncoding.Create;
  try
    AddEncoding('MBCS',mbcs);
  finally
    mbcs.Free;
  end;

  {$IFDEF POSIX}
  cs:=TCharSetEncoding.Create;
  try
    AddEncoding('CharSet',cs);
  finally
    cs.Free;
  end;
  {$ENDIF}

  Add;
end;

procedure TInspectDelphi.DoSystem;
{$IFDEF MSWINDOWS}
var AMemoryManagerState: TMemoryManagerState;
{$ENDIF}
begin
  Add('Compiler Version',FloatToStr(CompilerVersion)+' ('+RADIDE+')');
  Add('RTL Version',RTLVersion);

  Add('Platform',PlatformToString);

  Add;

  Add('Now',DateTimeToStr(Now));
  Add('Current Year',CurrentYear);
  Add('Is Leap Year',IsLeapYear(CurrentYear));

  Add('Min DateTime',DateTimeToStr(MinDateTime));
  Add('Max DateTime',DateTimeToStr(MaxDateTime));

  Add;

  Add('GetCurrentDir',GetCurrentDir);

  {$IF !Declared(FireMonkeyVersion)}
  Add('ExeName',Application.ExeName);
  {$ENDIF}

  Add('ParamStr(0)',ParamStr(0));
  Add('ParamCount',ParamCount);

  Add('CurrentGroup',CurrentGroup);

  Add('GetModuleName',GetModuleName(HInstance));

  Add('ExceptionClass',ExceptionClass.ClassName);

  Add('ExitCode',ExitCode);
  Add('ErrorAddr',ErrorAddr);

  Add('RandSeed',RandSeed);

  Add('IsConsole',IsConsole);
  Add('IsMultiThread',IsMultiThread);

  Add('Processor Count',TThread.ProcessorCount);
  Add('Is Single Processor',TThread.IsSingleProcessor);

  {$IFDEF D22}
  Add('Thread Pool Min',TThreadPool.Default.MinWorkerThreads);
  Add('Thread Pool Max',TThreadPool.Default.MaxWorkerThreads);
  {$ENDIF}

  Add('FileMode',FileMode);

{$IF defined(LINUX) or defined(MACOS) or defined(ANDROID)}
  Add('FileAccessRights',FileAccessRights);
  Add('ArgCount',ArgCount);
  Add('ArgValues',ArgValues);
  Add('envp',envp);
{$ENDIF LINUX or MACOS or ANDROID}

  {$IFDEF MSWINDOWS}
  Add('HPrevInst',HPrevInst);
  {$ENDIF}

  Add('Main Instance',MainInstance);
  Add('Main Thread ID',MainThreadID);
  Add('Current Thread ID',TThread.CurrentThread.ThreadID);
  Add('IsLibrary',IsLibrary);

  {$IFDEF MSWINDOWS}
  Add('CmdShow',CmdShow);
  Add('CmdLine',String(CmdLine));
  {$ENDIF}

  Add('CPU Count',CPUCount);

  {$IF defined(CPU64BITS)}
  Add('Bits','64');
  {$ENDIF}

  {$IF defined(CPUX86) or defined(CPUX64)}
  case TestSSE of
    0: Add('No SSE');
    1: Add('SSE');
  else
     Add('SSE2');
  end;

  case TestFDIV of
    -1: Add('Flawed Pentium');
     0: Add('Unknown Flawed Pentium');
  else
     Add('Not Flawed FDIV Pentium');
  end;

  case Test8086 of
    CPUi386 : Add('CPU','i386');
    CPUi486 : Add('CPU','i486');
  CPUPentium: Add('CPU','Pentium');
  else
     Add('CPU','Unknown');
  end;

  Add('Test8087',Test8087);
  {$ENDIF}

  {$IF Defined(CPUARM)}
  Add('DefaultFPSCR',DefaultFPSCR);
  {$ENDIF}

  {$IF Defined(CPUX86) or Defined(CPUX64)}
  Add('Default8087CW',IntToHex(Default8087CW,4));
  Add('DefaultMXCSR',IntToHex(DefaultMXCSR,4));
  {$ENDIF}

  Add('HeapAllocFlags',IntToHex(HeapAllocFlags,4));
  Add('DebugHook',DebugHook);
  Add('JITEnable',JITEnable);
  Add('NoErrMsg',NoErrMsg);

  {$IF defined(LINUX) or defined(MACOS) or defined(ANDROID)}
  Add('CoreDumpEnabled',CoreDumpEnabled);
  {$ENDIF}

  Add('DefaultSystemCodePage',DefaultSystemCodePage);
  Add('DefaultUnicodeCodePage',DefaultUnicodeCodePage);

  {$IFDEF MSWINDOWS}
  Add('UTF8CompareLocale',UTF8CompareLocale);
  {$ENDIF}

  case DefaultTextLineBreakStyle of
    tlbsLF: Add('DefaultTextLineBreakStyle','LF');
  else
    Add('DefaultTextLineBreakStyle','CRLF');
  end;

  Add('sLineBreak',ReadableString(sLineBreak));

  Add('IsMemoryManagerSet',IsMemoryManagerSet);

  Add('ReportMemoryLeaksOnShutdown',ReportMemoryLeaksOnShutdown);

  Add('NeverSleepOnMMThreadContention',NeverSleepOnMMThreadContention);

  {$IFDEF MSWINDOWS}
  GetMemoryManagerState(AMemoryManagerState);
  Add('AllocatedMediumBlockCount',AMemoryManagerState.AllocatedMediumBlockCount);
  Add('TotalAllocatedMediumBlockSize',AMemoryManagerState.TotalAllocatedMediumBlockSize);
  Add('ReservedMediumBlockAddressSpace',AMemoryManagerState.ReservedMediumBlockAddressSpace);
  Add('AllocatedLargeBlockCount',AMemoryManagerState.AllocatedLargeBlockCount);
  Add('TotalAllocatedLargeBlockSize',AMemoryManagerState.TotalAllocatedLargeBlockSize);
  Add('ReservedLargeBlockAddressSpace',AMemoryManagerState.ReservedLargeBlockAddressSpace);
  {$ENDIF}

  {$IFDEF PC_MAPPED_EXCEPTIONS}
  Add('IsUnwinderSet',IsUnwinderSet);
  Add('AreOSExceptionsBlocked',AreOSExceptionsBlocked);
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  case GetMinimumBlockAlignment of
    mba8Byte:  Add('GetMinimumBlockAlignment','8 byte');
  else
    Add('GetMinimumBlockAlignment','16 byte');
  end;
  {$ENDIF}

  {$IFNDEF ANDROID}
  {$IFNDEF NEXTGEN}
  Add('GetPackageTargets',GetPackageTargets(HInstance));
  {$ENDIF}
  {$ENDIF}

  Add('FGetRound',FGetRound);
  Add('FGetExceptFlag',FGetExceptFlag);
  Add('FGetExceptMask',FGetExceptMask);

  {$IF defined(CPUX86) or defined(CPUX64)}
  Add('Get8087CW',Get8087CW);
  Add('GetMXCSR',GetMXCSR);
  {$ENDIF}

  {$IF defined(CPUARM)}
  Add('GetFPSCR',GetFPSCR);
  {$ENDIF}

  {$IFDEF POSIX}
  Add('GetACP',GetACP);
  {$ENDIF}

  {$IF RTLVersion>25}
  Add('TMonitor.DefaultSpinCount',TMonitor.DefaultSpinCount);
  {$ENDIF}

  Add('HexDisplayPrefix',HexDisplayPrefix);

  AddHeader('Stop Watch');
  TStopwatch.StartNew;

  Add('Frequency',TStopwatch.Frequency);
  Add('High Resolution',TStopwatch.IsHighResolution);

  Add;

  Add('Min Time Span',''+TTimeSpan.MinValue);
  Add('Max Time Span',''+TTimeSpan.MaxValue);

  AddHeader('Compiler Options');

  Add('Debug Info',{$IFOPT D+}True{$ELSE}False{$ENDIF});
  Add('Local Symbols',{$IFOPT L+}True{$ELSE}False{$ENDIF});
  Add('Optimizations',{$IFOPT O+}True{$ELSE}False{$ENDIF});
  Add('Range Checks',{$IFOPT R+}True{$ELSE}False{$ENDIF});
  Add('I/O Checks',{$IFOPT I+}True{$ELSE}False{$ENDIF});
  Add('Overflow Checks',{$IFOPT Q+}True{$ELSE}False{$ENDIF});
  Add('Assertions',{$IFOPT C+}True{$ELSE}False{$ENDIF});

  AddHeader('Other');

  Add('New GUID',TGUID.NewGuid.ToString);
  Add('ZLib Version',String(ZLIB_VERSION));

  {$IFDEF D28}
  {$IFDEF ANDROID}
  Add('Android Enable Realloc Memory Gap',EnableReallocMemoryGap);
  {$ENDIF}
  {$ENDIF}

  Add;
end;

procedure TInspectDelphi.Add(const Text: String; const Value: Integer);
begin
  IStrings.Add(Text+': '+IntToStr(Value));
end;

procedure TInspectDelphi.Add(const Text: String; const Value: TSizeF);
begin
  IStrings.Add(Text+': X: '+Value.cx.ToString+' Y: '+Value.cy.ToString);
end;

procedure TInspectDelphi.Add(const Text: String; const Value: TRectF);
begin
  IStrings.Add(Text+': Left: '+Value.Left.ToString+' Top: '+Value.Top.ToString+' '+
               'Right: '+Value.Right.ToString+' Bottom: '+Value.Bottom.ToString);
end;

procedure TInspectDelphi.Add(const Text: String; const Value: Int64; const AFormat:String='');
begin
  if AFormat='' then
     IStrings.Add(Text+': '+IntToStr(Value))
  else
     IStrings.Add(Text+': '+FormatFloat(AFormat,Value));
end;

procedure TInspectDelphi.Add(const Text: String);
begin
  IStrings.Add(Text);
end;

procedure TInspectDelphi.Add(const Text, Value: String);
begin
  IStrings.Add(Text+': '+Value);
end;

procedure TInspectDelphi.Add(const Text: String; const Value: Boolean);
begin
  IStrings.Add(Text+': '+BoolToStr(Value,True));
end;

procedure TInspectDelphi.Add(const Text: String; const Value: Pointer);
var P : NativeInt;
begin
  P:=NativeInt(Value);
  IStrings.Add(Text+': '+IntToHex(P,{$IFDEF CPUX64}8{$ELSE}4{$ENDIF}));
end;

procedure TInspectDelphi.Add(const Text: String; const Value: Single);
begin
  IStrings.Add(Text+': '+FloatToStr(Value));
end;

function RectToString(const R: TRect): string;
begin
  Result := '(' + IntToStr(R.Left) + ',' + IntToStr(R.Top) + ',' +
             IntToStr(R.Right) + ',' + IntToStr(R.Bottom) + ')';
end;

{$IF FireMonkeyVersion>20}
procedure TInspectDelphi.Add(const Text: String; const Value: TRect);
begin
  IStrings.Add(Text+': '+RectToString(Value));
end;
{$ENDIF}

procedure TInspectDelphi.Add(const Text: String; const ASize: TSize);
begin
  IStrings.Add(Text+': Width='+IntToStr(ASize.Width)+', Height='+IntToStr(ASize.Height));
end;

procedure TInspectDelphi.Add(const Text: String;
  const AStrings: array of String);
var s: String;
    t: Integer;
begin
  s:='';

  for t := Low(AStrings) to High(AStrings) do
     if t=Low(AStrings) then
        s:=AStrings[t]
     else
        s:=s+' '+AStrings[t];

  Add(Text,s);
end;

procedure TInspectDelphi.Add(const Text:String; const AStrings:TArray<String>);
var s: String;
    t: Integer;
begin
  s:='';

  for t := Low(AStrings) to High(AStrings) do
     if t=Low(AStrings) then
        s:=AStrings[t]
     else
        s:=s+' '+AStrings[t];

  Add(Text,s);
end;

procedure TInspectDelphi.Add(const Text: String; const Value: Double);
begin
  IStrings.Add(Text+': '+FloatToStr(Value));
end;

procedure TInspectDelphi.Add(const Text: String; const Value: Extended);
begin
  IStrings.Add(Text+': '+FloatToStr(Value));
end;

procedure TInspectDelphi.Add(const Text: String; const Value: UInt64;
  const AFormat: String);
begin
  if AFormat='' then
     IStrings.Add(Text+': '+UIntToStr(Value))
  else
     IStrings.Add(Text+': '+FormatFloat(AFormat,Value));
end;

procedure TInspectDelphi.AddDate(const Text: String; const Value: TDate);
begin
  IStrings.Add(Text+': '+DateToStr(Value));
end;

{$IF CompilerVersion>=28}
function DeviceClassToStr(const AClass:TDeviceInfo.TDeviceClass):String;
begin
  case AClass of
    TDeviceInfo.TDeviceClass.Unknown: result:='Unknown';
    TDeviceInfo.TDeviceClass.Desktop: result:='Desktop';
    TDeviceInfo.TDeviceClass.Phone: result:='Phone';
    TDeviceInfo.TDeviceClass.MediaPlayer: result:='Media Player';
    TDeviceInfo.TDeviceClass.Tablet: result:='Tablet';
    TDeviceInfo.TDeviceClass.Automotive: result:='Automotive';
    TDeviceInfo.TDeviceClass.Industrial: result:='Industrial';
    TDeviceInfo.TDeviceClass.Embedded: result:='Embedded';
    TDeviceInfo.TDeviceClass.Watch: result:='Watch';
    TDeviceInfo.TDeviceClass.Glasses: result:='Glasses';
    TDeviceInfo.TDeviceClass.Elf: result:='Elf';
    TDeviceInfo.TDeviceClass.Dwarf: result:='Dwarf';
  else
    {TDeviceInfo.TDeviceClass.Wizard} result:='Wizard';
  end;
end;
{$ENDIF}

function TInspectDelphi.BidiMode:String;
begin
  case Application.BiDiMode of
    bdLeftToRight: result:='Left to Right';
    bdRightToLeft: result:='Right to Left';
    bdRightToLeftNoAlign: result:='Right to Left No Align';
    bdRightToLeftReadingOnly: result:='Right to Left Reading Only';
  else
    result:='(Unknown)';
  end;
end;

procedure TInspectDelphi.DoFireMonkey;

  procedure AddScrollingBehaviors(const ABehaviors:TScrollingBehaviours);
  begin
    if TScrollingBehaviour.BoundsAnimation in ABehaviors then
       Add('BoundsAnimation scrolling behavior');

    if TScrollingBehaviour.Animation in ABehaviors then
       Add('Animation scrolling behavior');

    if TScrollingBehaviour.TouchTracking in ABehaviors then
       Add('TouchTracking scrolling behavior');

    if TScrollingBehaviour.AutoShowing in ABehaviors then
       Add('AutoShowing scrolling behavior');
  end;

  procedure AddOrientations(const AOrientations:TScreenOrientations);
  begin
    if TScreenOrientation.{$IF FireMonkeyVersion>18}Portrait{$ELSE}soPortrait{$ENDIF} in AOrientations then
       Add('Portrait Orientation');

    if TScreenOrientation.{$IF FireMonkeyVersion>18}Landscape{$ELSE}soLandscape{$ENDIF} in AOrientations then
       Add('Landscape Orientation');

    if TScreenOrientation.{$IF FireMonkeyVersion>18}InvertedPortrait{$ELSE}soInvertedPortrait{$ENDIF} in AOrientations then
       Add('Inverted Portrait Orientation');

    if TScreenOrientation.{$IF FireMonkeyVersion>18}InvertedLandscape{$ELSE}soInvertedLandscape{$ENDIF} in AOrientations then
       Add('Inverted Landscape Orientation');
  end;

  procedure AddFilters;
  var Categories,
      Filters : TStrings;
      t,tt : Integer;
  begin
    AddHeader('Filters');

    Categories:=TStringList.Create;
    try
      TFilterManager.FillCategory(Categories);

      for t := 0 to Categories.Count-1 do
      begin
        AddHeader(' '+Categories[t]);

        Filters:=TStringList.Create;
        try
          TFilterManager.FillFiltersInCategory(Categories[t],Filters);

          for tt := 0 to Filters.Count-1 do
              Add('  '+Filters[tt]);
        finally
          Filters.Free;
        end;
      end;
    finally
      Categories.Free;
    end;
  end;

var
  ScreenService: IFMXScreenService;
  tmpStyle : TStyleDescription;
  DeviceSrv: IFMXDeviceService;

  LocaleService: IFMXLocaleService;
  TextService: IFMXTextService;

  tmpCanvas : TCanvas;

  {$IF FireMonkeyVersion>20}
  DeviceBehavior: IDeviceBehavior;
  DisplayMetrics: TDeviceDisplayMetrics;
  SaveService : IFMXSaveStateService;
  {$ENDIF}

  InfoService : IFMXSystemInformationService;

  FontService : IFMXSystemFontService;

  LoggingService : IFMXLoggingService;
  ListingService : IFMXListingService;

  g : TCustomGestureCollectionItem;

  t : Integer;

  tmpControl : TControl;
  tmpPrinter : String;

  {$IFDEF ANDROID}
  {$IF FireMonkeyVersion>=21}
  tmpTheme : String;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF IOS}
  tmpDevice : UIDevice;
  {$ENDIF}

  tmpCapture : TCaptureDeviceManager;
  tmpVideo : TVideoCaptureDevice;

  {$IFDEF HASPRINTER}
  tmpHasPrinter : Boolean;
  {$ENDIF}

begin
  tmpControl:=nil;

  if Assigned(Screen.ActiveForm) then
     for t := 0 to Screen.ActiveForm.ChildrenCount-1 do
        if Screen.ActiveForm.Children[t] is TControl then
        begin
          tmpControl:=TControl(Screen.ActiveForm.Children[t]);
          break;
        end;

  {$IF Declared(FireMonkeyVersion)}
  Add('FireMonkey Version',FireMonkeyVersion);
  {$ENDIF}

  Add;
  Add('Title',Application.Title);
  Add('DefaultTitle',Application.DefaultTitle);

  Add('TeeChart Version',TeeMsg_Version);

  {$IFDEF D22}
  Add('Action Update Delay',Application.ActionUpdateDelay);
  {$ENDIF}

  {$IFDEF D22}
  Add('Analytics',Application.AnalyticsManager.TrackingEnabled);
  Add('Track Activity',Application.TrackActivity);
  {$ENDIF}

  {$IFDEF ANDROID}
  AddHeader('Android');

  {$IF FireMonkeyVersion<21}
  Add('GetApplicationTitle',GetApplicationTitle);
  Add('GetPackagePath',GetPackagePath);
  {$ENDIF}

  {$IF FireMonkeyVersion>=21}
  if GetNativeTheme=TJAlertDialog.JavaClass.THEME_HOLO_LIGHT then
     tmpTheme:='Holo Light'
  else
  if GetNativeTheme=TJAlertDialog.JavaClass.THEME_HOLO_DARK then
     tmpTheme:='Holo Dark'
  else
     tmpTheme:=IntToStr(GetNativeTheme);

  Add('GetNativeTheme',tmpTheme);
  {$ENDIF}

  {$IFNDEF D28}
  Add('Is Gingerbread Device',IsGingerbreadDevice);
  {$ENDIF}

  AddHeader('Android Device');

  Add('Radio Version',JStringToString(TJBuild.JavaClass.getRadioVersion));
  Add('Board',JStringToString(TJBuild.JavaClass.BOARD));
  Add('Bootloader',JStringToString(TJBuild.JavaClass.BOOTLOADER));
  Add('Brand',JStringToString(TJBuild.JavaClass.BRAND));
  Add('CPU_ABI',JStringToString(TJBuild.JavaClass.CPU_ABI));
  Add('CPU_ABI2',JStringToString(TJBuild.JavaClass.CPU_ABI2));
  Add('Device',JStringToString(TJBuild.JavaClass.DEVICE));
  Add('Display',JStringToString(TJBuild.JavaClass.DISPLAY));
  Add('Fingerprint',JStringToString(TJBuild.JavaClass.FINGERPRINT));
  Add('Hardware',JStringToString(TJBuild.JavaClass.HARDWARE));

  Add('Host',JStringToString(TJBuild.JavaClass.HOST));
  Add('ID',JStringToString(TJBuild.JavaClass.ID));
  Add('Manufacturer',JStringToString(TJBuild.JavaClass.MANUFACTURER));
  Add('Model',JStringToString(TJBuild.JavaClass.MODEL));
  Add('Product',JStringToString(TJBuild.JavaClass.PRODUCT));
  Add('Radio',JStringToString(TJBuild.JavaClass.RADIO));
  Add('Serial',JStringToString(TJBuild.JavaClass.SERIAL));
  Add('Tags',JStringToString(TJBuild.JavaClass.TAGS));
  Add('Time',TJBuild.JavaClass.TIME);
  Add('Type',JStringToString(TJBuild.JavaClass.&TYPE));
  Add('Unknown',JStringToString(TJBuild.JavaClass.UNKNOWN));
  Add('User',JStringToString(TJBuild.JavaClass.USER));

  Add;
  {$ENDIF}

  {$IFDEF IOS}
  AddHeader('iOS');
  Add('IsPhone',IsPhone);
  Add('IsPad',IsPad);

  tmpDevice:=TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);

  AddHeader('iOS Device');

  Add('Model',UTF8ToString(tmpDevice.model.UTF8String));
  Add('Name',UTF8ToString(tmpDevice.name.UTF8String));
  Add('Battery Level',tmpDevice.batteryLevel);
  Add('Multitasking Supported',tmpDevice.isMultitaskingSupported);
  Add('Proximity Monitoring Enabled',tmpDevice.isProximityMonitoringEnabled);
  Add('System Name',UTF8ToString(tmpDevice.systemName.UTF8String));
  Add('System Version',UTF8ToString(tmpDevice.systemVersion.UTF8String));
  Add('Unique Identifier',UTF8ToString(tmpDevice.uniqueIdentifier.UTF8String));
  Add('Identifier for Vendor',UTF8ToString(tmpDevice.identifierForVendor.UUIDString.UTF8String));

  Add;
  {$ENDIF}

  {$IFDEF MACOSX}
  AddHeader('Mac OSX');
  Add('GetTimeZone',GetTimeZone);
  Add;
  {$ENDIF}

  // (Android & iOS only)
  if TPlatformServices.Current.SupportsPlatformService(IFMXDeviceService, IInterface(DeviceSrv)) then
  begin
    AddHeader('IFMXDeviceService');

    {$IF FireMonkeyVersion>20}
    Add('Has Touchscreen',(TDeviceFeature.HasTouchScreen in DeviceSrv.GetFeatures));

    {$IF CompilerVersion>=28}
    Add('Device Class',DeviceClassToStr(DeviceSrv.GetDeviceClass));
    {$ELSE}
    case DeviceSrv.GetKind of
      TDeviceKind.Desktop: Add('Kind','Desktop Device');
      TDeviceKind.iPhone: Add('Kind','iPhone Device');
    else
       Add('iPad Device');
    end;
    {$ENDIF}

    {$ENDIF}

    Add('Model',DeviceSrv.GetModel);

    if TDeviceFeature.HasTouchScreen in DeviceSrv.GetFeatures then
       Add('Has TouchScren',True);

    Add;
  end;

  {$IF FireMonkeyVersion>20}
  if Assigned(tmpControl) and
     TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior,DeviceBehavior, tmpControl) then
  begin
    AddHeader('IDeviceBehavior');

    DisplayMetrics:=DeviceBehavior.GetDisplayMetrics(tmpControl);

    Add('Physical Screen size',DisplayMetrics.PhysicalScreenSize);
    Add('Logical Screen size',DisplayMetrics.LogicalScreenSize);
    Add('Raw Screen size',DisplayMetrics.RawScreenSize);
    Add('Aspect Ratio',DisplayMetrics.AspectRatio);
    Add('Pixels per inch',DisplayMetrics.PixelsPerInch);
    Add('Screen Scale',DisplayMetrics.ScreenScale);
    Add('Font Scale',DisplayMetrics.FontScale);
  end;
  {$ENDIF}

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenService)) then
  begin
    AddHeader('IFMXScreenService');

    case ScreenService.GetScreenOrientation of
      TScreenOrientation.{$IF FireMonkeyVersion>18}Portrait{$ELSE}soPortrait{$ENDIF}: Add('Screen Orientation','Portrait');
      TScreenOrientation.{$IF FireMonkeyVersion>18}Landscape{$ELSE}soLandscape{$ENDIF}: Add('Screen Orientation','Landscape');
      TScreenOrientation.{$IF FireMonkeyVersion>18}InvertedPortrait{$ELSE}soInvertedPortrait{$ENDIF}: Add('Screen Orientation','Inverted Portrait');
    else
     Add('Screen Orientation','Inverted Landscape');
    end;
  end;

  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(LocaleService)) then
  begin
    AddHeader('IFMXLocaleService');

    Add('Current LangID',LocaleService.GetCurrentLangID);

    {$IFDEF D22}
    Add('Locale First Day Of Week',LocaleService.GetFirstWeekday);
    {$ELSE}
    Add('Locale First Day Of Week',LocaleService.GetLocaleFirstDayOfWeek);
    {$ENDIF}
  end;

  if TPlatformServices.Current.SupportsPlatformService(IFMXTextService, IInterface(TextService)) then
  begin
    AddHeader('IFMXTextService');

    Add('Text Service Class',TextService.GetTextServiceClass.ClassName);
  end;

  {$IF FireMonkeyVersion>20}
  if TPlatformServices.Current.SupportsPlatformService(IFMXSaveStateService, IInterface(SaveService)) then
  begin
    AddHeader('IFMXSaveStateService');

    Add('Storage Path',SaveService.GetStoragePath);
  end;
  {$ENDIF}

  if TPlatformServices.Current.SupportsPlatformService(IFMXSystemInformationService, IInterface(InfoService)) then
  begin
    AddHeader('IFMXSystemInformationService');

    //function GetScrollingBehaviour: TScrollingBehaviours;
    Add('Min Scroll Thumb Size',InfoService.GetMinScrollThumbSize);
    Add('Caret Width',InfoService.GetCaretWidth);

    {$IF FireMonkeyVersion>18}
    Add('Menu Show Delay',InfoService.GetMenuShowDelay);
    {$ENDIF}

    {$IF FireMonkeyVersion>20}
    AddScrollingBehaviors(InfoService.GetScrollingBehaviour);
    {$ENDIF}
  end;

  if TPlatformServices.Current.SupportsPlatformService(IFMXSystemFontService, IInterface(FontService)) then
  begin
    AddHeader('IFMXSystemFontService');

    Add('Default Font Family',FontService.GetDefaultFontFamilyName);

    {$IF FireMonkeyVersion>18}
    Add('Default Font Size',FontService.GetDefaultFontSize);
    {$ENDIF}
  end;

  Add('Bidi Mode',BidiMode);

  Add('FormFactor Width',Application.FormFactor.Width);
  Add('FormFactor Height',Application.FormFactor.Height);

  Add('Orientations');
  AddOrientations(Application.FormFactor.Orientations);

  AddHeader('Application FormFactor Devices');

  if TDeviceKind.{$IF FireMonkeyVersion>18}Desktop{$ELSE}dkDesktop{$ENDIF} in Application.FormFactor.Devices then
     Add('Desktop Device');

  if TDeviceKind.{$IF FireMonkeyVersion>18}iPhone{$ELSE}dkiPhone{$ENDIF} in Application.FormFactor.Devices then
     Add('iPhone Device');

  if TDeviceKind.{$IF FireMonkeyVersion>18}iPad{$ELSE}dkiPad{$ENDIF} in Application.FormFactor.Devices then
     Add('iPad Device');

  Add;

  Add('MousePos.X',Screen.MousePos.X);
  Add('MousePos.Y',Screen.MousePos.Y);

  AddHeader('Screen');

  Add('Size',Screen.Size);

  {$IF FireMonkeyVersion>20}
  Add('Width',Screen.Width);
  Add('Height',Screen.Height);
  Add('Multi Display Supported',Screen.MultiDisplaySupported);
  Add('DisplayCount',Screen.DisplayCount);
  Add('Desktop Rect',Screen.DesktopRect);
  Add('WorkArea Rect',Screen.WorkAreaRect);
  {$ENDIF}

  Add('DataModuleCount',Screen.DataModuleCount);
  Add('FormCount',Screen.FormCount);

  {$IF FireMonkeyVersion>18}
  Add('PopupFormCount',Screen.PopupFormCount);
  {$ENDIF}

  Add('PopupCount',GetPopupCount);

  {$IF FireMonkeyVersion>20}
  Add('Default Animation Frame Rate',TAnimation.DefaultAniFrameRate);
  Add('Animation Frame Rate',TAnimation.AniFrameRate);

  if Assigned(TAnimation.AniThread) then
     Add('Animation Thread Timer Interval',TAnimation.AniThread.Interval);
  {$ENDIF}

  AddHeader('Canvas');

  tmpCanvas:=TCanvasManager.DefaultCanvas.Create;

  try
    Add('Class',tmpCanvas.ClassName);

    if TCanvasManager.DefaultPrinterCanvas=nil then
       tmpPrinter:='(n/a)'
    else
       tmpPrinter:=TCanvasManager.DefaultPrinterCanvas.ClassName;

    Add('Printer Class',tmpPrinter);
    Add('Scale',tmpCanvas.Scale);
    Add('Is Scale Integer',tmpCanvas.IsScaleInteger);
    Add('Blending',tmpCanvas.Blending);

    Add('MaxAllowedBitmapSize',tmpCanvas.MaxAllowedBitmapSize);

    case tmpCanvas.Quality of
      TCanvasQuality.{$IF FireMonkeyVersion>18}SystemDefault{$ELSE}ccSystemDefault{$ENDIF}: Add('Quality','System Default');
      TCanvasQuality.{$IF FireMonkeyVersion>18}HighPerformance{$ELSE}ccHighPerformance{$ENDIF}:  Add('Quality','High Performance');
    else
      Add('Quality','High Quality');
    end;

    if TCanvasStyle.NeedGPUSurface in tmpCanvas.GetCanvasStyle then
       Add('Need GPU Surface');

    if TCanvasStyle.SupportClipRects in tmpCanvas.GetCanvasStyle then
       Add('Support Clip Rects');

    {$IF FireMonkeyVersion>18}
    if TCanvasStyle.SupportModulation in tmpCanvas.GetCanvasStyle then
       Add('Support Modulation');
    {$ENDIF}

  finally
    tmpCanvas.Free;
  end;

  Add;

  tmpCapture:=TCaptureDeviceManager.Current;

  if Assigned(tmpCapture) then
  begin
    AddHeader('Capture Devices');
    Add('Count',tmpCapture.Count);

    for t := 0 to tmpCapture.Count-1 do
    begin
     Add;
     Add('Descrption',tmpCapture.Devices[t].Description);
     Add('Name',tmpCapture.Devices[t].Name);
     Add('Unique ID',tmpCapture.Devices[t].UniqueID);
     Add('Is Default',tmpCapture.Devices[t].IsDefault);

     case tmpCapture.Devices[t].MediaType of
       TMediaType.Audio: Add('Type','Audio');
       TMediaType.Video: Add('Type','Video');
     else
       Add('Type','(unknown)');
     end;

     tmpVideo:=tmpCapture.DefaultVideoCaptureDevice;

     if Assigned(tmpVideo) then
     begin
       AddHeader('Default Video Capture Device');

       Add('Has Flash',tmpVideo.HasFlash);
       Add('Has Torch',tmpVideo.HasTorch);

       case tmpVideo.Quality of
         TVideoCaptureQuality.PhotoQuality: Add('Quality','Photo');
         TVideoCaptureQuality.HighQuality: Add('Quality','High');
         TVideoCaptureQuality.MediumQuality: Add('Quality','Medium');
       else
         // LowQuality
          Add('Quality','Low');
       end;

       case tmpVideo.FocusMode of
         TFocusMode.AutoFocus: Add('Focus Mode','Auto Focus');
         TFocusMode.ContinuousAutoFocus: Add('Focus Mode','Continuous Auto Focus');
       else
         // TFocusMode.Locked:
         Add('Focus Mode','Locked');
       end;

       case tmpVideo.Position of
         TDevicePosition.Unspecified: Add('Position','Unspecified');
         TDevicePosition.Front: Add('Position','Front');
       else
         //TDevicePosition.Back:
         Add('Position','Back');
       end;

       case tmpVideo.FlashMode of
         TFlashMode.AutoFlash: Add('Flash Mode','Auto Flash');
         TFlashMode.FlashOff: Add('Flash Mode','Off');
       else
         // TFlashMode.FlashOn:
         Add('Flash Mode','On');
       end;

       case tmpVideo.TorchMode of
         TTorchMode.ModeOff: Add('Torch Mode','Off');
         TTorchMode.ModeOn: Add('Torch Mode','On');
       else
         // TTorchMode.ModeAuto:
         Add('Torch Mode','Auto');
       end;
     end;
    end;

    Add;
  end
  else
    Add('No Capture Manager');

  Add('Text Layout Class',TTextLayoutManager.DefaultTextLayout.ClassName);
  Add('Media Codec Manager File Types',TMediaCodecManager.GetFileTypes);
  Add('Context Class',TContextManager.DefaultContextClass.ClassName);
  Add('Context Count',TContextManager.ContextCount);
  Add('Capture Device Count',TCaptureDeviceManager.Current.Count);

  AddHeader('Gestures');
  Add('Sensitivity',TGestureEngine.Sensitivity);
  Add;

  for g in TGestureManager.RegisteredGestures do
     Add(g.Name);

  Add;
  Add('GlobalUseHWEffects',GlobalUseHWEffects); // deprecated
  Add('GlobalDisableFocusEffect',GlobalDisableFocusEffect);

  {$IF FireMonkeyVersion>20}
  Add('GlobalUseDX',GlobalUseDX);
  Add('GlobalUseDXInDX9Mode',GlobalUseDXInDX9Mode);
  Add('GlobalUseDXSoftware',GlobalUseDXSoftware);
  {$ENDIF}

  Add('GlobalUseDirect2D',GlobalUseDirect2D);
  Add('GlobalUseGDIPlusClearType',GlobalUseGDIPlusClearType);
  Add('DigitRoundSize',DigitRoundSize);
  Add('GlobalUseGPUCanvas',GlobalUseGPUCanvas);

  {$IF FireMonkeyVersion<=20}
  Add('GlobalUseDX10',GlobalUseDX10);
  Add('GlobalUseDX10Software',GlobalUseDX10Software);
  {$ENDIF}

  Add;
  Add('Bitmap Codecs');
  Add(TBitmapCodecManager.GetFileTypes);

  {$IFDEF HASPRINTER}
  AddHeader('Printer');
  Add('Printer Class',PrinterClass.ClassName);

  try
    tmpHasPrinter:=(Printer<>nil) and (Printer.ActivePrinter<>nil);
  except
    // This is mainly for Mac OSX:
    on Exception do
       tmpHasPrinter:=False;
  end;

  if tmpHasPrinter then
  begin
    Add('Count',Printer.Count);
    Add('Page Width',Printer.PageWidth);
    Add('Page Height',Printer.PageHeight);

    AddHeader('Active Printer');
    Add('Device',Printer.ActivePrinter.Device);
    Add('Driver',Printer.ActivePrinter.Driver);
    Add('Port',Printer.ActivePrinter.Port);
    Add('Title',Printer.ActivePrinter.Title);
  end
  else
    Add('No Printer Assigned');

  Add;
  {$ENDIF}

  if Assigned(tmpControl) then
  begin
    tmpStyle:=TStyleManager.GetStyleDescriptionForControl(tmpControl);

    if Assigned(tmpStyle) then
    begin
      AddHeader('Active Style');
      Add('Class',TStyleManager.ActiveStyle{$IF FireMonkeyVersion>18}(tmpControl){$ENDIF}.ClassName);

      Add('Author',tmpStyle.Author);
      Add('Author Email',tmpStyle.AuthorEMail);
      Add('Author URL',tmpStyle.AuthorURL);
      Add('Platform Target',tmpStyle.PlatformTarget);
      Add('Mobile Platform',tmpStyle.MobilePlatform);

      {$IF FireMonkeyVersion>18}
      Add('Title',tmpStyle.Title);
      {$ELSE}
      Add('Retina Display',tmpStyle.RetinaDisplay);
      Add('Style Name',tmpStyle.StyleName);
      {$ENDIF}

      Add('Version',tmpStyle.Version);
    end;
  end;

  AddHeader('Styles');

  {$IF FireMonkeyVersion<21}
  for s in TStyleManager.StyleNames do
      Add(s);
  {$ELSE}
  TStyleManager.EnumStyleResources(procedure(const AResourceName: string; const APlatform: TOSPlatform)
  begin
    Add(AResourceName {+' '+APlatform.ToString});
  end);
  {$ENDIF}

  Add;

  AddFilters;

  if TPlatformServices.Current.SupportsPlatformService(IFMXLoggingService, IInterface(LoggingService)) then
     AddHeader('IFMXLoggingService');

  if TPlatformServices.Current.SupportsPlatformService(IFMXListingService, IInterface(ListingService)) then
  begin
    AddHeader('IFMXListingService');

    //TListingHeaderBehavior = (Sticky);
    //TListingSearchFeature = (StayOnTop, AsFirstItem);
    //TListingTransitionFeature = (EditMode, DeleteButtonSlide, PullToRefresh, ScrollGlow);
    //TListingEditModeFeature = (Delete);
  end;
end;

procedure TInspectDelphi.DoOS;
begin
  {$IFDEF MSWINDOWS}
  AddHeader('Disk');

  Add('Disk Size',DiskSize(0),'#,###');
  Add('Disk Free',DiskFree(0),'#,###');
  {$ENDIF}

//  EnumModules
//  LibModuleList
//  Add('UnitCount',GetPackageInfoTable.UnitCount);

  AddHeader('TOSVersion');

  Add('ToString',TOSVersion.ToString);

  case TOSVersion.Architecture of
    arIntelX86: Add('Architecture','Intel x86');
    arIntelX64: Add('Architecture','Intel x64');
       arARM32: Add('Architecture','ARM 32');
       arARM64: Add('Architecture','ARM 64');
  else
   Add('Architecture','(Unknown)');
  end;

  Add('Build',TOSVersion.Build);
  Add('Major',TOSVersion.Major);
  Add('Minor',TOSVersion.Minor);
  Add('Name',TOSVersion.Name);

  case TOSVersion.Platform of
    pfWindows: Add('Platform','Windows');
    pfMacOS:   Add('Platform','MacOS');
    pfiOS:     Add('Platform','iOS');
    pfAndroid: Add('Platform','Android');
    pfWinRT:   Add('Platform','WinRT');
    pfLinux:   Add('Platform','Linux');
  else
    Add('Platform','(Unknown)');
  end;

  Add('ServicePackMajor',TOSVersion.ServicePackMajor);
  Add('ServicePackMinor',TOSVersion.ServicePackMinor);

  {$IFDEF MSWINDOWS}
  Add;

  case Win32Platform of
    VER_PLATFORM_WIN32s: Add('Win32 Platform','Win32s');
    VER_PLATFORM_WIN32_WINDOWS: Add('Win32 Platform','Win32 Windows');
    VER_PLATFORM_WIN32_NT: Add('Win32 Platform','Win32 NT');
  else
    //VER_PLATFORM_WIN32_CE:
    Add('Win32 Platform','Win32 CE');
  end;
  {$ENDIF}

  Add;

  Add('Max Path Length',MAX_PATH);

  Add;
end;

procedure TInspectDelphi.DoHardware;

  procedure AddSensor(const ASensor:TCustomSensor);
  var c : String;
  begin
    Add;
    Add('Name',ASensor.Name);
    Add('Description',ASensor.Description);

    case ASensor.Category of
      TSensorCategory.Location: c:='Location';
      TSensorCategory.Environmental: c:='Environmental';
      TSensorCategory.Motion: c:='Motion';
      TSensorCategory.Orientation: c:='Orientation';
      TSensorCategory.Mechanical: c:='Mechanical';
      TSensorCategory.Electrical: c:='Electrical';
      TSensorCategory.Biometric: c:='Biometric';
      TSensorCategory.Light: c:='Light';
      TSensorCategory.Scanner: c:='Scanner';
    else
      c:='(Unknown)';
    end;

    Add('Category',c);

    Add('Manufacturer',ASensor.Manufacturer);
    Add('Model',ASensor.Model);
    Add('Serial No',ASensor.SerialNo);
    Add('Unique ID',ASensor.UniqueID);
  end;

  {$IF FireMonkeyVersion>20}
  procedure AddDevice(DeviceNum:Integer; const ADevice:TDeviceInfo);

    function PlatformToStr(const APlatform:TOSVersion.TPlatform):String;
    begin
      case APlatform of
        pfWindows: result:='Windows';
        pfMacOS: result:='Mac OS';
        pfiOS: result:='iOS';
        pfAndroid: result:='Android';
        pfWinRT: result:='WinRT';
        pfLinux: result:='Linux';
      else
        result:='(Unknown)';
      end;
    end;

  begin
    Add;

    Add('Number',DeviceNum.ToString);
    Add('ID',ADevice.ID);
    Add('Class',DeviceClassToStr(ADevice.DeviceClass));
    Add('Min Physical Screen Size',ADevice.MinPhysicalScreenSize);
    Add('Min Logical Screen Size',ADevice.MinLogicalScreenSize);
    Add('Max Physical Screen Size',ADevice.MaxPhysicalScreenSize);
    Add('Max Logical Screen Size',ADevice.MaxLogicalScreenSize);
    Add('Aspect Ratio',ADevice.AspectRatio);
    Add('Pixels Per Inch',ADevice.PixelsPerInch);
    Add('Max Diagonal',ADevice.MaxDiagonal);
    Add('Min Diagonal',ADevice.MinDiagonal);
    Add('Platform',PlatformToStr(ADevice.Platform));

    //Add('',ADevice.Attributes); ??

  end;
  {$ENDIF}

var t: Integer;
begin
  AddHeader('Tethering');

  Add('Adapters',TTetheringAdapters.Adapters);
  Add('Protocols',TTetheringProtocols.Protocols);
  Add('Profiles',TTetheringProfiles.Profiles);

  AddHeader('Sensors');

  TSensorManager.Current.Activate;

  Add('Count',TSensorManager.Current.Count);

  for t := 0 to TSensorManager.Current.Count-1 do
      AddSensor(TSensorManager.Current.Sensors[t]);

  {$IF FireMonkeyVersion>20}
  AddHeader('Devices');
  Add('Device Count',TDeviceInfo.DeviceCount);

  for t := 0 to TDeviceInfo.DeviceCount-1 do
      AddDevice(t,TDeviceInfo.Devices[t]);
  {$ENDIF}

  Add;


  // Bluetooth

  var Manager:=TBluetoothManager.Current;

  if Manager=nil then
     AddHeader('Bluetooth not enabled')
  else
  begin
    AddHeader('Bluetooth');

    var Devices:=Manager.LastDiscoveredDevices;

    Add('Device Count',Devices.Count.ToString);

    for var d:=0 to Devices.Count-1 do
        Add('#'+d.ToString,Devices[d].DeviceName);
  end;
end;

procedure TInspectDelphi.DoLocale;
var t : Integer;
begin
  Add('Languages Count',Languages.Count);
  Add('User Default Locale', TLanguages.UserDefaultLocale);
  Add('Default Locale Name',Languages.NameFromLocaleID[TLanguages.UserDefaultLocale]);

  AddHeader('SysLocale');

  Add('DefaultLCID',SysLocale.DefaultLCID);
  Add('PriLangID',SysLocale.PriLangID);
  Add('SubLangID',SysLocale.SubLangID);
  Add('FarEast',SysLocale.FarEast);
  Add('MiddleEast',SysLocale.MiddleEast);

  {$IFDEF MSWINDOWS}
  Add;
  Add('Default Fallback Languages',GetDefaultFallbackLanguages);
  Add('Preferred UI Languages',PreferredUILanguages);
  {$ENDIF}

  AddHeader('Format Settings');

  Add('Currency String',FormatSettings.CurrencyString);
  Add('Currency Format',FormatSettings.CurrencyFormat);
  Add('Currency Decimals',FormatSettings.CurrencyDecimals);
  Add('Date Separator',FormatSettings.DateSeparator);
  Add('Time Separator',FormatSettings.TimeSeparator);
  Add('List Separator',FormatSettings.ListSeparator);
  Add('Short DateFormat',FormatSettings.ShortDateFormat);
  Add('Long DateFormat',FormatSettings.LongDateFormat);
  Add('Time AM String',FormatSettings.TimeAMString);
  Add('Time PM String',FormatSettings.TimePMString);
  Add('Short Time Format',FormatSettings.ShortTimeFormat);
  Add('Long Time Format',FormatSettings.LongTimeFormat);
  Add('Short Month Names',FormatSettings.ShortMonthNames);
  Add('Long Month Names',FormatSettings.LongMonthNames);
  Add('Short Day Names',FormatSettings.ShortDayNames);
  Add('Long Day Names',FormatSettings.LongDayNames);

  if Length(FormatSettings.EraInfo)>0 then
  begin
    AddHeader('EraInfo');

    for t := 0 to High(FormatSettings.EraInfo) do
    begin
      Add(t.ToString);
      Add('Name',FormatSettings.EraInfo[t].EraName);
      Add('Offset',FormatSettings.EraInfo[t].EraOffset);
      AddDate('Start',FormatSettings.EraInfo[t].EraStart);
      AddDate('End',FormatSettings.EraInfo[t].EraEnd);
      Add;
    end;
  end;
  //EraInfo: array of TEraInfo;

  Add('Thousand Separator',FormatSettings.ThousandSeparator);
  Add('Decimal Separator',FormatSettings.DecimalSeparator);
  Add('Two Digit Year Century Window',FormatSettings.TwoDigitYearCenturyWindow);
  Add('Neg Curr Format',FormatSettings.NegCurrFormat);
  Add('Normalized Locale Name',FormatSettings.NormalizedLocaleName);

  Add;
  Add('Time Zone',TTimeZone.Local.DisplayName);
  Add('Time Zone Abbreviation',TTimeZone.Local.Abbreviation);
  Add('Time Zone UTC Offset',''+TTimeZone.Local.UtcOffset);
  Add('Time Zone ID',TTimeZone.Local.ID);

  Add;
  DoEncoding; // <-- move out from here, to a separate AddEncoding ?

  Add;
end;

procedure TInspectDelphi.DoPaths;
begin
  Add('Temp',System.IOUtils.TPath.GetTempPath);
  Add('Home',System.IOUtils.TPath.GetHomePath);
  Add('Documents',System.IOUtils.TPath.GetDocumentsPath);
  Add('Library',System.IOUtils.TPath.GetLibraryPath);

  {$IF RTLVersion>25}
  Add('Cache',System.IOUtils.TPath.GetCachePath);
  Add('Public',System.IOUtils.TPath.GetPublicPath);
  Add('Pictures',System.IOUtils.TPath.GetPicturesPath);
  Add('Camera',System.IOUtils.TPath.GetCameraPath);
  Add('Music',System.IOUtils.TPath.GetMusicPath);
  Add('Movies',System.IOUtils.TPath.GetMoviesPath);
  Add('Alarms',System.IOUtils.TPath.GetAlarmsPath);
  Add('Downloads',System.IOUtils.TPath.GetDownloadsPath);
  Add('Rightones',System.IOUtils.TPath.GetRingtonesPath);
  {$ENDIF}

  {$IF RTLVersion>25}
  AddHeader('Shared Paths');

  Add('Documents',System.IOUtils.TPath.GetSharedDocumentsPath);
  Add('Pictures',System.IOUtils.TPath.GetSharedPicturesPath);
  Add('Camera',System.IOUtils.TPath.GetSharedCameraPath);
  Add('Music',System.IOUtils.TPath.GetSharedMusicPath);
  Add('Movies',System.IOUtils.TPath.GetSharedMoviesPath);
  Add('Alarms',System.IOUtils.TPath.GetSharedAlarmsPath);
  Add('Downloads',System.IOUtils.TPath.GetSharedDownloadsPath);
  Add('Ringtones',System.IOUtils.TPath.GetSharedRingtonesPath);
  {$ENDIF}

  AddHeader('Separators');

  Add('Extension',System.IOUtils.TPath.ExtensionSeparatorChar);
  Add('Alt Directory',System.IOUtils.TPath.AltDirectorySeparatorChar);
  Add('Directory',System.IOUtils.TPath.DirectorySeparatorChar);
  Add('Path',System.IOUtils.TPath.PathSeparator);
  Add('Volume',System.IOUtils.TPath.VolumeSeparatorChar);

  Add;
end;

procedure TInspectDelphi.DoMath;

  procedure AddMask(const AMask:TArithmeticExceptionMask; const AText:String);
  begin
    if exInvalidOp in AMask then
       Add(AText,'Invalid Op');

    if exDenormalized in AMask then
       Add(AText,'Denormalized');

    if exZeroDivide in AMask then
       Add(AText,'Zero Divide');

    if exOverflow in AMask then
       Add(AText,'Overflow');

    if exUnderflow in AMask then
       Add(AText,'Underflow');

    if exPrecision in AMask then
       Add(AText,'Precision');
  end;

begin
  AddHeader('SizeOf (bytes)');

  Add('Comp',SizeOf(Comp));
  Add('Currency',SizeOf(Currency));
  Add('Extended',SizeOf(Extended));
  Add('Double',SizeOf(Double));
  Add('Single',SizeOf(Single));
  Add('NativeUInt',SizeOf(NativeUInt));
  Add('NativeInt',SizeOf(NativeInt));
  Add('UInt64',SizeOf(UInt64));
  Add('Int64',SizeOf(Int64));
  Add('Cardinal',SizeOf(Cardinal));
  Add('LongInt',SizeOf(LongInt));
  Add('LongBool',SizeOf(LongBool));
  Add('Integer',SizeOf(Integer));
  Add('Word',SizeOf(Word));
  Add('WordBool',SizeOf(WordBool));
  Add('ShortInt',SizeOf(ShortInt));
  Add('SmallInt',SizeOf(SmallInt));
  Add('Byte',SizeOf(Byte));
  Add('ByteBool',SizeOf(ByteBool));
  Add('Boolean',SizeOf(Boolean));
  Add('Char',SizeOf(Char));
  Add('Date',SizeOf(TDate));
  Add('Time',SizeOf(TTime));
  Add('DateTime',SizeOf(TDateTime));

  {$IFNDEF ANDROID}
  {$IFNDEF IOS}
  Add('AnsiChar',SizeOf(AnsiChar));
  {$ENDIF}
  {$ENDIF}

  AddHeader('MinValue and MaxValue');

  Add('MaxLongint',MaxLongint);
  Add;

  Add('MinSingle',MinSingle);
  Add('MaxSingle',MaxSingle);
  Add('Epsilon Single',Single.Epsilon);
  Add;

  Add('MinDouble',MinDouble);
  Add('MaxDouble',MaxDouble);
  Add('Epsilon Double',Double.Epsilon);
  Add;

  Add('MinExtended',MinExtended);
  Add('MaxExtended',MaxExtended);
  Add('Epsilon Extended',Extended.Epsilon);
  Add;

  Add('MinComp',MinComp);
  Add('MaxComp',MaxComp);
  Add;

  Add('MinValue Byte',Byte.MinValue);
  Add('MaxValue Byte',Byte.MaxValue);
  Add('MinValue ShortInt',ShortInt.MinValue);
  Add('MaxValue ShortInt',ShortInt.MaxValue);
  Add('MinValue SmallInt',SmallInt.MinValue);
  Add('MaxValue SmallInt',SmallInt.MaxValue);
  Add('MinValue Word',Word.MinValue);
  Add('MaxValue Word',Word.MaxValue);
  Add('MinValue Integer',Integer.MinValue);
  Add('MaxValue Integer',Integer.MaxValue);
  Add('MinValue Cardinal',Cardinal.MinValue);
  Add('MaxValue Cardinal',Cardinal.MaxValue);

  // Bug: FMX.Media redefines TMediaTimeHelper (Int64)
  // https://quality.embarcadero.com/browse/RSB-209
  {$IFNDEF VER290}
  Add('MinValue Int64',Int64.MinValue);
  Add('MaxValue Int64',Int64.MaxValue);
  {$ENDIF}

  Add('MinValue UInt64',UInt64.MinValue);
  Add('MaxValue UInt64',UInt64.MaxValue);
  Add('MinValue NativeInt',NativeInt.MinValue);
  Add('MaxValue NativeInt',NativeInt.MaxValue);
  Add('MinValue NativeUInt',NativeUInt.MinValue);
  Add('MaxValue NativeUInt',NativeUInt.MaxValue);

  AddHeader('Boolean Strings');
  Add('True',Boolean.ToString(True,TUseBoolStrs.True));
  Add('False',Boolean.ToString(False,TUseBoolStrs.True));
  Add;

  Add;

  {$IFNDEF ANDROID}
  {$IFNDEF NEXTGEN}
  case GetFPURoundMode of
    rmNearest: Add('FPU Round Mode','Nearest');
    rmDown: Add('FPU Round Mode','Down');
    rmUp: Add('FPU Round Mode','Up');
  else
    Add('FPU Round Mode','Truncate');
  end;

  case GetSSERoundMode of
    rmNearest: Add('SSE Round Mode','Nearest');
    rmDown: Add('SSE Round Mode','Down');
    rmUp: Add('SSE Round Mode','Up');
  else
    Add('SSE Round Mode','Truncate');
  end;
  {$ENDIF !NEXTGEN}
  {$ENDIF !ANDROID}

  case GetRoundMode of
    rmNearest: Add('Round Mode','Nearest');
    rmDown: Add('Round Mode','Down');
    rmUp: Add('Round Mode','Up');
  else
    Add('Round Mode','Truncate');
  end;

  {$IFNDEF ANDROID}
  {$IFNDEF NEXTGEN}
  case GetPrecisionMode of
    pmSingle: Add('Precision Mode','Single');
    pmReserved: Add('Precision Mode','Reserved');
    pmDouble: Add('Precision Mode','Double');
  else
    Add('Precision Mode','Extended');
  end;

  AddMask(GetFPUExceptionMask,'FPU Exception Mask');
  Add;
  AddMask(GetSSEExceptionMask,'SSE Exception Mask');
  Add;
  {$ENDIF !NEXTGEN}
  {$ENDIF !ANDROID}

  AddMask(GetExceptionMask,'Exception Mask');

  Add;
end;

procedure TInspectDelphi.AddDBXDrivers;
var t,tt : Integer;
    f : TDBXConnectionFactory;
    s : TStrings;
    p : TDBXProperties;
begin
  f:=TDBXConnectionFactory.GetConnectionFactory;

  if f<>nil then
  begin
    AddHeader('DBX Connection Factory');

    s:=TStringList.Create;
    try
      f.GetDriverNames(s);

      AddHeader('Drivers');

      for t := 0 to s.Count-1 do
      begin
        Add('Driver: ',s[t]);
        Add;

        p:=f.GetDriverProperties(s[t]);

        if p<>nil then
        begin
          for tt := 0 to p.Properties.Count-1 do
              Add('',p.Properties[tt]);

          Add;
        end;
      end;
    finally
      s.Free;
    end;

    s:=TStringList.Create;
    try
      f.GetConnectionItems(s);

      AddHeader('Connections');

      for t := 0 to s.Count-1 do
          Add(s[t]);

    finally
      s.Free;
    end;
  end;
end;

procedure TInspectDelphi.DoDatabase;
var t: Integer;
begin
  {$IFNDEF NO_INTERBASE}
  AddHeader('Interbase Express');

  Add('Version',IBX_Version);
  {$ENDIF}

  AddHeader('FireDAC');

  Add('Version',C_FD_Version);
  Add('Copyright',C_FD_Copyright);
  Add('Full Product',C_FD_ProductFull);

  {$IF CompilerVersion<31}
  Add('Link',C_FD_Link);
  Add('Documentation Link',C_FD_FAQ_Link);
  //C_FD_Forums_Link = 'https://forums.embarcadero.com/forum.jspa?forumID=503';
  {$ELSE}

  Add('Product',C_FD_Product);
  {$ENDIF}

  Add('Tool Name',C_FD_ToolName);

  AddHeader('REST');

  Add('REST Client Version',RESTCLIENT_VERSION);

  AddHeader('Database');

  Add('Max String Size',dsMaxStringSize);

  AddHeader('Field Types');

  for t:= Ord(Low(FieldTypeNames)) to Ord(High(FieldTypeNames)) do
      Add(FieldTypeNames[TFieldType(t)]);

  AddHeader('Datasnap');

  AddHeader('Registry');

  Add('DBX Registry Key',TDBXRegistryKey);
  Add('DBX Registry Driver Value',TDBXRegistryDriverValue);
  Add('DBX Registry Connection Value',TDBXRegistryConnectionValue);

  Add('DBX Driver File',TDBXDriverFile);
  Add('DBX Connection File',TDBXConnectionFile);

  Add;

  AddDBXDrivers;
end;

procedure TInspectDelphi.AddHeader(const Text:String);
begin
  // Future: Replace with TListBoxHeaderItem

  if IStrings.Count>0 then
     Add;

  Add(Text);
  Add;
end;

end.


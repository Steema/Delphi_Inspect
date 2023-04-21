{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2014 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.Platform.iOS;

interface

{$SCOPEDENUMS ON}

uses
  Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.GLKit, FMX.Types, FMX.Platform, FMX.Graphics, FMX.Forms, FMX.MultiTouch.IOS;

const
  FMXStartChangeDeviceOrientation = 'FMXWillAnimateRotationToInterfaceOrientation';

type
  TiOSWindowHandle = class(TWindowHandle)
  private
    FHandle: TOCLocal;
    function GetView: UIView;
    function GetGLView: GLKView;
    function GetForm: TCommonCustomForm;
    function GetWnd: UIWindow;
  public
    constructor Create(const AHandle: TOCLocal);
    property View: UIView read GetView;
    property Wnd: UIWindow read GetWnd;
    property GLView: GLKView read GetGLView;
    property Form: TCommonCustomForm read GetForm;
    property Handle: TOCLocal read FHandle;
  end;

  TiOSOpenApplicationContext = class
  private
    FSourceApp: string;
    FURL: string;
    FContext: Pointer;
  public
    constructor Create(ASourceApp: string; AURL: string; AContext: Pointer);
    property SourceApp: string read FSourceApp;
    property URL: string read FURL;
    property Context: Pointer read FContext;
  end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TiOSWindowHandle;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

implementation

uses
  System.RTLConsts, System.Classes, System.SysUtils, System.Types, System.UITypes, System.TypInfo, System.SyncObjs,
  System.Rtti, System.Math, System.UIConsts, System.Generics.Collections, System.Messaging, System.IOUtils,
  System.Devices, Macapi.ObjCRuntime, Macapi.CoreFoundation, Macapi.Helpers, iOSapi.CocoaTypes, iOSapi.Foundation,
  iOSapi.CoreGraphics, iOSapi.CoreImage, iOSapi.OpenGLES, FMX.Consts, FMX.Controls, FMX.Dialogs, FMX.Menus,
  FMX.Canvas.GPU, FMX.Pickers, FMX.TextLayout, FMX.Text, FMX.Styles, FMX.VirtualKeyboard, FMX.Gestures, FMX.Layouts,
  FMX.StdCtrls, FMX.WebBrowser, FMX.BehaviorManager, FMX.Context.GLES, FMX.Forms3D, FMX.Graphics.iOS,
  FMX.Context.GLES.iOS, FMX.Controls.iOS, FMX.Gestures.iOS;

const
  DBL_TAP_DELAY = 0.3; // Sec, Duration between first and second tap (Apple recommend use this value)

type
{$M+}

  id = Pointer;
  SEL = Pointer;
  PUIApplication = Pointer;
  PNSDictionary = Pointer;

  TFMXAlertViewInputDelegate = class(TOCLocal, UIAlertViewDelegate)
  private
    [Weak] FParentList: TList<TFMXAlertViewInputDelegate>;
    FResults: array of Integer;
    FModal: Boolean;
    FModalResult: TModalResult;
    FValues: array of string;
    FInputCloseQueryProc: TInputCloseQueryProc;
    procedure DoDialogClosed;
    procedure DoReadAlertView(const alertView: UIAlertView);
    procedure DoDismiss(const alertView: UIAlertView);
    procedure SetParentList(const AList: TList<TFMXAlertViewInputDelegate>);
  public
    constructor Create(const AButtons: TMsgDlgButtons; const AInputCloseQueryProc: TInputCloseQueryProc);
    procedure alertView(alertView: UIAlertView; clickedButtonAtIndex: NSInteger); overload; cdecl;
    [MethodName('alertView:didDismissWithButtonIndex:')]
    procedure alertViewDidDismissWithButtonIndex(alertView: UIAlertView; didDismissWithButtonIndex: NSInteger); cdecl;
    procedure alertViewCancel(alertView: UIAlertView); cdecl;
    property ModalResult: TModalResult read FModalResult;
    property Modal: Boolean read FModal write FModal;
    property ParentList: TList<TFMXAlertViewInputDelegate> read FParentList write SetParentList;
  end;

  IFMXWakeHandler = interface(NSObject)
  ['{ECEC50FA-6A4A-4DAE-9B23-A59A7C2CACC1}']
    procedure DoCheckSynchronize; cdecl;
  end;

  TFMXWakeHandler = class(TOCLocal)
  protected
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure DoCheckSynchronize; cdecl;
  end;

  FMXViewController = interface(UIViewController)
    ['{FB1283E6-B1AB-419F-B331-160096B10C62}']
    function shouldAutorotateToInterfaceOrientation(AinterfaceOrientation: UIInterfaceOrientation): Boolean; cdecl;
    function shouldAutorotate: Boolean; cdecl;
    function supportedInterfaceOrientations: NSUInteger; cdecl;
    procedure didReceiveMemoryWarning; cdecl;
    procedure didRotateFromInterfaceOrientation(fromInterfaceOrientation: UIInterfaceOrientation); cdecl;
    procedure viewDidAppear(animated: Boolean); cdecl;
    procedure viewWillAppear(animated: Boolean); cdecl;
    procedure viewWillDisappear(animated: Boolean); cdecl;
    procedure viewDidLayoutSubviews; cdecl;
    procedure willAnimateRotationToInterfaceOrientation(toInterfaceOrientation: UIInterfaceOrientation; duration: NSTimeInterval); cdecl;
    function prefersStatusBarHidden: Boolean; cdecl;
    function preferredStatusBarStyle: UIStatusBarStyle; cdecl;
  end;

  TMultiDisplayIOS = class(TInterfacedObject, IFMXMultiDisplayService)
  private
    FDisplayCount: Integer;
    FWorkAreaRect: TRect;
    FDesktopRect: TRect;
    FDisplayList: TList<TDisplay>;
    function CGRectToRect(const ACGRect: CGRect): TRect;
    procedure UpdateDisplays;
    function FindDisplay(const screen: UIScreen): TDisplay;
  public
    procedure UpdateDisplayInformation;
    function GetDisplayCount: Integer;
    function GetWorkAreaRect: TRect;
    function GetDesktopRect: TRect;
    function GetDisplay(const Index: Integer): TDisplay;
    function DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
    function DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
  end;

  TFMXViewController = class(TOCLocal)
  protected
    FStatusBarVisible: Boolean;
    FStatusBarView: UIView;
    FStatusBarLuminance: Single;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure SetStatusBarVisible(const Value:Boolean);
  public
    constructor Create;
    function shouldAutorotateToInterfaceOrientation(AinterfaceOrientation: UIInterfaceOrientation): Boolean; cdecl;
    function shouldAutorotate: Boolean; cdecl;
    function supportedInterfaceOrientations: NSUInteger; cdecl;
    procedure didReceiveMemoryWarning; cdecl;
    procedure didRotateFromInterfaceOrientation(fromInterfaceOrientation: UIInterfaceOrientation); cdecl;
    procedure viewDidAppear(animated: Boolean); cdecl;
    procedure viewWillAppear(animated: Boolean); cdecl;
    procedure viewWillDisappear(animated: Boolean); cdecl;
    procedure viewDidLayoutSubviews; cdecl;
    procedure willAnimateRotationToInterfaceOrientation(toInterfaceOrientation: UIInterfaceOrientation; duration: NSTimeInterval); cdecl;
    { iOS 7 Only }
    function prefersStatusBarHidden: Boolean; cdecl;
    function preferredStatusBarStyle: UIStatusBarStyle; cdecl;
    procedure UpdateStatusBarFrame;
    procedure SetStatusBarBackgroundColor(const ABackgroundColor: TAlphaColor);
    property StatusBarVisible: Boolean read FStatusBarVisible write SetStatusBarVisible;
    property StatusBarView: UIView read FStatusBarView write FStatusBarView;
  end;

  FMXWindow = interface(UIWindow)
    ['{B0EB8A41-2F1D-43DF-9207-25E3ACE7E08A}']
  end;

  TFMXWindow = class(TOCLocal)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    Text: UITextField;
    RootView: UIView;
    RootViewController: TFMXViewController;
    constructor Create(const ABounds: NSRect); overload;
  end;

  TApplicationDelegate = class{(TOCLocal, UIApplicationDelegate)}
  private
    FMainWindow: TFMXWindow;
  public
    procedure application(Sender: UIApplication; didChangeStatusBarFrame: CGRect); overload; cdecl;
    procedure application(Sender: UIApplication; didChangeStatusBarOrientation: UIInterfaceOrientation); overload; cdecl;
    procedure application(Sender: UIApplication; didFailToRegisterForRemoteNotificationsWithError: NSError); overload; cdecl;
    function application(Sender: UIApplication; didFinishLaunchingWithOptions: NSDictionary): Boolean; overload; cdecl;
    procedure application(Sender: UIApplication; didReceiveLocalNotification: UILocalNotification); overload; cdecl;
//    procedure application(Sender: UIApplication; didReceiveRemoteNotification: NSDictionary); cdecl; overload;
    procedure application(Sender: UIApplication; didRegisterForRemoteNotificationsWithDeviceToken: NSData); overload; cdecl;
    function application(const openURL, sourceApplication: string; annotation: Pointer): Boolean; overload; cdecl;
//    procedure application(Sender: UIApplication; willChangeStatusBarFrame: CGRect); cdecl; overload;
    procedure application(const Sender: UIApplication; const willChangeStatusBarOrientation: UIInterfaceOrientation; const duration: NSTimeInterval); overload; cdecl;
    procedure applicationDidBecomeActive(const Sender: UIApplication); cdecl;
    procedure applicationDidEnterBackground(const Sender: UIApplication); cdecl;
//    procedure applicationDidFinishLaunching(Sender: UIApplication); cdecl;
    procedure applicationDidRegisterForRemoteNotificationsWithDeviceToken(Sender: UIApplication; AToken: NSData); cdecl;
    procedure applicationDidReceiveRemoteNotification(Sender: UIApplication; ANotification: NSDictionary); cdecl;
    procedure didFailToRegisterForRemoteNotificationsWithError(Sender: UIApplication; AError: NSError); cdecl;
    procedure applicationDidReceiveMemoryWarning(Sender: UIApplication); cdecl;
    procedure applicationProtectedDataDidBecomeAvailable(Sender: UIApplication); cdecl;
    procedure applicationProtectedDataWillBecomeUnavailable(Sender: UIApplication); cdecl;
    procedure applicationSignificantTimeChange(Sender: UIApplication); cdecl;
    procedure applicationWillEnterForeground(Sender: UIApplication); cdecl;
    procedure applicationWillResignActive(Sender: UIApplication); cdecl;
    procedure applicationWillTerminate(Sender: UIApplication); cdecl;
    procedure setWindow(window: UIWindow); cdecl;
    function window: UIWindow; cdecl;
    property MainWindow: TFMXWindow read FMainWindow;
  end;

  { TPlatformCocoaTouch }

  TPlatformCocoaTouch = class(TInterfacedObject, IFMXApplicationService, IFMXSystemFontService, IFMXTimerService,
    IFMXWindowService, IFMXClipboardService, IFMXScreenService, IFMXLocaleService, IFMXDialogService, IFMXTextService,
    IFMXContextService, IFMXCanvasService, IFMXDeviceService, IFMXStyleService, IFMXSystemInformationService,
    IFMXLoggingService, IFMXApplicationEventService, IFMXDefaultMetricsService,
    IFMXDefaultPropertyValueService, IFMXGestureRecognizersService, IFMXMouseService, IFMXListingService,
    IFMXTextEditingService, IFMXRenderingSetupService, IFMXSaveStateService, IFMXDeviceMetricsService)
  private const
    DefaultiOSFontSize = 14;
    DefaultiOSFontName = 'Helvetica';
  private
    FWakeHandler: TFMXWakeHandler;
    FInputDelegates: TList<TFMXAlertViewInputDelegate>;
    UIApp: UIApplication;
    FMainScreen: UIScreen;
    FAppDelegate: TApplicationDelegate;
    FIdleTimer: TFmxHandle;
    FHandleCounter: TFmxHandle;
    FObjectiveCMap: TDictionary<TFmxHandle, IObjectiveC>;
    FObjectMap: TDictionary<TFmxHandle, TObject>;
    FTimers: TList<TFmxHandle>;
    FFormHandle: TWindowHandle;
    FTerminating: Boolean;
    FOnApplicationEvent: TApplicationEventHandler;
    FRotationView: UIImageView;
    FRotationViewAnimated: Boolean;
    FMouseCoord: TPointF;
    FStatusBarHeight: Single;
    FCanSetState: Boolean;
    FTitle: string;
    RenderingSetupCallback: TRenderingSetupCallback;
    [Weak] RenderingSetupContext: TObject;
    FSaveStateStoragePath: string;
    function NewFmxHandle: TFmxHandle;
    procedure ValidateHandle(FmxHandle: TFmxHandle);
    function AllocObjectHandle(const Objc: TObject): TFmxHandle;
    procedure DeleteObjectHandle(const FmxHandle: TFmxHandle);
    function HandleToObject(const FmxHandle: TFmxHandle): TObject;
    procedure ResetIdleTimer;
    procedure IdleTimerFunc;
    procedure InternalWaitMessage(AInterval: Single);
    function IsPopupForm(const AForm: TCommonCustomForm): Boolean;
    procedure InitializeFormFactor(FormFactor: TFormFactor);
    function DictionaryToJson(ADictionary: NSDictionary): string;
    // IFMXListingService
    function GetListingHeaderBehaviors: TListingHeaderBehaviors;
    function GetListingSearchFeatures: TListingSearchFeatures;
    function GetListingTransitionFeatures: TListingTransitionFeatures;
    function GetListingEditModeFeatures: TListingEditModeFeatures;
    function IFMXListingService.GetHeaderBehaviors = GetListingHeaderBehaviors;
    function IFMXListingService.GetSearchFeatures = GetListingSearchFeatures;
    function IFMXListingService.GetTransitionFeatures = GetListingTransitionFeatures;
    function IFMXListingService.GetEditModeFeatures = GetListingEditModeFeatures;
    // IFMXRenderingSetupService
    procedure SubscribeRenderingSetup(const Callback: TRenderingSetupCallback; const Context: TObject);
    procedure UnsubscribeRenderingSetup;
    procedure InvokeRenderingSetup(var ColorBits, DepthBits: Integer; var Stencil: Boolean; var Multisamples: Integer);
    procedure IFMXRenderingSetupService.Subscribe = SubscribeRenderingSetup;
    procedure IFMXRenderingSetupService.Unsubscribe = UnsubscribeRenderingSetup;
    procedure IFMXRenderingSetupService.Invoke = InvokeRenderingSetup;
    // IFMXSaveStateService
    function GetSaveStateFileName(const ABlockName: string): string;
    function GetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;
    function SetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;
    function GetSaveStateStoragePath: string;
    procedure SetSaveStateStoragePath(const ANewPath: string);
    function GetSaveStateNotifications: Boolean;
    function IFMXSaveStateService.GetBlock = GetSaveStateBlock;
    function IFMXSaveStateService.SetBlock = SetSaveStateBlock;
    function IFMXSaveStateService.GetStoragePath = GetSaveStateStoragePath;
    procedure IFMXSaveStateService.SetStoragePath = SetSaveStateStoragePath;
    function IFMXSaveStateService.GetNotifications = GetSaveStateNotifications;
    // IFMXDeviceMetricsService
    function GetDisplayMetrics: TDeviceDisplayMetrics;
    procedure WakeMainThread(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXApplicationService }
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function Terminating: Boolean;
    procedure Terminate;
    { IFMXSystemFontService }
    function GetDefaultFontFamilyName: string;
    function GetDefaultFontSize: Single;
    { IFMXMouseService }
    function GetMousePos: TPointF;
    { IFMXTimerService }
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    function GetTick: Extended;
    { IFMXWindowService }
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
    procedure DestroyWindow(const AForm: TCommonCustomForm);
    procedure ReleaseWindow(const AForm: TCommonCustomForm);
    procedure ShowWindow(const AForm: TCommonCustomForm);
    procedure HideWindow(const AForm: TCommonCustomForm);
    procedure BringToFront(const AForm: TCommonCustomForm);
    procedure SendToBack(const AForm: TCommonCustomForm);
    procedure Activate(const AForm: TCommonCustomForm);
    function ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
    procedure InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
    procedure InvalidateImmediately(const AForm: TCommonCustomForm);
    procedure SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
    function GetWindowRect(const AForm: TCommonCustomForm): TRectF;
    function GetClientSize(const AForm: TCommonCustomForm): TPointF;
    procedure SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
    procedure SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
    procedure SetCapture(const AForm: TCommonCustomForm);
    procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
    procedure ReleaseCapture(const AForm: TCommonCustomForm);
    function ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function GetWindowScale(const AForm: TCommonCustomForm): Single;
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
    { IFMXClipboardService }
    procedure SetClipboard(Value: TValue);
    function GetClipboard: TValue;
    { IFMXScreenService }
    function GetScreenSize: TPointF;
    function GetScreenScale: Single;
    function GetScreenOrientation: TScreenOrientation;
    procedure SetScreenOrientation(AOrientations: TScreenOrientations);
    { IFMXLocaleService }
    function GetCurrentLangID: string;
    function GetLocaleFirstDayOfWeek: string;
    { IFMXDialogService }
    function DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
    function DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
    function PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
      AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
      var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
    function DialogPrinterSetup: Boolean;
    function MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
	    const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt;
      const AHelpFileName: string): Integer; overload;
    procedure MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt; const AHelpFileName: string;
      const ACloseDialogProc: TInputCLoseDialogProc); overload;
    function InputQuery(const ACaption: string; const APrompts: array of string;
	    var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc = nil): Boolean; overload;
    procedure InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
	    const ACloseQueryProc: TInputCloseQueryProc); overload;
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXContextService }
    procedure RegisterContextClasses;
    procedure UnregisterContextClasses;
    { IFMXCanvasService }
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    { IFMXDeviceService }
    function GetModel: string;
    function GetFeatures: TDeviceFeatures;
    function GetDeviceKind: TDeviceKind;
    function GetDeviceClass: TDeviceInfo.TDeviceClass;
    {$WARN SYMBOL_DEPRECATED OFF}
    function IFMXDeviceService.GetKind = GetDeviceKind;
    {$WARN SYMBOL_DEPRECATED DEFAULT}
    { IFMXStyleService }
    function GetSystemStyle(const Context: TFmxObject): TFmxObject;
    { IFMXSystemInformationService }
    function GetScrollingBehaviour: TScrollingBehaviours;
    function GetMinScrollThumbSize: Single;
    function GetCaretWidth: Integer;
    function GetMenuShowDelay: Integer;
    { IFMXLoggingService }
    procedure Log(const Fmt: string; const Params: array of const);
    { IFMXApplicationEventService }
    procedure SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
    function HandleApplicationEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
    { IFMXDefaultMetricsService }
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;
    { IFMXDefaultPropertyValueService }
    function GetDefaultPropertyValue(const AClassName, PropertyName: string): TValue;
    { IFMXGestureRecognizersService }
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    { IFMXTextEditingService }
    function GetCaretBehaviors: TCaretBehaviors;
    { Status Bar }
    function CalculateFormViewFrame(const AForm: TCommonCustomForm): NSRect;
    procedure UpdateFormViewSize(const AForm: TCommonCustomForm);
    function HasFormStatusBar(const AForm: TCommonCustomForm): Boolean;
    procedure ShowStatusBar;
    procedure HideStatusBar;
    procedure UpdateStatusBarVisibility(const AForm: TCommonCustomForm);
    procedure UpdateStatusBarColor(const AForm: TCommonCustomForm);
    procedure CalculateStatusBarHeight;
    { }
    procedure ReceivedDeviceToken(const ADeviceToken: NSString);
    procedure ReceivedRemoteNotification(const ANotification: NSDictionary);
    procedure ReceivedStartupNotification(const ANotification: NSDictionary);
    procedure DidFailToRegisterForRemoteNotification(const AError: NSError);
    property AppDelegate: TApplicationDelegate read FAppDelegate;
    property MainScreen: UIScreen read FMainScreen write FMainScreen;
    property StatusBarHeight: Single read FStatusBarHeight;
  end;

(*
{$M+}
  TFMXWindow = class;
  TTextServiceCocoa = class;
*)


{ TFMXEditActionsMenu }

  TStandardActionType = (Unknown, Cut, Copy, Paste, Select,
                         SelectAll, PromptForReplace, Replace,
                         Spell1, Spell2, Spell3);

  { Context menu with standart edit actions: Cut, Copy, Past, Select, SelectAll }
  TFMXEditActionsMenu = class abstract
  strict private
    [weak] FParentView: UIView;
    procedure SetControl(const AControl: TControl);
  protected
    FMenuController: UIMenuController;
    [weak] FControl: TControl;
    FReplaceMenu: Boolean;
    procedure DoControlChanged; virtual;
    procedure DoDefineSelectionFrame(var Frame: CGRect); virtual;
  public
    constructor Create(const AParentView: UIView);
    destructor Destroy; override;
    // Do we need to show action in the context menu in the current conditions?
    function CanPerformAction(const AAction: SEL): Boolean; virtual; abstract;
    function DefineActionType(const AAction: SEL): TStandardActionType;
    procedure Show;
    procedure Hide;
    function IsVisible: Boolean;
    function HasControl: Boolean;
    { Standart Actions }
    procedure Cut; virtual; abstract;
    procedure Copy; virtual; abstract;
    procedure Paste; virtual; abstract;
    procedure Select; virtual; abstract;
    procedure SelectAll; virtual; abstract;
    procedure PromptForReplace; virtual; abstract;
    procedure Spell1; virtual; abstract;
    procedure Spell2; virtual; abstract;
    procedure Spell3; virtual; abstract;
    property Control: TControl read FControl write SetControl;
  end;

{ TFMXTextEditActionsMenu }

  { Implemented context menu with standart edit actions for text controls }
  TFMXTextEditActionsMenu = class (TFMXEditActionsMenu)
  private
    FTextInput: ITextInput;
    FTextActions: ITextActions;
    FSpellCheck: ITextSpellCheck;
    FSpellActions: ITextSpellCheckActions;
    FSpells: TArray<string>;
    FShowSpellItems: Boolean;
    FSpellItem1: UIMenuItem;
    FSpellItem2: UIMenuItem;
    FSpellItem3: UIMenuItem;
  protected
    procedure DoControlChanged; override;
    procedure DoDefineSelectionFrame(var Frame: CGRect); override;
    function GetClipboardService: IFMXClipboardService;
  public
    function CanPerformAction(const AAction: SEL): Boolean; override;
    procedure Cut; override;
    procedure Copy; override;
    procedure Paste; override;
    procedure Select; override;
    procedure SelectAll; override;
    procedure PromptForReplace; override;
    procedure Spell1; override;
    procedure Spell2; override;
    procedure Spell3; override;
    //
    procedure SetSpellItems(items: TArray<string>);
    procedure HighlightSpell;
    procedure HideHighlightSpell;
    //
    property ShowSpellItems: Boolean read FShowSpellItems write FShowSpellItems;
  end;

  TFMXViewBase = class(TOCLocal, UIKeyInput, UITextInput, UITextInputTraits, UIGestureRecognizerDelegate)
  private
    FGestureControl: TComponent;
    FMultiTouchManager: TMultiTouchManagerIOS;
    FMarkedTextRange: UITextRange;
    FNoOfTouches: Integer;
    function GetTouchCoord(const touches: NSSet; const Window: UIView; var x, y: single): Boolean;
    procedure SendTouches(const ATouches: NSSet; Action: TTouchAction; const Control: IControl);
    procedure AddRecognizer(const Gesture: TInteractiveGesture);
    procedure DoLMouseUp(const X, Y: Single; DoClick: Boolean = True);
    procedure DoLMouseDown(const X, Y: Single);
    procedure DoLMouseMove(const X, Y: Single);
    procedure DefineFocusControl;
    procedure FormKeyPress(Ch: Char; Key: Word; Shift: TShiftState);
    procedure PrepareClosePopups(const SaveForm: TCommonCustomForm);
    procedure ClosePopups;
  protected
    FContextMenu: TFMXTextEditActionsMenu;
    FIgnorePosition: Boolean;
    FCarretPositionChanged: Boolean;
    FLastCaretPosition: TPoint;
    FLastContextMenuVisibility: Boolean;
    FClickedAnotherControl: Boolean;
    FChangedFocusedControl: Boolean;
    FText: string;
    [Weak]FForm: TCommonCustomForm;
    FSelectRange: NSRange;
    FMarkText: string;
    FMarkRange: NSRange;
    FKeyboardType: TVirtualKeyboardType;
    FReturnKeyType: TReturnKeyType;
    FPassword: Boolean;
    FDown: Boolean;
    FTap: Boolean;
    FResigned: Boolean;
    destructor Destroy; override;
    function GetMultiTouchManager: TMultiTouchManagerIOS;
    property MultiTouchManager: TMultiTouchManagerIOS read GetMultiTouchManager;
  public
    constructor Create(const AForm: TCommonCustomForm);
    { UIView overrides }
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canResignFirstResponder: Boolean; cdecl;
    function isFirstResponder: Boolean; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
    function isAccessibilityElement: Boolean; cdecl;
    { Touch Actions }
    procedure SingleTap(Sender: id); cdecl;
    procedure LongTap(gestureRecognizer: UILongPressGestureRecognizer); cdecl;
    procedure DblTap(X, Y: Single);
    { UIGestureRecognizerDelegate }
    function gestureRecognizer(gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch): Boolean; overload; cdecl;
    function gestureRecognizer(gestureRecognizer: UIGestureRecognizer; shouldRecognizeSimultaneouslyWithGestureRecognizer: UIGestureRecognizer): Boolean; overload; cdecl;
    function gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean; cdecl;
    { GestureRecognizer }
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    procedure HandleRotate(gestureRecognizer: UIRotationGestureRecognizer); cdecl;
    procedure HandleTwoFingerTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
    procedure HandleZoom(gestureRecognizer: UIPinchGestureRecognizer); cdecl;
    { Cut, Copy, Paste, Replace, SpellChecker Actions }
    procedure cut(Sender: id); cdecl;
    procedure copy(Sender: id); cdecl;
    procedure paste(Sender: id); cdecl;
    procedure select(Sender: id); cdecl;
    procedure selectAll(Sender: id); cdecl;
    procedure spell1(Sender: id); cdecl;
    procedure spell2(Sender: id); cdecl;
    procedure spell3(Sender: id); cdecl;
    { Context Menu Showing }
    procedure ShowContextMenu;
    procedure HideContextMenu;
    { UIKeyInput }
    procedure deleteBackward; cdecl;
    function hasText: Boolean; cdecl;
    procedure insertText(text: NSString); cdecl;
    { UITextInput }
    function baseWritingDirectionForPosition(position: UITextPosition; inDirection: UITextStorageDirection): UITextWritingDirection; cdecl;
    function beginningOfDocument: UITextPosition; cdecl;
    function endOfDocument: UITextPosition; cdecl;
    function inputDelegate: Pointer; cdecl;
    function markedTextRange: UITextRange; cdecl;
    function markedTextStyle: NSDictionary; cdecl;
    function selectedTextRange: UITextRange; cdecl;
    procedure setBaseWritingDirection(writingDirection: UITextWritingDirection; forRange: UITextRange); cdecl;
    procedure setInputDelegate(inputDelegate: Pointer); cdecl;
    procedure setMarkedText(markedText: NSString; selectedRange: NSRange); cdecl;
    procedure setMarkedTextStyle(markedTextStyle: NSDictionary); cdecl;
    procedure setSelectedTextRange(selectedTextRange: UITextRange); cdecl;
    { Returning and replacing text by text range }
    function textInRange(range: UITextRange): NSString; cdecl;
    procedure replaceRange(range: UITextRange; withText: NSString); cdecl;
    { Computing text ranges and text positions }
    function positionFromPosition(position: UITextPosition; offset: NSInteger): UITextPosition; overload; cdecl;
    function positionFromPosition(position: UITextPosition; inDirection: UITextLayoutDirection; offset: NSInteger): UITextPosition; overload; cdecl;
    function textRangeFromPosition(fromPosition: UITextPosition; toPosition: UITextPosition): UITextRange; cdecl;
    { Evaluating text positions }
    function comparePosition(position: UITextPosition; toPosition: UITextPosition): NSComparisonResult; cdecl;
    function offsetFromPosition(from: UITextPosition; toPosition: UITextPosition): NSInteger; cdecl;
    { Answering layout questions }
    function positionWithinRange(range: UITextRange; farthestInDirection: UITextLayoutDirection): UITextPosition; cdecl; //overload;
    function characterRangeByExtendingPosition(position: UITextPosition; inDirection: UITextLayoutDirection): UITextRange; cdecl;
    { Hit-testing }
    function closestPositionToPoint(point: CGPoint): UITextPosition; overload; cdecl;
    function closestPositionToPoint(point: CGPoint; withinRange: UITextRange): UITextPosition; overload; cdecl;
    function characterRangeAtPoint(point: CGPoint): UITextRange; cdecl;
    { Returning rectangles for text ranges and text positions }
    function firstRectForRange(range: UITextRange): CGRect; cdecl;
    function caretRectForPosition(position: UITextPosition): CGRect; cdecl;

    function tokenizer: Pointer; cdecl;
    procedure unmarkText; cdecl;
    { UITextInputTraits }
    function autocapitalizationType: UITextAutocapitalizationType; cdecl;
    function autocorrectionType: UITextAutocorrectionType; cdecl;
    function enablesReturnKeyAutomatically: Boolean; cdecl;
    function isSecureTextEntry: Boolean; cdecl;
    function keyboardAppearance: UIKeyboardAppearance; cdecl;
    function keyboardType: UIKeyboardType; cdecl;
    function returnKeyType: UIReturnKeyType; cdecl;
    procedure setAutocapitalizationType(autocapitalizationType: UITextAutocapitalizationType); cdecl;
    procedure setAutocorrectionType(autocorrectionType: UITextAutocorrectionType); cdecl;
    procedure setEnablesReturnKeyAutomatically(enablesReturnKeyAutomatically: Boolean); cdecl;
    procedure setKeyboardAppearance(keyboardAppearance: UIKeyboardAppearance); cdecl;
    procedure setKeyboardType(keyboardType: UIKeyboardType); cdecl;
    procedure setReturnKeyType(returnKeyType: UIReturnKeyType); cdecl;
    procedure setSecureTextEntry(secureTextEntry: Boolean); cdecl;
    procedure setSpellCheckingType(spellCheckingType: Integer); cdecl;
    function spellCheckingType: Integer; cdecl;
    property Form: TCommonCustomForm read FForm;
  end;

  FMXView = interface(UIView)
    ['{0A6E8339-D32C-4A5F-BDD1-4CC3A711CF01}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canResignFirstResponder: Boolean; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
    function isFirstResponder: Boolean; cdecl;
    procedure drawRect(R: CGRect); cdecl;
    { Cut, Copy, Paste }
    procedure cut(Sender: id); cdecl;
    procedure copy(Sender: id); cdecl;
    procedure paste(Sender: id); cdecl;
    procedure select(Sender: id); cdecl;
    procedure selectAll(Sender: id); cdecl;
    procedure spell1(Sender: id); cdecl;
    procedure spell2(Sender: id); cdecl;
    procedure spell3(Sender: id); cdecl;
    { Touches }
    procedure SingleTap(Sender: id); cdecl;
    procedure LongTap(gestureRecognizer: UILongPressGestureRecognizer); cdecl;
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    procedure HandleRotate(gestureRecognizer: UIRotationGestureRecognizer); cdecl;
    procedure HandleTwoFingerTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
    procedure HandleZoom(gestureRecognizer: UIPinchGestureRecognizer); cdecl;
    { Accessibility }
    function isAccessibilityElement: Boolean; cdecl;
  end;

  TFMXView = class(TFMXViewBase, UIKeyInput, UITextInput, UITextInputTraits)
  public
    constructor Create(const AOwner: TCommonCustomForm; AFRameRect: NSRect);
    function GetObjectiveCClass: PTypeInfo; override;
    procedure drawRect(R: CGRect); cdecl;
  end;

  FMXView3D = interface(GLKView)
    ['{CC0FB04D-56B0-446D-9464-F18D1B4AFE22}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canResignFirstResponder: Boolean; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
    function isFirstResponder: Boolean; cdecl;
    procedure drawRect(R: CGRect); cdecl;
    { Cut, Copy, Paste }
    procedure cut(Sender: id); cdecl;
    procedure copy(Sender: id); cdecl;
    procedure paste(Sender: id); cdecl;
    procedure select(Sender: id); cdecl;
    procedure selectAll(Sender: id); cdecl;
    procedure spell1(Sender: id); cdecl;
    procedure spell2(Sender: id); cdecl;
    procedure spell3(Sender: id); cdecl;
    { Touches }
    procedure SingleTap(Sender: id); cdecl;
    procedure LongTap(gestureRecognizer: UILongPressGestureRecognizer); cdecl;
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    procedure HandleRotate(gestureRecognizer: UIRotationGestureRecognizer); cdecl;
    procedure HandleTwoFingerTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
    procedure HandleZoom(gestureRecognizer: UIPinchGestureRecognizer); cdecl;
    { Accessibility }
    function isAccessibilityElement: Boolean; cdecl;
  end;

  TFMXView3D = class(TFMXViewBase, UIKeyInput, UITextInput, UITextInputTraits, UIGestureRecognizerDelegate)
  private
  public
    constructor Create(const AOwner: TCommonCustomForm; AFRameRect: NSRect);
    function GetObjectiveCClass: PTypeInfo; override;
    procedure drawRect(R: CGRect); cdecl;
  end;

  { TTextServiceCocoa }
  TTextServiceCocoa = class (TTextService)
  private
    FView: TFMXViewBase;
    FCaretPostion: TPoint;
    FText : string;
    FMarkedText : string;
    FImeMode: TImeMode;
    FCursorShift: Integer;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function GetCaretPostion: TPoint; override;
    procedure SetCaretPosition(const Value: TPoint); override;
    procedure SetCursorShift(const Value: Integer);
  public
    procedure InternalSetMarkedText( const AMarkedText: string ); override;
    function InternalGetMarkedText: string; override;
    procedure InternalStartIMEInput;
    procedure InternalBreakIMEInput;
    procedure InternalEndIMEInput;

    function CombinedText: string; override;
    function TargetClausePosition: TPoint; override;

    procedure EnterControl(const FormHandle: TWindowHandle); override;
    procedure ExitControl(const FormHandle: TWindowHandle); override;

    procedure DrawSingleLine(const  Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload;  override;

    procedure DrawSingleLine(const Canvas: TCanvas;
      const S: string;
      const ARect: TRectF;
      const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload; override;

    function HasMarkedText: Boolean; override;

    function GetImeMode: TImeMode; override;
    procedure SetImeMode(const Value: TImeMode); override;

    { Selection }
    procedure BeginSelection; override;
    procedure EndSelection; override;

    { Cocoa }
  private
    FSelectedRange: NSRange;
  public
    constructor Create(const Owner: IControl; SupportMultiLine: Boolean); override;
    destructor Destroy; override;
    procedure SetSelectedRange(const Value: NSRange);
  end;

var
  PlatformCocoa: TPlatformCocoaTouch;
  MultiDisplayIOS: TMultiDisplayIOS;


procedure RegisterCorePlatformServices;
begin
  PlatformCocoa := TPlatformCocoaTouch.Create;
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXApplicationEventService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXTimerService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXWindowService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXClipboardService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXScreenService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXLocaleService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDialogService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXContextService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXStyleService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXLoggingService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultPropertyValueService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXGestureRecognizersService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXListingService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXTextEditingService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXRenderingSetupService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXSaveStateService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, PlatformCocoa);
  MultiDisplayIOS := TMultiDisplayIOS.Create;
  TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, MultiDisplayIOS);
end;

procedure UnregisterCorePlatformServices;
begin
end;

// Application delegates

procedure applicationDidChangeStatusBarFrame(self: id; _cmd: SEL; application: PUIApplication;
  frame: CGRect); cdecl;
begin
  PlatformCocoa.FAppDelegate.application(TUIApplication.Wrap(application), frame);
end;

procedure applicationdidChangeStatusBarOrientation(self: id; _cmd: SEL; application: PUIApplication;
  orientation: UIInterfaceOrientation); cdecl;
begin

end;

function applicationDidFinishLaunchingWithOptions(self: id; _cmd: SEL;
  application: PUIApplication; options: PNSDictionary): Boolean; cdecl;
begin
  Result := PlatformCocoa.FAppDelegate.application(TUIApplication.Wrap(application), TNSDictionary.Wrap(options));
end;

procedure applicationDidReceiveLocalNotification(self: id; _cmd: SEL; application: PUIApplication;
  notification: Pointer); cdecl;
begin
  PlatformCocoa.FAppDelegate.application(TUIApplication.Wrap(application), TUILocalNotification.Wrap(notification));
end;

procedure didReceiveRemoteNotification(self: id; _cmd: SEL; app: PUIApplication; ANotification: PNSDictionary); cdecl;
begin
  PlatformCocoa.FAppDelegate.applicationDidReceiveRemoteNotification(TUIApplication.Wrap(app), TNSDictionary.Wrap(ANotification));
end;

procedure didFailToRegisterForRemoteNotificationsWithError(self: id; _cmd: SEL; app: PUIApplication; error: PNSError); cdecl;
begin
  PlatformCocoa.FAppDelegate.didFailToRegisterForRemoteNotificationsWithError(TUIApplication.Wrap(application), TNSError.Wrap(error));
end;

procedure didRegisterForRemoteNotificationsWithDeviceToken(self: id; _cmd: SEL; application: PUIApplication; deviceToken: PNSData); cdecl;
begin
  PlatformCocoa.FAppDelegate.applicationDidRegisterForRemoteNotificationsWithDeviceToken(TUIApplication.Wrap(application), TNSData.Wrap(deviceToken));
end;

procedure applicationOpenURLWithSourceAnnotation(self: id; _cmd: SEL; application: PUIApplication; url: Pointer; sourceApplication: PNSString; annotation: id);
var
  URLString: string;
  SourceAppString: string;
begin
  if url <> nil then
    URLString := NSStrToStr(TNSURL.Wrap(url).absoluteString)
  else
    URLString := '';
  if sourceApplication <> nil then
    SourceAppString := NSStrToStr(TNSString.Wrap(sourceApplication))
  else
    SourceAppString := '';
  PlatformCocoa.FAppDelegate.application(URLString, SourceAppString, annotation);
end;

procedure applicationDidBecomeActive(self: id; _cmd: SEL; application: PUIApplication); cdecl;
begin
  PlatformCocoa.FAppDelegate.applicationDidBecomeActive(TUIApplication.Wrap(application));
end;

procedure applicationDidEnterBackground(self: id; _cmd: SEL; application: PUIApplication); cdecl;
begin
  // This is called.
  PlatformCocoa.FAppDelegate.applicationDidEnterBackground(TUIApplication.Wrap(application));
end;

procedure applicationWillEnterForeground(self: id; _cmd: SEL; application: PUIApplication); cdecl;
begin
  PlatformCocoa.FAppDelegate.applicationWillEnterForeground(TUIApplication.Wrap(application));
end;

procedure applicationWillResignActive(self: id; _cmd: SEL; application: PUIApplication); cdecl;
begin
  // This seems not to be called.
end;

procedure applicationWillTerminate(self: id; _cmd: SEL; application: PUIApplication); cdecl;
begin
  PlatformCocoa.FAppDelegate.applicationWillTerminate(TUIApplication.Wrap(application));
end;

procedure applicationDidReceiveMemoryWarning(self: id; _cmd: SEL; application: PUIApplication); cdecl;
begin
  PlatformCocoa.FAppDelegate.applicationDidReceiveMemoryWarning(TUIApplication.Wrap(application));
end;

procedure setWindow(self: id; _cmd: SEL; window: Pointer); cdecl;
begin

end;

function window(self: id; _cmd: SEL): Pointer; cdecl;
begin
  Result := nil;
end;

{ TApplicationDelegate }

procedure TApplicationDelegate.application(Sender: UIApplication;
  didRegisterForRemoteNotificationsWithDeviceToken: NSData);
begin

end;

procedure TApplicationDelegate.application(Sender: UIApplication; didReceiveLocalNotification: UILocalNotification);
begin
  if UIApplicationStateInactive = Sender.applicationState then
    TMessageManager.DefaultManager.SendMessage(nil, TMessage<UILocalNotification>.Create(didReceiveLocalNotification));
end;

procedure TApplicationDelegate.application(const Sender: UIApplication;
  const willChangeStatusBarOrientation: UIInterfaceOrientation;
  const duration: NSTimeInterval);
begin

end;

function TApplicationDelegate.application(const openURL, sourceApplication: string; annotation: Pointer): Boolean;
begin
  Result := PlatformCocoa.HandleApplicationEvent(TApplicationEvent.OpenURL,
    TiOSOpenApplicationContext.Create(sourceApplication, openURL, annotation));
end;

procedure TApplicationDelegate.application(Sender: UIApplication;
  didChangeStatusBarFrame: CGRect);
begin

end;

procedure TApplicationDelegate.application(Sender: UIApplication;
  didChangeStatusBarOrientation: UIInterfaceOrientation);
begin

end;

function TApplicationDelegate.application(Sender: UIApplication; didFinishLaunchingWithOptions: NSDictionary): Boolean;
var
  UIWin: UIWindow;
  LocalNotification: UILocalNotification;
  StatusBarView: UIView;
  RootView: UIView;
  RootViewController: TFMXViewController;
  RemoteNotification : Pointer;
begin
  PlatformCocoa.UIApp := Sender;
  PlatformCocoa.CalculateStatusBarHeight;
  if Assigned(didFinishLaunchingWithOptions.valueForKey(UIApplicationLaunchOptionsLocalNotificationKey)) then
  begin
    LocalNotification := TUILocalNotification.Wrap(didFinishLaunchingWithOptions.valueForKey(UIApplicationLaunchOptionsLocalNotificationKey));
    TMessageManager.DefaultManager.SendMessage(nil, TMessage<UILocalNotification>.Create(LocalNotification));
  end;

  RemoteNotification := didFinishLaunchingWithOptions.valueForKey(
      UIApplicationLaunchOptionsRemoteNotificationKey);
  if Assigned(RemoteNotification) then
    PlatformCocoa.ReceivedStartupNotification( TNSDictionary.Wrap(RemoteNotification) );

  // Creating window
  FMainWindow := TFMXWindow.Create(PlatformCocoa.MainScreen.bounds);
  UIWin := UIWindow(FMainWindow.Super);
  UIWin.setAutoresizesSubviews(True);
  UIWin.makeKeyAndVisible;

  // Creating root view - container for all FM Forms Views
  RootView := TUIView.Alloc;
  RootView := TUIView.Wrap(RootView.initWithFrame(PlatformCocoa.MainScreen.bounds));
  RootView.setAutoResizesSubviews(True);
  RootView.setAutoresizingMask(UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight);
  RootView.setOpaque(True);
  FMainWindow.RootView := RootView;

  // Creating View Controller
  RootViewController := TFMXViewController.Create;
  FMainWindow.RootViewController := RootViewController;

  // Creating Status bar view Holder and append it to Root View
  StatusBarView := TUIView.Alloc;
  StatusBarView := TUIView.Wrap(StatusBarView.initWithFrame(Sender.statusBarFrame));
  StatusBarView.setHidden(True);
  FMainWindow.RootViewController.StatusBarView := StatusBarView;
  RootView.addSubview(StatusBarView);

  // Setting Root View for ViewController and setting View Controller for Window
  UIViewController(RootViewController.Super).setView(RootView);
  if not TOSVersion.Check(7, 0) then
    UIViewController(RootViewController.Super).setWantsFullScreenLayout(True);
  UIWin.setRootViewController(UIViewController(FMainWindow.RootViewController.Super));
  FMainWindow.RootView.setFrame(PlatformCocoa.MainScreen.bounds);

  PlatformCocoa.HandleApplicationEvent(TApplicationEvent.FinishedLaunching, nil);
  FMX.Forms.Application.RealCreateForms;

  TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).beginGeneratingDeviceOrientationNotifications;
  Result := True;
end;

procedure TApplicationDelegate.application(Sender: UIApplication;
  didFailToRegisterForRemoteNotificationsWithError: NSError);
begin

end;

procedure TApplicationDelegate.applicationDidBecomeActive(const Sender: UIApplication);
begin
  PlatformCocoa.HandleApplicationEvent(TApplicationEvent.BecameActive, nil);
end;

procedure TApplicationDelegate.applicationDidEnterBackground(const Sender: UIApplication);
begin
  TMessageManager.DefaultManager.SendMessage(Self, TSaveStateMessage.Create);
  PlatformCocoa.HandleApplicationEvent(TApplicationEvent.EnteredBackground, nil);
end;

procedure TApplicationDelegate.applicationDidReceiveMemoryWarning(Sender: UIApplication);
begin
  PlatformCocoa.HandleApplicationEvent(TApplicationEvent.LowMemory, nil);
end;

procedure TApplicationDelegate.applicationDidReceiveRemoteNotification(
  Sender: UIApplication; ANotification: NSDictionary);
begin
  PlatformCocoa.ReceivedRemoteNotification(ANotification);
end;

procedure TApplicationDelegate.applicationDidRegisterForRemoteNotificationsWithDeviceToken(
  Sender: UIApplication; AToken: NSData);
begin
  if (AToken <> nil) then
    PlatformCocoa.ReceivedDeviceToken(AToken.description);
end;

procedure TApplicationDelegate.applicationProtectedDataDidBecomeAvailable(Sender: UIApplication);
begin

end;

procedure TApplicationDelegate.applicationProtectedDataWillBecomeUnavailable(Sender: UIApplication);
begin

end;

procedure TApplicationDelegate.applicationSignificantTimeChange(Sender: UIApplication);
begin
  PlatformCocoa.HandleApplicationEvent(TApplicationEvent.TimeChange, nil);
end;

procedure TApplicationDelegate.applicationWillEnterForeground(Sender: UIApplication);
begin
  PlatformCocoa.HandleApplicationEvent(TApplicationEvent.WillBecomeForeground, nil);
end;

procedure TApplicationDelegate.applicationWillResignActive(Sender: UIApplication);
begin
  PlatformCocoa.HandleApplicationEvent(TApplicationEvent.WillBecomeInactive, nil);
end;

procedure TApplicationDelegate.applicationWillTerminate(Sender: UIApplication);
begin
  PlatformCocoa.HandleApplicationEvent(TApplicationEvent.WillTerminate, nil);
end;

procedure TApplicationDelegate.didFailToRegisterForRemoteNotificationsWithError(Sender: UIApplication; AError: NSError);
begin
  PlatformCocoa.DidFailToRegisterForRemoteNotification(AError);
end;

procedure TApplicationDelegate.setWindow(window: UIWindow);
begin

end;

function TApplicationDelegate.window: UIWindow;
begin
  Result := nil;
end;

{ TPlatformCocoaTouch }

constructor TPlatformCocoaTouch.Create;
var
  appDelegateClass: Pointer;
begin
  inherited;
  FAppDelegate := TApplicationDelegate.Create;
  FMainScreen := TUIScreen.Wrap(TUIScreen.OCClass.mainScreen);
  Application := TApplication.Create(nil);
  InitializeFormFactor(Application.FormFactor);

  // Set up application delegate manually for now

  // Create a class to serve as our application delegate
  appDelegateClass := objc_allocateClassPair(objc_getClass('NSObject'), 'DelphiAppDelegate', 0);

  // Add the UIApplciationDelegate protocol
  class_addProtocol(appDelegateClass, objc_getProtocol('UIApplicationDelegate'));

  // Add the "finish launching" delegate method
  class_addMethod(appDelegateClass, sel_getUid('application:didFinishLaunchingWithOptions:'),
    @applicationDidFinishLaunchingWithOptions, 'v@:@@');

  // Add additional application delegate methods
  class_addMethod(appDelegateClass, sel_getUid('applicationDidEnterBackground:'),
    @applicationDidEnterBackground, 'v@:@');
  class_addMethod(appDelegateClass, sel_getUid('applicationWillResignActive:'),
    @applicationWillResignActive, 'v@:@');
  class_addMethod(appDelegateClass, sel_getUid('applicationDidBecomeActive:'),
    @applicationDidBecomeActive, 'v@:@');
  class_addMethod(appDelegateClass, sel_getUid('application:didChangeStatusBarFrame:'),
    @applicationDidChangeStatusBarFrame, 'v@:@{CGRect={CGPoint=ff}{CGSize=ff}}');
  class_addMethod(appDelegateClass, sel_getUid('application:didChangeStatusBarOrientation:'),
    @applicationdidChangeStatusBarOrientation, 'v@:@l');
  class_addMethod(appDelegateClass, sel_getUid('application:didReceiveLocalNotification:'),
    @applicationDidReceiveLocalNotification, 'v@:@@');
  class_addMethod(appDelegateClass, sel_getUid('applicationWillEnterForeground:'),
    @applicationWillEnterForeground, 'v@:@');
  class_addMethod(appDelegateClass, sel_getUid('applicationWillTerminate:'),
    @applicationWillTerminate, 'v@:@');
  class_addMethod(appDelegateClass, sel_getUid('applicationDidReceiveMemoryWarning:'),
    @applicationDidReceiveMemoryWarning, 'v@:@');
  class_addMethod(appDelegateClass, sel_getUid('setWindow:'), @setWindow, 'v@:@');
  class_addMethod(appDelegateClass, sel_getUid('window'), @window, '@@:'); // this mangling may not be right

  class_addMethod(appDelegateClass, sel_getUid('application:didReceiveRemoteNotification:'),
    @didReceiveRemoteNotification, 'v@:@@');
  class_addMethod(appDelegateClass, sel_getUid('application:didRegisterForRemoteNotificationsWithDeviceToken:'),
    @didRegisterForRemoteNotificationsWithDeviceToken, 'v@:@@');
  class_addMethod(appDelegateClass, sel_getUid('application:didFailToRegisterForRemoteNotificationsWithError:'),
    @didFailToRegisterForRemoteNotificationsWithError, 'v@:@@');
  class_addMethod(appDelegateClass, sel_getUid('application:openURL:sourceApplication:annotation:'),
    @applicationOpenURLWithSourceAnnotation, 'v@:@@@@');

  // Add the application dealloc delegate
//    class_addMethod(appDelegateClass, sel_getUid('dealloc'),
//      @applicationDealloc, 'v@:');

  // Add a instance variable for the main window
//    class_addIvar(appDelegateClass, 'window', sizeof(id), 2, '@');

  // Register the delegate class
  objc_registerClassPair(appDelegateClass);

  FObjectiveCMap := TDictionary<TFmxHandle, IObjectiveC>.Create;
  FObjectMap := TDictionary<TFmxHandle, TObject>.Create;
  FTimers := TList<TFmxHandle>.Create;

  FFormHandle := nil;
  FCanSetState := True;

  FIdleTimer := CreateTimer(10, IdleTimerFunc);
  FInputDelegates := TList<TFMXAlertViewInputDelegate>.Create;
  FWakeHandler := TFMXWakeHandler.Create;
  System.Classes.WakeMainThread := WakeMainThread;
end;

destructor TPlatformCocoaTouch.Destroy;
begin
  DestroyTimer(FIdleTimer);
  FreeAndNil(FTimers);
  FObjectMap.Free;
  FObjectiveCMap.Free;
  FInputDelegates.Free;
  System.Classes.WakeMainThread := nil;
  FreeAndNil(FWakeHandler);
  inherited;
end;

procedure TPlatformCocoaTouch.IdleTimerFunc;
var
  Done: Boolean;
begin
  Done := False;
  if (TThread.CurrentThread.ThreadID = MainThreadID) then
    CheckSynchronize;
  Application.DoIdle(Done);
end;

{ App =========================================================================}

procedure RunLoopObserverCallback(observer: CFRunLoopObserverRef; activity: CFRunLoopActivity; info: Pointer); cdecl;
begin
  if (TThread.CurrentThread.ThreadID = MainThreadID) then
    CheckSynchronize;
//  Application.DoIdle(Done);
end;

procedure TPlatformCocoaTouch.Run;
begin
//  ExitCode := UIApplicationMain(ParamCount, MarshaledAString(ParamStr(0)), nil, (StrToNSStr('DelphiAppDelegate') as ILocalObject).GetObjectID);
  ExitCode := UIApplicationMain(System.ArgCount, System.ArgValues, nil, (StrToNSStr('DelphiAppDelegate') as ILocalObject).GetObjectID);
end;

function TPlatformCocoaTouch.Terminating: Boolean;
begin
  Result := FTerminating;
end;

procedure TPlatformCocoaTouch.Terminate;
var
  I: Integer;
begin
  for I := FTimers.Count - 1 downto 0 do
  try
    DestroyTimer(FTimers[I]);
  except
    Continue;
  end;
  FTerminating := True;
  raise EUnsupportedPlatformService.CreateFMT(SUnsupportedPlatformService, ['Terminate']);
end;

function TPlatformCocoaTouch.HandleApplicationEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
var
  ApplicationEventMessage: TApplicationEventMessage;
begin
  Result := False;

  { Send broadcast message }
  ApplicationEventMessage := TApplicationEventMessage.Create(TApplicationEventData.Create(AEvent, AContext));
  TMessageManager.DefaultManager.SendMessage(nil, ApplicationEventMessage);

  { Invoke application event}
  if Assigned(FOnApplicationEvent) then
    try
      Result := FOnApplicationEvent(AEvent, AContext);
    except
      Application.HandleException(Self);
    end;
end;

function TPlatformCocoaTouch.HandleMessage: Boolean;
begin
  WaitMessage;
  Result := False;
end;

procedure TPlatformCocoaTouch.InternalWaitMessage(AInterval: Single);
var
  TimeoutDate: NSDate;
begin
  TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(AInterval));
  TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop).runMode(NSDefaultRunLoopMode, TimeoutDate);
end;

procedure TPlatformCocoaTouch.WaitMessage;
begin
  InternalWaitMessage(0.0);
end;

procedure TPlatformCocoaTouch.WakeMainThread(Sender: TObject);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TNSObject.Wrap(FWakeHandler.GetObjectID).performSelectorOnMainThread(sel_getUid('DoCheckSynchronize'), nil, false,
      TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((NSDefaultRunLoopMode as ILocalObject).GetObjectID)));
end;

function TPlatformCocoaTouch.GetDefaultTitle: string;
var
  AppNameKey: Pointer;
  AppBundle: NSBundle;
  NSAppName: NSString;
begin
  AppNameKey := (StrToNSStr('CFBundleName') as ILocalObject).GetObjectID;
  AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
  NSAppName := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppNameKey));
  Result := UTF8ToString(NSAppName.UTF8String);
end;

function TPlatformCocoaTouch.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TPlatformCocoaTouch.SetTitle(const Value: string);
begin
  if FTitle <> Value then
    FTitle := Value;
end;

{ Timer =======================================================================}

type
  CocoaTimer = interface(NSObject)
    ['{B65CD0E6-21EA-4E77-BF5E-981C3B0EE632}']
    procedure timerEvent; cdecl;
  end;

  TCocoaTimer = class(TOCLocal)
  private
    FFunc : TTimerProc;
    FTimer: NSTimer;
  public
    function GetObjectiveCClass: PTypeInfo; override;
    procedure timerEvent; cdecl;
    procedure SetTimerFunc(AFunc: TTimerProc);
  end;

function TCocoaTimer.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(CocoaTimer);
end;

procedure TCocoaTimer.timerEvent;
begin
  if Assigned(@FFunc) then
  try
    FFunc;
  except
    on E: Exception do
    begin
      if Assigned(Application) then
        Application.HandleException(nil);
    end;
  end;
end;

procedure TCocoaTimer.SetTimerFunc(AFunc: TTimerProc);
begin
  FFunc := AFunc;
end;

function TPlatformCocoaTouch.CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
var
  LInterval: NSTimeInterval;
  User: TCocoaTimer;
begin
  Result := 0;
  if (not FTerminating) and (Interval > 0) and (Assigned(TimerFunc)) then
  begin
    User := TCocoaTimer.Create;
    try
      User.SetTimerFunc(TimerFunc);
      LInterval := Interval/1000;

      User.FTimer := TNSTimer.Wrap(TNSTimer.OCClass.scheduledTimerWithTimeInterval(LInterval,
        User.GetObjectID, sel_getUid('timerEvent'), User.GetObjectID, True));

      Result := AllocObjectHandle(User);
      FTimers.Add(Result);
    finally
      {user is retained (twice, because it's target), by the timer and }
      {released (twice) on timer invalidation}
      NSObject(User.Super).release;
    end;
  end;
end;

function TPlatformCocoaTouch.DestroyTimer(Timer: TFmxHandle): Boolean;
var
  User: TCocoaTimer;
  I: Integer;
begin
  Result := False;
  User := TCocoaTimer(HandleToObject(Timer));
  if Assigned(User) then
  begin
    Result := True;
    User.FTimer.invalidate;
    User.FTimer := nil;
    DeleteObjectHandle(Timer);
    for I := FTimers.Count - 1 downto 0 do
      if FTimers[I] = Timer then
      begin
        FTimers.Delete(I);
        Break;
      end;
  end;
end;

function TPlatformCocoaTouch.GetTick: Extended;
var
  H, M, S, MS: Word;
begin
  DecodeTime(System.SysUtils.Time, H, M, S, MS);
  Result := ((((H * SecsPerHour) + (M * SecsPerMin) + S) * MSecsPerSec) + MS) / 1000;
end;

{ IFMXSystemFontService }

function TPlatformCocoaTouch.GetDefaultFontFamilyName: string;
begin
  Result := DefaultiOSFontName;
end;

function TPlatformCocoaTouch.GetDefaultFontSize: Single;
begin
  Result := DefaultiOSFontSize;
end;

{ IFMXClipboardService }

procedure TPlatformCocoaTouch.SetClipboard(Value: TValue);
begin
  if not Value.IsObject then
    TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard).setString(StrToNSStr(Value.ToString));
end;

function TPlatformCocoaTouch.GetClipboard: TValue;
var
  A: NSArray;
  S: NSString;
begin
  if TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard).isPersistent then
  begin
    A := TNSArray.Wrap(TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard).strings);
    if A.count > 0 then
    begin
      S := TNSString.Wrap(A.objectAtIndex(0));
      Result := UTF8ToString(S.UTF8String);
    end;
  end
  else
    Result := '';
end;

{ IFMXWindowService }

{ Text Service }

constructor TTextServiceCocoa.Create(const Owner: IControl; SupportMultiLine: Boolean);
begin
  inherited;
end;

destructor TTextServiceCocoa.Destroy;
begin
  inherited;
end;

function TTextServiceCocoa.GetText: string;
begin
  Result := FText;
end;

procedure TTextServiceCocoa.SetText(const Value: string);
begin
  FText := Value;
end;

function TTextServiceCocoa.GetCaretPostion: TPoint;
begin
  Result := FCaretPostion;
end;

procedure TTextServiceCocoa.SetCaretPosition(const Value: TPoint);
begin
  FCaretPostion := Value;
end;

procedure TTextServiceCocoa.SetCursorShift(const Value: Integer);
begin
  FCursorShift := Value;
end;

procedure TTextServiceCocoa.InternalSetMarkedText( const AMarkedText: string );
begin
  FMarkedText := AMarkedText;
  (FOwner as ITextInput).UpdateCaretPoint;
end;

procedure TTextServiceCocoa.InternalBreakIMEInput;
begin
  FMarkedText := EmptyStr;
  FCursorShift := 0;
  (FOwner as ITextInput).UpdateCaretPoint;
end;

procedure TTextServiceCocoa.InternalEndIMEInput;
begin
  (FOwner as ITextInput).EndIMEInput;
  FMarkedText := EmptyStr;
  FCursorShift := 0;
end;

procedure TTextServiceCocoa.InternalStartIMEInput;
begin
  (FOwner as ITextInput).StartIMEInput;
end;

function TTextServiceCocoa.InternalGetMarkedText: string;
begin
  Result := FMarkedText;
end;

procedure TTextServiceCocoa.BeginSelection;
begin
  FView.HideContextMenu;
end;

function TTextServiceCocoa.CombinedText: string;
begin
  if FMarkedText <> '' then
    Result := System.Copy(FText, 1, FCaretPostion.X) + FMarkedText + System.Copy(FText, FCaretPostion.X + 1, MaxInt)
  else
    Result := FText;
end;

function TTextServiceCocoa.TargetClausePosition: TPoint;
begin
  Result := CaretPosition;
  Result.X := Result.X + FCursorShift;
end;

procedure TTextServiceCocoa.EndSelection;
begin
  if Assigned(FView) then
    FView.FContextMenu.Show;
end;

procedure TTextServiceCocoa.EnterControl(const FormHandle: TWindowHandle);
var
  View: TFMXViewBase;
  VirtKBControl: IVirtualKeyboardControl;
  TSObj: ITextInput;
begin
  View := TFMXViewBase(WindowHandleToPlatform(FormHandle).Handle);
  FView := View;
  if Assigned(View.Form.Focused) and Supports(View.Form.Focused, IVirtualKeyboardControl, VirtKBControl) then
  begin
    View.FKeyboardType := VirtKBControl.KeyboardType;
    View.FReturnKeyType := VirtKBControl.ReturnKeyType;
    View.FPassword := VirtKBControl.IsPassword;
  end
  else
  begin
    View.FKeyboardType := TVirtualKeyboardType.Default;
    View.FReturnKeyType := TReturnKeyType.Default;
    View.FPassword := False;
  end;
  View.unmarkText;
  View.FText := '';
  if Assigned(View.Form.Focused) and Supports(View.Form.Focused, ITextInput, TSObj) then
    View.FSelectRange := NSMakeRange(TSObj.GetTextService.CaretPosition.X, 0)
  else
    View.FSelectRange := NSMakeRange(0, 0);
  View.FMarkRange := NSMakeRange(NSNotFound, 0);
end;

procedure TTextServiceCocoa.ExitControl(const FormHandle: TWindowHandle);
var
  View: TFMXViewBase;
begin
  View := TFMXViewBase(WindowHandleToPlatform(FormHandle).Handle);
  View.unmarkText;
  View.FText := '';
  FView := nil;
end;

procedure TTextServiceCocoa.DrawSingleLine(const Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False);
var
  I: Integer;
  S: string;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.WordWrap := AWordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Font;
    Layout.Color := Canvas.Fill.Color;
    Layout.Opacity := AOpacity;
    Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
    S := CombinedText;
    Layout.Text := S.Substring(FirstVisibleChar - 1, S.Length - FirstVisibleChar + 1);
    Layout.EndUpdate;
    Layout.RenderLayout(Canvas);

    if not FMarkedText.IsEmpty then
    try
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X, FMarkedText.Length));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);

      if FSelectedRange.length > 0 then
      begin
        Canvas.StrokeThickness := 3;
        Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X + 1 + Integer(FSelectedRange.location), FSelectedRange.length));
        for I := Low(Region) to High(Region) do
          Canvas.DrawLine(
            PointF(Region[I].Left, Region[I].Bottom),
            PointF(Region[I].Right, Region[I].Bottom),
            AOpacity, Canvas.Stroke);
      end;
    finally
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

procedure TTextServiceCocoa.DrawSingleLine(const Canvas: TCanvas;
      const S: string;
      const ARect: TRectF;
      const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False);
var
  I: Integer;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.WordWrap := AWordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Font;
    Layout.Color := Canvas.Fill.Color;
    Layout.Opacity := AOpacity;
    Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
    Layout.Text := S;
    Layout.EndUpdate;
    Layout.RenderLayout(Canvas);

    if not FMarkedText.IsEmpty then
    try
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X, FMarkedText.Length));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);

      if FSelectedRange.length > 0 then
      begin
        Canvas.StrokeThickness := 3;
        Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X + Integer(FSelectedRange.location), FSelectedRange.length));
        for I := Low(Region) to High(Region) do
          Canvas.DrawLine(
            PointF(Region[I].Left, Region[I].Bottom),
            PointF(Region[I].Right, Region[I].Bottom),
            AOpacity, Canvas.Stroke);
      end;
    finally
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

function TTextServiceCocoa.HasMarkedText: boolean;
begin
  Result := not FMarkedText.IsEmpty;
end;

function TTextServiceCocoa.GetImeMode: TImeMode;
begin
  Result := FImeMode;
end;

procedure TTextServiceCocoa.SetImeMode(const Value: TImeMode);
begin
  FImeMode := Value;
end;

procedure TTextServiceCocoa.SetSelectedRange(const Value: NSRange);
begin
  FSelectedRange := Value;
end;

function TPlatformCocoaTouch.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceCocoa;
end;

function TPlatformCocoaTouch.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := WindowHandleToPlatform(AHandle).Form
end;

function TPlatformCocoaTouch.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
var
  View: TFMXViewBase;
  ViewFrame: CGRect;
begin
  Result := nil;

  ViewFrame := CalculateFormViewFrame(AForm);
  if TWindowStyle.GPUSurface in AForm.WindowStyle then
    View := TFMXView3D.Create(AForm, ViewFrame)
  else
    View := TFMXView.Create(AForm, ViewFrame);

  if not IsPopupForm(AForm) then
    UIView(View.Super).setAutoresizingMask(UIViewAutoresizingFlexibleHeight or UIViewAutoresizingFlexibleWidth);

  if AForm.Transparency and Assigned(Application.MainForm) then
    UIView(View.Super).setOpaque(False)
  else
    UIView(View.Super).setOpaque(True);

  UIView(View.Super).setHidden(True);

  Result := TiOSWindowHandle.Create(View);
end;

procedure TPlatformCocoaTouch.DestroyWindow(const AForm: TCommonCustomForm);
begin
  if Assigned(AForm.Handle) then
    WindowHandleToPlatform(AForm.Handle).View.removeFromSuperview;
end;

procedure TPlatformCocoaTouch.ReleaseWindow(const AForm: TCommonCustomForm);
begin
  if (AForm <> nil) and (AForm.Handle <> nil) then
    WindowHandleToPlatform(AForm.Handle).View.removeFromSuperview;
end;

procedure TPlatformCocoaTouch.RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
end;

procedure TPlatformCocoaTouch.ResetIdleTimer;
begin
  DestroyTimer(FIdleTimer);
  FIdleTimer := CreateTimer(10, IdleTimerFunc);
end;

procedure TPlatformCocoaTouch.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  if Assigned(AForm.Handle) and not ((AForm = Application.MainForm) or not IsPopupForm(AForm)) then
    WindowHandleToPlatform(AForm.Handle).View.setFrame(CGRectMake(ARect.Left, ARect.Top,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top))
end;

function TPlatformCocoaTouch.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
var
  LBounds: CGRect;
begin
  if (AForm = Application.MainForm) or not IsPopupForm(AForm) then
  begin
    LBounds := AppDelegate.MainWindow.RootView.bounds;
    Result := RectF(LBounds.origin.x, LBounds.origin.y, LBounds.origin.x + LBounds.size.width,
      LBounds.origin.y + LBounds.size.height);
  end
  else
  begin
    LBounds := WindowHandleToPlatform(AForm.Handle).View.frame;
    Result := RectF(LBounds.origin.x, LBounds.origin.y, LBounds.origin.x + LBounds.size.width,
      LBounds.origin.y + LBounds.size.height);
  end;
end;

function TPlatformCocoaTouch.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  Result := GetScreenScale;
end;

procedure TPlatformCocoaTouch.SetFullScreen(const AForm: TCommonCustomForm;
  const AValue: Boolean);
begin

end;

function TPlatformCocoaTouch.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
begin
  Result := not IsPopupForm(AForm);
end;

function TPlatformCocoaTouch.GetDeviceKind: TDeviceKind;
begin
  if TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPad then
    Result := TDeviceKind.iPad
  else
    Result := TDeviceKind.iPhone;
end;

function TPlatformCocoaTouch.GetDeviceClass: TDeviceInfo.TDeviceClass;
begin
  if TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPad then
    Result := TDeviceInfo.TDeviceClass.Tablet
  else
    Result := TDeviceInfo.TDeviceClass.Phone;
end;

procedure TPlatformCocoaTouch.SetScreenOrientation(
  AOrientations: TScreenOrientations);
begin
  // Not needed for iOS
end;

procedure TPlatformCocoaTouch.SetShowFullScreenIcon(const AForm: TCommonCustomForm;
  const AValue: Boolean);
begin

end;

procedure TPlatformCocoaTouch.InvalidateImmediately(const AForm: TCommonCustomForm);
begin
  if Assigned(WindowHandleToPlatform(AForm.Handle).GLView) then
    WindowHandleToPlatform(AForm.Handle).GLView.display
  else
    WindowHandleToPlatform(AForm.Handle).View.setNeedsDisplay;
end;

procedure TPlatformCocoaTouch.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
begin
  WindowHandleToPlatform(AForm.Handle).View.setNeedsDisplayInRect(CGRectMake(R.left, R.top, R.right - R.left, R.bottom - R.top))
end;

function TPlatformCocoaTouch.IsPopupForm(const AForm: TCommonCustomForm): Boolean;
begin
  Result := Assigned(AForm) and
   ((AForm.FormStyle = TFormStyle.Popup) or (AForm.Owner is TPopup));
end;

procedure TPlatformCocoaTouch.Log(const Fmt: string; const Params: array of const);
begin
  NSLog((StrToNSStr(Format(Fmt, Params)) as ILocalObject).GetObjectID);
end;

procedure TPlatformCocoaTouch.AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  if Assigned(AForm) and Assigned(AForm.Handle) then
    TFMXViewBase(WindowHandleToPlatform(AForm.Handle).Handle).AddRecognizer(ARec);
end;

function TPlatformCocoaTouch.AllocObjectHandle(const Objc: TObject): TFmxHandle;
begin
  Result := NewFmxHandle;
  TMonitor.Enter(FObjectiveCMap);
  try
    FObjectMap.Add(Result, Objc);
  finally
    TMonitor.Exit(FObjectiveCMap);
  end;
end;

function TPlatformCocoaTouch.NewFmxHandle: TFmxHandle;
begin
{$IF defined(CPUX64)}
  Result := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ELSEIF defined(CPUX86) or defined(CPUARM)}
  Result := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
end;


procedure TPlatformCocoaTouch.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
begin
  // NOP on iOS
end;

procedure TPlatformCocoaTouch.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
begin
  if FCanSetState then
  try
    FCanSetState := False;
    if AForm.Visible and (AState = TWindowState.wsMinimized) then
      AForm.Visible := False;
    if AForm.Visible then
      if IsPopupForm(AForm) then
        AForm.WindowState := TWindowState.wsNormal
      else
        AForm.WindowState := TWindowState.wsMaximized
    else
      AForm.WindowState := TWindowState.wsMinimized;
  finally
    FCanSetState := True;
  end;
end;

procedure TPlatformCocoaTouch.RegisterCanvasClasses;
begin
  FMX.Canvas.GPU.RegisterCanvasClasses;
end;

procedure TPlatformCocoaTouch.UnregisterCanvasClasses;
begin
  FMX.Canvas.GPU.UnregisterCanvasClasses;
end;

procedure TPlatformCocoaTouch.RegisterContextClasses;
begin
  FMX.Context.GLES.iOS.RegisterContextClasses;
end;

procedure TPlatformCocoaTouch.UnregisterContextClasses;
begin
  FMX.Context.GLES.iOS.UnregisterContextClasses;
end;

procedure TPlatformCocoaTouch.UpdateFormViewSize(const AForm: TCommonCustomForm);
var
  FormView: UIView;
  FormBounds: NSRect;
begin
  FormView := WindowHandleToPlatform(AForm.Handle).View;
  if not IsPopupForm(AForm) then
  begin
    FormBounds := CalculateFormViewFrame(AForm);
    FormView.setFrame(FormBounds);
    AForm.SetBounds(Round(FormBounds.origin.x), Round(FormBounds.origin.y), Round(FormBounds.size.width), Round(FormBounds.size.height));
    FormView.setNeedsDisplay;
    FormView.setNeedsLayout;
  end;
end;

procedure TPlatformCocoaTouch.UpdateStatusBarColor(const AForm: TCommonCustomForm);
var
  BackgroundColor: TAlphaColor;
begin
  if AForm is TCustomForm then
  begin
    BackgroundColor := (AForm as TCustomForm).Fill.Color;
    AppDelegate.MainWindow.RootViewController.SetStatusBarBackgroundColor(BackgroundColor);
  end;
end;

procedure TPlatformCocoaTouch.UpdateStatusBarVisibility(const AForm: TCommonCustomForm);
begin
  if not IsPopupForm(AForm) then
  begin
    if AForm.BorderStyle = TFmxFormBorderStyle.None then
      HideStatusBar
    else
      ShowStatusBar;
  end;
end;

procedure TPlatformCocoaTouch.ReleaseCapture(const AForm: TCommonCustomForm);
begin
  // NOP on iOS
end;

procedure TPlatformCocoaTouch.SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
begin
  FOnApplicationEvent := AEventHandler;
end;

procedure TPlatformCocoaTouch.SetCapture(const AForm: TCommonCustomForm);
begin
  // NOP on iOS
end;

function TPlatformCocoaTouch.GetCaretBehaviors: TCaretBehaviors;
begin
  Result := [TCaretBehavior.DisableCaretInsideWords];
end;

function TPlatformCocoaTouch.GetCaretWidth: Integer;
begin
  Result := 3;
end;

function TPlatformCocoaTouch.GetMenuShowDelay: Integer;
begin
  Result := 0;
end;

function TPlatformCocoaTouch.GetClientSize(const AForm: TCommonCustomForm): TPointF;
var
  LOrientation: Cardinal;
begin
  if (AForm = Application.MainForm) or not IsPopupForm(AForm) then
  begin
    Result := TPointF(FMainScreen.bounds.size);
    LOrientation := UIApp.statusBarOrientation;
    if not TOSVersion.Check(8) and // RSP-9664
       (LOrientation = UIDeviceOrientationLandscapeLeft) or (LOrientation = UIDeviceOrientationLandscapeRight) then
    begin
      Result := TPointF.Create(Result.Y, Result.X);
      if AForm.BorderStyle <> TFmxFormBorderStyle.None then
        Result.Y := Result.Y - PlatformCocoa.StatusBarHeight;
    end
    else
      if AForm.BorderStyle <> TFmxFormBorderStyle.None then
        Result.Y := Result.Y - PlatformCocoa.StatusBarHeight;
  end
  else
    Result := TPointF(WindowHandleToPlatform(AForm.Handle).View.bounds.size);
end;

procedure TPlatformCocoaTouch.HideStatusBar;
begin
  UIApp.setStatusBarHidden(True);
  AppDelegate.MainWindow.RootViewController.StatusBarVisible := False;
  if TOSVersion.Check(7, 0) then
    UIViewController(AppDelegate.MainWindow.RootViewController.Super).setNeedsStatusBarAppearanceUpdate;
end;

procedure TPlatformCocoaTouch.HideWindow(const AForm: TCommonCustomForm);
begin
  if (AForm <> nil) and Assigned(AForm.Handle) then
    WindowHandleToPlatform(AForm.Handle).View.setHidden(True);
  if FCanSetState then
  try
    FCanSetState := False;
    AForm.WindowState := TWindowState.wsMinimized;
  finally
    FCanSetState := True;
  end;
end;

procedure TPlatformCocoaTouch.ShowStatusBar;
begin
  UIApp.setStatusBarHidden(False);
  AppDelegate.MainWindow.RootViewController.StatusBarVisible := True;
  if TOSVersion.Check(7, 0) then
    UIViewController(AppDelegate.MainWindow.RootViewController.Super).setNeedsStatusBarAppearanceUpdate;
end;

procedure TPlatformCocoaTouch.ShowWindow(const AForm: TCommonCustomForm);
var
  FormView: UIView;
  PickerService: IFMXPickerService;
begin
  FormView := UIView(TFMXViewBase(WindowHandleToPlatform(AForm.Handle).Handle).Super);
  if not IsPopupForm(AForm) then
  begin
    UpdateStatusBarColor(AForm);
    UpdateStatusBarVisibility(AForm);
    UpdateFormViewSize(AForm);
  end;

  // Added form view to application Root view
  AppDelegate.MainWindow.RootView.addSubview(FormView);
  AppDelegate.MainWindow.RootView.layoutIfNeeded;
  AppDelegate.MainWindow.RootView.bringSubviewToFront(FormView);
  FormView.setHidden(False);
  if FCanSetState then
  try
    FCanSetState := False;
    if IsPopupForm(AForm) then
      AForm.WindowState := TWindowState.wsNormal
    else
      AForm.WindowState := TWindowState.wsMaximized;
  finally
    FCanSetState := True;
  end;
  AForm.Activate;

  // Close all opened pickers
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, PickerService) then
    PickerService.CloseAllPickers;
end;

procedure TPlatformCocoaTouch.BringToFront(const AForm: TCommonCustomForm);
var
  LFMXView: TFMXViewBase;
  LView: UIView;
begin
  if AForm.Visible then
  begin
    LFMXView := TFMXViewBase(WindowHandleToPlatform(AForm.Handle).Handle);
    LView := UIView(LFMXView.Super);
    AppDelegate.MainWindow.RootView.bringSubviewToFront(LView);
  end;
end;

procedure TPlatformCocoaTouch.SendToBack(const AForm: TCommonCustomForm);
var
  LFMXView: TFMXViewBase;
  LView: UIView;
begin
  if AForm.Visible then
  begin
    LFMXView := TFMXViewBase(WindowHandleToPlatform(AForm.Handle).Handle);
    LView := UIView(LFMXView.Super);
    AppDelegate.MainWindow.RootView.sendSubviewToBack(LView);
  end;
end;

procedure TPlatformCocoaTouch.Activate(const AForm: TCommonCustomForm);
var
  BrowserManager : IFMXWBService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXWBService, BrowserManager) then
    BrowserManager.RealignBrowsers;
  UpdateStatusBarVisibility(AForm);
  UpdateStatusBarColor(AForm);
  UpdateFormViewSize(AForm);
end;

function TPlatformCocoaTouch.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
var
  R: CGRect;
  LBounds: CGRect;
begin
  LBounds := AppDelegate.MainWindow.RootView.bounds;
  R := CGRectMake(0, 0, LBounds.size.Width, LBounds.size.Height);

  AForm.Show;
  AForm.ModalResult := mrNone;
  repeat
    if not Application.HandleMessage then
      InternalWaitMessage(0.1);
    if Application.Terminated then
      AForm.ModalResult := mrCancel
    else if AForm.ModalResult <> mrNone then
      AForm.CloseModal;
  until AForm.ModalResult <> mrNone;
  AForm.Hide;
  Result := AForm.ModalResult;
end;

function TPlatformCocoaTouch.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  case AComponent of
    TComponentKind.Button: Result := TSize.Create(73, 44);
    TComponentKind.Label: Result := TSize.Create(82, 21);
    TComponentKind.Edit: Result := TSize.Create(97, 30);
    TComponentKind.ScrollBar: Result := TSize.Create(7, 7);
    TComponentKind.ListBoxItem: Result := TSize.Create(44, 44);
  else
    Result := TSize.Create(80, 22);
  end;
end;

function TPlatformCocoaTouch.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  case AComponent of
    TComponentKind.Button: Result := True;
    TComponentKind.Label: Result := True;
    TComponentKind.Edit: Result := True;
    TComponentKind.ScrollBar: Result := True;
    TComponentKind.ListBoxItem: Result := True;
  else
    Result := False;
  end;
end;

function TPlatformCocoaTouch.GetDefaultPropertyValue(const AClassName, PropertyName: string): TValue;

  function GetSpinBoxPropertyDefaultValue: TValue;
  begin
    Result := TValue.Empty;
    if string.Compare(PropertyName, 'CanFocusOnPlusMinus', True) = 0 then
      Result := False;
  end;

begin
  Result := TValue.Empty;

  if string.Compare(AClassName, 'tcolorcombobox', True) = 0 then
    Result := TValue.From<TDropDownKind>(TDropDownKind.Native)
  else if string.Compare(AClassName, 'tspinbox', True) = 0 then
    Result := GetSpinBoxPropertyDefaultValue
  else
    Result := False;
end;

function TPlatformCocoaTouch.CalculateFormViewFrame(const AForm: TCommonCustomForm): NSRect;
var
  Orientation: NSUInteger;
  StatusBarHeight: Single;
  Tmp: Single;
begin
  if IsPopupForm(AForm) then
    Result := CGRectMake(AForm.Left, AForm.Top, AForm.Width, AForm.Height)
  else
  begin
    Result := FMainScreen.bounds;
    StatusBarHeight := 0;
    if AForm.BorderStyle <> TFmxFormBorderStyle.None then
    begin
      if UIApp.isStatusBarHidden then
        StatusBarHeight := FStatusBarHeight
      else
        StatusBarHeight := Min(UIApp.statusBarFrame.size.width, UIApp.statusBarFrame.size.height);
      Result.origin := CGPointMake(0, StatusBarHeight);
    end
    else
    begin
      Result.origin := CGPointMake(0, 0);
      Result.size := FMainScreen.bounds.size;
    end;

    Orientation := UIApp.statusBarOrientation;
    if (Orientation = UIDeviceOrientationLandscapeLeft) or (Orientation = UIDeviceOrientationLandscapeRight) then
    begin
      Tmp := Result.size.width;
      Result.size.width := Result.size.height;
      Result.size.height := Tmp;
    end;

    Result.size.height := Result.size.height - StatusBarHeight;
  end;
end;

procedure TPlatformCocoaTouch.CalculateStatusBarHeight;
begin
  FStatusBarHeight := Min(UIApp.statusBarFrame.size.width, UIApp.statusBarFrame.size.height);
end;

function TPlatformCocoaTouch.ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
begin
  Result := Point + TPointF.Create(AForm.Left, AForm.Top);
end;

function TPlatformCocoaTouch.ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
begin
  Result := Point - TPointF.Create(AForm.Left, AForm.Top);
end;

function TPlatformCocoaTouch.HandleToObject(const FmxHandle: TFmxHandle): TObject;
begin
  TMonitor.Enter(FObjectiveCMap);
  try
    ValidateHandle(FmxHandle);
    if FObjectMap.ContainsKey(FmxHandle) then
      Result := FObjectMap[FmxHandle]
    else
      Result := nil;
  finally
    TMonitor.Exit(FObjectiveCMap);
  end;
end;

function TPlatformCocoaTouch.HasFormStatusBar(const AForm: TCommonCustomForm): Boolean;
begin
  Result := (AForm <> nil) and (AForm.BorderStyle <> TFmxFormBorderStyle.None);
end;

procedure TPlatformCocoaTouch.ValidateHandle(FmxHandle: TFmxHandle);
begin
  if (FmxHandle and $F <> 0) then
    raise Exception.Create('invalid handle'); //EInvalidFmxHandle.CreateResFmt(@SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(TFmxHandle) * 2, FmxHandle]);
end;

procedure TPlatformCocoaTouch.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
begin
  if (AForm <> Application.MainForm) or IsPopupForm(AForm) then
    AForm.SetBounds(AForm.Left, AForm.Top, Round(ASize.X), Round(ASize.Y));
end;

function TPlatformCocoaTouch.GetMousePos: TPointF;
begin
  Result := FMouseCoord;
end;


{ IFMXScreenService }

function TPlatformCocoaTouch.GetScreenSize: TPointF;
var
  ScreenSize: TPointF;
begin
  ScreenSize := TPointF(MainScreen.bounds.size);
  if (not TOSVersion.Check(8)) and ( // RSP-9664
     GetScreenOrientation in [TScreenOrientation.Landscape, TScreenOrientation.InvertedLandscape]) then
    Result := TPointF.Create(ScreenSize.Y, ScreenSize.X)
  else
    Result := ScreenSize;
end;

function TPlatformCocoaTouch.GetScreenScale: Single;
begin
  if Assigned(FMainScreen) then
    Result := FMainScreen.scale
  else
    Result := 1.0;
end;

function TPlatformCocoaTouch.GetScreenOrientation: TScreenOrientation;
var
  InterfaceOrientation: UIInterfaceOrientation;
begin
  InterfaceOrientation := UIViewController(FAppDelegate.FMainWindow.RootViewController.Super).interfaceOrientation;
  case InterfaceOrientation of
    UIInterfaceOrientationLandscapeLeft:
      Result := TScreenOrientation.Landscape;
    UIInterfaceOrientationLandscapeRight:
      Result := TScreenOrientation.InvertedLandscape;
    UIInterfaceOrientationPortrait:
      Result := TScreenOrientation.Portrait;
    UIInterfaceOrientationPortraitUpsideDown:
      Result := TScreenOrientation.InvertedPortrait;
  else
    Result := TScreenOrientation.Portrait
  end;
end;

{ IFMXSystemInformationService }

function TPlatformCocoaTouch.GetScrollingBehaviour: TScrollingBehaviours;
begin
  Result := [TScrollingBehaviour.BoundsAnimation, TScrollingBehaviour.Animation, TScrollingBehaviour.AutoShowing, TScrollingBehaviour.TouchTracking];
end;

function TPlatformCocoaTouch.GetFeatures: TDeviceFeatures;
begin
  Result := [TDeviceFeature.HasTouchScreen];
end;

function TPlatformCocoaTouch.GetMinScrollThumbSize: Single;
begin
  Result := 30;
end;

{ IFMXStyleService }

function TPlatformCocoaTouch.GetSystemStyle(const Context: TFmxObject): TFmxObject;
begin
  Result := FMX.Controls.iOS.GetSystemStyle(Context);
end;

{ International ===============================================================}

function TPlatformCocoaTouch.GetCurrentLangID: string;
var
  CurrentLocale: NSLocale;
  LanguageISO: NSString;
begin
  CurrentLocale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
  LanguageISO := TNSString.Wrap(CurrentLocale.objectForKey((NSLocaleLanguageCode as ILocalObject).GetObjectID));
  Result := UTF8ToString(LanguageISO.UTF8String);
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;


function TPlatformCocoaTouch.GetLocaleFirstDayOfWeek: string;
var
  cal: NSCalendar;
  firstDay: NSUInteger;
begin
  cal:= TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
  firstDay:= Cal.firstWeekday;
  Result:= IntToStr(firstDay);
end;

{ IDeviceServices }

function TPlatformCocoaTouch.GetModel: string;
begin
  Result := UTF8ToString(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).model.UTF8String);
end;

{ Dialogs ===============================================================}

function TPlatformCocoaTouch.DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
begin
  Result := False;
end;

function TPlatformCocoaTouch.DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformCocoaTouch.PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF; AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformCocoaTouch.DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
  var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformCocoaTouch.DialogPrinterSetup: Boolean;
begin
  Result := False;
end;

function TPlatformCocoaTouch.DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
begin
  Result := False;
end;

procedure TPlatformCocoaTouch.DidFailToRegisterForRemoteNotification(const AError: NSError);
var
  LMessage:  TPushFailToRegisterMessage;
begin
  LMessage := TPushFailToRegisterMessage.Create(TPushFailToRegisterData.Create(UTF8ToString(AError.userInfo.description.UTF8String)));
  TMessageManager.DefaultManager.SendMessage(nil, LMessage);
end;

procedure TPlatformCocoaTouch.ReceivedDeviceToken(const ADeviceToken: NSString);
var
  LMessage:  TPushDeviceTokenMessage;
  LToken: string;
begin
  LToken := NSStrToStr(ADeviceToken);
  //The token comes in like "< abcdef ghij klmno >" - we want it plain: "abcdefghijklmno"
  LToken := LToken.Replace('<', '', [rfReplaceAll]).Replace('>', '', [rfReplaceAll]).Replace(' ', '', [rfReplaceAll]);
  LMessage := TPushDeviceTokenMessage.Create(TPushDeviceTokenData.Create(LToken));
  TMessageManager.DefaultManager.SendMessage(nil, LMessage);
end;

function TPlatformCocoaTouch.DictionaryToJson(ADictionary: NSDictionary): string;
var
  LData: NSData;
  LString: NSString;
  LError: NSError;
begin
  LData := TNSJSONSerialization.OCClass.dataWithJSONObject((ADictionary as ILocalObject).getObjectId, 0, Addr(LError));
  if (LData <> nil) and (LError = nil) then
  begin
    LString := TNSString.Wrap(TNSString.Alloc.initWithData(LData, NSUTF8StringEncoding));
    Result :=  NSStrToStr(LString);
  end
  else
    Result := '';
end;


procedure TPlatformCocoaTouch.ReceivedRemoteNotification( const ANotification : NSDictionary );
var
  LMessage: TPushRemoteNotificationMessage;
begin
  LMessage := TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(DictionaryToJson(ANotification)));
  TMessageManager.DefaultManager.SendMessage(nil, LMessage);
end;

procedure TPlatformCocoaTouch.ReceivedStartupNotification( const ANotification: NSDictionary);
var
  LMessage: TPushStartupNotificationMessage;
begin
  LMessage := TPushStartupNotificationMessage.Create(TPushNotificationData.Create(DictionaryToJson(ANotification)));
  TMessageManager.DefaultManager.SendMessage(nil, LMessage);
end;

function TPlatformCocoaTouch.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons;	const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt;
  const AHelpFileName: string): Integer;
var
  AlertView: UIAlertView;
  B: TMsgDlgBtn;
  Delegate: TFMXAlertViewInputDelegate;
  RunLoop: NSRunLoop;
  DistantFuture: NSDate;
  DefaultRunLoopMode: NSString;
begin
  Delegate := TFMXAlertViewInputDelegate.Create(AButtons, nil);
  Delegate.Modal := True;
  Delegate.ParentList := FInputDelegates;

  AlertView := TUIAlertView.Alloc;
  try
    AlertView := TUIAlertView.Wrap(AlertView.initWithTitle(StrToNSStr(MsgTitles[ADialogType]), StrToNSStr(AMessage),
      ILocalObject(Delegate).GetObjectID, nil, nil));
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in AButtons then
        AlertView.addButtonWithTitle(StrToNSStr(ButtonCaptions[B]));
    AlertView.Show;

    RunLoop := TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop);
    DistantFuture := TNSDate.Wrap(TNSDate.OCClass.distantFuture);
    DefaultRunLoopMode := NSDefaultRunLoopMode;
    while Delegate.Modal and RunLoop.runMode(DefaultRunLoopMode, DistantFuture) do;

    Result := Delegate.ModalResult;
  finally
    AlertView.release;
  end
end;

procedure TPlatformCocoaTouch.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt;
  const AHelpFileName: string;	const ACloseDialogProc: TInputCLoseDialogProc);
var
  AlertView: UIAlertView;
  B: TMsgDlgBtn;
  Delegate: TFMXAlertViewInputDelegate;
begin
  Delegate := TFMXAlertViewInputDelegate.Create(AButtons,
    procedure (const AResult: TModalResult; const AValues: array of string)
    begin
      if Assigned(ACloseDialogProc) then
        ACloseDialogProc(AResult);
    end);
  Delegate.ParentList := FInputDelegates;
  AlertView := TUIAlertView.Alloc;
  AlertView := TUIAlertView.Wrap(AlertView.initWithTitle(StrToNSStr(MsgTitles[ADialogType]), StrToNSStr(AMessage),
    ILocalObject(Delegate).GetObjectID, nil, nil));
  for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if B in AButtons then
      AlertView.addButtonWithTitle(StrToNSStr(ButtonCaptions[B]));
  AlertView.Show;
end;

procedure TPlatformCocoaTouch.InitializeFormFactor(FormFactor: TFormFactor);
begin
  if Pos(UpperCase(GetModel), 'IPAD') > 0 then
    FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.Landscape,
      TFormOrientation.InvertedPortrait, TFormOrientation.InvertedLandscape]
  else
    FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.Landscape,
      TFormOrientation.InvertedLandscape]
end;

function TPlatformCocoaTouch.InputQuery(const ACaption: string; const APrompts: array of string;
	var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc): Boolean;
var
  AlertView: UIAlertView;
  TextField: UITextField;
  Delegate: TFMXAlertViewInputDelegate;
  RunLoop: NSRunLoop;
  DistantFuture: NSDate;
  DefaultRunLoopMode: NSString;
begin
  if (Length(AValues) > 0) and (Length(APrompts) > 0) then
  begin
    Delegate := TFMXAlertViewInputDelegate.Create(mbYesNo, nil);
    Delegate.ParentList := FInputDelegates;
    Delegate.Modal := True;

    AlertView := TUIAlertView.Alloc;
    try
      AlertView.initWithTitle(StrToNSStr(ACaption), StrToNSStr(APrompts[0]), ILocalObject(Delegate).GetObjectID,
        StrToNSStr(SMsgDlgOK), nil);
      AlertView.addButtonWithTitle(StrToNSStr(SMsgDlgCancel));
      AlertView.setAlertViewStyle(UIAlertViewStylePlainTextInput);
      TextField := TUITextField.Wrap(AlertView.textFieldAtIndex(0));
      TextField.setText(StrToNSStr(AValues[0]));
      AlertView.show;

      RunLoop := TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop);
      DistantFuture := TNSDate.Wrap(TNSDate.OCClass.distantFuture);
      DefaultRunLoopMode := NSDefaultRunLoopMode;
      while Delegate.Modal and RunLoop.runMode(DefaultRunLoopMode, DistantFuture) do;

      Result := Delegate.ModalResult = mrYes;
      if Result then
        AValues[0] := NSStrToStr(TNSString.Wrap(TextField.text));
      finally
        AlertView.release;
      end;
    end
  else
    Result := False;
end;

procedure TPlatformCocoaTouch.InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
  const ACloseQueryProc: TInputCloseQueryProc);
var
  AlertView: UIAlertView;
  Delegate: TFMXAlertViewInputDelegate;
begin
  if (Length(ADefaultValues) > 0) and (Length(APrompts) > 0) then
  begin
    Delegate := TFMXAlertViewInputDelegate.Create(mbOKCancel, ACloseQueryProc);
    Delegate.ParentList := FInputDelegates;
    AlertView := TUIAlertView.Alloc;
    AlertView.initWithTitle(StrToNSStr(ACaption), StrToNSStr(APrompts[0]), ILocalObject(Delegate).GetObjectID,
      StrToNSStr(SMsgDlgOK), nil);
    AlertView.addButtonWithTitle(StrToNSStr(SMsgDlgCancel));
    AlertView.setAlertViewStyle(UIAlertViewStylePlainTextInput);
    TUITextField.Wrap(AlertView.textFieldAtIndex(0)).setText(StrToNSStr(ADefaultValues[0]));
    AlertView.show;
  end;
end;

{ }

procedure TPlatformCocoaTouch.DeleteObjectHandle(const FmxHandle: TFmxHandle);
begin
  ValidateHandle(FmxHandle);
  TMonitor.Enter(FObjectiveCMap);
  try
    FObjectMap.Remove(FmxHandle);
  finally
    TMonitor.Exit(FObjectiveCMap);
  end;
end;

{ IFMXListingService }

function TPlatformCocoaTouch.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [TListingHeaderBehavior.Sticky];
end;

function TPlatformCocoaTouch.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop, TListingSearchFeature.AsFirstItem];
end;

function TPlatformCocoaTouch.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [TListingTransitionFeature.EditMode, TListingTransitionFeature.DeleteButtonSlide,
    TListingTransitionFeature.PullToRefresh];
end;

function TPlatformCocoaTouch.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [TListingEditModeFeature.Delete];
end;

{ IFMXRenderingSetupService }

procedure TPlatformCocoaTouch.SubscribeRenderingSetup(const Callback: TRenderingSetupCallback; const Context: TObject);
begin
  RenderingSetupCallback := Callback;
  RenderingSetupContext := Context;
end;

procedure TPlatformCocoaTouch.UnsubscribeRenderingSetup;
begin
  RenderingSetupContext := nil;
  RenderingSetupCallback := nil;
end;

procedure TPlatformCocoaTouch.InvokeRenderingSetup(var ColorBits, DepthBits: Integer; var Stencil: Boolean;
  var Multisamples: Integer);
begin
  if Assigned(RenderingSetupCallback) then
    RenderingSetupCallback(Self, RenderingSetupContext, ColorBits, DepthBits, Stencil, Multisamples);

  // Approximate color bits to either 16 or 24.
  ColorBits := Round(Min(Max(ColorBits, 16), 24) / 8) * 8;

  // Approximate depth bits to 0, 16 or 24.
  if DepthBits > 0 then
    DepthBits := Round(Min(Max(DepthBits, 16), 24) / 8) * 8
  else
    DepthBits := 0;

  // Approximate multisamples to 0 or 4.
  if Multisamples > 0 then
    Multisamples :=  4
  else
    Multisamples := 0;
end;

function TPlatformCocoaTouch.GetSaveStateFileName(const ABlockName: string): string;
const
  Separator = '_';
var
  S: TStringBuilder;
  FilePath: string;
begin
  if FSaveStateStoragePath.IsEmpty then
    FilePath := IncludeTrailingPathDelimiter(TPath.GetTempPath)
  else
    FilePath := FSaveStateStoragePath;
  S := TStringBuilder.Create(FilePath.Length + Length(Separator) + ABlockName.Length);
  try
    S.Append(FilePath);
    S.Append(ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
    S.Append(Separator);
    S.Append(ABlockName);
    Result := S.ToString;
  finally
    S.Free;
  end;
end;

function TPlatformCocoaTouch.GetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

  procedure ReadPersistent(const AFileName: string);
  var
    S: TFileStream;
  begin
    S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      ABlockData.CopyFrom(S, S.Size);
    finally
      S.Free;
    end;
  end;

var
  LFileName: string;
begin
  if ABlockName.IsEmpty or (ABlockData = nil) then
    Exit(False);
  LFileName := GetSaveStateFileName(ABlockName);
  if not TFile.Exists(LFileName) then
    Exit(False);
  try
    ReadPersistent(LFileName);
  except
    Exit(False);
  end;
  Result := True;
end;

function TPlatformCocoaTouch.SetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

  procedure WritePersistent(const AFileName: string);
  var
    S: TFileStream;
  begin
    S := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
    try
      ABlockData.Seek(0, TSeekOrigin.soBeginning);
      S.CopyFrom(ABlockData, ABlockData.Size);
    finally
      S.Free;
    end;
  end;

var
  LFileName: string;
begin
  if ABlockName.IsEmpty then
    Exit(False);
  LFileName := GetSaveStateFileName(ABlockName);
  if (ABlockData = nil) or (ABlockData.Size < 1) then
  begin
    if TFile.Exists(LFileName) then
      TFile.Delete(LFileName);
  end
  else
    try
      WritePersistent(LFileName);
    except
      Exit(False);
    end;
  Result := True;
end;

function TPlatformCocoaTouch.GetSaveStateStoragePath: string;
begin
  Result := FSaveStateStoragePath;
end;

procedure TPlatformCocoaTouch.SetSaveStateStoragePath(const ANewPath: string);
begin
  if not ANewPath.IsEmpty then
    FSaveStateStoragePath := IncludeTrailingPathDelimiter(ANewPath)
  else
    FSaveStateStoragePath := '';
end;

function TPlatformCocoaTouch.GetSaveStateNotifications: Boolean;
begin
  Result := True;
end;

function TPlatformCocoaTouch.GetDisplayMetrics: TDeviceDisplayMetrics;
const
  IOSBasePPI = 163;
var
  ScreenSize: TPointF;
  ScreenScale: Single;
begin
  ScreenSize := GetScreenSize;
  ScreenScale := GetScreenScale;
  Result.PhysicalScreenSize := TSize.Create(Round(ScreenSize.X * ScreenScale), Round(ScreenSize.Y * ScreenScale));
  Result.LogicalScreenSize := TSize.Create(Round(ScreenSize.X), Round(ScreenSize.Y));
  if Abs(ScreenSize.X) > 0 then
    Result.AspectRatio := ScreenSize.Y / ScreenSize.X
  else
    Result.AspectRatio := 1;
  Result.PixelsPerInch := Round(IOSBasePPI * ScreenScale);
  Result.ScreenScale := ScreenScale;
  Result.FontScale := ScreenScale;
end;

{ TFMXViewController }

constructor TFMXViewController.Create;
var
  V: Pointer;
begin
  inherited;
  V := UIViewController(Super).initWithNibName(nil, nil);
  if GetObjectID <> V then
    UpdateObjectID(V);
end;

procedure CaptureSnapShotOfForm(Form: TCommonCustomForm);
var
  Image: UIImage;
  CtxRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
  ImageRef: CGImageRef;
  W, H: Integer;
  Bitmap: TBitmap;
  BitmapData: TBitmapData;
begin
  if not PlatformCocoa.FRotationViewAnimated and Assigned(Form) and Assigned(WindowHandleToPlatform(Form.Handle).GLView) then
  begin
    W := WindowHandleToPlatform(Form.Handle).GLView.drawableWidth;
    H := WindowHandleToPlatform(Form.Handle).GLView.drawableHeight;
    if (W > 0) and (H > 0) then
    begin
      Bitmap := TBitmap.Create(W, H);
      try
        Bitmap.BitmapScale := WindowHandleToPlatform(Form.Handle).View.contentScaleFactor;

        TCustomForm(Form).PaintTo(Bitmap.Canvas);

        ColorSpace := CGColorSpaceCreateDeviceRGB;
        try
          if Bitmap.Map(TMapAccess.Read, BitmapData) then
          try
            CtxRef := CGBitmapContextCreate(BitmapData.Data, W, H, 8, BitmapData.Pitch, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
            try
              ImageRef := CGBitmapContextCreateImage(CtxRef);
              try
                Image := TUIImage.Alloc;
                try
                  Image.initWithCGImage(ImageRef, Bitmap.BitmapScale, UIImageOrientationUp);
                  PlatformCocoa.FRotationView := TUIImageView.Wrap(TUIImageView.Alloc.initWithImage(Image));
                finally
                  Image.release;
                end;

                PlatformCocoa.FRotationView.setAutoresizingMask(UIViewAutoresizingFlexibleHeight or UIViewAutoresizingFlexibleWidth);
                PlatformCocoa.FRotationView.setFrame(CGRectMake(Form.Left, Form.Top, Form.Width, Form.Height));
              finally
                CGImageRelease(ImageRef);
              end;
            finally
              CGContextRelease(CtxRef);
            end;
          finally
            Bitmap.Unmap(BitmapData);
          end;
        finally
          CGColorSpaceRelease(ColorSpace);
        end;
      finally
        Bitmap.DisposeOf;
      end;
    end;
  end;
end;

procedure TFMXViewController.willAnimateRotationToInterfaceOrientation(toInterfaceOrientation: UIInterfaceOrientation;
  duration: NSTimeInterval);
var
  LFrame: NSRect;
  Form: TCommonCustomForm;
begin
  if Assigned(Screen.ActiveForm) then
    Form := Screen.ActiveForm
  else if Application.MainForm <> nil then
    Form := Application.MainForm
  else
    Form := nil;

  if Assigned(Form) then
  begin
    LFrame := WindowHandleToPlatform(Form.Handle).View.frame;

    if Form is TCustomForm then
    begin
      CaptureSnapShotOfForm(Form);
      if Assigned(PlatformCocoa.FRotationView) then
      begin
        PlatformCocoa.FRotationViewAnimated := True;
        TUIView.OCClass.setAnimationsEnabled(False);
        try
          WindowHandleToPlatform(Form.Handle).View.setAlpha(0.0);

          PlatformCocoa.FRotationView.setAlpha(1.0);
          PlatformCocoa.AppDelegate.MainWindow.RootView.insertSubview(PlatformCocoa.FRotationView, WindowHandleToPlatform(Form.Handle).View);
        finally
          TUIView.OCClass.setAnimationsEnabled(True);
        end;
        // Cross fade
        PlatformCocoa.FRotationView.setFrame(WindowHandleToPlatform(Form.Handle).View.frame);
        PlatformCocoa.FRotationView.setAlpha(0.0);
        WindowHandleToPlatform(Form.Handle).View.setAlpha(1.0);
      end;
    end;

    Form.SetBounds(round(LFrame.origin.x), round(LFrame.origin.y), round(LFrame.size.width), round(LFrame.size.height));
  end;
  if MultiDisplayIOS <> nil then
    MultiDisplayIOS.UpdateDisplayInformation;
  UpdateStatusBarFrame;
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).postNotificationName((StrToNSStr(FMXStartChangeDeviceOrientation) as ILocalObject).GetObjectID, nil);
end;

procedure TFMXViewController.didRotateFromInterfaceOrientation(fromInterfaceOrientation: UIInterfaceOrientation);
var
  LFrame: NSRect;
  Form: TCommonCustomForm;
  I: Integer;
begin
  UIViewController(Super).didRotateFromInterfaceOrientation(fromInterfaceOrientation);

  if Assigned(PlatformCocoa.FRotationView) and PlatformCocoa.FRotationViewAnimated then
  begin
    PlatformCocoa.FRotationView.removeFromSuperview;
    PlatformCocoa.FRotationView.release;
    PlatformCocoa.FRotationView := nil;
    PlatformCocoa.FRotationViewAnimated := False;
  end;

  if Assigned(Screen.ActiveForm) then
    Form := Screen.ActiveForm
  else if Application.MainForm <> nil then
    Form := Application.MainForm
  else
    Form := nil;

  if Assigned(Form) then
  begin
    LFrame := WindowHandleToPlatform(Form.Handle).View.frame;
    Form.SetBounds(round(LFrame.origin.x), round(LFrame.origin.y), round(LFrame.size.width), round(LFrame.size.height));
  end;

  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    if Form.Visible and not PlatformCocoa.IsPopupForm(Form) then
    begin
      LFrame := WindowHandleToPlatform(Form.Handle).View.frame;
      Form.SetBounds(round(LFrame.origin.x), round(LFrame.origin.y), round(LFrame.size.width), round(LFrame.size.height));
    end;
  end;
  TMessageManager.DefaultManager.SendMessage(Self, TOrientationChangedMessage.Create, True);
end;

procedure TFMXViewController.didReceiveMemoryWarning;
begin
  UIViewController(Super).didReceiveMemoryWarning;
end;

function TFMXViewController.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXViewController);
end;

function TFMXViewController.preferredStatusBarStyle: UIStatusBarStyle;
begin
  if FStatusBarLuminance < 0.5 then
    Result := UIStatusBarStyleLightContent
  else
    Result := UIStatusBarStyleDefault;
end;

function TFMXViewController.prefersStatusBarHidden: Boolean;
begin
  Result := not FStatusBarVisible;
end;

procedure TFMXViewController.SetStatusBarBackgroundColor(const ABackgroundColor: TAlphaColor);
var
  Red: Single;
  Green: Single;
  Blue: Single;
  ColorCI: CIColor;
  ColorUI: UIColor;
begin
  Red := TAlphaColorRec(ABackgroundColor).R / 255;
  Green := TAlphaColorRec(ABackgroundColor).G / 255;
  Blue := TAlphaColorRec(ABackgroundColor).B / 255;
  ColorCI := TCIColor.Wrap(TCIColor.OCClass.colorWithRed(Red, Green, Blue));
  ColorUI := TUIColor.Wrap(TUIColor.OCClass.colorWithCIColor(ColorCI));
  FStatusBarView.setBackgroundColor(ColorUI);
  FStatusBarLuminance := Luminance(ABackgroundColor);
  if TOSVersion.Check(7, 0) then
    UIViewController(Super).setNeedsStatusBarAppearanceUpdate;
end;

procedure TFMXViewController.SetStatusBarVisible(const Value: Boolean);
begin
  FStatusBarVisible := Value;
  if TOSVersion.Check(7, 0) then
  begin
    UpdateStatusBarFrame;
    FStatusBarView.setHidden(not Value);
    if Value then
      TUIViewController.Wrap(GetObjectID).view.bringSubviewToFront(FStatusBarView);
  end;
end;

function TFMXViewController.shouldAutorotate: Boolean;
begin
   Result := True;
end;

function TFMXViewController.shouldAutorotateToInterfaceOrientation(AinterfaceOrientation: UIInterfaceOrientation): Boolean;
begin
  case AinterfaceOrientation of
    UIInterfaceOrientationLandscapeLeft:
      Result := TFormOrientation.Landscape in Application.FormFactor.Orientations;
    UIInterfaceOrientationLandscapeRight:
      Result := TFormOrientation.InvertedLandscape in Application.FormFactor.Orientations;
    UIInterfaceOrientationPortrait:
      Result := TFormOrientation.Portrait in Application.FormFactor.Orientations;
    UIInterfaceOrientationPortraitUpsideDown:
      Result := TFormOrientation.InvertedPortrait in Application.FormFactor.Orientations;
  else
    Result := False;
  end;
end;

function TFMXViewController.supportedInterfaceOrientations: NSUInteger;
begin
  Result := 0;
  if Assigned(Application) then
  begin
    if TFormOrientation.Landscape in Application.FormFactor.Orientations then
      Result := Result or UIInterfaceOrientationMaskLandscapeLeft;
    if TFormOrientation.InvertedLandscape in Application.FormFactor.Orientations then
      Result := Result or UIInterfaceOrientationMaskLandscapeRight;
    if TFormOrientation.Portrait in Application.FormFactor.Orientations then
      Result := Result or UIInterfaceOrientationMaskPortrait;
    if TFormOrientation.InvertedPortrait in Application.FormFactor.Orientations then
      Result := Result or UIInterfaceOrientationMaskPortraitUpsideDown;
  end;
end;

procedure TFMXViewController.UpdateStatusBarFrame;
begin
  StatusBarView.setFrame(CGRectMake(0, 0, Screen.Size.Width, PlatformCocoa.StatusBarHeight));
end;

procedure TFMXViewController.viewDidAppear(animated: Boolean);
begin
  UIViewController(Super).viewDidAppear(animated);
end;

procedure TFMXViewController.viewDidLayoutSubviews;
var
  LOrientation: Cardinal;
begin
  if Application.MainForm = nil then
  begin
    LOrientation := PlatformCocoa.UIApp.statusBarOrientation;
    case LOrientation of
      UIDeviceOrientationLandscapeLeft:
        PlatformCocoa.AppDelegate.MainWindow.RootView.setTransform(CGAffineTransform.Create(0, 1, -1, 0, 0, 0));
      UIDeviceOrientationLandscapeRight:
        PlatformCocoa.AppDelegate.MainWindow.RootView.setTransform(CGAffineTransform.Create(0, -1, 1, 0, 0, 0));
    end;
  end;
  UIViewController(Super).viewDidLayoutSubviews;
end;

procedure TFMXViewController.viewWillAppear(animated: Boolean);
var
  Form: TCommonCustomForm;
begin
  UIViewController(Super).viewWillAppear(animated);
  // Update View form frame
  Form := Screen.ActiveForm;
  if Form <> nil then
  begin
    PlatformCocoa.UpdateStatusBarColor(Form);
    PlatformCocoa.UpdateStatusBarVisibility(Form);
    PlatformCocoa.UpdateFormViewSize(Form);
    UpdateStatusBarFrame;
  end;
end;

procedure TFMXViewController.viewWillDisappear(animated: Boolean);
begin
  UIViewController(Super).viewWillDisappear(animated);
end;

{ TFMXWindow }

constructor TFMXWindow.Create(const ABounds: NSRect);
var
  V: Pointer;
begin
  inherited Create;
  V :=  UIWindow(Super).initWithFrame(ABounds);
  if GetObjectID <> V then
    UpdateObjectID(V);
end;

function TFMXWindow.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXWindow);
end;

{ TFMXViewBase }

function CorrectLocationInView(const Touch: UITouch; const View: UIView; const Form: TCommonCustomForm): CGPoint; overload;
begin
  Result := Touch.locationInView(View);
end;

function CorrectLocationInView(const GestureRecognizer: UIGestureRecognizer; const View: UIView; const Form: TCommonCustomForm): CGPoint; overload;
begin
  Result := GestureRecognizer.locationInView(View);
end;

procedure TFMXViewBase.AddRecognizer(const Gesture: TInteractiveGesture);
var
  TwoFingerTapRecognizer: UITapGestureRecognizer;
  RotateRecognizer: UIRotationGestureRecognizer;
  ZoomRecognizer: UIPinchGestureRecognizer;
  PanRecognizer: UIPanGestureRecognizer;
  LongTapRecognizer: UILongPressGestureRecognizer;
begin
  case Gesture of
    TInteractiveGesture.Zoom:
      begin
        ZoomRecognizer := TUIPinchGestureRecognizer.Alloc;
        ZoomRecognizer := TUIPinchGestureRecognizer.Wrap(ZoomRecognizer.initWithTarget(GetObjectID, sel_getUid('HandleZoom:')));
        ZoomRecognizer.setDelaysTouchesBegan(False);
        ZoomRecognizer.setCancelsTouchesInView(True);
        ZoomRecognizer.setDelegate(GetObjectID);
        UIView(Super).addGestureRecognizer(ZoomRecognizer);
        ZoomRecognizer.release;
      end;
    TInteractiveGesture.Rotate:
      begin
        RotateRecognizer := TUIRotationGestureRecognizer.Alloc;
        RotateRecognizer := TUIRotationGestureRecognizer.Wrap(RotateRecognizer.initWithTarget(GetObjectID, sel_getUid('HandleRotate:')));
        RotateRecognizer.setDelaysTouchesBegan(False);
        RotateRecognizer.setCancelsTouchesInView(True);
        RotateRecognizer.setDelegate(GetObjectID);
        UIView(Super).addGestureRecognizer(RotateRecognizer);
        RotateRecognizer.release;
      end;
    TInteractiveGesture.Pan:
      begin
        PanRecognizer := TUIPanGestureRecognizer.Alloc;
        PanRecognizer := TUIPanGestureRecognizer.Wrap(PanRecognizer.initWithTarget(GetObjectID, sel_getUid('HandlePan:')));
        PanRecognizer.setMinimumNumberOfTouches(1);
        PanRecognizer.setMaximumNumberOfTouches(2);
        PanRecognizer.setDelaysTouchesBegan(False);
        PanRecognizer.setCancelsTouchesInView(False);
        PanRecognizer.setDelegate(GetObjectID);
        UIView(Super).addGestureRecognizer(PanRecognizer);
        PanRecognizer.release;
      end;
    TInteractiveGesture.TwoFingerTap:
      begin
        TwoFingerTapRecognizer := TUITapGestureRecognizer.Alloc;
        TwoFingerTapRecognizer := TUITapGestureRecognizer.Wrap(TwoFingerTapRecognizer.initWithTarget(GetObjectID, sel_getUid('HandleTwoFingerTap:')));
        TwoFingerTapRecognizer.setNumberOfTapsRequired(1);
        TwoFingerTapRecognizer.setNumberOfTouchesRequired(2);
        TwoFingerTapRecognizer.setDelaysTouchesBegan(False);
        TwoFingerTapRecognizer.setCancelsTouchesInView(True);
        TwoFingerTapRecognizer.setDelegate(GetObjectID);
        UIView(Super).addGestureRecognizer(TwoFingerTapRecognizer);
        TwoFingerTapRecognizer.release;
      end;
    TInteractiveGesture.LongTap:
      begin
        LongTapRecognizer := TUILongPressGestureRecognizer.Alloc;
        LongTapRecognizer := TUILongPressGestureRecognizer.Wrap(LongTapRecognizer.initWithTarget(GetObjectID, sel_getUid('LongTap:')));
        LongTapRecognizer.setDelaysTouchesBegan(False);
        LongTapRecognizer.setCancelsTouchesInView(True);
        LongTapRecognizer.setDelegate(GetObjectID);
        UIView(Super).addGestureRecognizer(LongTapRecognizer);
        LongTapRecognizer.release;
      end;
  end;
end;

function TFMXViewBase.autocapitalizationType: UITextAutocapitalizationType;
begin
  if not FPassword then
    Result := UITextAutocapitalizationTypeSentences
  else
    Result := UITextAutocapitalizationTypeNone;
end;

function TFMXViewBase.autocorrectionType: UITextAutocorrectionType;
begin
  if FKeyboardType <> TVirtualKeyboardType.Default then
    Result := UITextAutocorrectionTypeNo
  else
    Result := UITextAutocorrectionTypeDefault;
end;

function TFMXViewBase.baseWritingDirectionForPosition(position: UITextPosition;
  inDirection: UITextStorageDirection): UITextWritingDirection;
begin
  Result := UITextWritingDirectionLeftToRight;
end;

function TFMXViewBase.beginningOfDocument: UITextPosition;
begin
  Result := nil;
end;

function TFMXViewBase.canBecomeFirstResponder: Boolean;
begin
  FResigned := False;
  Result := True;
end;

function TFMXViewBase.canPerformAction(action: SEL; withSender: Pointer): Boolean;
begin
  Result := FContextMenu.CanPerformAction(action);
end;

function TFMXViewBase.canResignFirstResponder: Boolean;
begin
  FResigned := True;
  Result := True;
end;

function TFMXViewBase.caretRectForPosition(position: UITextPosition): CGRect;
begin
  Result := CGRectMake(0, 0, 0, 0);
end;

function TFMXViewBase.characterRangeAtPoint(point: CGPoint): UITextRange;
begin
  Result := nil;
end;

function TFMXViewBase.characterRangeByExtendingPosition(position: UITextPosition; inDirection: UITextLayoutDirection): UITextRange;
begin
  Result := nil;
end;

function TFMXViewBase.closestPositionToPoint(point: CGPoint; withinRange: UITextRange): UITextPosition;
begin
  Result := nil;
end;

function TFMXViewBase.closestPositionToPoint(point: CGPoint): UITextPosition;
begin
  Result := nil;
end;

function TFMXViewBase.comparePosition(position, toPosition: UITextPosition): NSComparisonResult;
begin
  Result := NSOrderedSame;
end;

procedure TFMXViewBase.copy(Sender: id);
begin
  FContextMenu.Copy;
end;

constructor TFMXViewBase.Create(const AForm: TCommonCustomForm);
begin
  inherited Create;
  FForm := AForm;
  FContextMenu := TFMXTextEditActionsMenu.Create(UIView(Super));
  FSelectRange := NSMakeRange(0, 0);
  FMarkRange := NSMakeRange(NSNotFound, 0);
  FMultiTouchManager := nil;
end;

procedure TFMXViewBase.cut(Sender: id);
begin
  FContextMenu.Cut;
end;

procedure TFMXViewBase.DblTap(X, Y: Single);

  function GetTappedControl: TFmxObject;
  var
    Obj: IControl;
  begin
    Obj := IControl(FForm.ObjectAtPoint(FForm.ClientToScreen(PointF(X, Y))));
    if Obj <> nil then
      Result := Obj.GetObject
    else
      Result := FForm;
  end;

var
  EventInfo: TGestureEventInfo;
  TextInput: ITextInput;
  LGObj: IGestureControl;
begin
  FGestureControl := GetTappedControl;
  if Supports(FGestureControl, IGestureControl, LGObj) then
    FGestureControl := LGObj.GetFirstControlWithGesture(TInteractiveGesture.DoubleTap)
  else
    FGestureControl := nil;

  if FGestureControl <> nil then
  begin
    FillChar(EventInfo, Sizeof(EventInfo), 0);
    EventInfo.Location := TPointF.Create(X, Y);
    EventInfo.GestureID := igiDoubleTap;

    // send message to the control
    if Supports(FGestureControl, IGestureControl, LGObj) then
      LGObj.CMGesture(EventInfo);

    if Supports(FGestureControl, ITextInput, TextInput) then
    begin
      TextInput := nil;
      FIgnorePosition := True;
      FLastContextMenuVisibility := False;
      FClickedAnotherControl := False;
      FChangedFocusedControl := False;
      ShowContextMenu;
    end;
  end;
end;

procedure TFMXViewBase.DefineFocusControl;
var
  FocusedControl: TControl;
begin
  if Assigned(Form.Focused) and (Form.Focused.GetObject is TControl) then
    FocusedControl := Form.Focused.GetObject as TControl
  else
    FocusedControl := nil;

  if Assigned(Form.Focused) and (FContextMenu.Control <> FocusedControl) then
    FCarretPositionChanged := True;
  FChangedFocusedControl := FContextMenu.Control <> FocusedControl;
  FContextMenu.Control := FocusedControl;
end;

procedure TFMXViewBase.deleteBackward;
begin
  if FMarkRange.location <> NSNotFound then
  begin
    FText := FText.Remove(FMarkRange.location, FMarkRange.length);
    FSelectRange.location := FMarkRange.location;
    FSelectRange.length := 0;
    FMarkRange := NSMakeRange(NSNotFound, 0);
  end
  else
    if FSelectRange.length > 0 then
    begin
      FText := FText.Remove(FSelectRange.location, FSelectRange.length);
      FSelectRange.length := 0;
    end
    else
    begin
      if FSelectRange.location > 0 then
        FSelectRange.location := FSelectRange.location - 1;
      FSelectRange.length := 0;
      FText := FText.Remove(FMarkRange.location, 1);
    end;

  FormKeyPress(#0, vkBack, []);

  PlatformCocoa.ResetIdleTimer;
end;

destructor TFMXViewBase.Destroy;
begin
  iOSapi.Foundation.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget(Self.GetObjectID);
  FreeAndNil(FContextMenu);
  FreeAndNil(FMultiTouchManager);
  NSObject(Super).release;
  if FMarkedTextRange <> nil then
    FMarkedTextRange.release;
  inherited Destroy;
end;

function TFMXViewBase.enablesReturnKeyAutomatically: Boolean;
begin
  Result := False;
end;

function TFMXViewBase.endOfDocument: UITextPosition;
begin
  Result := nil;
end;

function TFMXViewBase.firstRectForRange(range: UITextRange): CGRect;
var
  glyphRect: CGRect;
  R: TRectF;
  TSObj: ITextInput;
begin
  if Form.Focused <> nil then
    if Supports(Form.Focused, ITextInput, TSObj) then
      R := TRectF.Create(TSObj.GetTargetClausePointF)
    else
      R := TControl(Form.Focused.GetObject).AbsoluteRect
  else
    R := TRectF.Empty;

  glyphRect := CGRectMake(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  Result := glyphRect;
end;

function TFMXViewBase.gestureRecognizer(gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch): Boolean;
var
  Obj: IControl;
  GestureObj: IGestureControl;
  LControl: TComponent;
  LPoint: NSPoint;
  GestureBeingRecognized: TInteractiveGesture;
begin
  Result := False;
  LControl := nil;

  if gestureRecognizer.isKindOfClass(objc_getClass('UIPanGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.Pan
  else if gestureRecognizer.isKindOfClass(objc_getClass('UIRotationGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.Rotate
  else if gestureRecognizer.isKindOfClass(objc_getClass('UIPinchGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.Zoom
  else if gestureRecognizer.isKindOfClass(objc_getClass('UITapGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.TwoFingerTap
  else if gestureRecognizer.isKindOfClass(objc_getClass('UILongPressGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.LongTap
  else
    Exit;

  LPoint := CorrectLocationInView(shouldReceiveTouch, UIView(Super), Form);

  Obj := IControl(FForm.ObjectAtPoint(FForm.ClientToScreen(TPointF.Create(LPoint.X, LPoint.Y))));
  if Obj <> nil then
    LControl := Obj.GetObject
  else
    LControl := FForm;

  if Supports(LControl, IGestureControl, GestureObj) then
    LControl := GestureObj.GetFirstControlWithGesture(GestureBeingRecognized)
  else
    LControl := nil;

  if LControl <> nil then
    Result := True;
end;

function TFMXViewBase.gestureRecognizer(gestureRecognizer, shouldRecognizeSimultaneouslyWithGestureRecognizer: UIGestureRecognizer): Boolean;
begin
  Result := True;
end;

function TFMXViewBase.gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean;
begin
  Result := True;
end;

procedure TFMXViewBase.HandlePan(gestureRecognizer: UIPanGestureRecognizer);
var
  LPoint, LPoint2: NSPoint;
  Distance: Integer;
  State: TInteractiveGestureFlags;
begin
  State := [];
  Distance := 0;
  case gestureRecognizer.state of
    UIGestureRecognizerStateBegan:
      begin
        State := [TInteractiveGestureFlag.gfBegin];
        LPoint := CorrectLocationInView(gestureRecognizer, UIView(Super), Form);
        gestureRecognizer.setTranslation(LPoint, UIView(Super));
      end;
    UIGestureRecognizerStateEnded:
      State := [TInteractiveGestureFlag.gfEnd];
    UIGestureRecognizerStateCancelled:
      State := [TInteractiveGestureFlag.gfEnd];
  end;

  if gestureRecognizer.numberOfTouches = 2 then
  begin
    LPoint := gestureRecognizer.locationOfTouch(0, UIView(Super));
    LPoint2 := gestureRecognizer.locationOfTouch(1, UIView(Super));
    Distance := Round(Sqrt(Sqr(LPoint.X - LPoint2.X) + Sqr(LPoint.Y - LPoint2.Y)));
  end;
  LPoint := gestureRecognizer.translationInView(UIView(Super));

  MultiTouchManager.HandlePan(TPointF.Create(LPoint.X, LPoint.Y), Distance, State, gestureRecognizer.numberOfTouches);
end;

procedure TFMXViewBase.HandleRotate(gestureRecognizer: UIRotationGestureRecognizer);
var
  State: TInteractiveGestureFlags;
  LPoint: NSPoint;
begin
  LPoint := CorrectLocationInView(gestureRecognizer, UIView(Super), Form);

  State := [];
  case gestureRecognizer.state of
    UIGestureRecognizerStateBegan:
      State := [TInteractiveGestureFlag.gfBegin];
    UIGestureRecognizerStateEnded:
      State := [TInteractiveGestureFlag.gfEnd];
    UIGestureRecognizerStateCancelled:
      State := [TInteractiveGestureFlag.gfEnd];
  end;

  MultiTouchManager.HandleRotate(TPointF.Create(LPoint.X, LPoint.Y), -gestureRecognizer.Rotation, State, gestureRecognizer.numberOfTouches);
end;

procedure TFMXViewBase.HandleTwoFingerTap(gestureRecognizer: UITapGestureRecognizer);
var
  LPoint: NSPoint;
  State: TInteractiveGestureFlags;
begin
  LPoint := CorrectLocationInView(gestureRecognizer, UIView(Super), Form);

  State := [];
  case gestureRecognizer.state of
    UIGestureRecognizerStateBegan:
      State := [TInteractiveGestureFlag.gfBegin];
    UIGestureRecognizerStateEnded:
      State := [TInteractiveGestureFlag.gfEnd];
    UIGestureRecognizerStateCancelled:
      State := [TInteractiveGestureFlag.gfEnd];
  end;

  MultiTouchManager.HandleTwoFingerTap(TPointF.Create(LPoint.X, LPoint.Y), State, gestureRecognizer.numberOfTouches);
end;

procedure TFMXViewBase.HandleZoom(gestureRecognizer: UIPinchGestureRecognizer);
var
  LPoint, LPoint2: NSPoint;
  State: TInteractivegestureFlags;
  Distance: Integer;
begin
  State := [];
  Distance := 0;
  case gestureRecognizer.state of
    UIGestureRecognizerStateBegan:
      State := [TInteractiveGestureFlag.gfBegin];
    UIGestureRecognizerStateEnded:
      State := [TInteractiveGestureFlag.gfEnd];
    UIGestureRecognizerStateCancelled:
      State := [TInteractiveGestureFlag.gfEnd];
  end;

  if gestureRecognizer.numberOfTouches = 2 then
  begin
    LPoint := gestureRecognizer.locationOfTouch(0, UIView(Super));
    LPoint2 := gestureRecognizer.locationOfTouch(1, UIView(Super));
    Distance := Round(Sqrt(Sqr(LPoint.X - LPoint2.X) + Sqr(LPoint.Y - LPoint2.Y)));
  end;
  LPoint := CorrectLocationInView(gestureRecognizer, UIView(Super), Form);

  MultiTouchManager.HandleZoom(TPointF.Create(LPoint.X, LPoint.Y), Distance, State, gestureRecognizer.numberOfTouches);
end;

function TFMXViewBase.hasText: Boolean;
begin
  Result := Length(FText) > 0;
end;


procedure TFMXViewBase.HideContextMenu;
begin
  if FContextMenu.ShowSpellItems then
    FContextMenu.HideHighlightSpell;
  FContextMenu.Hide;
end;

function TFMXViewBase.inputDelegate: Pointer;
begin
  Result := nil;
end;

procedure TFMXViewBase.insertText(text: NSString);
var
  I: Integer;
  Ch: Char;
  Str: string;
begin
  if FMarkRange.location <> NSNotFound then
  begin
    TTextServiceCocoa((Form.Focused as ITextInput).GetTextService).InternalSetMarkedText(EmptyStr);
    TTextServiceCocoa((Form.Focused as ITextInput).GetTextService).InternalEndIMEInput;
    FText := FText.Substring(0, FMarkRange.location) + UTF8ToUnicodeString(text.UTF8String) + FText.Substring(FMarkRange.location);
    FSelectRange.location := FMarkRange.location + text.length;
    FSelectRange.length := 0;
    FMarkRange := NSMakeRange(NSNotFound, 0);
  end
  else
    if FSelectRange.length > 0 then
    begin
      FText := FText.Substring(0, FSelectRange.location) + UTF8ToUnicodeString(text.UTF8String) + FText.Substring(FSelectRange.location);
      FSelectRange.length := 0;
      FSelectRange.location := FSelectRange.location + text.length;
    end
    else
    begin
      FText := FText.Insert(FSelectRange.location, UTF8ToUnicodeString(text.UTF8String));
      FSelectRange.location := FSelectRange.location + text.length;
    end;

  if text.length > 0 then
  begin
    Str := UTF8ToUnicodeString(text.UTF8String);
    for I := 0 to Str.Length - 1 do
    begin
      Ch := Str.Chars[I];
      if Ch = #10 then
        FormKeyPress(#0, vkReturn, [])
      else
        FormKeyPress(Ch, 0, []);
    end;
  end;

  PlatformCocoa.ResetIdleTimer;
end;

function TFMXViewBase.isAccessibilityElement: Boolean;
begin
  Result := False;
end;

function TFMXViewBase.isFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXViewBase.isSecureTextEntry: Boolean;
begin
  Result := False;
end;

function TFMXViewBase.keyboardAppearance: UIKeyboardAppearance;
begin
  Result := UIKeyboardAppearanceDefault;
end;

function TFMXViewBase.keyboardType: UIKeyboardType;
begin
  case FKeyboardType of
    TVirtualKeyboardType.Default:               Result := UIKeyboardTypeDefault;
    TVirtualKeyboardType.NumbersAndPunctuation: Result := UIKeyboardTypeNumbersAndPunctuation;
    TVirtualKeyboardType.NumberPad:             Result := UIKeyboardTypeNumberPad;
    TVirtualKeyboardType.PhonePad:              Result := UIKeyboardTypePhonePad;
    TVirtualKeyboardType.Alphabet:              Result := UIKeyboardTypeDefault;
    TVirtualKeyboardType.URL:                   Result := UIKeyboardTypeURL;
    TVirtualKeyboardType.NamePhonePad:          Result := UIKeyboardTypeNamePhonePad;
    TVirtualKeyboardType.EmailAddress:          Result := UIKeyboardTypeEmailAddress;
  else
    Result := UIKeyboardTypeDefault;
  end;
end;

procedure TFMXViewBase.FormKeyPress(Ch: Char; Key: Word; Shift: TShiftState);
var
  LCh: Char;
  LKey: Word;
begin
  LCh := Ch;
  LKey := Key;
  try
    Form.KeyDown(LKey, LCh, Shift);
  except
    Application.HandleException(Form);
  end;

  LCh := Ch;
  LKey := Key;
  try
    Form.KeyUp(LKey, LCh, Shift);
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.LongTap(gestureRecognizer: UILongPressGestureRecognizer);
var
  TouchPoint: NSPoint;
  EventInfo: TGestureEventInfo;
  Handled: Boolean;
  Obj: IControl;
  GestureObj: IGestureControl;
begin
  TouchPoint := CorrectLocationInView(gestureRecognizer, UIView(Super), Form);

  if gestureRecognizer.state = UIGestureRecognizerStateBegan then
  begin
    // Get the control from "under" the gesture.
    Obj := Form.ObjectAtPoint(FForm.ClientToScreen(PointF(TouchPoint.X, TouchPoint.Y)));
    if Obj <> nil then
      FGestureControl := Obj.GetObject
    else
      FGestureControl := Form;

    if Supports(FGestureControl, IGestureControl, GestureObj) then
      FGestureControl := GestureObj.GetFirstControlWithGesture(TInteractiveGesture.LongTap);
  end;

  if FGestureControl <> nil then
  begin
    Handled := True;
    FillChar(EventInfo, Sizeof(EventInfo), 0);
    EventInfo.Location := TPointF.Create(TouchPoint.X, TouchPoint.Y);
    EventInfo.GestureID := igiLongTap;
    // set flags
    if gestureRecognizer.state = UIGestureRecognizerStateBegan then
      EventInfo.Flags := [TInteractiveGestureFlag.gfBegin]
    else if ((gestureRecognizer.state = UIGestureRecognizerStateEnded) or (gestureRecognizer.state = UIGestureRecognizerStateCancelled)) then
      EventInfo.Flags := [TInteractiveGestureFlag.gfEnd];

    HideContextMenu;
    DoLMouseDown(TouchPoint.x, TouchPoint.y);
    DefineFocusControl;
    // send message to the control
    if Supports(FGestureControl, IGestureControl, GestureObj) then
        GestureObj.CMGesture(EventInfo);

    if gestureRecognizer.state = UIGestureRecognizerStateChanged then
      Form.MouseMove([ssTouch], TouchPoint.X, TouchPoint.Y);

    if gestureRecognizer.state = UIGestureRecognizerStateEnded then
    begin
      FContextMenu.ShowSpellItems := False;
      FContextMenu.Show;
    end;

    if ((gestureRecognizer.state = UIGestureRecognizerStateEnded) or (gestureRecognizer.state = UIGestureRecognizerStateCancelled)) then
    begin
      FGestureControl := nil;
      if Handled then
        DoLMouseUp(TouchPoint.X, TouchPoint.Y, False)
      else
        DoLMouseUp(TouchPoint.X, TouchPoint.Y);
    end;
  end;
end;

function TFMXViewBase.markedTextRange: UITextRange;
begin
  if FMarkRange.Length = 0 then
    Result := nil
  else
  begin
    if FMarkedTextRange = nil then
      FMarkedTextRange := TUITextRange.Create;
    Result := FMarkedTextRange;
  end;
end;

function TFMXViewBase.markedTextStyle: NSDictionary;
begin
  Result := nil;
end;

function TFMXViewBase.offsetFromPosition(from, toPosition: UITextPosition): NSInteger;
begin
  Result := 0;
end;

procedure TFMXViewBase.paste(Sender: id);
begin
  try
    FContextMenu.Paste;
  except
    Application.HandleException(Self);
  end;
end;

function TFMXViewBase.positionFromPosition(position: UITextPosition;
  offset: NSInteger): UITextPosition;
begin
  Result := nil;
end;

function TFMXViewBase.positionFromPosition(position: UITextPosition;
  inDirection: UITextLayoutDirection; offset: NSInteger): UITextPosition;
begin
  Result := nil;
{  case inDirection of
    UITextLayoutDirectionRight: Result := TUITextPosition.alloc.initWith_Value(TUITextPosition(position).FValue + offset);
    UITextLayoutDirectionLeft: Result := TUITextPosition.alloc.initWith_Value(TUITextPosition(position).FValue + offset);
    UITextLayoutDirectionUp: Result := TUITextPosition.alloc.initWith_Value(TUITextPosition(position).FValue + offset);
    UITextLayoutDirectionDown: Result := TUITextPosition.alloc.initWith_Value(TUITextPosition(position).FValue + offset);
  end; }
end;

function TFMXViewBase.positionWithinRange(range: UITextRange; farthestInDirection: UITextLayoutDirection): UITextPosition;
begin
  Result := nil;
end;

procedure TFMXViewBase.replaceRange(range: UITextRange; withText: NSString);
{var
  i: Integer;
  K: Word;
  Ch: System.WideChar;
  Str: string;}
begin
  unmarkText;

  PlatformCocoa.ResetIdleTimer;
{  if FText.length > 0 then
  begin
    for i := 0 to FText.length - 1 do
    begin
      K := 0;
      Ch := FText[i];
      Form.KeyDown(K, Ch, []);
    end;
  end;}
end;

function TFMXViewBase.returnKeyType: UIReturnKeyType;
begin
  case FReturnKeyType of
    TReturnKeyType.Default:
      Result := UIReturnKeyDefault;
    TReturnKeyType.Done:
      Result := UIReturnKeyDone;
    TReturnKeyType.Go:
      Result := UIReturnKeyGo;
    TReturnKeyType.Next:
      Result := UIReturnKeyNext;
    TReturnKeyType.Search:
      Result := UIReturnKeySearch;
    TReturnKeyType.Send:
      Result := UIReturnKeySend;
  else
    Result := UIReturnKeyDefault;
  end;
end;

procedure TFMXViewBase.select(Sender: id);
begin
  FContextMenu.Select;
end;

procedure TFMXViewBase.selectAll(Sender: id);
begin
  FContextMenu.SelectAll;
end;

function TFMXViewBase.selectedTextRange: UITextRange;
begin
  Result := nil;
end;

procedure TFMXViewBase.SendTouches(const ATouches: NSSet; Action: TTouchAction; const Control: IControl);
var
  Touch: UITouch;
  TouchesArray: NSArray;
  FMXTouches: TTouches;
  FMXTouch: TTouch;
  Point: CGPoint;
  I: Integer;
begin
  if (ATouches <> nil) and (ATouches.Count > 0) then
  begin
    TouchesArray := ATouches.allObjects;
    SetLength(FMXTouches, TouchesArray.Count);
    for I := 0 to TouchesArray.Count - 1 do
    begin
      Touch := TUITouch.Wrap(TouchesArray.objectAtIndex(I));
      Point := CorrectLocationInView(Touch, UIView(Super), Form);
      FMXTouch.Location.X := Point.x;
      FMXTouch.Location.Y := Point.y;
      FMXTouches[I] := FMXTouch;

      if ((Action = TTouchAction.Move) and ((Touch.phase = UITouchPhaseBegan) or
        (TouchesArray.Count > FNoOfTouches))) then
        Action := TTouchAction.Down;
    end;
    FNoOfTouches := TouchesArray.Count;
  end;

  MultiTouchManager.HandleTouches(FMXTouches, Action, Control);
end;

procedure TFMXViewBase.setAutocapitalizationType(autocapitalizationType: UITextAutocapitalizationType);
begin
end;

procedure TFMXViewBase.setAutocorrectionType(autocorrectionType: UITextAutocorrectionType);
begin
end;

procedure TFMXViewBase.setBaseWritingDirection(writingDirection: UITextWritingDirection; forRange: UITextRange);
begin
end;

procedure TFMXViewBase.setEnablesReturnKeyAutomatically(enablesReturnKeyAutomatically: Boolean);
begin
end;

procedure TFMXViewBase.setInputDelegate(inputDelegate: Pointer);
begin
end;

procedure TFMXViewBase.setKeyboardAppearance(keyboardAppearance: UIKeyboardAppearance);
begin
end;

procedure TFMXViewBase.setKeyboardType(keyboardType: UIKeyboardType);
begin
end;

procedure TFMXViewBase.setMarkedText(markedText: NSString;  selectedRange: NSRange);
var
  TSC: ITextInput;
begin
  if not Assigned(Form.Focused) or not Supports(Form.Focused, ITextInput, TSC) or FResigned or (markedText.length = 0) then
    Exit;

  if FMarkRange.location <> NSNotFound then
  begin
    if not FText.IsEmpty then
    begin
      FText := FText.Substring(0, FMarkRange.location) + UTF8ToUnicodeString(markedText.UTF8String) + FText.Substring(FMarkRange.location + FMarkRange.length)
    end
    else
      FText := UTF8ToUnicodeString(markedText.UTF8String);
    FMarkRange.length := markedText.length;
  end
  else
    if FSelectRange.length > 0 then
    begin
      FText := FText.Substring(0, FSelectRange.location) +
        UTF8ToUnicodeString(markedText.UTF8String) +
        FText.Substring(FSelectRange.location + FSelectRange.length);
      FMarkRange.location := FSelectRange.location;
      FMarkRange.length := markedText.length;
    end
    else
    try
      FText := FText.Insert(FSelectRange.location, UTF8ToUnicodeString(markedText.UTF8String));
      FMarkRange.location := FSelectRange.location;
      FMarkRange.length := markedText.length;
      if Assigned(Form.Focused) and Supports(Form.Focused, ITextInput, TSC) then
        TTextServiceCocoa(TSC.GetTextService).InternalStartIMEInput;
    except
      Application.HandleException(Self);
    end;

  FSelectRange := NSMakeRange(selectedRange.location + FMarkRange.location, selectedRange.length);
  TTextServiceCocoa(TSC.GetTextService).SetCursorShift(markedText.length);
  TTextServiceCocoa(TSC.GetTextService).InternalSetMarkedText(UTF8ToString(markedText.UTF8String));
  FMarkText := UTF8ToString(markedText.UTF8String);
end;

procedure TFMXViewBase.setMarkedTextStyle(markedTextStyle: NSDictionary);
begin
end;

procedure TFMXViewBase.setReturnKeyType(returnKeyType: UIReturnKeyType);
begin
end;

procedure TFMXViewBase.setSecureTextEntry(secureTextEntry: Boolean);
begin
end;

procedure TFMXViewBase.setSelectedTextRange(selectedTextRange: UITextRange);
begin
end;

procedure TFMXViewBase.setSpellCheckingType(spellCheckingType: Integer);
begin
end;

procedure TFMXViewBase.ShowContextMenu;

  procedure DefineSelectionStates;
  var
    TextInput: ITextInput;
  begin
    if not FIgnorePosition and FContextMenu.HasControl and Assigned(Form) and Assigned(Form.Focused) and (Form.Focused.GetObject.GetInterface(ITextInput, TextInput)) then
      FCarretPositionChanged := TextInput.GetTextService.CaretPosition <> FLastCaretPosition
    else
      FCarretPositionChanged := False;
    FIgnorePosition := False;
  end;

begin
  DefineSelectionStates;
  if not FCarretPositionChanged and not FLastContextMenuVisibility and
     not FClickedAnotherControl and not FChangedFocusedControl then
  begin
    if FContextMenu.ShowSpellItems then
      FContextMenu.HighlightSpell
    else
      FContextMenu.HideHighlightSpell;
    FContextMenu.Show;
  end
  else
    FContextMenu.HideHighlightSpell;
end;

procedure TFMXViewBase.SingleTap(Sender: id);
var
  SpellControl: ITextSpellCheck;
begin
  if Form <> nil then
  begin
    FContextMenu.ShowSpellItems := (Form.Focused <> nil) and
      Supports(Form.Focused, ITextSpellCheck, SpellControl) and
      SpellControl.IsSpellCheckEnabled and SpellControl.IsCurrentWordWrong;
    if FContextMenu.ShowSpellItems then
      FContextMenu.SetSpellItems(SpellControl.GetListOfPrepositions)
    else
      FContextMenu.SetSpellItems(nil);
    ShowContextMenu;
  end;
end;

procedure TFMXViewBase.spell1(Sender: id);
begin
  FContextMenu.Spell1;
end;

procedure TFMXViewBase.spell2(Sender: id);
begin
  FContextMenu.Spell2;
end;

procedure TFMXViewBase.spell3(Sender: id);
begin
  FContextMenu.Spell3;
end;

function TFMXViewBase.spellCheckingType: Integer;
begin
  Result := 0;
end;

function TFMXViewBase.textInRange(range: UITextRange): NSString;
begin
  Result := nil;
end;

function TFMXViewBase.textRangeFromPosition(fromPosition, toPosition: UITextPosition): UITextRange;
begin
  Result := nil;
end;

function TFMXViewBase.tokenizer: Pointer;
begin
  Result := TUITextInputStringTokenizer.Alloc.initWithTextInput(UIView(super));
end;

function TFMXViewBase.GetMultiTouchManager: TMultiTouchManagerIOS;
begin
  if FMultiTouchManager = nil then
    FMultiTouchManager := TMultiTouchManagerIOS.Create(Form);
  Result := FMultiTouchManager;
end;

function TFMXViewBase.GetTouchCoord(const touches: NSSet; const Window: UIView; var x, y: single): Boolean;
var
  touch : UITouch;
  p     : CGPoint;
begin
  Result := False;
  if (touches <> nil) and (touches.count = 1) then
  begin
    touch := TUITouch.Wrap(touches.anyObject);
    p := CorrectLocationInView(touch, Window, Form);
    x := p.x;
    y := p.y;
    PlatformCocoa.FMouseCoord.X := X;
    PlatformCocoa.FMouseCoord.Y := Y;
    PlatformCocoa.FMouseCoord := Form.ClientToScreen(PlatformCocoa.FMouseCoord);
    Result := True;
  end;
end;

procedure TFMXViewBase.PrepareClosePopups(const SaveForm: TCommonCustomForm);
begin
  if Assigned(Screen) then
  begin
    if SaveForm <> nil then
      Screen.PrepareClosePopups(SaveForm)
    else
      Screen.PrepareClosePopups(nil);
  end;
end;

procedure TFMXViewBase.ClosePopups;
begin
  if Assigned(Screen) then
    Screen.ClosePopupForms;
end;

procedure TFMXViewBase.DoLMouseDown(const X, Y: Single);
begin
  if not FDown then
  try
    FDown := True;
    FTap := True;
    PrepareClosePopups(Form);
    if (Form <> nil) and (not Form.Released) then
    begin
      Form.MouseMove([ssTouch], X, Y);
      Form.MouseMove([], X, Y); // Require for correct IsMouseOver handle
      Form.MouseDown(TMouseButton.mbLeft, [ssLeft, ssTouch], X, Y);
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.DoLMouseMove(const X, Y: Single);
begin
  if FDown then
  try
    if (Form <> nil) and (not Form.Released) then
    begin
      Form.MouseMove([ssLeft, ssTouch], X, Y);
    end;
  except
    Application.HandleException(Form);
  end;
  FTap := False;
end;

procedure TFMXViewBase.DoLMouseUp(const X, Y: Single; DoClick: Boolean = True);
begin
  if FDown then
  try
    FDown := False;
    if (Form <> nil) and (not Form.Released) then
    begin
      Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], X, Y, DoClick);
      if Form <> nil then
        Form.MouseLeave;
    end;
    ClosePopups;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.touchesBegan(touches: NSSet; withEvent: UIEvent);

  procedure ResetSelectionStates;
  begin
    FLastContextMenuVisibility := FContextMenu.IsVisible;
    FCarretPositionChanged := False;
  end;

var
  X, Y : single;
  TextInput: ITextInput;
  LPoint: TPointF;
  Obj: IControl;
  Touch: UITouch;
begin
  try
    if not GetTouchCoord(touches, UIView(Super), X, Y) then
      Exit;

    LPoint := PointF(X, Y);

    // find the control from under the gesture
    Obj := IControl(Form.ObjectAtPoint(LPoint));

    SendTouches(withEvent.allTouches, TTouchAction.Down, Obj);

    ResetSelectionStates;

    // Hide old context menu and set new focused control
    if FContextMenu.IsVisible then
      FContextMenu.Hide;

    // Save caret position for define selection change before mouse down
    if FContextMenu.HasControl and FContextMenu.Control.GetInterface(ITextInput, TextInput) then
      FLastCaretPosition := TextInput.GetTextService.CaretPosition;
    try
      DoLMouseDown(X, Y);
      Touch := TUITouch.Wrap(touches.anyObject);
      if Touch.tapCount = 2 then
        iOSapi.Foundation.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget(Self.GetObjectID);
      DefineFocusControl;
    finally
      FClickedAnotherControl := (Obj <> nil) and (FContextMenu.Control <> Obj.GetObject);
      UIView(Super).touchesBegan(touches, withEvent);
      PlatformCocoa.ResetIdleTimer;
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.touchesCancelled(touches: NSSet; withEvent: UIEvent);

  procedure DefineSelectionStates;
  var
    TextInput: ITextInput;
  begin
    if FContextMenu.HasControl and (Form.Focused <> nil) and (Form.Focused.GetObject.GetInterface(ITextInput, TextInput)) then
      FCarretPositionChanged := TextInput.GetTextService.CaretPosition <> FLastCaretPosition
    else
      FCarretPositionChanged := False;
  end;

var
  X, Y : single;
  Obj: IControl;
  LPoint: TPointF;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
begin
  try
    if not GetTouchCoord(touches, UIView(Super), X, Y) then
      Exit;
    LPoint := TPointF.Create(X, Y);

    Obj := IControl(Form.ObjectAtPoint(LPoint));
    SendTouches(withEvent.allTouches, TTouchAction.Up, Obj);

    try
      DoLMouseUp(X, Y, False);
      DefineSelectionStates;
    finally
      UIView(Super).touchesCancelled(touches, withEvent);
      PlatformCocoa.ResetIdleTimer;
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.touchesEnded(touches: NSSet; withEvent: UIEvent);

  procedure DefineSelectionStates;
  var
    TextInput: ITextInput;
  begin
     if (FContextMenu <> nil) and FContextMenu.HasControl and (Form <> nil) and
      (Form.Focused <> nil) and (Form.Focused.GetObject.GetInterface(ITextInput, TextInput)) then
      FCarretPositionChanged := TextInput.GetTextService.CaretPosition <> FLastCaretPosition
    else
      FCarretPositionChanged := False;
  end;

var
  X, Y : single;
  Touch: UITouch;
  Obj: IControl;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
begin
  try
    if not GetTouchCoord(touches, UIView(Super), X, Y) then
      Exit;

    // find the control from under the gesture
    Obj := IControl(Form.ObjectAtPoint(TPointF.Create(X, Y)));
    SendTouches(withEvent.allTouches, TTouchAction.Up, Obj);

    try
      DoLMouseUp(X, Y);
      Touch := TUITouch.Wrap(touches.anyObject);
      if FTap then
      begin
        if Touch.tapCount = 2 then
          DblTap(X, Y)
        else
          if Form <> nil then
            NSObject(Self.Super).performSelector(sel_getUid('SingleTap:'), nil, DBL_TAP_DELAY);
      end;
      DefineSelectionStates;
    finally
      if Form <> nil then
        UIView(Super).touchesEnded(touches, withEvent);
      PlatformCocoa.ResetIdleTimer;
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.touchesMoved(touches: NSSet; withEvent: UIEvent);
var
  X, Y : single;
  Obj: IControl;
  LPoint: TPointF;
begin
  try
    if not GetTouchCoord(touches, UIView(Super), X, Y) then
      Exit;

    LPoint := TPointF.Create(X, Y);
    Obj := IControl(Form.ObjectAtPoint(LPoint));
    SendTouches(withEvent.allTouches, TTouchAction.Move, Obj);

    try
      DoLMouseMove(X, Y);
    finally
      UIView(Super).touchesMoved(touches, withEvent);
      PlatformCocoa.ResetIdleTimer;
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.unmarkText;
var
  I: Integer;
  Ch: Char;
  LITextInput: ITextInput;
begin
  if (Form.Focused <> nil) and (FMarkRange.location <> NSNotFound) and Supports(Form.Focused, ITextInput, LITextInput) then
  try
    TTextServiceCocoa(LITextInput.GetTextService).InternalBreakIMEInput;
    FMarkRange := NSMakeRange(NSNotFound, 0);
    for I := 0 to FMarkText.Length - 1 do
    begin
      Ch := FMarkText.Chars[I];
      if Ch = #10 then
        FormKeyPress(#0, vkReturn, [])
      else
        FormKeyPress(Ch, 0, []);
    end;
    TTextServiceCocoa(LITextInput.GetTextService).InternalEndIMEInput;
    FMarkText := EmptyStr;

    PlatformCocoa.ResetIdleTimer;
  finally
    LITextInput := nil;
  end;
end;

{ TFMXView }

constructor TFMXView.Create(const AOwner: TCommonCustomForm; AFRameRect: NSRect);
var
  V: Pointer;
begin
  inherited Create(AOwner);
  V := UIView(Super).initWithFrame(AFrameRect);
  if V <> GetObjectID then
    UpdateObjectID(V);
end;

procedure TFMXView.drawRect(R: CGRect);
var
  PaintControl: IPaintControl;
begin
  if (Form <> nil) and Supports(Form, IPaintControl, PaintControl) then
  begin
    PaintControl.ContextHandle := THandle(UIGraphicsGetCurrentContext);
    PaintControl.PaintRects([RectF(R.origin.x, R.origin.y, R.origin.x + R.size.width, R.origin.y + R.size.height)]);
    PaintControl.ContextHandle := 0;
  end;
  PlatformCocoa.ResetIdleTimer;
end;

function TFMXView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXView);
end;

{ TFMXView3D }

var
  GLKitMod: HMODULE;

constructor TFMXView3D.Create(const AOwner: TCommonCustomForm; AFRameRect: NSRect);

  function GetDefaultMultisamples: Integer;
  const
    HighQualitySamples = 4;
  begin
    Result := 0;
    if Application.MainForm = nil then
      Exit;

    if (Application.MainForm is TCustomForm) and
      (TCustomForm(Application.MainForm).Quality = TCanvasQuality.HighQuality) then
      Exit(HighQualitySamples);

    if Application.MainForm is TCustomForm3D then
      Result := MultisampleTypeToNumber(TCustomForm3D(Application.MainForm).Multisample);
  end;

var
  V: Pointer;
  RenderingSetupService: IFMXRenderingSetupService;
  ColorBits, DepthBits, Multisamples: Integer;
  Stencil: Boolean;
begin
  GLKitMod := LoadLibrary(PWideChar(libGLKit));
  inherited Create(AOwner);
  V := GLKView(Super).initWithFrame(AFrameRect, TCustomContextIOS.SharedContext);
  GLKView(Super).setContentScaleFactor(PlatformCocoa.MainScreen.scale);

  // Default rendering configuration.
  ColorBits := 24;
  DepthBits := 24;
  Stencil := True;
  Multisamples := GetDefaultMultisamples;

  // Request adjustment of rendering configuration.
  if TPlatformServices.Current.SupportsPlatformService(IFMXRenderingSetupService, RenderingSetupService) then
    RenderingSetupService.Invoke(ColorBits, DepthBits, Stencil, Multisamples);

  // Color Bitdepth.
  if ColorBits <= 16 then
    GLKView(Super).setDrawableColorFormat(GLKViewDrawableColorFormatRGB565);

  // Depth Buffer.
  if DepthBits > 0 then
  begin
    if DepthBits > 16 then
      GLKView(Super).setDrawableDepthFormat(GLKViewDrawableDepthFormat24)
    else
      GLKView(Super).setDrawableDepthFormat(GLKViewDrawableDepthFormat16);
  end
  else
    GLKView(Super).setDrawableDepthFormat(GLKViewDrawableDepthFormatNone);

  // Stencil Buffer.
  if Stencil then
    GLKView(Super).setDrawableStencilFormat(GLKViewDrawableStencilFormat8)
  else
    GLKView(Super).setDrawableStencilFormat(GLKViewDrawableStencilFormatNone);

  // Multisamples.
  if Multisamples > 0 then
    GLKView(Super).setDrawableMultisample(GLKViewDrawableMultisample4X)
  else
    GLKView(Super).setDrawableMultisample(GLKViewDrawableMultisampleNone);

  if V <> GetObjectID then
    UpdateObjectID(V);

end;

function TFMXView3D.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXView3D);
end;

procedure TFMXView3D.drawRect(R: CGRect);
var
  PaintControl: IPaintControl;
begin
  if (Form <> nil) and Supports(Form, IPaintControl, PaintControl) then
    try
      PaintControl.PaintRects([Form.ClientRect]);
    except
      Application.HandleException(Form);
    end;
  PlatformCocoa.ResetIdleTimer;
end;

{ TFMXEditActionsMenu }

constructor TFMXEditActionsMenu.Create(const AParentView: UIView);
begin
  Assert(Assigned(AParentView), 'Context menu must have parent UIView');
  FParentView := AParentView;
  FMenuController := TUIMenuController.Wrap(TUIMenuController.OCClass.sharedMenuController);
end;

function TFMXEditActionsMenu.DefineActionType(
  const AAction: SEL): TStandardActionType;
begin
  if AAction = sel_getUid('select:') then
    Result := TStandardActionType.Select
  else
  if AAction = sel_getUid('selectAll:') then
    Result := TStandardActionType.SelectAll
  else
  if AAction = sel_getUid('copy:') then
    Result := TStandardActionType.Copy
  else
  if AAction = sel_getUid('paste:') then
    Result := TStandardActionType.Paste
  else
  if AAction = sel_getUid('cut:') then
    Result := TStandardActionType.Cut
  else
  if AAction = sel_getUid('PromptForReplace:') then
    Result := TStandardActionType.PromptForReplace
  else
  if AAction = sel_getUid('replace:') then
    Result := TStandardActionType.Replace
  else
  if AAction = sel_getUid('spell1:') then
    Result := TStandardActionType.Spell1
  else
  if AAction = sel_getUid('spell2:') then
    Result := TStandardActionType.Spell2
  else
  if AAction = sel_getUid('spell3:') then
    Result := TStandardActionType.Spell3
  else
    Result := TStandardActionType.Unknown;
end;

destructor TFMXEditActionsMenu.Destroy;
begin
  FParentView := nil;
  FMenuController := nil;
  inherited Destroy;
end;

procedure TFMXEditActionsMenu.DoControlChanged;
begin
  // Nothing
end;

procedure TFMXEditActionsMenu.DoDefineSelectionFrame(var Frame: CGRect);
begin

end;

function TFMXEditActionsMenu.HasControl: Boolean;
begin
  Result := Assigned(Control);
end;

procedure TFMXEditActionsMenu.Hide;
begin
  if IsVisible then
    FMenuController.setMenuVisible(False{, True});
end;

function TFMXEditActionsMenu.IsVisible: Boolean;
begin
  Result := FMenuController.isMenuVisible;
end;

procedure TFMXEditActionsMenu.SetControl(const AControl: TControl);
begin
  if FControl <> AControl then
  begin
    FControl := AControl;
    DoControlChanged;
    Hide;
  end;
end;

procedure TFMXEditActionsMenu.Show;
var
  AbsolutePos: TPointF;
  ControlFrame: CGRect;
begin
  FReplaceMenu := False;
  if not HasControl then
    Exit;
  // Define default control frame
  AbsolutePos := FControl.LocalToAbsolute(PointF(0, 0));
  ControlFrame := CGRectMake(AbsolutePos.X, AbsolutePos.Y, FControl.Width, FControl.Height);
  // Define user control frame
  DoDefineSelectionFrame(ControlFrame);
  // Show menu
  FMenuController.setTargetRect(ControlFrame, FParentView);
  FMenuController.update;
  FMenuController.setMenuVisible(True, True);
end;

{ TFMXTextEditActionsMenu }

function TFMXTextEditActionsMenu.CanPerformAction(const AAction: SEL): Boolean;
var
  ClipboardService: IFMXClipboardService;
begin
  ClipboardService := GetClipboardService;
  try
    Result := Assigned(FTextInput) and Assigned(ClipboardService);
    if Result then
      case DefineActionType(AAction) of
        TStandardActionType.Cut:
          Result := not FShowSpellItems and not FTextInput.GetSelection.IsEmpty and not FReplaceMenu;
        TStandardActionType.Copy:
          Result := not FShowSpellItems and not FTextInput.GetSelection.IsEmpty and not FReplaceMenu;
        TStandardActionType.Paste:
          Result := not FShowSpellItems and ClipboardService.GetClipboard.IsType<string> and
                    not ClipboardService.GetClipboard.IsEmpty and not FReplaceMenu;
        TStandardActionType.Select:
          Result := not FShowSpellItems and FTextInput.GetSelection.IsEmpty and FTextInput.HasText and not FReplaceMenu;
        TStandardActionType.SelectAll:
          Result := not FShowSpellItems and FTextInput.GetSelection.IsEmpty and FTextInput.HasText and not FReplaceMenu;
        TStandardActionType.Unknown:
          Result := False;
        TStandardActionType.PromptForReplace:
          Result := not FShowSpellItems and not FTextInput.GetSelection.IsEmpty and not FReplaceMenu;
        TStandardActionType.Replace:
          Result := not FShowSpellItems and not FTextInput.GetSelection.IsEmpty and FReplaceMenu;
        TStandardActionType.Spell1:
          Result := FShowSpellItems and (Length(FSpells) > 0);
        TStandardActionType.Spell2:
          Result := FShowSpellItems and (Length(FSpells) > 1);
        TStandardActionType.Spell3:
          Result := FShowSpellItems and (Length(FSpells) > 2);
      else
        Result := False;
      end;
  finally
    ClipboardService := nil;
  end;
end;

procedure TFMXTextEditActionsMenu.Copy;
begin
  if Assigned(FTextActions) then
    FTextActions.CopyToClipboard;
end;

procedure TFMXTextEditActionsMenu.Cut;
begin
  if Assigned(FTextActions) then
    FTextActions.CutToClipboard;
end;

procedure TFMXTextEditActionsMenu.DoControlChanged;
var
  MenuItems: NSMutableArray;
  SpellTitle: NSString;
begin
  if Assigned(Control) then
  begin
    Supports(Control, ITextInput, FTextInput);
    Supports(Control, ITextActions, FTextActions);
    Supports(Control, ITextSpellCheck, FSpellCheck);
    Supports(Control, ITextSpellCheckActions, FSpellActions);
  end
  else
  begin
    FTextInput := nil;
    FTextActions := nil;
    FSpellCheck := nil;
    FSpellActions := nil;
  end;
  if Assigned(FSpellActions) then
  begin
    if not Assigned(FSpellItem1) then
    begin
      MenuItems := TNSMutableArray.Create;
      FSpellItem1 := TUIMenuItem.Alloc;
      if Length(FSpells) > 0 then
        SpellTitle := StrToNSStr(FSpells[0])
      else
        SpellTitle := StrToNSStr('Spell1');
      FSpellItem1 := TUIMenuItem.Wrap(FSpellItem1.initWithTitle(SpellTitle, sel_getUid('spell1:')));
      MenuItems.addObject((FSpellItem1 as ILocalObject).GetObjectID);
      FSpellItem2 := TUIMenuItem.Alloc;
      if Length(FSpells) > 1 then
        SpellTitle := StrToNSStr(FSpells[1])
      else
        SpellTitle := StrToNSStr('Spell2');
      FSpellItem2 := TUIMenuItem.Wrap(FSpellItem2.initWithTitle(SpellTitle, sel_getUid('spell2:')));
      MenuItems.addObject((FSpellItem2 as ILocalObject).GetObjectID);
      FSpellItem3 := TUIMenuItem.Alloc;
      if Length(FSpells) > 2 then
        SpellTitle := StrToNSStr(FSpells[2])
      else
        SpellTitle := StrToNSStr('Spell3');
      FSpellItem3 := TUIMenuItem.Wrap(FSpellItem3.initWithTitle(SpellTitle, sel_getUid('spell3:')));
      MenuItems.addObject((FSpellItem3 as ILocalObject).GetObjectID);
      //
      FMenuController.setMenuItems(MenuItems);
    end;
  end;
end;

procedure TFMXTextEditActionsMenu.DoDefineSelectionFrame(var Frame: CGRect);
var
  SelectionRect: TRectF;
  AbsolutePos: TPointF;
begin
  if Assigned(FTextInput) then
  begin
    SelectionRect := FTextInput.GetSelectionRect;
    AbsolutePos := FControl.LocalToAbsolute(SelectionRect.TopLeft);
    Frame := CGRectMake(AbsolutePos.X, AbsolutePos.Y, SelectionRect.Width, SelectionRect.Height);
  end;
end;

function TFMXTextEditActionsMenu.GetClipboardService: IFMXClipboardService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Result);
end;

procedure TFMXTextEditActionsMenu.HideHighlightSpell;
begin
  if Assigned(FSpellCheck) then
    FSpellCheck.HideHighlightSpell;
end;

procedure TFMXTextEditActionsMenu.HighlightSpell;
begin
  if Assigned(FSpellCheck) then
    FSpellCheck.HighlightSpell;
end;

procedure TFMXTextEditActionsMenu.Paste;
begin
  if Assigned(FTextActions) then
    FTextActions.PasteFromClipboard;
end;

procedure TFMXTextEditActionsMenu.PromptForReplace;

  function GetLanguage: NSString;
  begin
    Result := TNSString.Wrap(TUITextChecker.OCClass.availableLanguages.objectAtIndex(0));
    if not Assigned(Result) then
      Result := StrToNSStr('en_US');
  end;

var
  TheLanguage: NSString;
  StringRange: NSRange;
  TextChecker: UITextChecker;
  CurrentOffset: NSInteger;
  CurrentRange: NSRange;
  Guesses: NSArray;

  TheText: NSString;
  Word: NSString;
  MenuTmp: UIMenuItem;
  I: Integer;

  GuessesMenuItems: NSMutableArray;
begin
  if not Assigned(FTextInput) or not Assigned(FTextActions) then
    Exit;

  TextChecker := TUITextChecker.Create;
  CurrentOffset := 0;
  StringRange.location := 0;
  TheText := StrToNSStr(FTextInput.GetSelection);
  StringRange.length := TheText.length - 1;

  TheLanguage := GetLanguage;

  currentRange := TextChecker.rangeOfMisspelledWordInString(TheText,
    StringRange, CurrentOffset, False, TheLanguage);

  if CurrentRange.location = NSNotFound then
    Exit;

  Guesses := TextChecker.guessesForWordRange(currentRange, theText, theLanguage);
  GuessesMenuItems := TNSMutableArray.Create;
  for I := 0 to Guesses.count - 1 do
  begin
    MenuTmp := TUIMenuItem.Alloc;
    Word := TNSString.Wrap(Guesses.objectAtIndex(I));
    MenuTmp.initWithTitle(Word, sel_getUid('replace:'));
    GuessesMenuItems.addObject((MenuTmp as ILocalObject).GetObjectID);
  end;

  Hide;
  FReplaceMenu := True;
  FMenuController.setMenuItems( GuessesMenuItems);
//  FMenuController.setTargetRect(ControlFrame, FParentView);
  FMenuController.setMenuVisible(True{, True});

//  currentOffset := currentOffset + currentRange.length - 1;
end;

procedure TFMXTextEditActionsMenu.Select;
begin
  if Assigned(FTextActions) then
  begin
    FTextActions.SelectWord;
    Show;
  end;
end;

procedure TFMXTextEditActionsMenu.SelectAll;
begin
  if Assigned(FTextActions) then
  begin
    FTextActions.SelectAll;
    Show;
  end;
end;

procedure TFMXTextEditActionsMenu.SetSpellItems(items: TArray<string>);
begin
  FSpells := items;
  if (FSpellItem1 <> nil) and (Length(FSpells) > 0) then
  begin
    FSpellItem1.setTitle(StrToNSStr(FSpells[0]));
    if (Length(FSpells) > 1) and (FSpellItem2 <> nil) then
    begin
      FSpellItem2.setTitle(StrToNSStr(FSpells[1]));
      if (Length(FSpells) > 2) and (FSpellItem3 <> nil) then
        FSpellItem3.setTitle(StrToNSStr(FSpells[2]));
    end;
  end;
end;

procedure TFMXTextEditActionsMenu.Spell1;
begin
  if Assigned(FSpellActions) then
    FSpellActions.Spell(FSpells[0]);
end;

procedure TFMXTextEditActionsMenu.Spell2;
begin
  if Assigned(FSpellActions) then
    FSpellActions.Spell(FSpells[1]);
end;

procedure TFMXTextEditActionsMenu.Spell3;
begin
  if Assigned(FSpellActions) then
    FSpellActions.Spell(FSpells[2]);
end;

{ TiOSWindowHandle }

function WindowHandleToPlatform(const AHandle: TWindowHandle): TiOSWindowHandle;
begin
  Result := TiOSWindowHandle(AHandle);
end;

constructor TiOSWindowHandle.Create(const AHandle: TOCLocal);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TiOSWindowHandle.GetForm: TCommonCustomForm;
begin
  Result := TFMXViewBase(FHandle).Form;
end;

function TiOSWindowHandle.GetGLView: GLKView;
begin
  if TWindowStyle.GPUSurface in Form.WindowStyle then
    Result := GLKView(TFMXViewBase(FHandle).Super)
  else
    Result := nil;
end;

function TiOSWindowHandle.GetView: UIView;
begin
  Result := UIView(TFMXViewBase(FHandle).Super);
end;

function TiOSWindowHandle.GetWnd: UIWindow;
begin
  if View <> nil then
    Result := View.window
  else
    Result := nil;
end;

{ TiOSOpenApplicationContext }

constructor TiOSOpenApplicationContext.Create(ASourceApp: string; AURL: string; AContext: Pointer);
begin
  inherited Create;
  FSourceApp := ASourceApp;
  FURL := AURL;
  FContext := AContext;
end;

{ TMultiDisplayIOS }

procedure TMultiDisplayIOS.UpdateDisplayInformation;
begin
  FDisplayCount := 0;
  FWorkAreaRect := TRect.Empty;
  FDesktopRect := TRect.Empty;
  FreeAndNil(FDisplayList);
end;

function TMultiDisplayIOS.GetDisplayCount: Integer;
begin
  if FDisplayCount = 0 then
    FDisplayCount := TUIScreen.OCClass.screens.count;
  Result := FDisplayCount;
end;

function TMultiDisplayIOS.GetDesktopRect: TRect;
var
  I: Integer;
begin
  if (FDesktopRect.Width <= 0) or (FDesktopRect.Height <= 0) then
  begin
    FDesktopRect := TRect.Empty;
    for I := 0 to GetDisplayCount - 1 do
      FDesktopRect.Union(GetDisplay(I).BoundsRect);
  end;
  Result := FDesktopRect;
end;

function TMultiDisplayIOS.CGRectToRect(const ACGRect: CGRect): TRect;
var
  LSize: CGSize;
begin
  case PlatformCocoa.GetScreenOrientation of
    TScreenOrientation.Portrait:
    begin
      Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.x), Round(ACGRect.origin.y)), Round(ACGRect.size.width),
        Round(ACGRect.size.height));
    end;
    TScreenOrientation.Landscape:
    begin
      Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.y),
        Round(ACGRect.origin.x)), Round(ACGRect.size.height), Round(ACGRect.size.width));
    end;
    TScreenOrientation.InvertedPortrait:
    begin
      LSize :=PlatformCocoa.mainScreen.bounds.size;
      Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.x),
        Round(LSize.height - ACGRect.origin.y - ACGRect.size.height)), Round(ACGRect.size.width),
        Round(ACGRect.size.height));
    end;
    TScreenOrientation.InvertedLandscape:
    begin
      LSize := PlatformCocoa.mainScreen.bounds.size;
      Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.y),
        Round(LSize.width - ACGRect.origin.x - ACGRect.size.width)), Round(ACGRect.size.height),
        Round(ACGRect.size.width));
    end;
  end;
end;

procedure TMultiDisplayIOS.UpdateDisplays;
var
  I: Integer;
  AutoReleasePool: NSAutoreleasePool;
  LIScreen: UIScreen;
begin
  UpdateDisplayInformation;
  FDisplayCount := TUIScreen.OCClass.screens.count;
  if FDisplayList = nil then
    FDisplayList := TList<TDisplay>.Create
  else
    FDisplayList.Clear;
  for I := 0 to FDisplayCount - 1 do
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      LIScreen := TUIScreen.Wrap(TUIScreen.OCClass.screens.objectAtIndex(I));
      FDisplayList.Add(TDisplay.Create(I, I = 0, CGRectToRect(LIScreen.bounds),
        CGRectToRect(LIScreen.applicationFrame)));
    finally
      AutoReleasePool.release;
    end;
  end;
end;

function TMultiDisplayIOS.FindDisplay(const screen: UIScreen): TDisplay;
  function DoFind(const R: TRect): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    if FDisplayList <> nil then
      for I := 0 to FDisplayList.Count - 1 do
        if R = FDisplayList[I].BoundsRect then
          Exit(I);
  end;
var
  Index: Integer;
  R: TRect;
begin
  if screen = nil then
    raise EInvalidFmxHandle.Create(sArgumentInvalid);
  R := CGRectToRect(screen.bounds);
  Index := DoFind(R);
  if Index = -1 then
  begin
    UpdateDisplays;
    Index := DoFind(R);
  end;
  if Index = -1 then
    raise EInvalidArgument.Create(sArgumentInvalid)
  else
    Result := FDisplayList[Index];
end;

function TMultiDisplayIOS.DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
  function IsPopupForm(const Form: TCommonCustomForm): Boolean;
  begin
    Result := (Form <> nil) and ((Form.FormStyle = TFormStyle.Popup) or (Form is TCustomPopupForm));
  end;
var
  Wnd: TiOSWindowHandle;
  ParentForm: TCommonCustomForm;
begin
  if Handle = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Wnd := WindowHandleToPlatform(Handle);
  if IsPopupForm(Wnd.Form) then
  begin
    ParentForm := Wnd.Form.ParentForm;
    while IsPopupForm(ParentForm) do
      ParentForm := ParentForm.ParentForm;
    if ParentForm <> nil then
      Wnd := WindowHandleToPlatform(ParentForm.Handle);
  end;
  if (Wnd = nil) or (Wnd.Wnd = nil) then
    raise EArgumentException.Create(sArgumentInvalid);
  Result := FindDisplay(Wnd.Wnd.screen);
end;

function TMultiDisplayIOS.DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
begin
  Result := DisplayFromWindow(Handle);
end;

function TMultiDisplayIOS.GetDisplay(const Index: Integer): TDisplay;
begin
  if Index < 0 then
    raise EListError.CreateFMT(SListIndexError, [Index]);
  if (FDisplayList = nil) or (FDisplayList.Count <> GetDisplayCount) then
    UpdateDisplays;
  if Index >= GetDisplayCount then
    raise EListError.CreateFMT(SListIndexError, [Index]);
  Result := FDisplayList[Index];
end;

function TMultiDisplayIOS.GetWorkAreaRect: TRect;
begin
  if (FWorkAreaRect.Width <= 0) or (FWorkAreaRect.Height <= 0) then
    FWorkAreaRect := CGRectToRect(PlatformCocoa.mainScreen.applicationFrame);
  Result := FWorkAreaRect;
end;

{ TFMXAlertViewInputDelegate }

procedure TFMXAlertViewInputDelegate.alertView(alertView: UIAlertView; clickedButtonAtIndex: NSInteger);
begin
  if (clickedButtonAtIndex >= 0) and (clickedButtonAtIndex < Length(FResults)) then
    FModalResult := FResults[clickedButtonAtIndex]
  else
    FModalResult := mrCancel;
  DoReadAlertView(alertView);
  TThread.Queue(nil, DoDialogClosed);
  FModal := False;
end;

procedure TFMXAlertViewInputDelegate.alertViewCancel(alertView: UIAlertView);
begin
  FModalResult := mrCancel;
  DoReadAlertView(alertView);
  TThread.Queue(nil, DoDialogClosed);
  FModal := False;
  DoDismiss(alertView);
end;

procedure TFMXAlertViewInputDelegate.alertViewDidDismissWithButtonIndex(alertView: UIAlertView;
  didDismissWithButtonIndex: NSInteger);
begin
  DoDismiss(alertView);
end;

constructor TFMXAlertViewInputDelegate.Create(const AButtons: TMsgDlgButtons; const AInputCloseQueryProc: TInputCloseQueryProc);
var
  B: TMsgDlgBtn;
begin
  inherited Create;
  FParentList := nil;
  FInputCloseQueryProc := AInputCloseQueryProc;
  for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if B in AButtons then
    begin
      SetLength(FResults, Length(FResults) + 1);
      FResults[High(FResults)] := ModalResults[B]
    end;
end;

procedure TFMXAlertViewInputDelegate.DoDialogClosed;
begin
  if Assigned(FInputCloseQueryProc) then
    FInputCloseQueryProc(FModalResult, FValues);
end;

procedure TFMXAlertViewInputDelegate.DoDismiss(const alertView: UIAlertView);
begin
  if FParentList <> nil then
    FParentList.Remove(Self);
  FParentList := nil;
  alertView.setDelegate(nil);
end;

procedure TFMXAlertViewInputDelegate.DoReadAlertView(const alertView: UIAlertView);
begin
  case alertView.alertViewStyle of
    UIAlertViewStyleDefault: SetLength(FValues, 0);
    UIAlertViewStyleSecureTextInput,
    UIAlertViewStylePlainTextInput:
      begin
        SetLength(FValues, 1);
        FValues[0] := NSStrToStr(TUITextView.Wrap(alertView.textFieldAtIndex(0)).text);
      end;
    UIAlertViewStyleLoginAndPasswordInput:
      begin
        SetLength(FValues, 2);
        FValues[0] := NSStrToStr(TUITextView.Wrap(alertView.textFieldAtIndex(0)).text);
        FValues[1] := NSStrToStr(TUITextView.Wrap(alertView.textFieldAtIndex(1)).text);
      end;
  end;
end;

procedure TFMXAlertViewInputDelegate.SetParentList(const AList: TList<TFMXAlertViewInputDelegate>);
begin
  FParentList := AList;
  if FParentList <> nil then
    FParentList.Add(Self);
end;

{ TFMXWakeHandler }

procedure TFMXWakeHandler.DoCheckSynchronize;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    CheckSynchronize;
end;

function TFMXWakeHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXWakeHandler);
end;

initialization

finalization
  FreeLibrary(GLKitMod);
end.

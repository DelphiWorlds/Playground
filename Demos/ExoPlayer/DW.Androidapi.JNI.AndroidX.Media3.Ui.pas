unit DW.Androidapi.JNI.AndroidX.Media3.Ui;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Widget, Androidapi.JNI.Util,
  DW.Androidapi.JNI.API, //!!!! Adds CaptioningManager (available since API 19)
  DW.Androidapi.JNI.AndroidX.Media3.Common;

type
  JAspectRatioFrameLayout_AspectRatioListener = interface;
  JCaptionStyleCompat = interface;
  JPlayerControlView_OnFullScreenModeChangedListener = interface;
  JPlayerControlView_VisibilityListener = interface;
  JPlayerView = interface;
  JPlayerView_ControllerVisibilityListener = interface;
  JPlayerView_FullscreenButtonClickListener = interface;
  JSubtitleView = interface;

  JSubtitleViewClass = interface(JFrameLayoutClass)
    ['{0C297B81-6526-4DC7-8BCD-B7F5BFF6A5B3}']
    {class} function _GetDEFAULT_BOTTOM_PADDING_FRACTION: Single; cdecl;
    {class} function _GetDEFAULT_TEXT_SIZE_FRACTION: Single; cdecl;
    {class} function _GetVIEW_TYPE_CANVAS: Integer; cdecl;
    {class} function _GetVIEW_TYPE_WEB: Integer; cdecl;
    {class} function init(context: JContext; attributeset: JAttributeSet): JSubtitleView; cdecl; overload;
    {class} function init(context: JContext): JSubtitleView; cdecl; overload;
    {class} property DEFAULT_BOTTOM_PADDING_FRACTION: Single read _GetDEFAULT_BOTTOM_PADDING_FRACTION;
    {class} property DEFAULT_TEXT_SIZE_FRACTION: Single read _GetDEFAULT_TEXT_SIZE_FRACTION;
    {class} property VIEW_TYPE_CANVAS: Integer read _GetVIEW_TYPE_CANVAS;
    {class} property VIEW_TYPE_WEB: Integer read _GetVIEW_TYPE_WEB;
  end;

  [JavaSignature('androidx/media3/ui/SubtitleView')]
  JSubtitleView = interface(JFrameLayout)
    ['{3D123B60-94B1-459A-B10A-C9F124CF1553}']
    procedure setApplyEmbeddedFontSizes(boolean: Boolean); cdecl;
    procedure setApplyEmbeddedStyles(boolean: Boolean); cdecl;
    procedure setBottomPaddingFraction(float: Single); cdecl;
    procedure setCues(list: JList); cdecl;
    procedure setFixedTextSize(int: Integer; float: Single); cdecl;
    procedure setFractionalTextSize(float: Single); cdecl; overload;
    procedure setFractionalTextSize(float: Single; boolean: Boolean); cdecl; overload;
    procedure setStyle(captionstylecompat: JCaptionStyleCompat); cdecl;
    procedure setUserDefaultStyle; cdecl;
    procedure setUserDefaultTextSize; cdecl;
    procedure setViewType(int: Integer); cdecl;
  end;
  TJSubtitleView = class(TJavaGenericImport<JSubtitleViewClass, JSubtitleView>) end;

  JPlayerViewClass = interface(JFrameLayoutClass)
    ['{6AAFAA9B-266C-435C-A047-CFB1273A068A}']
    {class} function _GetARTWORK_DISPLAY_MODE_FILL: Integer; cdecl;
    {class} function _GetARTWORK_DISPLAY_MODE_FIT: Integer; cdecl;
    {class} function _GetARTWORK_DISPLAY_MODE_OFF: Integer; cdecl;
    {class} function _GetSHOW_BUFFERING_ALWAYS: Integer; cdecl;
    {class} function _GetSHOW_BUFFERING_NEVER: Integer; cdecl;
    {class} function _GetSHOW_BUFFERING_WHEN_PLAYING: Integer; cdecl;
    {class} function init(context: JContext; attributeset: JAttributeSet; int: Integer): JPlayerView; cdecl; overload;
    {class} function init(context: JContext; attributeset: JAttributeSet): JPlayerView; cdecl; overload;
    {class} function init(context: JContext): JPlayerView; cdecl; overload;
    {class} procedure switchTargetView(player: JPlayer; playerview: JPlayerView; playerview_1: JPlayerView); cdecl;
    {class} property ARTWORK_DISPLAY_MODE_FILL: Integer read _GetARTWORK_DISPLAY_MODE_FILL;
    {class} property ARTWORK_DISPLAY_MODE_FIT: Integer read _GetARTWORK_DISPLAY_MODE_FIT;
    {class} property ARTWORK_DISPLAY_MODE_OFF: Integer read _GetARTWORK_DISPLAY_MODE_OFF;
    {class} property SHOW_BUFFERING_ALWAYS: Integer read _GetSHOW_BUFFERING_ALWAYS;
    {class} property SHOW_BUFFERING_NEVER: Integer read _GetSHOW_BUFFERING_NEVER;
    {class} property SHOW_BUFFERING_WHEN_PLAYING: Integer read _GetSHOW_BUFFERING_WHEN_PLAYING;
  end;

  [JavaSignature('androidx/media3/ui/PlayerView')]
  JPlayerView = interface(JFrameLayout)
    ['{EED204DB-BD0C-4344-AD55-ADF3870EA5F2}']
    function dispatchKeyEvent(keyevent: JKeyEvent): Boolean; cdecl;
    function dispatchMediaKeyEvent(keyevent: JKeyEvent): Boolean; cdecl;
    function getAdOverlayInfos: JList; cdecl;
    function getAdViewGroup: JViewGroup; cdecl;
    function getArtworkDisplayMode: Integer; cdecl;
    function getControllerAutoShow: Boolean; cdecl;
    function getControllerHideOnTouch: Boolean; cdecl;
    function getControllerShowTimeoutMs: Integer; cdecl;
    function getDefaultArtwork: JDrawable; cdecl;
    function getOverlayFrameLayout: JFrameLayout; cdecl;
    function getPlayer: JPlayer; cdecl;
    function getResizeMode: Integer; cdecl;
    function getSubtitleView: JSubtitleView; cdecl;
    function getUseArtwork: Boolean; cdecl;
    function getUseController: Boolean; cdecl;
    function getVideoSurfaceView: JView; cdecl;
    procedure hideController; cdecl;
    function isControllerFullyVisible: Boolean; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
    function onTrackballEvent(motionevent: JMotionEvent): Boolean; cdecl;
    function performClick: Boolean; cdecl;
    procedure setArtworkDisplayMode(int: Integer); cdecl;
    procedure setAspectRatioListener(aspectratiolistener: JAspectRatioFrameLayout_AspectRatioListener); cdecl;
    procedure setControllerAutoShow(boolean: Boolean); cdecl;
    procedure setControllerHideDuringAds(boolean: Boolean); cdecl;
    procedure setControllerHideOnTouch(boolean: Boolean); cdecl;
    procedure setControllerOnFullScreenModeChangedListener(onfullscreenmodechangedlistener: JPlayerControlView_OnFullScreenModeChangedListener); cdecl;
    procedure setControllerShowTimeoutMs(int: Integer); cdecl;
    procedure setControllerVisibilityListener(controllervisibilitylistener: JPlayerView_ControllerVisibilityListener); cdecl; overload;
    procedure setControllerVisibilityListener(visibilitylistener: JPlayerControlView_VisibilityListener); cdecl; overload;
    procedure setCustomErrorMessage(charsequence: JCharSequence); cdecl;
    procedure setDefaultArtwork(drawable: JDrawable); cdecl;
    procedure setErrorMessageProvider(errormessageprovider: JErrorMessageProvider); cdecl;
    procedure setExtraAdGroupMarkers(longs: TJavaArray<Int64>; booleans: TJavaArray<Boolean>); cdecl;
    procedure setFullscreenButtonClickListener(fullscreenbuttonclicklistener: JPlayerView_FullscreenButtonClickListener); cdecl;
    procedure setKeepContentOnPlayerReset(boolean: Boolean); cdecl;
    procedure setPlayer(player: JPlayer); cdecl;
    procedure setRepeatToggleModes(int: Integer); cdecl;
    procedure setResizeMode(int: Integer); cdecl;
    procedure setShowBuffering(int: Integer); cdecl;
    procedure setShowFastForwardButton(boolean: Boolean); cdecl;
    procedure setShowMultiWindowTimeBar(boolean: Boolean); cdecl;
    procedure setShowNextButton(boolean: Boolean); cdecl;
    procedure setShowPlayButtonIfPlaybackIsSuppressed(boolean: Boolean); cdecl;
    procedure setShowPreviousButton(boolean: Boolean); cdecl;
    procedure setShowRewindButton(boolean: Boolean); cdecl;
    procedure setShowShuffleButton(boolean: Boolean); cdecl;
    procedure setShowSubtitleButton(boolean: Boolean); cdecl;
    procedure setShowVrButton(boolean: Boolean); cdecl;
    procedure setShutterBackgroundColor(int: Integer); cdecl;
    procedure setUseArtwork(boolean: Boolean); cdecl;
    procedure setUseController(boolean: Boolean); cdecl;
    procedure setVisibility(int: Integer); cdecl;
    procedure showController; cdecl;
  end;
  TJPlayerView = class(TJavaGenericImport<JPlayerViewClass, JPlayerView>) end;

  JPlayerView_FullscreenButtonClickListenerClass = interface(IJavaClass)
    ['{A0E6331C-807D-4986-86EE-2FCA73472B27}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerView$FullscreenButtonClickListener')]
  JPlayerView_FullscreenButtonClickListener = interface(IJavaInstance)
    ['{4D1C500A-B46E-4559-A03C-1EA1F2AF8D60}']
    procedure onFullscreenButtonClick(boolean: Boolean); cdecl;
  end;
  TJPlayerView_FullscreenButtonClickListener = class(TJavaGenericImport<JPlayerView_FullscreenButtonClickListenerClass, JPlayerView_FullscreenButtonClickListener>) end;

  JPlayerView_ControllerVisibilityListenerClass = interface(IJavaClass)
    ['{20955CE7-0A1A-44C9-9CBE-39C5E1343348}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerView$ControllerVisibilityListener')]
  JPlayerView_ControllerVisibilityListener = interface(IJavaInstance)
    ['{D744AED8-7071-42E3-8082-843BA4E42629}']
    procedure onVisibilityChanged(int: Integer); cdecl;
  end;
  TJPlayerView_ControllerVisibilityListener = class(TJavaGenericImport<JPlayerView_ControllerVisibilityListenerClass, JPlayerView_ControllerVisibilityListener>) end;

  JPlayerControlView_VisibilityListenerClass = interface(IJavaClass)
    ['{16F03EBB-8CFC-4032-836E-C8BC96A25225}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerControlView$VisibilityListener')]
  JPlayerControlView_VisibilityListener = interface(IJavaInstance)
    ['{9C957FA1-F38C-4588-8833-BD6755824C33}']
    procedure onVisibilityChange(int: Integer); cdecl;
  end;
  TJPlayerControlView_VisibilityListener = class(TJavaGenericImport<JPlayerControlView_VisibilityListenerClass, JPlayerControlView_VisibilityListener>) end;

  JPlayerControlView_OnFullScreenModeChangedListenerClass = interface(IJavaClass)
    ['{C2910F02-5A8B-4614-89A8-73C27312B21B}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerControlView$OnFullScreenModeChangedListener')]
  JPlayerControlView_OnFullScreenModeChangedListener = interface(IJavaInstance)
    ['{D80AF4EF-29B2-43E2-B8B0-DDDF2D319947}']
    procedure onFullScreenModeChanged(boolean: Boolean); cdecl;
  end;
  TJPlayerControlView_OnFullScreenModeChangedListener = class(TJavaGenericImport<JPlayerControlView_OnFullScreenModeChangedListenerClass, JPlayerControlView_OnFullScreenModeChangedListener>) end;

  JCaptionStyleCompatClass = interface(JObjectClass)
    ['{FA96F5FE-32B9-4E10-9B24-5FA540DA6BDE}']
    {class} function _GetDEFAULT: JCaptionStyleCompat; cdecl;
    {class} function _GetEDGE_TYPE_DEPRESSED: Integer; cdecl;
    {class} function _GetEDGE_TYPE_DROP_SHADOW: Integer; cdecl;
    {class} function _GetEDGE_TYPE_NONE: Integer; cdecl;
    {class} function _GetEDGE_TYPE_OUTLINE: Integer; cdecl;
    {class} function _GetEDGE_TYPE_RAISED: Integer; cdecl;
    {class} function _GetUSE_TRACK_COLOR_SETTINGS: Integer; cdecl;
    {class} function createFromCaptionStyle(captionstyle: JCaptioningManager_CaptionStyle): JCaptionStyleCompat; cdecl;
    {class} function init(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; typeface: JTypeface): JCaptionStyleCompat; cdecl;
    {class} property &DEFAULT: JCaptionStyleCompat read _GetDEFAULT;
    {class} property EDGE_TYPE_DEPRESSED: Integer read _GetEDGE_TYPE_DEPRESSED;
    {class} property EDGE_TYPE_DROP_SHADOW: Integer read _GetEDGE_TYPE_DROP_SHADOW;
    {class} property EDGE_TYPE_NONE: Integer read _GetEDGE_TYPE_NONE;
    {class} property EDGE_TYPE_OUTLINE: Integer read _GetEDGE_TYPE_OUTLINE;
    {class} property EDGE_TYPE_RAISED: Integer read _GetEDGE_TYPE_RAISED;
    {class} property USE_TRACK_COLOR_SETTINGS: Integer read _GetUSE_TRACK_COLOR_SETTINGS;
  end;

  [JavaSignature('androidx/media3/ui/CaptionStyleCompat')]
  JCaptionStyleCompat = interface(JObject)
    ['{814A150F-2830-437F-B784-AA38BBCC82FE}']
    function _GetbackgroundColor: Integer; cdecl;
    function _GetedgeColor: Integer; cdecl;
    function _GetedgeType: Integer; cdecl;
    function _GetforegroundColor: Integer; cdecl;
    function _Gettypeface: JTypeface; cdecl;
    function _GetwindowColor: Integer; cdecl;
    property backgroundColor: Integer read _GetbackgroundColor;
    property edgeColor: Integer read _GetedgeColor;
    property edgeType: Integer read _GetedgeType;
    property foregroundColor: Integer read _GetforegroundColor;
    property typeface: JTypeface read _Gettypeface;
    property windowColor: Integer read _GetwindowColor;
  end;
  TJCaptionStyleCompat = class(TJavaGenericImport<JCaptionStyleCompatClass, JCaptionStyleCompat>) end;

  JAspectRatioFrameLayout_AspectRatioListenerClass = interface(IJavaClass)
    ['{CB837FB1-E42D-49D3-A3D6-37D446CD30A8}']
  end;

  [JavaSignature('androidx/media3/ui/AspectRatioFrameLayout$AspectRatioListener')]
  JAspectRatioFrameLayout_AspectRatioListener = interface(IJavaInstance)
    ['{469BE5FB-4B50-4CB9-9F50-77BE2488A2B7}']
    procedure onAspectRatioUpdated(float: Single; float_1: Single; boolean: Boolean); cdecl;
  end;
  TJAspectRatioFrameLayout_AspectRatioListener = class(TJavaGenericImport<JAspectRatioFrameLayout_AspectRatioListenerClass, JAspectRatioFrameLayout_AspectRatioListener>) end;

implementation

end.

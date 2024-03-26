unit DW.Androidapi.JNI.API;

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText;

type
  JCaptioningManager = interface;
  JCaptioningManager_CaptionStyle = interface;
  JCaptioningManager_CaptioningChangeListener = interface;

  JCaptioningManager_CaptioningChangeListenerClass = interface(JObjectClass)
    ['{60C54E70-2075-4F82-A243-78CAB9D490DD}']
    {class} function init: JCaptioningManager_CaptioningChangeListener; cdecl;
  end;

  [JavaSignature('android/view/accessibility/CaptioningManager$CaptioningChangeListener')]
  JCaptioningManager_CaptioningChangeListener = interface(JObject)
    ['{3C74D3FD-6519-43E0-977A-DC68A3A2CBE5}']
    procedure onEnabledChanged(boolean: Boolean); cdecl;
    procedure onFontScaleChanged(float: Single); cdecl;
    procedure onLocaleChanged(locale: JLocale); cdecl;
    procedure onUserStyleChanged(captionstyle: JCaptioningManager_CaptionStyle); cdecl;
  end;
  TJCaptioningManager_CaptioningChangeListener = class(TJavaGenericImport<JCaptioningManager_CaptioningChangeListenerClass,
    JCaptioningManager_CaptioningChangeListener>) end;

  JCaptioningManager_CaptionStyleClass = interface(JObjectClass)
    ['{14F210A6-D2A7-4EAA-9DDB-D0610776056E}']
    {class} function _GetEDGE_TYPE_DEPRESSED: Integer; cdecl;
    {class} function _GetEDGE_TYPE_DROP_SHADOW: Integer; cdecl;
    {class} function _GetEDGE_TYPE_NONE: Integer; cdecl;
    {class} function _GetEDGE_TYPE_OUTLINE: Integer; cdecl;
    {class} function _GetEDGE_TYPE_RAISED: Integer; cdecl;
    {class} function _GetEDGE_TYPE_UNSPECIFIED: Integer; cdecl;
    {class} property EDGE_TYPE_DEPRESSED: Integer read _GetEDGE_TYPE_DEPRESSED;
    {class} property EDGE_TYPE_DROP_SHADOW: Integer read _GetEDGE_TYPE_DROP_SHADOW;
    {class} property EDGE_TYPE_NONE: Integer read _GetEDGE_TYPE_NONE;
    {class} property EDGE_TYPE_OUTLINE: Integer read _GetEDGE_TYPE_OUTLINE;
    {class} property EDGE_TYPE_RAISED: Integer read _GetEDGE_TYPE_RAISED;
    {class} property EDGE_TYPE_UNSPECIFIED: Integer read _GetEDGE_TYPE_UNSPECIFIED;
  end;

  [JavaSignature('android/view/accessibility/CaptioningManager$CaptionStyle')]
  JCaptioningManager_CaptionStyle = interface(JObject)
    ['{D79D3007-B31C-4817-8E0E-C564FAFF60EF}']
    function _GetbackgroundColor: Integer; cdecl;
    function _GetedgeColor: Integer; cdecl;
    function _GetedgeType: Integer; cdecl;
    function _GetforegroundColor: Integer; cdecl;
    function _GetwindowColor: Integer; cdecl;
    function getTypeface: JTypeface; cdecl;
    function hasBackgroundColor: Boolean; cdecl;
    function hasEdgeColor: Boolean; cdecl;
    function hasEdgeType: Boolean; cdecl;
    function hasForegroundColor: Boolean; cdecl;
    function hasWindowColor: Boolean; cdecl;
    property backgroundColor: Integer read _GetbackgroundColor;
    property edgeColor: Integer read _GetedgeColor;
    property edgeType: Integer read _GetedgeType;
    property foregroundColor: Integer read _GetforegroundColor;
    property windowColor: Integer read _GetwindowColor;
  end;
  TJCaptioningManager_CaptionStyle = class(TJavaGenericImport<JCaptioningManager_CaptionStyleClass, JCaptioningManager_CaptionStyle>) end;

  JCaptioningManagerClass = interface(JObjectClass)
    ['{6B19AE30-958B-49AE-B840-DEAE5D4E6ED0}']
  end;

  [JavaSignature('android/view/accessibility/CaptioningManager')]
  JCaptioningManager = interface(JObject)
    ['{82E2830B-2811-489F-B3C6-2C909F5A846D}']
    procedure addCaptioningChangeListener(captioningchangelistener: JCaptioningManager_CaptioningChangeListener); cdecl;
    function getFontScale: Single; cdecl;
    function getLocale: JLocale; cdecl;
    function getUserStyle: JCaptioningManager_CaptionStyle; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure removeCaptioningChangeListener(captioningchangelistener: JCaptioningManager_CaptioningChangeListener); cdecl;
  end;
  TJCaptioningManager = class(TJavaGenericImport<JCaptioningManagerClass, JCaptioningManager>) end;

implementation

end.

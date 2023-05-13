unit DW.VKObserver.iOS;

interface

uses
  System.TypInfo, System.Types,
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  DW.VKObserver;

type
  IKeyboardObserver = interface(NSObject)
    ['{F153AA7D-6F53-43E3-8A7B-069205AE34C8}']
    procedure KeyboardDidChangeFrame(notification: Pointer); cdecl;
    procedure KeyboardDidHide(notification: Pointer); cdecl;
    procedure KeyboardDidShow(notification: Pointer); cdecl;
  end;

  TPlatformVKObserver = class;

  TKeyboardObserver = class(TOCLocal)
  private
    FObserver: TPlatformVKObserver;
    procedure KeyboardEvent(const ANotification: Pointer; const AIsVisible: Boolean = True);
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    { IKeyboardEvents }
    procedure KeyboardDidChangeFrame(notification: Pointer); cdecl;
    procedure KeyboardDidHide(notification: Pointer); cdecl;
    procedure KeyboardDidShow(notification: Pointer); cdecl;
  public
    constructor Create(const AObserver: TPlatformVKObserver);
  end;

  TPlatformVKObserver = class(TCustomPlatformVKObserver)
  private
    FKeyboardObserver: TKeyboardObserver;
  protected
    procedure FrameChanged(const AFrame: TRect); override;
  public
    constructor Create(const AVKObserver: TVKObserver); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  Macapi.ObjCRuntime, Macapi.Helpers,
  iOSapi.UIKit, iOSapi.CoreGraphics,
  FMX.VirtualKeyboard, FMX.Platform,
  DW.UIHelper;

{ TKeyboardObserver }

constructor TKeyboardObserver.Create(const AObserver: TPlatformVKObserver);
var
  LNotificationCenter: NSNotificationCenter;
begin
  inherited Create;
  FObserver := AObserver;
  LNotificationCenter := TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter);
  LNotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardDidChangeFrame:'), NSObjectToID(UIKeyboardDidChangeFrameNotification), nil);
  LNotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardDidHide:'), NSObjectToID(UIKeyboardDidHideNotification), nil);
  LNotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardDidShow:'), NSObjectToID(UIKeyboardDidShowNotification), nil);
end;

function TKeyboardObserver.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IKeyboardObserver);
end;

procedure TKeyboardObserver.KeyboardDidChangeFrame(notification: Pointer);
begin
  KeyboardEvent(notification);
end;

procedure TKeyboardObserver.KeyboardDidHide(notification: Pointer);
begin
  TOSLog.d('TKeyboardObserver.KeyboardDidHide');
  KeyboardEvent(notification, False);
end;

procedure TKeyboardObserver.KeyboardDidShow(notification: Pointer);
begin
  // KeyboardEvent(notification);
end;

procedure TKeyboardObserver.KeyboardEvent(const ANotification: Pointer; const AIsVisible: Boolean = True);
var
  LValue: Pointer;
  LNativeRect: CGRect;
  LRect: TRect;
  LKeyboardToolbarService: IFMXVirtualKeyboardToolbarService;
begin
  LRect := TRect.Empty;
  if AIsVisible then
  begin
    LValue := TNSNotification.Wrap(ANotification).userInfo.valueForKey(UIKeyboardFrameEndUserInfoKey);
    if LValue <> nil then
    begin
      LNativeRect := TNSValue.Wrap(LValue).CGRectValue;
      LRect := TRect.Create(LNativeRect.origin.ToPointF.Round, Round(LNativeRect.size.width), Round(LNativeRect.size.height));
      if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, LKeyboardToolbarService) and
        LKeyboardToolbarService.IsToolbarEnabled then
      begin
        LRect.Top := LRect.Top - 44;
      end;
      LRect.Top := Round(LRect.Top - TUIHelper.GetStatusBarOffset);
    end;
  end;
  FObserver.FrameChanged(LRect);
end;

{ TPlatformVKObserver }

constructor TPlatformVKObserver.Create(const AVKObserver: TVKObserver);
begin
  inherited;
  FKeyboardObserver := TKeyboardObserver.Create(Self);
end;

destructor TPlatformVKObserver.Destroy;
begin
  FKeyboardObserver.Free;
  inherited;
end;

procedure TPlatformVKObserver.FrameChanged(const AFrame: TRect);
begin
  inherited;
end;

end.

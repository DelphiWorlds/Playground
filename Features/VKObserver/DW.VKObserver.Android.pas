unit DW.VKObserver.Android;

interface

uses
  System.Types,
  Androidapi.JNIBridge, Androidapi.JNI.Embarcadero, Androidapi.JNI.GraphicsContentViewText,
  DW.VKObserver;

type
  TPlatformVKObserver = class;

  TKeyboardStateChangedListener = class(TJavaLocal, JOnKeyboardStateChangedListener)
  private
    FObserver: TPlatformVKObserver;
  public
    { JOnKeyboardStateChangedListener }
    procedure onVirtualKeyboardWillShown; cdecl;
    procedure onVirtualKeyboardFrameChanged(newFrame: JRect); cdecl;
    procedure onVirtualKeyboardWillHidden; cdecl;
  public
    constructor Create(const AObserver: TPlatformVKObserver);
  end;

  TPlatformVKObserver = class(TCustomPlatformVKObserver)
  private
    FKeyboardStateChangedListener: TKeyboardStateChangedListener;
  protected
    procedure FrameChanged(const AFrame: TRect); override;
  public
    constructor Create(const AVKObserver: TVKObserver); override;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Forms, FMX.Platform.Android, FMX.Platform.UI.Android,
  DW.UIHelper;

{ TKeyboardStateChangedListener }

constructor TKeyboardStateChangedListener.Create(const AObserver: TPlatformVKObserver);
begin
  inherited Create;
  FObserver := AObserver;
  MainActivity.getVirtualKeyboard.addOnKeyboardStateChangedListener(Self);
end;

procedure TKeyboardStateChangedListener.onVirtualKeyboardFrameChanged(newFrame: JRect);
var
  LRect: TRect;
  LHeight: Integer;
begin
  LRect.TopLeft := ConvertPixelToPoint(TPointF.Create(newFrame.Left, newFrame.Top)).Round;
  LRect.BottomRight := ConvertPixelToPoint(TPointF.Create(newFrame.Right, newFrame.Bottom)).Round;
  LHeight := LRect.Height;
  LRect.Top := Round(Screen.Height - LRect.Height);
  LRect.Height := LHeight;
  if Screen.Width > Screen.Height then
    LRect.Top := Round(LRect.Top - TUIHelper.GetStatusBarOffset);
  FObserver.FrameChanged(LRect);
end;

procedure TKeyboardStateChangedListener.onVirtualKeyboardWillHidden;
begin
  //
end;

procedure TKeyboardStateChangedListener.onVirtualKeyboardWillShown;
begin
  //
end;

{ TPlatformVKObserver }

constructor TPlatformVKObserver.Create(const AVKObserver: TVKObserver);
begin
  inherited;
  FKeyboardStateChangedListener := TKeyboardStateChangedListener.Create(Self);
end;

destructor TPlatformVKObserver.Destroy;
begin
  FKeyboardStateChangedListener.Free;
  inherited;
end;

procedure TPlatformVKObserver.FrameChanged(const AFrame: TRect);
begin
  inherited;
end;

end.

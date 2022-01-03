unit DW.Advertising.Android;

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.AdMob,
  DW.Advertising;

type
  JDWInterstitialAdCallback = interface;
  JDWInterstitialAdCallbackDelegate = interface;

  JDWInterstitialAdCallbackClass = interface(JInterstitialAdLoadCallbackClass)
    ['{5CC7467D-5131-4C6A-BB4E-FE8945F6F5B6}']
    {class} function init(delegate: JDWInterstitialAdCallbackDelegate): JDWInterstitialAdCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWInterstitialAdCallback')]
  JDWInterstitialAdCallback = interface(JInterstitialAdLoadCallback)
    ['{8818EEA7-36A6-4246-8079-CA10E5F141DB}']
  end;
  TJDWInterstitialAdCallback = class(TJavaGenericImport<JDWInterstitialAdCallbackClass, JDWInterstitialAdCallback>) end;

  JDWInterstitialAdCallbackDelegateClass = interface(IJavaClass)
    ['{81A3438F-D0A8-4392-B604-81A47B875FD4}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWInterstitialAdCallbackDelegate')]
  JDWInterstitialAdCallbackDelegate = interface(IJavaInstance)
    ['{11DEBF75-98FA-42B8-B9DE-5661D0CEC5AF}']
    procedure onAdLoaded(interstitialAd: JInterstitial_InterstitialAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJDWInterstitialAdCallbackDelegate = class(TJavaGenericImport<JDWInterstitialAdCallbackDelegateClass, JDWInterstitialAdCallbackDelegate>) end;

  TPlatformInterstitialAd = class;

  TInterstitialAdCallbackDelegate = class(TJavaLocal, JDWInterstitialAdCallbackDelegate)
  private
    FPlatformInterstitialAd: TPlatformInterstitialAd;
    FCallback: JDWInterstitialAdCallback;
  protected
    property Callback: JDWInterstitialAdCallback read FCallback;
  public
    { JDWInterstitialAdCallbackDelegate }
    procedure onAdLoaded(interstitialAd: JInterstitial_InterstitialAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  public
    constructor Create(APlatformInterstitialAd: TPlatformInterstitialAd);
  end;

  TPlatformInterstitialAd = class(TCustomPlatformInterstitialAd)
  private
    FAd: Jinterstitial_InterstitialAd;
    FCallbackDelegate: TInterstitialAdCallbackDelegate;
  protected
    procedure DoAdDismissedFullScreenContent; override;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); override;
    procedure DoAdLoaded(const AInterstitialAd: JInterstitial_InterstitialAd);
    procedure DoAdShowedFullScreenContent; override;
    procedure Load; override;
  public
    constructor Create(const AInterstitialAd: TInterstitialAd); override;
    destructor Destroy; override;
  end;

implementation

uses
  Androidapi.Helpers;

{ TInterstitialAdCallbackDelegate }

constructor TInterstitialAdCallbackDelegate.Create(APlatformInterstitialAd: TPlatformInterstitialAd);
begin
  inherited Create;
  FPlatformInterstitialAd := APlatformInterstitialAd;
  FCallback := TJDWInterstitialAdCallback.JavaClass.init(Self);
end;

procedure TInterstitialAdCallbackDelegate.onAdDismissedFullScreenContent;
begin
  FPlatformInterstitialAd.DoAdDismissedFullScreenContent;
end;

procedure TInterstitialAdCallbackDelegate.onAdFailedToShowFullScreenContent(adError: JAdError);
var
  LError: TAdError;
begin
  LError.ErrorCode := adError.getCode;
  LError.Message := JStringToString(adError.getMessage);
  FPlatformInterstitialAd.DoAdFailedToShowFullScreenContent(LError);
end;

procedure TInterstitialAdCallbackDelegate.onAdLoaded(interstitialAd: JInterstitial_InterstitialAd);
begin
  FPlatformInterstitialAd.DoAdLoaded(interstitialAd);
end;

procedure TInterstitialAdCallbackDelegate.onAdShowedFullScreenContent;
begin
  FPlatformInterstitialAd.DoAdShowedFullScreenContent;
end;

{ TPlatformInterstitialAd }

constructor TPlatformInterstitialAd.Create(const AInterstitialAd: TInterstitialAd);
begin
  inherited;
  FCallbackDelegate := TInterstitialAdCallbackDelegate.Create(Self);
end;

destructor TPlatformInterstitialAd.Destroy;
begin
  FCallbackDelegate.Free;
  inherited;
end;

procedure TPlatformInterstitialAd.DoAdDismissedFullScreenContent;
begin
  FAd := nil;
  inherited;
end;

procedure TPlatformInterstitialAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  FAd := nil;
  inherited;
end;

procedure TPlatformInterstitialAd.DoAdLoaded(const AInterstitialAd: JInterstitial_InterstitialAd);
begin
  FAd := AInterstitialAd;
  FAd.show(TAndroidHelper.Activity);
end;

procedure TPlatformInterstitialAd.DoAdShowedFullScreenContent;
begin
  inherited;
end;

procedure TPlatformInterstitialAd.Load;
var
  LRequest: JAdRequest;
  LAdUnitId: JString;
begin
  if FAd = nil then
  begin
    LRequest := TJAdRequest_Builder.JavaClass.init.build;
    LAdUnitId := StringToJString(AdUnitID);
    TJinterstitial_InterstitialAd.JavaClass.load(TAndroidHelper.Context, LAdUnitId, LRequest, FCallbackDelegate.Callback);
  end;
  // else ad already showing
end;

end.

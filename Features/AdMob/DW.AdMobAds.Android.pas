unit DW.AdMobAds.Android;

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.AdMob, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.App,
  DW.Androidapi.JNI.AdMob, DW.AdMob, DW.AdMobAds;

type
  JDWAppOpenAdLoadCallback = interface;
  JDWAppOpenAdLoadCallbackDelegate = interface;
  JDWInterstitialAdCallback = interface;
  JDWInterstitialAdCallbackDelegate = interface;
  JDWRewardedAdLoadCallback = interface;
  JDWRewardedAdLoadCallbackDelegate = interface;

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

  JDWRewardedAdLoadCallbackClass = interface(JRewardedAdLoadCallbackClass)
    ['{5CC7467D-5131-4C6A-BB4E-FE8945F6F5B6}']
    {class} function init(delegate: JDWRewardedAdLoadCallbackDelegate): JDWRewardedAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWRewardedAdLoadCallback')]
  JDWRewardedAdLoadCallback = interface(JRewardedAdLoadCallback)
    ['{8818EEA7-36A6-4246-8079-CA10E5F141DB}']
  end;
  TJDWRewardedAdCallback = class(TJavaGenericImport<JDWRewardedAdLoadCallbackClass, JDWRewardedAdLoadCallback>) end;

  JDWRewardedAdLoadCallbackDelegateClass = interface(IJavaClass)
    ['{81A3438F-D0A8-4392-B604-81A47B875FD4}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWRewardedAdLoadCallbackDelegate')]
  JDWRewardedAdLoadCallbackDelegate = interface(IJavaInstance)
    ['{11DEBF75-98FA-42B8-B9DE-5661D0CEC5AF}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(rewardedAd: JRewardedAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJDWRewardedAdCallbackDelegate = class(TJavaGenericImport<JDWRewardedAdLoadCallbackDelegateClass, JDWRewardedAdLoadCallbackDelegate>) end;

  JDWAppOpenAdLoadCallbackClass = interface(JAppOpenAd_AppOpenAdLoadCallbackClass)
    ['{C6E850B3-3CB4-4FEF-BF18-BC76EFD112B4}']
    {class} function init(delegate: JDWAppOpenAdLoadCallbackDelegate): JDWAppOpenAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWAppOpenAdLoadCallback')]
  JDWAppOpenAdLoadCallback = interface(JAppOpenAd_AppOpenAdLoadCallback)
    ['{05C207B7-5D2E-4809-89F5-3867D01A143E}']
  end;
  TJDWAppOpenAdLoadCallback = class(TJavaGenericImport<JDWAppOpenAdLoadCallbackClass, JDWAppOpenAdLoadCallback>) end;

  JDWAppOpenAdLoadCallbackDelegateClass = interface(IJavaClass)
    ['{6DDDEFE6-8FAA-4CE1-A8C0-DA14EEDFE06C}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWAppOpenAdLoadCallbackDelegate')]
  JDWAppOpenAdLoadCallbackDelegate = interface(IJavaInstance)
    ['{C98A7860-16ED-4675-9100-75A7524ADC3E}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(ad: JAppOpenAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJDWAppOpenAdLoadCallbackDelegate = class(TJavaGenericImport<JDWAppOpenAdLoadCallbackDelegateClass, JDWAppOpenAdLoadCallbackDelegate>) end;

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
    constructor Create(const APlatformInterstitialAd: TPlatformInterstitialAd);
  end;

  TPlatformInterstitialAd = class(TCustomPlatformInterstitialAd)
  private
    FAd: Jinterstitial_InterstitialAd;
    FCallbackDelegate: TInterstitialAdCallbackDelegate;
  protected
    procedure AdLoaded(const AInterstitialAd: JInterstitial_InterstitialAd);
    procedure DoAdDismissedFullScreenContent; override;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); override;
    procedure DoAdShowedFullScreenContent; override;
    function GetAdUnitID: string; override;
    procedure Load; override;
  public
    constructor Create(const AInterstitialAd: TInterstitialAd); override;
    destructor Destroy; override;
  end;

  TPlatformRewardedAd = class;

  TUserEarnedRewardListener = class(TJavaLocal, JOnUserEarnedRewardListener)
  private
    FPlatformRewardedAd: TPlatformRewardedAd;
  public
    { JOnUserEarnedRewardListener }
    procedure onUserEarnedReward(rewardItem: JRewardItem); cdecl;
  public
    constructor Create(const APlatformRewardedAd: TPlatformRewardedAd);
  end;

  TRewardedAdCallbackDelegate = class(TJavaLocal, JDWRewardedAdLoadCallbackDelegate)
  private
    FPlatformRewardedAd: TPlatformRewardedAd;
    FCallback: JDWRewardedAdLoadCallback;
  protected
    property Callback: JDWRewardedAdLoadCallback read FCallback;
  public
    { JDWRewardedAdCallbackDelegate }
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdLoaded(rewardedAd: JRewardedAd); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  public
    constructor Create(const APlatformRewardedAd: TPlatformRewardedAd);
  end;

  TPlatformRewardedAd = class(TCustomPlatformRewardedAd)
  private
    FAd: JRewardedAd;
    FCallbackDelegate: TRewardedAdCallbackDelegate;
    FUserEarnedRewardListener: JOnUserEarnedRewardListener;
  protected
    procedure AdLoaded(const ARewardedAd: JRewardedAd);
    procedure DoAdDismissedFullScreenContent; override;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); override;
    procedure DoAdShowedFullScreenContent; override;
    function GetAdUnitID: string; override;
    procedure Load; override;
    procedure UserEarnedReward(const ARewardItem: JRewardItem);
  public
    constructor Create(const ARewardedAd: TRewardedAd); override;
    destructor Destroy; override;
  end;

  TPlatformAppOpenAd = class;

  TAppOpenAdLoadCallbackDelegate = class(TJavaLocal, JDWAppOpenAdLoadCallbackDelegate)
  private
    FAppOpenAd: TPlatformAppOpenAd;
    FCallback: JDWAppOpenAdLoadCallback;
  protected
    property Callback: JDWAppOpenAdLoadCallback read FCallback;
  public
    { JDWAppOpenAdLoadCallbackDelegate }
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdLoaded(ad: JAppOpenAd); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  public
    constructor Create(const AAppOpenAd: TPlatformAppOpenAd);
  end;

  TPlatformAppOpenAd = class(TCustomPlatformAppOpenAd)
  private
    FAd: JAppOpenAd;
    FCallbackDelegate: TAppOpenAdLoadCallbackDelegate;
    function GetOrientation: Integer;
    procedure LoadAd;
    procedure ShowAdIfAvailable(const ACanShow: Boolean);
  protected
    procedure AdLoaded(const AAd: JAppOpenAd);
    procedure ApplicationBecameActive; override;
    procedure ApplicationEnteredBackground; override;
    procedure DoAdDismissedFullScreenContent; override;
    procedure DoAdFailedToLoad(const AError: TAdError); override;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); override;
    procedure DoAdLoaded; override;
    procedure DoAdShowedFullScreenContent; override;
    function GetAdUnitID: string; override;
  public
    constructor Create(const AAppOpenAd: TAppOpenAd); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Androidapi.Helpers;

{ TInterstitialAdCallbackDelegate }

constructor TInterstitialAdCallbackDelegate.Create(const APlatformInterstitialAd: TPlatformInterstitialAd);
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
  FPlatformInterstitialAd.AdLoaded(interstitialAd);
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
  //!!!!
end;

procedure TPlatformInterstitialAd.AdLoaded(const AInterstitialAd: JInterstitial_InterstitialAd);
begin
  FAd := AInterstitialAd;
  FAd.show(TAndroidHelper.Activity);
  DoAdLoaded;
end;

procedure TPlatformInterstitialAd.DoAdShowedFullScreenContent;
begin
  inherited;
end;

function TPlatformInterstitialAd.GetAdUnitID: string;
begin
  if TestMode then
    Result := cTestAdUnitIdInterstitial
  else
    Result := FAdUnitId;
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

{ TUserEarnedRewardListener }

constructor TUserEarnedRewardListener.Create(const APlatformRewardedAd: TPlatformRewardedAd);
begin
  inherited Create;
  FPlatformRewardedAd := APlatformRewardedAd;
end;

procedure TUserEarnedRewardListener.onUserEarnedReward(rewardItem: JRewardItem);
begin
  FPlatformRewardedAd.UserEarnedReward(rewardItem);
end;

{ TRewardedAdCallbackDelegate }

constructor TRewardedAdCallbackDelegate.Create(const APlatformRewardedAd: TPlatformRewardedAd);
begin
  inherited Create;
  FPlatformRewardedAd := APlatformRewardedAd;
  FCallback := TJDWRewardedAdCallback.JavaClass.init(Self);
end;

procedure TRewardedAdCallbackDelegate.onAdDismissedFullScreenContent;
begin
  FPlatformRewardedAd.DoAdDismissedFullScreenContent;
end;

procedure TRewardedAdCallbackDelegate.onAdFailedToLoad(loadAdError: JLoadAdError);
var
  LError: TAdError;
begin
  LError.ErrorCode := loadAdError.getCode;
  LError.Message := JStringToString(loadAdError.getMessage);
  FPlatformRewardedAd.DoAdFailedToLoad(LError);
end;

procedure TRewardedAdCallbackDelegate.onAdFailedToShowFullScreenContent(adError: JAdError);
var
  LError: TAdError;
begin
  LError.ErrorCode := adError.getCode;
  LError.Message := JStringToString(adError.getMessage);
  FPlatformRewardedAd.DoAdFailedToShowFullScreenContent(LError);
end;

procedure TRewardedAdCallbackDelegate.onAdLoaded(rewardedAd: JRewardedAd);
begin
  FPlatformRewardedAd.AdLoaded(rewardedAd);
end;

procedure TRewardedAdCallbackDelegate.onAdShowedFullScreenContent;
begin
  FPlatformRewardedAd.DoAdShowedFullScreenContent;
end;

{ TPlatformRewardedAd }

constructor TPlatformRewardedAd.Create(const ARewardedAd: TRewardedAd);
begin
  inherited;
  FCallbackDelegate := TRewardedAdCallbackDelegate.Create(Self);
  FUserEarnedRewardListener := TUserEarnedRewardListener.Create(Self);
end;

destructor TPlatformRewardedAd.Destroy;
begin
  FCallbackDelegate.Free;
  inherited;
end;

procedure TPlatformRewardedAd.AdLoaded(const ARewardedAd: JRewardedAd);
begin
  FAd := ARewardedAd;
  FAd.show(TAndroidHelper.Activity, FUserEarnedRewardListener);
end;

procedure TPlatformRewardedAd.DoAdDismissedFullScreenContent;
begin
  FAd := nil;
  inherited;
end;

procedure TPlatformRewardedAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  inherited;
end;

procedure TPlatformRewardedAd.DoAdShowedFullScreenContent;
begin
  inherited;
end;

function TPlatformRewardedAd.GetAdUnitID: string;
begin
  if TestMode then
    Result := cTestAdUnitIdRewarded
  else
    Result := FAdUnitId;
end;

procedure TPlatformRewardedAd.Load;
var
  LRequest: JAdRequest;
  LAdUnitId: JString;
begin
  if FAd = nil then
  begin
    LRequest := TJAdRequest_Builder.JavaClass.init.build;
    LAdUnitId := StringToJString(AdUnitID);
    TJRewardedAd.JavaClass.load(TAndroidHelper.Context, LAdUnitId, LRequest, FCallbackDelegate.Callback);
  end;
  // else ad already showing
end;

procedure TPlatformRewardedAd.UserEarnedReward(const ARewardItem: JRewardItem);
begin
  //
end;

{ TAppOpenAdLoadCallbackDelegate }

constructor TAppOpenAdLoadCallbackDelegate.Create(const AAppOpenAd: TPlatformAppOpenAd);
begin
  inherited Create;
  FAppOpenAd := AAppOpenAd;
  FCallback := TJDWAppOpenAdLoadCallback.JavaClass.init(Self);
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdDismissedFullScreenContent;
begin
  FAppOpenAd.DoAdDismissedFullScreenContent;
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdFailedToLoad(loadAdError: JLoadAdError);
var
  LError: TAdError;
begin
  LError.ErrorCode := loadAdError.getCode;
  LError.Message := JStringToString(loadAdError.getMessage);
  FAppOpenAd.DoAdFailedToLoad(LError);
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdFailedToShowFullScreenContent(adError: JAdError);
var
  LError: TAdError;
begin
  LError.ErrorCode := adError.getCode;
  LError.Message := JStringToString(adError.getMessage);
  FAppOpenAd.DoAdFailedToShowFullScreenContent(LError);
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdLoaded(ad: JAppOpenAd);
begin
  FAppOpenAd.AdLoaded(ad);
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdShowedFullScreenContent;
begin
  FAppOpenAd.DoAdShowedFullScreenContent;
end;

{ TPlatformAppOpenAd }

constructor TPlatformAppOpenAd.Create(const AAppOpenAd: TAppOpenAd);
begin
  inherited;
  FCallbackDelegate := TAppOpenAdLoadCallbackDelegate.Create(Self);
end;

destructor TPlatformAppOpenAd.Destroy;
begin
  FCallbackDelegate.Free;
  inherited;
end;

procedure TPlatformAppOpenAd.AdLoaded(const AAd: JAppOpenAd);
begin
  FAd := AAd;
  DoAdLoaded;
end;

procedure TPlatformAppOpenAd.ApplicationBecameActive;
begin
  ShowAdIfAvailable(IsWarmStart);
end;

procedure TPlatformAppOpenAd.ApplicationEnteredBackground;
begin
  //
end;

procedure TPlatformAppOpenAd.DoAdDismissedFullScreenContent;
begin
  FAd := nil;
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdFailedToLoad(const AError: TAdError);
begin
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdLoaded;
begin
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdShowedFullScreenContent;
begin
  inherited;
end;

function TPlatformAppOpenAd.GetAdUnitID: string;
begin
  if TestMode then
    Result := cTestAdUnitIdAppOpen
  else
    Result := FAdUnitId;
end;

function TPlatformAppOpenAd.GetOrientation: Integer;
begin
  if Orientation = TAppOpenAdOrientation.Landscape then
    Result := TJAppOpenAd.JavaClass.APP_OPEN_AD_ORIENTATION_LANDSCAPE
  else
    Result := TJAppOpenAd.JavaClass.APP_OPEN_AD_ORIENTATION_PORTRAIT;
end;

procedure TPlatformAppOpenAd.LoadAd;
var
  LRequest: JAdRequest;
  LAdUnitId: JString;
begin
  if FAd = nil then
  begin
    LRequest := TJAdRequest_Builder.JavaClass.init.build;
    LAdUnitId := StringToJString(AdUnitID);
    TJAppOpenAd.JavaClass.load(TAndroidHelper.Context, LAdUnitId, LRequest, GetOrientation, FCallbackDelegate.Callback);
  end;
end;

procedure TPlatformAppOpenAd.ShowAdIfAvailable(const ACanShow: Boolean);
begin
  if (FAd <> nil) and ACanShow then // and load time was less than 4 hours ago??
    FAd.show(TAndroidHelper.Activity)
  else if FAd = nil then
    LoadAd;
end;

end.

unit DW.Androidapi.JNI.AdMob;

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.AdMob, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os;

type
  JAdManagerAdRequest = interface;
  JAdManagerAdRequest_Builder = interface;
  JAppOpenAd = interface;
  JAppOpenAd_AppOpenAdLoadCallback = interface;
  JOnAdMetadataChangedListener = interface;
  JRewardedAd = interface;
  JRewardedAdLoadCallback = interface;
  JRewardedInterstitialAd = interface;
  JRewardedInterstitialAdLoadCallback = interface;
  JRewardItem = interface;
  JServerSideVerificationOptions = interface;
  JServerSideVerificationOptions_Builder = interface;
  JOnUserEarnedRewardListener = interface;

  JAdManagerAdRequestClass = interface(JAdRequestClass)
    ['{8E42703E-BFF3-47D3-8F96-051881577F4F}']
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AdManagerAdRequest')]
  JAdManagerAdRequest = interface(JAdRequest)
    ['{71E6F778-E11E-4E68-BE10-8A505B234998}']
    function getCustomTargeting: JBundle; cdecl;
    function getPublisherProvidedId: JString; cdecl;
  end;
  TJAdManagerAdRequest = class(TJavaGenericImport<JAdManagerAdRequestClass, JAdManagerAdRequest>) end;

  JAdManagerAdRequest_BuilderClass = interface(JAdRequest_BuilderClass)
    ['{0BFC5AC0-B0E5-42B2-A414-17EEC263834D}']
    {class} function init: JAdManagerAdRequest_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AdManagerAdRequest$Builder')]
  JAdManagerAdRequest_Builder = interface(JAdRequest_Builder)
    ['{9950ED86-37FA-41E8-9400-07B509268B73}']
    function addCategoryExclusion(categoryExclusion: JString): JAdManagerAdRequest_Builder; cdecl;
    function addCustomTargeting(key: JString; value: JString): JAdManagerAdRequest_Builder; cdecl; overload;
    function addCustomTargeting(key: JString; values: JList): JAdManagerAdRequest_Builder; cdecl; overload;
    function build: JAdManagerAdRequest; cdecl;
    function setAdInfo(adInfo: JAdInfo): JAdManagerAdRequest_Builder; cdecl; // Not documented at https://developers.google.com/android/reference/com/google/android/gms/ads/admanager/AdManagerAdRequest.Builder
    function setPublisherProvidedId(publisherProvidedId: JString): JAdManagerAdRequest_Builder; cdecl;
  end;
  TJAdManagerAdRequest_Builder = class(TJavaGenericImport<JAdManagerAdRequest_BuilderClass, JAdManagerAdRequest_Builder>) end;

  JOnAdMetadataChangedListenerClass = interface(IJavaClass)
    ['{96737818-5D55-4AC1-8A3B-25BC0DB3F33B}']
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/OnAdMetadataChangedListener')]
  JOnAdMetadataChangedListener = interface(IJavaInstance)
    ['{80250F25-6423-4785-8898-B873B83C22F4}']
    procedure onAdMetadataChanged; cdecl;
  end;
  TJOnAdMetadataChangedListener = class(TJavaGenericImport<JOnAdMetadataChangedListenerClass, JOnAdMetadataChangedListener>) end;

  JServerSideVerificationOptionsClass = interface(JObjectClass)
    ['{1531CF1E-C0EF-4870-BF9F-28DA952E28CD}']
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/ServerSideVerificationOptions')]
  JServerSideVerificationOptions = interface(JObject)
    ['{7BCE4208-7FA2-4521-9D8E-06B8DCEF5F34}']
    function getCustomData: JString; cdecl;
    function getUserId: JString; cdecl;
  end;
  TJServerSideVerificationOptions = class(TJavaGenericImport<JServerSideVerificationOptionsClass, JServerSideVerificationOptions>) end;

  JServerSideVerificationOptions_BuilderClass = interface(JObjectClass)
    ['{C1551C7E-F144-41AA-B48A-63126380EEEF}']
    {class} function init: JServerSideVerificationOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/ServerSideVerificationOptions$Builder')]
  JServerSideVerificationOptions_Builder = interface(JObject)
    ['{09F5F371-C6C4-4602-86AB-C898619C3860}']
    function build: JServerSideVerificationOptions; cdecl;
    function setCustomData(customData: JString): JServerSideVerificationOptions_Builder; cdecl;
    function setUserId(userId: JString): JServerSideVerificationOptions_Builder; cdecl;
  end;
  TJServerSideVerificationOptions_Builder = class(TJavaGenericImport<JServerSideVerificationOptions_BuilderClass, JServerSideVerificationOptions_Builder>) end;

  JOnUserEarnedRewardListenerClass = interface(IJavaClass)
    ['{437EC0DD-1347-4EFE-A746-0F64AF4758B6}']
  end;

  [JavaSignature('com/google/android/gms/ads/OnUserEarnedRewardListener')]
  JOnUserEarnedRewardListener = interface(IJavaInstance)
    ['{398DC86E-DB82-4D04-9D35-E442959C234D}']
    procedure onUserEarnedReward(rewardItem: JRewardItem); cdecl;
  end;
  TJOnUserEarnedRewardListener = class(TJavaGenericImport<JOnUserEarnedRewardListenerClass, JOnUserEarnedRewardListener>) end;

  JRewardedAdClass = interface(JObjectClass)
    ['{207D6207-01C8-4C90-8F6E-05C126AA1781}']
    {class} function init: JRewardedAd; cdecl;
    {class} procedure load(context: JContext; adUnitId: JString; adRequest: JAdRequest; loadCallback: JRewardedAdLoadCallback); cdecl; overload;
    {class} procedure load(context: JContext; adUnitId: JString; adManagerAdRequest: JAdManagerAdRequest;
      loadCallback: JRewardedAdLoadCallback); cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/RewardedAd')]
  JRewardedAd = interface(JObject)
    ['{2BEB3585-63A4-4837-B2A5-E387CAF82971}']
    function getAdMetadata: JBundle; cdecl;
    function getAdUnitId: JString; cdecl;
    function getFullScreenContentCallback: JFullScreenContentCallback; cdecl;
    function getOnAdMetadataChangedListener: JOnAdMetadataChangedListener; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    function getRewardItem: JRewardItem; cdecl;
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure setImmersiveMode(immersiveModeEnabed: Boolean); cdecl;
    procedure setOnAdMetadataChangedListener(onAdMetadataChangedEventListener: JOnAdMetadataChangedListener); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setServerSideVerificationOptions(options: JServerSideVerificationOptions); cdecl;
    procedure show(activity: JActivity; onUserEarnedRewardListener: JOnUserEarnedRewardListener); cdecl;
  end;
  TJRewardedAd = class(TJavaGenericImport<JRewardedAdClass, JRewardedAd>) end;

  JRewardedAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{BA605914-4B95-4BFF-99F4-798640FFC410}']
    {class} function init: JRewardedAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/RewardedAdLoadCallback')]
  JRewardedAdLoadCallback = interface(JAdLoadCallback)
    ['{00E1C8B6-19B4-4163-A73D-735595543833}']
  end;
  TJRewardedAdLoadCallback = class(TJavaGenericImport<JRewardedAdLoadCallbackClass, JRewardedAdLoadCallback>) end;

  JRewardedInterstitialAdClass = interface(JObjectClass)
    ['{CA3EE48A-A9E0-4DDC-B4A2-535E9AF010B2}']
    {class} function init: JRewardedInterstitialAd; cdecl;
    {class} procedure load(context: JContext; adUnitId: JString; adRequest: JAdRequest;
      loadCallback: JRewardedInterstitialAdLoadCallback); cdecl; overload;
    {class} procedure load(context: JContext; adUnitId: JString; adManagerAdRequest: JAdManagerAdRequest;
      loadCallback: JRewardedInterstitialAdLoadCallback); cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/ads/rewardedinterstitial/RewardedInterstitialAd')]
  JRewardedInterstitialAd = interface(JObject)
    ['{0C482D95-82D3-4F8C-A5AE-244724C60764}']
    function getAdMetadata: JBundle; cdecl;
    function getAdUnitId: JString; cdecl;
    function getFullScreenContentCallback: JFullScreenContentCallback; cdecl;
    function getOnAdMetadataChangedListener: JOnAdMetadataChangedListener; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    function getRewardItem: JRewardItem; cdecl;
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure setImmersiveMode(immersiveModeEnabed: Boolean); cdecl;
    procedure setOnAdMetadataChangedListener(onAdMetadataChangedEventListener: JOnAdMetadataChangedListener); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setServerSideVerificationOptions(options: JServerSideVerificationOptions); cdecl;
    procedure show(activity: JActivity; onUserEarnedRewardListener: JOnUserEarnedRewardListener); cdecl;
  end;
  TJRewardedInterstitialAd = class(TJavaGenericImport<JRewardedInterstitialAdClass, JRewardedInterstitialAd>) end;

  JRewardedInterstitialAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{516D8730-6332-4131-838D-F970EC52FC3E}']
    {class} function init: JRewardedInterstitialAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewardedinterstitial/RewardedInterstitialAdLoadCallback')]
  JRewardedInterstitialAdLoadCallback = interface(JAdLoadCallback)
    ['{901F40C2-B8F3-4092-8139-F74E1ADFEEF4}']
  end;
  TJRewardedInterstitialAdLoadCallback = class(TJavaGenericImport<JRewardedInterstitialAdLoadCallbackClass, JRewardedInterstitialAdLoadCallback>) end;

  JRewardItemClass = interface(JObjectClass)
    ['{4581F808-6897-4913-8DBC-F85211B4F1A3}']
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/RewardItem')]
  JRewardItem = interface(JObject)
    ['{775A5CC4-25F8-4ACF-9B4E-CC80F2D2D405}']
    function getAmount: Integer; cdecl;
    function getType: JString; cdecl;
  end;
  TJRewardItem = class(TJavaGenericImport<JRewardItemClass, JRewardItem>) end;

  JAppOpenAdClass = interface(JObjectClass)
    ['{CF7B2FAF-2AD3-4FEC-8C7C-039FF2B6D17C}']
    {class} function _GetAPP_OPEN_AD_ORIENTATION_LANDSCAPE: Integer; cdecl;
    {class} function _GetAPP_OPEN_AD_ORIENTATION_PORTRAIT: Integer; cdecl;
    {class} function init: JAppOpenAd; cdecl;
    {class} procedure load(context: JContext; adUnitId: JString; adRequest: JAdRequest; orientation: Integer;
      loadCallback: JAppOpenAd_AppOpenAdLoadCallback); cdecl; overload;
    {class} procedure load(context: JContext; adUnitId: JString; adManagerAdRequest: JAdManagerAdRequest; orientation: Integer;
      loadCallback: JAppOpenAd_AppOpenAdLoadCallback); cdecl; overload;
    {class} property APP_OPEN_AD_ORIENTATION_LANDSCAPE: Integer read _GetAPP_OPEN_AD_ORIENTATION_LANDSCAPE;
    {class} property APP_OPEN_AD_ORIENTATION_PORTRAIT: Integer read _GetAPP_OPEN_AD_ORIENTATION_PORTRAIT;
  end;

  [JavaSignature('com/google/android/gms/ads/appopen/AppOpenAd')]
  JAppOpenAd = interface(JObject)
    ['{BC8CEE4E-CD43-42E0-8781-64459942580A}']
    function getAdUnitId: JString; cdecl;
    function getFullScreenContentCallback: JFullScreenContentCallback; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure setImmersiveMode(immersiveModeEnabled: Boolean); cdecl;
    procedure setOnPaidEventListener(listener: JOnPaidEventListener); cdecl;
    procedure show(activity: JActivity); cdecl;
  end;
  TJAppOpenAd = class(TJavaGenericImport<JAppOpenAdClass, JAppOpenAd>) end;

  JAppOpenAd_AppOpenAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{4E538ED4-0D7B-4BC7-94E2-79C19B7D33FE}']
    {class} function init: JAppOpenAd_AppOpenAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/appopen/AppOpenAd$AppOpenAdLoadCallback')]
  JAppOpenAd_AppOpenAdLoadCallback = interface(JAdLoadCallback)
    ['{83402806-C6C1-4945-A1F8-C1305405BF5E}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(ad: JAppOpenAd); cdecl;
  end;
  TJAppOpenAd_AppOpenAdLoadCallback = class(TJavaGenericImport<JAppOpenAd_AppOpenAdLoadCallbackClass, JAppOpenAd_AppOpenAdLoadCallback>) end;

implementation

end.

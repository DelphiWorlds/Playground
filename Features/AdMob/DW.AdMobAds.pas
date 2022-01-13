unit DW.AdMobAds;

interface

uses
  System.Classes, System.Messaging,
  DW.AdMob;

type
  TBaseAd = class;

  TCustomPlatformBaseAd = class(TObject)
  private
    class var FIsWarmStart: Boolean;
  private
    FBaseAd: TBaseAd;
    FTestMode: Boolean;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
  protected
    FAdUnitID: string;
    procedure ApplicationBecameActive; virtual;
    procedure ApplicationEnteredBackground; virtual;
    procedure DoAdFailedToLoad(const AError: TAdError); virtual;
    procedure DoAdLoaded; virtual;
    function GetAdUnitID: string; virtual;
    procedure Load; virtual;
    function IsWarmStart: Boolean;
    property AdUnitID: string read GetAdUnitID write FAdUnitID;
    property TestMode: Boolean read FTestMode write FTestMode;
  public
    constructor Create(const ABaseAd: TBaseAd);
    destructor Destroy; override;
  end;

  TFullScreenAd = class;

  TCustomPlatformFullScreenAd = class(TCustomPlatformBaseAd)
  private
    FFullScreenAd: TFullScreenAd;
  protected
    procedure DoAdDismissedFullScreenContent; virtual;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); virtual;
    procedure DoAdShowedFullScreenContent; virtual;
  public
    constructor Create(const AFullScreenAd: TFullScreenAd);
  end;

  TInterstitialAd = class;

  TCustomPlatformInterstitialAd = class(TCustomPlatformFullScreenAd)
  private
    FInterstitialAd: TInterstitialAd;
  public
    constructor Create(const AInterstitialAd: TInterstitialAd); virtual;
  end;

  TRewardedAd = class;

  TCustomPlatformRewardedAd = class(TCustomPlatformFullScreenAd)
  private
    FRewardedAd: TRewardedAd;
  public
    constructor Create(const ARewardedAd: TRewardedAd); virtual;
  end;

  TAppOpenAdOrientation = (Portrait, Landscape);

  TAppOpenAd = class;

  TCustomPlatformAppOpenAd = class(TCustomPlatformFullScreenAd)
  private
    FAppOpenAd: TAppOpenAd;
    FOrientation: TAppOpenAdOrientation;
  protected
    property Orientation: TAppOpenAdOrientation read FOrientation write FOrientation;
  public
    constructor Create(const AAppOpenAd: TAppOpenAd); virtual;
  end;

  TBaseAd = class(TObject)
  private
    FOnAdFailedToLoad: TAdErrorEvent;
    FOnAdLoaded: TNotifyEvent;
    function GetAdUnitID: string;
    procedure SetAdUnitID(const Value: string);
    function GetTestMode: Boolean;
    procedure SetTestMode(const Value: Boolean);
  protected
    procedure DoAdFailedToLoad(const AError: TAdError);
    procedure DoAdLoaded;
    function GetPlatformBaseAd: TCustomPlatformBaseAd; virtual; abstract;
    property PlatformBaseAd: TCustomPlatformBaseAd read GetPlatformBaseAd;
  public
    procedure Load;
    property AdUnitID: string read GetAdUnitID write SetAdUnitID;
    property TestMode: Boolean read GetTestMode write SetTestMode;
    property OnAdFailedToLoad: TAdErrorEvent read FOnAdFailedToLoad write FOnAdFailedToLoad;
    property OnAdLoaded: TNotifyEvent read FOnAdLoaded write FOnAdLoaded;
  end;

  TFullScreenAd = class(TBaseAd)
  private
    FOnAdDismissedFullScreenContent: TNotifyEvent;
    FOnAdFailedToShowFullScreenContent: TAdErrorEvent;
    FOnAdShowedFullScreenContent: TNotifyEvent;
  protected
    procedure DoAdDismissedFullScreenContent;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError);
    procedure DoAdShowedFullScreenContent;
  public
    property OnAdDismissedFullScreenContent: TNotifyEvent read FOnAdDismissedFullScreenContent write FOnAdDismissedFullScreenContent;
    property OnAdFailedToShowFullScreenContent: TAdErrorEvent read FOnAdFailedToShowFullScreenContent write FOnAdFailedToShowFullScreenContent;
    property OnAdShowedFullScreenContent: TNotifyEvent read FOnAdShowedFullScreenContent write FOnAdShowedFullScreenContent;
  end;

  TInterstitialAd = class(TFullScreenAd)
  private
    FPlatformInterstitialAd: TCustomPlatformInterstitialAd;
  protected
    function GetPlatformBaseAd: TCustomPlatformBaseAd; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRewardedAd = class(TFullScreenAd)
  private
    FPlatformRewardedAd: TCustomPlatformRewardedAd;
  protected
    function GetPlatformBaseAd: TCustomPlatformBaseAd; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAppOpenAd = class(TFullScreenAd)
  private
    FPlatformAppOpenAd: TCustomPlatformAppOpenAd;
    function GetOrientation: TAppOpenAdOrientation;
    procedure SetOrientation(const Value: TAppOpenAdOrientation);
  protected
    function GetPlatformBaseAd: TCustomPlatformBaseAd; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Orientation: TAppOpenAdOrientation read GetOrientation write SetOrientation;
  end;

implementation

uses
{$IF Defined(ANDROID)}
  DW.AdMobAds.Android,
{$ENDIF}
  System.SysUtils,
  FMX.Platform;

{$IF not Defined(ANDROID)}
type
  TPlatformInterstitialAd = class(TCustomPlatformInterstitialAd);
  TPlatformRewardedAd = class(TCustomPlatformRewardedAd);
  TPlatformAppOpenAd = class(TCustomPlatformAppOpenAd);
{$ENDIF}

{ TCustomPlatformBaseAd }

constructor TCustomPlatformBaseAd.Create(const ABaseAd: TBaseAd);
begin
  inherited Create;
  FBaseAd := ABaseAd;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TCustomPlatformBaseAd.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TCustomPlatformBaseAd.DoAdFailedToLoad(const AError: TAdError);
begin
  FBaseAd.DoAdFailedToLoad(AError);
end;

procedure TCustomPlatformBaseAd.DoAdLoaded;
begin
  FBaseAd.DoAdLoaded;
end;

function TCustomPlatformBaseAd.GetAdUnitID: string;
begin
  Result := FAdUnitID;
end;

function TCustomPlatformBaseAd.IsWarmStart: Boolean;
begin
  Result := FIsWarmStart;
end;

procedure TCustomPlatformBaseAd.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      ApplicationBecameActive;
      FIsWarmStart := True;
    end;
    TApplicationEvent.EnteredBackground:
      ApplicationEnteredBackground;
  end;
end;

procedure TCustomPlatformBaseAd.ApplicationBecameActive;
begin
  //
end;

procedure TCustomPlatformBaseAd.ApplicationEnteredBackground;
begin
  //
end;

procedure TCustomPlatformBaseAd.Load;
begin
  //
end;

{ TCustomPlatformFullScreenAd }

constructor TCustomPlatformFullScreenAd.Create(const AFullScreenAd: TFullScreenAd);
begin
  inherited Create(TBaseAd(AFullScreenAd));
  FFullScreenAd := AFullScreenAd;
end;

procedure TCustomPlatformFullScreenAd.DoAdDismissedFullScreenContent;
begin
  FFullScreenAd.DoAdDismissedFullScreenContent;
end;

procedure TCustomPlatformFullScreenAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  FFullScreenAd.DoAdFailedToShowFullScreenContent(AError);
end;

procedure TCustomPlatformFullScreenAd.DoAdShowedFullScreenContent;
begin
  FFullScreenAd.DoAdShowedFullScreenContent;
end;

{ TCustomPlatformInterstitialAd }

constructor TCustomPlatformInterstitialAd.Create(const AInterstitialAd: TInterstitialAd);
begin
  inherited Create(TFullScreenAd(AInterstitialAd));
  FInterstitialAd := AInterstitialAd;
end;

{ TCustomPlatformRewardedAd }

constructor TCustomPlatformRewardedAd.Create(const ARewardedAd: TRewardedAd);
begin
  inherited Create(TFullScreenAd(ARewardedAd));
  FRewardedAd := ARewardedAd;
end;

{ TCustomPlatformAppOpenAd }

constructor TCustomPlatformAppOpenAd.Create(const AAppOpenAd: TAppOpenAd);
begin
  inherited Create(TFullScreenAd(AAppOpenAd));
  FAppOpenAd := AAppOpenAd;
end;

{ TBaseAd }

procedure TBaseAd.DoAdFailedToLoad(const AError: TAdError);
begin
  if Assigned(FOnAdFailedToLoad) then
    FOnAdFailedToLoad(Self, AError);
end;

procedure TBaseAd.DoAdLoaded;
begin
  if Assigned(FOnAdLoaded) then
    FOnAdLoaded(Self);
end;

function TBaseAd.GetAdUnitID: string;
begin
  Result := PlatformBaseAd.AdUnitID;
end;

function TBaseAd.GetTestMode: Boolean;
begin
  Result := PlatformBaseAd.TestMode;
end;

procedure TBaseAd.Load;
begin
  if not AdUnitID.IsEmpty then
    PlatformBaseAd.Load;
end;

procedure TBaseAd.SetAdUnitID(const Value: string);
begin
  PlatformBaseAd.AdUnitID := Value;
end;

procedure TBaseAd.SetTestMode(const Value: Boolean);
begin
  PlatformBaseAd.TestMode := Value;
end;

{ TFullScreenAd }

procedure TFullScreenAd.DoAdDismissedFullScreenContent;
begin
  if Assigned(FOnAdDismissedFullScreenContent) then
    FOnAdDismissedFullScreenContent(Self);
end;

procedure TFullScreenAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  if Assigned(FOnAdFailedToShowFullScreenContent) then
    FOnAdFailedToShowFullScreenContent(Self, AError);
end;

procedure TFullScreenAd.DoAdShowedFullScreenContent;
begin
  if Assigned(FOnAdShowedFullScreenContent) then
    FOnAdShowedFullScreenContent(Self);
end;

{ TInterstitialAd }

constructor TInterstitialAd.Create;
begin
  inherited;
  FPlatformInterstitialAd := TPlatformInterstitialAd.Create(Self);
end;

destructor TInterstitialAd.Destroy;
begin
  FPlatformInterstitialAd.Free;
  inherited;
end;

function TInterstitialAd.GetPlatformBaseAd: TCustomPlatformBaseAd;
begin
  Result := FPlatformInterstitialAd;
end;

{ TRewardedAd }

constructor TRewardedAd.Create;
begin
  inherited;
  FPlatformRewardedAd := TPlatformRewardedAd.Create(Self);
end;

destructor TRewardedAd.Destroy;
begin
  FPlatformRewardedAd.Free;
  inherited;
end;

function TRewardedAd.GetPlatformBaseAd: TCustomPlatformBaseAd;
begin
  Result := FPlatformRewardedAd;
end;

{ TAppOpenAd }

constructor TAppOpenAd.Create;
begin
  inherited;
  FPlatformAppOpenAd := TPlatformAppOpenAd.Create(Self);
end;

destructor TAppOpenAd.Destroy;
begin
  FPlatformAppOpenAd.Free;
  inherited;
end;

function TAppOpenAd.GetOrientation: TAppOpenAdOrientation;
begin
  Result := FPlatformAppOpenAd.Orientation;
end;

function TAppOpenAd.GetPlatformBaseAd: TCustomPlatformBaseAd;
begin
  Result := FPlatformAppOpenAd;
end;

procedure TAppOpenAd.SetOrientation(const Value: TAppOpenAdOrientation);
begin
  FPlatformAppOpenAd.Orientation := Value;
end;

end.

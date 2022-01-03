unit DW.Advertising;

interface

uses
  System.Classes;

type
  TAdError = record
    ErrorCode: Integer;
    Message: string;
  end;

  TInterstitialAd = class;

  TCustomPlatformInterstitialAd = class(TObject)
  private
    FAdUnitID: string;
    FInterstitialAd: TInterstitialAd;
  protected
    procedure DoAdDismissedFullScreenContent; virtual;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); virtual;
    procedure DoAdShowedFullScreenContent; virtual;
    procedure Load; virtual;
    property AdUnitID: string read FAdUnitID write FAdUnitID;
  public
    constructor Create(const AInterstitialAd: TInterstitialAd); virtual;
  end;

  TAdErrorEvent = procedure(Sender: TObject; const Error: TAdError) of object;

  TInterstitialAd = class(TObject)
  private
    FPlatformInterstitialAd: TCustomPlatformInterstitialAd;
    FOnAdDismissedFullScreenContent: TNotifyEvent;
    FOnAdFailedToShowFullScreenContent: TAdErrorEvent;
    FOnAdShowedFullScreenContent: TNotifyEvent;
    function GetAdUnitID: string;
    procedure SetAdUnitID(const Value: string);
  protected
    procedure DoAdDismissedFullScreenContent;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError);
    procedure DoAdShowedFullScreenContent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load; virtual;
    property AdUnitID: string read GetAdUnitID write SetAdUnitID;
    property OnAdDismissedFullScreenContent: TNotifyEvent read FOnAdDismissedFullScreenContent write FOnAdDismissedFullScreenContent;
    property OnAdFailedToShowFullScreenContent: TAdErrorEvent read FOnAdFailedToShowFullScreenContent write FOnAdFailedToShowFullScreenContent;
    property OnAdShowedFullScreenContent: TNotifyEvent read FOnAdShowedFullScreenContent write FOnAdShowedFullScreenContent;
  end;

implementation

uses
{$IF Defined(ANDROID)}
  DW.Advertising.Android,
{$ENDIF}
  System.SysUtils;

{$IF not Defined(ANDROID)}
type
  TPlatformInterstitialAd = class(TCustomPlatformInterstitialAd);
{$ENDIF}

{ TCustomPlatformInterstitialAd }

constructor TCustomPlatformInterstitialAd.Create(const AInterstitialAd: TInterstitialAd);
begin
  inherited Create;
  FInterstitialAd := AInterstitialAd;
end;

procedure TCustomPlatformInterstitialAd.DoAdDismissedFullScreenContent;
begin
  FInterstitialAd.DoAdDismissedFullScreenContent;
end;

procedure TCustomPlatformInterstitialAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  FInterstitialAd.DoAdFailedToShowFullScreenContent(AError);
end;

procedure TCustomPlatformInterstitialAd.DoAdShowedFullScreenContent;
begin
  FInterstitialAd.DoAdShowedFullScreenContent;
end;

procedure TCustomPlatformInterstitialAd.Load;
begin
  //
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

procedure TInterstitialAd.DoAdDismissedFullScreenContent;
begin
  if Assigned(FOnAdDismissedFullScreenContent) then
    FOnAdDismissedFullScreenContent(Self);
end;

procedure TInterstitialAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  if Assigned(FOnAdFailedToShowFullScreenContent) then
    FOnAdFailedToShowFullScreenContent(Self, AError);
end;

procedure TInterstitialAd.DoAdShowedFullScreenContent;
begin
  if Assigned(FOnAdShowedFullScreenContent) then
    FOnAdShowedFullScreenContent(Self);
end;

function TInterstitialAd.GetAdUnitID: string;
begin
  Result := FPlatformInterstitialAd.AdUnitID;
end;

procedure TInterstitialAd.Load;
begin
  if not AdUnitID.IsEmpty then
    FPlatformInterstitialAd.Load;
end;

procedure TInterstitialAd.SetAdUnitID(const Value: string);
begin
  FPlatformInterstitialAd.AdUnitID := Value;
end;

end.

unit DW.Network.Provider;

interface

uses
  DW.Network.Types,
  DW.OSTimer;

type
  INetworkListener = interface(IInterface)
    ['{72F000FC-9E89-4E92-81DF-52DA8B2BA444}']
    procedure LocalAddressesChange;
  end;

  INetworkProvider = interface(IInterface)
    ['{4C664B3A-7399-462A-8383-0B4EE8D4D0CA}']
    procedure AddListener(const AListener: INetworkListener);
    function GetLocalAddresses: TIPAddresses;
    function IsLocalAddress(const AIP: string): Boolean;
    procedure RemoveListener(const AListener: INetworkListener);
  end;

  TNetworkListeners = TArray<INetworkListener>;

  TCustomPlatformNetworkProvider = class(TInterfacedObject, INetworkProvider)
  private
    FInterfaceFlagsFilter: TInterfaceFlags;
    FLocalAddresses: TIPAddressesList;
    FListeners: TNetworkListeners;
    FTimer: TOSTimer;
    procedure TimerIntervalHandler(Sender: TObject);
  protected
    procedure NotifyLocalAddresesChange; virtual;
    property InterfaceFlagsFilter: TInterfaceFlags read FInterfaceFlagsFilter write FInterfaceFlagsFilter;
  public
    { INetworkProvider }
    procedure AddListener(const AListener: INetworkListener);
    function GetLocalAddresses: TIPAddresses; virtual;
    function IsLocalAddress(const AIP: string): Boolean;
    procedure RemoveListener(const AListener: INetworkListener);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  NetworkProvider: INetworkProvider;

implementation

uses
  DW.OSLog,
{$IF Defined(MacOS)}
  DW.Network.Socket.Mac;
{$ENDIF}
{$IF Defined(MSWINDOWS)}
  DW.Network.Socket.Win;
{$ENDIF}
{$IF Defined(ANDROID) or Defined(ANDROID64)}
  DW.Network.Provider.Android;
{$ENDIF}

{ TCustomPlatformNetworkProvider }

constructor TCustomPlatformNetworkProvider.Create;
begin
  inherited;
  FTimer := TOSTimer.Create;
  FTimer.Interval := 1000;
  FTimer.OnInterval := TimerIntervalHandler;
end;

destructor TCustomPlatformNetworkProvider.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TCustomPlatformNetworkProvider.TimerIntervalHandler(Sender: TObject);
begin
  if FLocalAddresses.Update(GetLocalAddresses) then
    NotifyLocalAddresesChange;
end;

function TCustomPlatformNetworkProvider.GetLocalAddresses: TIPAddresses;
begin
  Result := [];
end;

procedure TCustomPlatformNetworkProvider.NotifyLocalAddresesChange;
var
  LListener: INetworkListener;
begin
  for LListener in FListeners do
    LListener.LocalAddressesChange;
end;

procedure TCustomPlatformNetworkProvider.RemoveListener(const AListener: INetworkListener);
var
  I: Integer;
begin
  for I := Length(FListeners) - 1 downto 0 do
  begin
    if FListeners[I] = AListener then
      Delete(FListeners, I, 1);
  end;
  FTimer.Enabled := Length(FListeners) > 0;
end;

function TCustomPlatformNetworkProvider.IsLocalAddress(const AIP: string): Boolean;
var
  LList: TIPAddressesList;
begin
  LList.Items := GetLocalAddresses;
  Result := LList.IndexOfIP(AIP) > -1;
end;

procedure TCustomPlatformNetworkProvider.AddListener(const AListener: INetworkListener);
begin
  FListeners := FListeners + [AListener];
  FTimer.Enabled := Length(FListeners) > 0;
end;

end.

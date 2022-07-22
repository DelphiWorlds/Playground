unit DW.Network;

interface

uses
  System.SysUtils, System.Net.Socket, System.Generics.Collections, System.Messaging,
  DW.OSTimer;

type
  TIPVersion = (IPv4, IPv6);

  TInterfaceFlag = (Up, Broadcast, Debug, Loopback, PointToPoint, NoTrailers, Running, NoARP, Promiscuous, AllMulti, OActive, Simplex, Link0, Link1,
    Link2, AltPhys, Multicast);

  TInterfaceFlags = set of TInterfaceFlag;

  TLocalAddress = record
    InterfaceFlags: TInterfaceFlags;
    InterfaceIndex: Integer;
    InterfaceName: string;
    IP: string;
    IPVersion: TIPVersion;
    function Equals(const AAddress: TLocalAddress): Boolean;
  end;

  TLocalAddresses = TArray<TLocalAddress>;

  TLocalAddressesHelper = record helper for TLocalAddresses
  public
    function Add(const AAddress: TLocalAddress): Boolean;
    procedure Clear;
    function Count: Integer;
    function Exists(const AAddress: TLocalAddress): Boolean;
    function IndexOfIP(const AIP: string): Integer;
    function Update(const AAddresses: TLocalAddresses): Boolean;
  end;

  INetworkNotifier = interface(IInterface)
    ['{72F000FC-9E89-4E92-81DF-52DA8B2BA444}']
    procedure LocalAddressesChange(const ALocalAddresses: TLocalAddresses);
  end;

  TNetworkNotifiers = TArray<INetworkNotifier>;

  TNetworkNotifiersHelper = record helper for TNetworkNotifiers
  public
    procedure Add(const ANotifier: INetworkNotifier);
    function Count: Integer;
    function Exists(const ANotifier: INetworkNotifier): Boolean;
    procedure NotifyLocalAddresesChange(const ALocalAddresses: TLocalAddresses);
    procedure Remove(const ANotifier: INetworkNotifier);
  end;

  TNetwork = class(TObject)
  private
    class var FCurrent: TNetwork;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    FInterfaceFlagsFilter: TInterfaceFlags;
    FLocalAddresses: TLocalAddresses;
    FNotifiers: TNetworkNotifiers;
    FTimer: TOSTimer;
    procedure InternalAddNotifier(const ANotifier: INetworkNotifier);
    procedure InternalRemoveNotifier(const ANotifier: INetworkNotifier);
    procedure TimerIntervalHandler(Sender: TObject);
  protected
    function DoGetLocalAddresses: TLocalAddresses; virtual;
    procedure NotifyLocalAddresesChange; virtual;
    property Notifiers: TNetworkNotifiers read FNotifiers;
  public
    class procedure AddNotifier(const ANotifier: INetworkNotifier);
    class procedure Error(const AOperation: string);
    class function GetLocalAddresses: TLocalAddresses;
    class function IsLocalAddress(const AIP: string): Boolean;
    class procedure RemoveNotifier(const ANotifier: INetworkNotifier);
    class property Current: TNetwork read FCurrent;
  public
    constructor Create;
    destructor Destroy; override;
    property InterfaceFlagsFilter: TInterfaceFlags read FInterfaceFlagsFilter write FInterfaceFlagsFilter;
  end;

  TDataReceivedEvent = procedure(Sender: TObject; const IP: string; const Data: TBytes) of object;

  TSocket = class(TObject)
  private
    FActive: Boolean;
    // FBroadcastEnabled: Boolean;
    FBuffer: TBytes;
    FGroup: string;
    FHandle: TSocketHandle;
    FInterfaceIndex: Integer;
    FInterfaceName: string;
    FIP: string;
    FIPVersion: TIPVersion;
    FIsBound: Boolean;
    // FLoopback: Boolean;
    FPort: Integer;
    // FTTL: Integer;
    function AllocateHandle: Boolean;
    procedure DeallocateHandle;
    procedure SetActive(const AValue: Boolean);
  protected
    function ApplyBroadcastEnabled: Boolean; virtual;
    function ApplyLoopback: Boolean; virtual;
    function ApplyTimeToLive: Boolean; virtual;
    function Bind: Boolean;
    function CreateHandle: TSocketHandle; virtual;
    procedure CloseHandle; virtual;
    function DoBind: Boolean; virtual;
    function DoRead(var AIP: string; var AData: TBytes): Boolean; virtual;
    function InvalidHandle: TSocketHandle;
    function JoinGroup: Boolean; virtual;
    function SendTo(const AData: TBytes; const APort: Integer): Boolean; overload;
    function SendTo(const AIP: string; const AData: TBytes; const APort: Integer): Boolean; overload; virtual;
    procedure SetPort(const AValue: Integer);
    property Buffer: TBytes read FBuffer write FBuffer;
  public
    constructor Create(const AAddress: TLocalAddress; const APort: Integer);
    destructor Destroy; override;
    function Broadcast(const AData: string; const APort: Integer): Boolean; overload;
    function Broadcast(const AData: TBytes; const APort: Integer): Boolean; overload;
    function Read(var AIP: string; var AData: TBytes): Boolean;
    property Active: Boolean read FActive write SetActive;
    property Group: string read FGroup write FGroup;
    property Handle: TSocketHandle read FHandle;
    property InterfaceIndex: Integer read FInterfaceIndex;
    property InterfaceName: string read FInterfaceName;
    property IPVersion: TIPVersion read FIPVersion write FIPVersion;
    property IP: string read FIP;
    property Port: Integer read FPort;
  end;

  TSockets = class(TList<TSocket>)
  public
    function AddSocket(const AAddress: TLocalAddress; const APort: Integer): TSocket;
    function Bind: Boolean;
    function Broadcast(const AData: TBytes; const APort: Integer): Boolean;
    procedure ClearSockets;
    function IndexOf(const AIPVersion: TIPVersion): Integer;
  end;

implementation

uses
  DW.OSLog,
{$IF Defined(MacOS)}
  DW.Network.Mac;
{$ENDIF}
{$IF Defined(MSWINDOWS)}
  DW.Network.Win;
{$ENDIF}
{$IF Defined(ANDROID) or Defined(ANDROID64)}
  DW.Network.Android;

type
  TPlatformSocket = class(TSocket);
{$ENDIF}

{ TLocalAddress }

function TLocalAddress.Equals(const AAddress: TLocalAddress): Boolean;
begin
  Result := IP.Equals(AAddress.IP) and (IPVersion = AAddress.IPVersion);
end;

{ TLocalAddressesHelper }

function TLocalAddressesHelper.Add(const AAddress: TLocalAddress): Boolean;
begin
  Result := False;
  if not Exists(AAddress) then
  begin
    Self := Self + [AAddress];
    Result := True;
  end;
end;

procedure TLocalAddressesHelper.Clear;
begin
  SetLength(Self, 0);
end;

function TLocalAddressesHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TLocalAddressesHelper.Exists(const AAddress: TLocalAddress): Boolean;
var
  LAddress: TLocalAddress;
begin
  for LAddress in Self do
  begin
    if LAddress.Equals(AAddress) then
      Exit(True);
  end;
  Result := False;
end;

function TLocalAddressesHelper.IndexOfIP(const AIP: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Self[I].IP.ToLower.Equals(AIP.ToLower) then
      Exit(I);
  end;
end;

function TLocalAddressesHelper.Update(const AAddresses: TLocalAddresses): Boolean;
var
  LAddress: TLocalAddress;
  I: Integer;
begin
  Result := False;
  for LAddress in AAddresses do
  begin
    if Add(LAddress) then
      Result := True;
  end;
  for I := Count - 1 downto 0 do
  begin
    if not AAddresses.Exists(Self[I]) then
    begin
      Delete(Self, I, 1);
      Result := True;
    end;
  end;
end;

{ TNetworkNotifiersHelper }

procedure TNetworkNotifiersHelper.Add(const ANotifier: INetworkNotifier);
begin
  if not Exists(ANotifier) then
    Self := Self + [ANotifier];
end;

function TNetworkNotifiersHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TNetworkNotifiersHelper.Exists(const ANotifier: INetworkNotifier): Boolean;
var
  LNotifier: INetworkNotifier;
begin
  for LNotifier in Self do
  begin
    if LNotifier = ANotifier then
      Exit(True);
  end;
  Result := False;
end;

procedure TNetworkNotifiersHelper.NotifyLocalAddresesChange(const ALocalAddresses: TLocalAddresses);
var
  LNotifier: INetworkNotifier;
begin
  for LNotifier in Self do
    LNotifier.LocalAddressesChange(ALocalAddresses);
end;

procedure TNetworkNotifiersHelper.Remove(const ANotifier: INetworkNotifier);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Self[I] = ANotifier then
    begin
      Delete(Self, I, 1);
      Break;
    end;
  end;
end;

{ TNetwork }

class constructor TNetwork.CreateClass;
begin
  FCurrent := TPlatformNetwork.Create;
end;

class destructor TNetwork.DestroyClass;
begin
  FCurrent.Free;
end;

procedure TNetwork.TimerIntervalHandler(Sender: TObject);
begin
  if FLocalAddresses.Update(DoGetLocalAddresses) then
    NotifyLocalAddresesChange;
end;

constructor TNetwork.Create;
begin
  inherited;
  FTimer := TOSTimer.Create;
  FTimer.Interval := 1000;
  FTimer.OnInterval := TimerIntervalHandler;
end;

destructor TNetwork.Destroy;
begin
  FTimer.Free;
  inherited;
end;

function TNetwork.DoGetLocalAddresses: TLocalAddresses;
begin
  Result := [];
end;

class function TNetwork.GetLocalAddresses: TLocalAddresses;
begin
  Result := Current.DoGetLocalAddresses;
end;

procedure TNetwork.NotifyLocalAddresesChange;
begin
  FNotifiers.NotifyLocalAddresesChange(FLocalAddresses);
end;

procedure TNetwork.InternalRemoveNotifier(const ANotifier: INetworkNotifier);
begin
  FNotifiers.Remove(ANotifier);
  FTimer.Enabled := FNotifiers.Count > 0;
end;

class function TNetwork.IsLocalAddress(const AIP: string): Boolean;
begin
  Result := GetLocalAddresses.IndexOfIP(AIP) > -1;
end;

class procedure TNetwork.RemoveNotifier(const ANotifier: INetworkNotifier);
begin
  Current.InternalRemoveNotifier(ANotifier);
end;

class procedure TNetwork.Error(const AOperation: string);
begin
  TOSLog.d('Error during %s: %s', [AOperation, SysErrorMessage(GetLastError)]);
end;

procedure TNetwork.InternalAddNotifier(const ANotifier: INetworkNotifier);
begin
  FNotifiers.Add(ANotifier);
  FTimer.Enabled := FNotifiers.Count > 0;
end;

class procedure TNetwork.AddNotifier(const ANotifier: INetworkNotifier);
begin
  Current.InternalAddNotifier(ANotifier);
end;

{ TSocket }

constructor TSocket.Create(const AAddress: TLocalAddress; const APort: Integer);
begin
  inherited Create;
  FHandle := InvalidHandle;
  SetLength(FBuffer, 1024);
  FIP := AAddress.IP;
  FIPVersion := AAddress.IPVersion;
  FInterfaceIndex := AAddress.InterfaceIndex;
  FInterfaceName := AAddress.InterfaceName;
  SetPort(APort);
end;

destructor TSocket.Destroy;
begin
  Active := False;
  inherited;
end;

function TSocket.Bind: Boolean;
begin
  if not FIsBound then
  begin
    FIsBound := {(IPVersion = TIPVersion.IPv6) or } DoBind;
    if not FIsBound then
      TNetwork.Error('TSocket.Bind')
    else
      TOSLog.d('Bound %s to %d', [IP, Port]);
  end;
  Result := FIsBound;
end;

procedure TSocket.CloseHandle;
begin
  //
end;

procedure TSocket.DeallocateHandle;
begin
  if FHandle <> InvalidHandle then
    CloseHandle;
  FIsBound := False;
  FHandle := InvalidHandle;
end;

function TSocket.DoBind: Boolean;
begin
  Result := False;
end;

function TSocket.DoRead(var AIP: string; var AData: TBytes): Boolean;
begin
  Result := False;
end;

function TSocket.InvalidHandle: TSocketHandle;
begin
  Result := TSocketHandle(-1);
end;

function TSocket.AllocateHandle: Boolean;
begin
  Result := FHandle <> InvalidHandle;
  if not Result then
  begin
    FHandle := CreateHandle;
    Result := FHandle <> InvalidHandle;
    if not Result then
      TNetwork.Error('TSocket.AllocateHandle');
  end;
end;

function TSocket.ApplyBroadcastEnabled: Boolean;
begin
  Result := False;
end;

function TSocket.ApplyLoopback: Boolean;
begin
  Result := False;
end;

function TSocket.ApplyTimeToLive: Boolean;
begin
  Result := False;
end;

function TSocket.Broadcast(const AData: string; const APort: Integer): Boolean;
var
  LData: TBytes;
begin
  LData := TEncoding.Default.GetBytes(AData);
  Result := Broadcast(LData, APort);
end;

function TSocket.Broadcast(const AData: TBytes; const APort: Integer): Boolean;
begin
  Result := False;
  if AllocateHandle and Bind then
  begin
    if ApplyLoopback and ApplyTimeToLive and JoinGroup then
      Result := SendTo(AData, APort)
    else
      TNetwork.Error('TSocket.Broadcast');
  end;
end;

function TSocket.CreateHandle: TSocketHandle;
begin
  Result := InvalidHandle;
end;

function TSocket.JoinGroup: Boolean;
begin
  Result := False;
end;

function TSocket.Read(var AIP: string; var AData: TBytes): Boolean;
begin
  Result := DoRead(AIP, AData);
end;

procedure TSocket.SetActive(const AValue: Boolean);
begin
  if AValue and not FActive then
  begin
    FActive := AllocateHandle and Bind and JoinGroup;
    if not FActive then
      DeallocateHandle;
  end
  else if not AValue and FActive then
  begin
    FActive := False;
    DeallocateHandle;
  end;
end;

function TSocket.SendTo(const AIP: string; const AData: TBytes; const APort: Integer): Boolean;
begin
  Result := False;
end;

function TSocket.SendTo(const AData: TBytes; const APort: Integer): Boolean;
begin
  Result := SendTo(Group, AData, APort);
  if not Result then
    TNetwork.Error('TSocket.SendTo');
end;

procedure TSocket.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

{ TSockets }

function TSockets.AddSocket(const AAddress: TLocalAddress; const APort: Integer): TSocket;
var
  LSocket: TSocket;
begin
  Result := nil;
  for LSocket in Self do
  begin
    if LSocket.IP.Equals(AAddress.IP) then
    begin
      Result := LSocket;
      Result.SetPort(APort);
      Break;
    end;
  end;
  if Result = nil then
  begin
    Result := TPlatformSocket.Create(AAddress, APort);
    Add(Result);
  end;
end;

function TSockets.Bind: Boolean;
var
  LSocket: TSocket;
  LActive: Integer;
begin
  LActive := 0;
  for LSocket in Self do
  begin
    LSocket.Active := True;
    if LSocket.Active then
      Inc(LActive);
  end;
  Result := (Count = 0) or (LActive > 0); // Might want a "fail if any bind fails" option
end;

function TSockets.Broadcast(const AData: TBytes; const APort: Integer): Boolean;
var
  I: Integer;
begin
  Result := Count > 0;
  for I := 0 to Count - 1 do
  begin
    if not Items[I].Broadcast(AData, APort) then
      Result := False;
  end;
end;

procedure TSockets.ClearSockets;
begin
  while Count > 0 do
  begin
    Items[0].Free;
    Delete(0);
  end;
end;

function TSockets.IndexOf(const AIPVersion: TIPVersion): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].IPVersion = AIPVersion then
      Exit(I);
  end;
end;

end.

unit DW.Multicaster;

interface

uses
  System.SysUtils, System.Classes,
  DW.Network;

type
  TMulticaster = class;

  TCustomPlatformMulticaster = class(TObject)
  private
    FActive: Boolean;
    FGroupIPv4: string;
    FGroupIPv6: string;
    FMaxPacketSize: Integer;
    FMulticaster: TMulticaster;
    FPort: Integer;
  protected
    procedure Broadcast(const AData: TBytes; const APort: Integer); virtual;
    procedure DoDataReceived(const AIP: string; const AData: TBytes);
    procedure SetActive(const Value: Boolean);
    procedure SetGroupIPv4(const Value: string); virtual;
    procedure SetGroupIPv6(const Value: string); virtual;
    procedure SetPort(const Value: Integer); virtual;
    function Start: Boolean; virtual;
    procedure Stop; virtual;
    property Active: Boolean read FActive write SetActive;
    property GroupIPv4: string read FGroupIPv4 write SetGroupIPv4;
    property GroupIPv6: string read FGroupIPv6 write SetGroupIPv6;
    property MaxPacketSize: Integer read FMaxPacketSize write FMaxPacketSize;
    property Port: Integer read FPort write SetPort;
  public
    constructor Create(const AMulticaster: TMulticaster); virtual;
  end;

  TCustomPlatformSocketMulticaster = class;

  TMulticasterNetworkNotifier = class(TInterfacedObject, INetworkNotifier)
  private
    FMulticaster: TCustomPlatformSocketMulticaster;
  protected
    { INetworkNotifier }
    procedure LocalAddressesChange(const ALocalAddresses: TLocalAddresses);
  public
    constructor Create(const AMulticaster: TCustomPlatformSocketMulticaster);
  end;

  TCustomPlatformSocketMulticaster = class(TCustomPlatformMulticaster)
  private
    FNotifier: INetworkNotifier;
    FReadThread: TThread;
    FReceivers: TSockets;
    FSenders: TSockets;
    function AddAnySocket: TSocket;
    procedure UpdateSocketGroup(const ASocket: TSocket);
  protected
    procedure Broadcast(const AData: TBytes; const APort: Integer); override;
    procedure LocalAddressesChange(const ALocalAddresses: TLocalAddresses);
    procedure ReadData;
    function Start: Boolean; override;
    procedure Stop; override;
    procedure UpdateReceivers;
    procedure UpdateSenders;
  public
    constructor Create(const AMulticaster: TMulticaster); override;
    destructor Destroy; override;
  end;

  TMulticaster = class(TObject)
  private
    FPlatformMulticaster: TCustomPlatformMulticaster;
    FOnDataReceived: TDataReceivedEvent;
    function GetActive: Boolean;
    function GetGroupIPv4: string;
    function GetMaxPacketSize: Integer;
    function GetPort: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetGroupIPv4(const Value: string);
    procedure SetGroupIPv6(const Value: string);
    procedure SetMaxPacketSize(const Value: Integer);
    procedure SetPort(const Value: Integer);
    function GetGroupIPv6: string;
  protected
    procedure DoDataReceived(const AIP: string; const AData: TBytes);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Broadcast(const AData: string; const APort: Integer); overload;
    procedure Broadcast(const AData: TBytes; const APort: Integer); overload;
    property Active: Boolean read GetActive write SetActive;
    property GroupIPv4: string read GetGroupIPv4 write SetGroupIPv4;
    property GroupIPv6: string read GetGroupIPv6 write SetGroupIPv6;
    property MaxPacketSize: Integer read GetMaxPacketSize write SetMaxPacketSize;
    property Port: Integer read GetPort write SetPort;
    property OnDataReceived: TDataReceivedEvent read FOnDataReceived write FOnDataReceived;
  end;

const
  // New values based on Indy
  cMulticastGroupDefaultIPv4 = '224.0.0.1';
  cMulticastGroupDefaultIPv6 = 'FF01::1';

implementation

uses
  {$IF Defined(ANDROID) or DEFINED(ANDROID64)}
  DW.Multicaster.Android,
  {$ENDIF}
  DW.OSLog;

type
  {$IF not Defined(ANDROID) and not DEFINED(ANDROID64)}
  TPlatformMulticaster = class(TCustomPlatformSocketMulticaster);
  {$ENDIF}

  TReadThread = class(TThread)
  private
    FMulticaster: TCustomPlatformSocketMulticaster;
  protected
    procedure Execute; override;
  public
    constructor Create(const AMulticaster: TCustomPlatformSocketMulticaster);
  end;

{ TReadThread }

constructor TReadThread.Create(const AMulticaster: TCustomPlatformSocketMulticaster);
begin
  FMulticaster := AMulticaster;
  inherited Create(False);
end;

procedure TReadThread.Execute;
begin
  while not Terminated do
    FMulticaster.ReadData;
end;

{ TMulticasterNetworkNotifier }

constructor TMulticasterNetworkNotifier.Create(const AMulticaster: TCustomPlatformSocketMulticaster);
begin
  inherited Create;
  FMulticaster := AMulticaster;
end;

procedure TMulticasterNetworkNotifier.LocalAddressesChange(const ALocalAddresses: TLocalAddresses);
begin
  FMulticaster.LocalAddressesChange(ALocalAddresses);
end;

{ TCustomPlatformMulticaster }

constructor TCustomPlatformMulticaster.Create(const AMulticaster: TMulticaster);
begin
  inherited Create;
  FMaxPacketSize := 1024;
  FMulticaster := AMulticaster;
  FGroupIPv4 := cMulticastGroupDefaultIPv4;
  FGroupIPv6 := cMulticastGroupDefaultIPv6;
end;

function TCustomPlatformMulticaster.Start: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformMulticaster.Stop;
begin
  //
end;

procedure TCustomPlatformMulticaster.DoDataReceived(const AIP: string; const AData: TBytes);
begin
  if not TNetwork.IsLocalAddress(AIP) then
    FMulticaster.DoDataReceived(AIP, AData);
end;

procedure TCustomPlatformMulticaster.Broadcast(const AData: TBytes; const APort: Integer);
begin
  //
end;

procedure TCustomPlatformMulticaster.SetActive(const Value: Boolean);
begin
  if Value and not FActive then
  begin
    FActive := Start;
    if not FActive then
      Stop;
  end
  else if not Value and FActive then
  begin
    FActive := False;
    Stop;
  end;
end;

procedure TCustomPlatformMulticaster.SetGroupIPv4(const Value: string);
begin
  FGroupIPv4 := Value;
end;

procedure TCustomPlatformMulticaster.SetGroupIPv6(const Value: string);
begin
  FGroupIPv6 := Value;
end;

procedure TCustomPlatformMulticaster.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

{ TCustomPlatformSocketMulticaster }

constructor TCustomPlatformSocketMulticaster.Create(const AMulticaster: TMulticaster);
begin
  inherited;
  FReceivers := TSockets.Create;
  FSenders := TSockets.Create;
  FNotifier := TMulticasterNetworkNotifier.Create(Self);
  TNetwork.AddNotifier(FNotifier);
end;

destructor TCustomPlatformSocketMulticaster.Destroy;
begin
  TNetwork.RemoveNotifier(FNotifier);
  Stop;
  FReceivers.Free;
  FSenders.Free;
  inherited;
end;

procedure TCustomPlatformSocketMulticaster.LocalAddressesChange(const ALocalAddresses: TLocalAddresses);
begin
  if Active then
    Start;
  FSenders.ClearSockets; // They'll update next Broadcast
end;

function TCustomPlatformSocketMulticaster.Start: Boolean;
begin
  Stop;
  UpdateReceivers;
  Result := FReceivers.Bind;
  if Result then
    FReadThread := TReadThread.Create(Self);
end;

procedure TCustomPlatformSocketMulticaster.Stop;
begin
  if FReadThread <> nil then
  begin
    FReadThread.Terminate;
    FReadThread.WaitFor;
  end;
  FReadThread.Free;
  FReadThread := nil;
end;

function TCustomPlatformSocketMulticaster.AddAnySocket: TSocket;
var
  LLocalAddress: TLocalAddress;
  LIndex: Integer;
begin
  LIndex := FReceivers.IndexOf(TIPVersion.IPv4);
  if LIndex = -1 then
  begin
    LLocalAddress.IP := '0.0.0.0';
    LLocalAddress.IPVersion := TIPVersion.IPv4;
    LLocalAddress.InterfaceIndex := 0;
    Result := FReceivers.AddSocket(LLocalAddress, Port);
  end
  else
    Result := FReceivers[LIndex];
end;

procedure TCustomPlatformSocketMulticaster.UpdateReceivers;
var
  LLocalAddress: TLocalAddress;
  LSocket: TSocket;
begin
  FReceivers.ClearSockets;
  for LLocalAddress in TNetwork.GetLocalAddresses do
  begin
    // It appears macOS/iOS cannot listen on individual IPv4 addresses - sucky!
    if (LLocalAddress.IPVersion = TIPVersion.IPv4) and (TOSVersion.Platform in [TOSVersion.TPlatform.pfMacOS, TOSVersion.TPlatform.pfIOS]) then
      LSocket := AddAnySocket
    else
      LSocket := FReceivers.AddSocket(LLocalAddress, Port);
    UpdateSocketGroup(LSocket);
  end;
end;

procedure TCustomPlatformSocketMulticaster.UpdateSenders;
var
  LLocalAddress: TLocalAddress;
  LSocket: TSocket;
begin
  FSenders.ClearSockets;
  for LLocalAddress in TNetwork.GetLocalAddresses do
  begin
    LSocket := FSenders.AddSocket(LLocalAddress, 0);
    UpdateSocketGroup(LSocket);
  end;
end;

procedure TCustomPlatformSocketMulticaster.UpdateSocketGroup(const ASocket: TSocket);
begin
  case ASocket.IPVersion of
    TIPVersion.IPV4:
      ASocket.Group := GroupIPv4;
    TIPVersion.IPV6:
      ASocket.Group := GroupIPv6;
  end;
end;

procedure TCustomPlatformSocketMulticaster.Broadcast(const AData: TBytes; const APort: Integer);
begin
  if FSenders.Count = 0 then
    UpdateSenders;
  FSenders.Broadcast(AData, APort);
end;

procedure TCustomPlatformSocketMulticaster.ReadData;
var
  I: Integer;
  LIP: string;
  LData: TBytes;
begin
  for I := 0 to FReceivers.Count - 1 do
  begin
    if FReceivers[I].Read(LIP, LData) then
      DoDataReceived(LIP, LData);
  end;
  Sleep(0);
end;

{ TMulticaster }

constructor TMulticaster.Create;
begin
  inherited;
  FPlatformMulticaster := TPlatformMulticaster.Create(Self);
end;

destructor TMulticaster.Destroy;
begin
  FPlatformMulticaster.Free;
  inherited;
end;

procedure TMulticaster.DoDataReceived(const AIP: string; const AData: TBytes);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(Self, AIP, AData);
end;

function TMulticaster.GetActive: Boolean;
begin
  Result := FPlatformMulticaster.Active;
end;

function TMulticaster.GetGroupIPv4: string;
begin
  Result := FPlatformMulticaster.GroupIPv4;
end;

function TMulticaster.GetGroupIPv6: string;
begin
  Result := FPlatformMulticaster.GroupIPv6;
end;

function TMulticaster.GetMaxPacketSize: Integer;
begin
  Result := FPlatformMulticaster.MaxPacketSize;
end;

function TMulticaster.GetPort: Integer;
begin
  Result := FPlatformMulticaster.Port;
end;

procedure TMulticaster.Broadcast(const AData: TBytes; const APort: Integer);
begin
  FPlatformMulticaster.Broadcast(AData, APort);
end;

procedure TMulticaster.Broadcast(const AData: string; const APort: Integer);
begin
  Broadcast(TEncoding.Default.GetBytes(AData), APort);
end;

procedure TMulticaster.SetActive(const Value: Boolean);
begin
  FPlatformMulticaster.Active := Value;
end;

procedure TMulticaster.SetGroupIPv4(const Value: string);
begin
  FPlatformMulticaster.GroupIPv4 := Value;
end;

procedure TMulticaster.SetGroupIPv6(const Value: string);
begin
  FPlatformMulticaster.GroupIPv6 := Value;
end;

procedure TMulticaster.SetMaxPacketSize(const Value: Integer);
begin
  FPlatformMulticaster.MaxPacketSize := Value;
end;

procedure TMulticaster.SetPort(const Value: Integer);
begin
  FPlatformMulticaster.Port := Value;
end;

end.

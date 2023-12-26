unit DW.Network.Multicast;

interface

uses
  System.SysUtils, System.Classes,
  DW.Network.Types, DW.Network.Provider, DW.Network.Socket;

type
  IMulticast = interface(IInterface)
    ['{A3492393-4EE4-4B1E-9198-22657158B7C7}']
    function GetGroupIPv4: string;
    function GetGroupIPv6: string;
    procedure SetGroupIPv4(const Value: string);
    procedure SetGroupIPv6(const Value: string);
    property GroupIPv4: string read GetGroupIPv4 write SetGroupIPv4;
    property GroupIPv6: string read GetGroupIPv6 write SetGroupIPv6;
  end;

  IMulticastSender = interface(IMulticast)
    ['{A3492393-4EE4-4B1E-9198-22657158B7C7}']
    procedure Broadcast(const AData: string; const APort: Integer); overload;
    procedure Broadcast(const AData: TBytes; const APort: Integer); overload;
  end;

  TDataReceivedEvent = procedure(Sender: TObject; const IP: string; const Data: TBytes) of object;

  IMulticastReceiver = interface(IMulticast)
    ['{A3492393-4EE4-4B1E-9198-22657158B7C7}']
    function GetIsActive: Boolean;
    function GetOnDataReceived: TDataReceivedEvent;
    function GetPort: Integer;
    procedure SetIsActive(const Value: Boolean);
    procedure SetOnDataReceived(const Value: TDataReceivedEvent);
    procedure SetPort(const Value: Integer);
    procedure ReadData;
    function Start: Boolean;
    procedure Stop;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property Port: Integer read GetPort write SetPort;
    property OnDataReceived: TDataReceivedEvent read GetOnDataReceived write SetOnDataReceived;
  end;

  IMulticastProvider = interface(IInterface)
    ['{A2AB94BC-6775-4776-93C1-61846A651B26}']
    function CreateReceiver: IMulticastReceiver;
    function CreateSender: IMulticastSender;
  end;

//    property MaxPacketSize: Integer read FMaxPacketSize write FMaxPacketSize;

  TMulticast = class(TInterfacedObject, IMulticast, INetworkListener)
  private
    FGroupIPv4: string;
    FGroupIPv6: string;
    procedure UpdateSocketGroup(const ASocket: TSocket);
  public
    { IMulticast }
    function GetGroupIPv4: string;
    function GetGroupIPv6: string;
    procedure SetGroupIPv4(const Value: string);
    procedure SetGroupIPv6(const Value: string);
    { INetworkListener }
    procedure LocalAddressesChange; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property GroupIPv4: string read GetGroupIPv4;
    property GroupIPv6: string read GetGroupIPv6;
  end;

  TCustomMulticastReceiver = class(TMulticast, IMulticastReceiver)
  private
    FIsActive: Boolean;
    FPort: Integer;
    FOnDataReceived: TDataReceivedEvent;
  protected
    procedure DoDataReceived(const AIP: string; const AData: TBytes);
  public
    { IMulticastReceiver }
    function GetIsActive: Boolean;
    function GetOnDataReceived: TDataReceivedEvent;
    function GetPort: Integer;
    procedure SetIsActive(const Value: Boolean);
    procedure SetOnDataReceived(const Value: TDataReceivedEvent);
    procedure SetPort(const Value: Integer);
    procedure ReadData; virtual;
    function Start: Boolean; virtual;
    procedure Stop; virtual;
  public
    property IsActive: Boolean read GetIsActive;
    property Port: Integer read GetPort;
  end;

  TMulticastReceiver = class(TCustomMulticastReceiver)
  private
    FReadThread: TThread;
    FReceivers: TSockets;
    function AddAnySocket: TSocket;
    procedure UpdateReceivers;
  public
    { IMulticastReceiver }
    procedure ReadData; override;
    function Start: Boolean; override;
    procedure Stop; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LocalAddressesChange; override;
  end;

  TCustomMulticastSender = class(TMulticast, IMulticastSender)
  public
    { IMulticastSender }
    procedure Broadcast(const AData: string; const APort: Integer); overload;
    procedure Broadcast(const AData: TBytes; const APort: Integer); overload; virtual;
  end;

  TMulticastSender = class(TCustomMulticastSender)
  private
    FSenders: TSockets;
    procedure UpdateSenders;
  public
    procedure Broadcast(const AData: TBytes; const APort: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LocalAddressesChange; override;
  end;

const
  // New values based on Indy
  cMulticastGroupDefaultIPv4 = '224.0.0.1';
  cMulticastGroupDefaultIPv6 = 'FF01::1';

var
  MulticastProvider: IMulticastProvider;

implementation

uses
  {$IF Defined(ANDROID) or DEFINED(ANDROID64)}
  DW.Network.Multicast.Android,
  {$ENDIF}
  DW.OSLog;

type
  TMulticastReadThread = class(TThread)
  private
    FReceiver: IMulticastReceiver;
  protected
    procedure Execute; override;
  public
    constructor Create(const AReceiver: IMulticastReceiver);
  end;

  TMulticastProvider = class(TInterfacedObject, IMulticastProvider)
  public
    { IMulticastProvider }
    function CreateReceiver: IMulticastReceiver;
    function CreateSender: IMulticastSender;
  end;

{ TMulticastReadThread }

constructor TMulticastReadThread.Create(const AReceiver: IMulticastReceiver);
begin
  FReceiver := AReceiver;
  inherited Create(False);
end;

procedure TMulticastReadThread.Execute;
begin
  while not Terminated do
    FReceiver.ReadData;
end;

{ TMulticast }

constructor TMulticast.Create;
begin
  inherited Create;
  NetworkProvider.AddListener(Self);
  // FMaxPacketSize := 1024;
  FGroupIPv4 := cMulticastGroupDefaultIPv4;
  FGroupIPv6 := cMulticastGroupDefaultIPv6;
end;

destructor TMulticast.Destroy;
begin
  if NetworkProvider <> nil then
    NetworkProvider.RemoveListener(Self);
  inherited;
end;

function TMulticast.GetGroupIPv4: string;
begin
  Result := FGroupIPv4;
end;

function TMulticast.GetGroupIPv6: string;
begin
  Result := FGroupIPv6;
end;

procedure TMulticast.LocalAddressesChange;
begin
  //
end;

procedure TMulticast.SetGroupIPv4(const Value: string);
begin
  FGroupIPv4 := Value;
end;

procedure TMulticast.SetGroupIPv6(const Value: string);
begin
  FGroupIPv6 := Value;
end;

procedure TMulticast.UpdateSocketGroup(const ASocket: TSocket);
begin
  case ASocket.IPVersion of
    TIPVersion.IPV4:
      ASocket.Group := FGroupIPv4;
    TIPVersion.IPV6:
      ASocket.Group := FGroupIPv6;
  end;
end;

{ TCustomMulticastReceiver }

procedure TCustomMulticastReceiver.DoDataReceived(const AIP: string; const AData: TBytes);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(Self, AIP, AData);
end;

function TCustomMulticastReceiver.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

function TCustomMulticastReceiver.GetPort: Integer;
begin
  Result := FPort;
end;

procedure TCustomMulticastReceiver.ReadData;
begin
  //
end;

procedure TCustomMulticastReceiver.SetPort(const Value: Integer);
begin
  //!!!!! Need to change all the sockets, bro
  FPort := Value;
end;

function TCustomMulticastReceiver.Start: Boolean;
begin
  Result := False;
end;

procedure TCustomMulticastReceiver.Stop;
begin
  //
end;

function TCustomMulticastReceiver.GetOnDataReceived: TDataReceivedEvent;
begin
  Result := FOnDataReceived;
end;

procedure TCustomMulticastReceiver.SetOnDataReceived(const Value: TDataReceivedEvent);
begin
  FOnDataReceived := Value;
end;

procedure TCustomMulticastReceiver.SetIsActive(const Value: Boolean);
begin
  if Value and not FIsActive then
  begin
    FIsActive := Start;
    if not FIsActive then
      Stop;
  end
  else if not Value and FIsActive then
  begin
    FIsActive := False;
    Stop;
  end;
end;

{ TMulticastReceiver }

constructor TMulticastReceiver.Create;
begin
  inherited;
  FReceivers := TSockets.Create;
end;

destructor TMulticastReceiver.Destroy;
begin
  FReceivers.Free;
  inherited;
end;

procedure TMulticastReceiver.LocalAddressesChange;
begin
  UpdateReceivers;
end;

function TMulticastReceiver.Start: Boolean;
begin
  Stop;
  // UpdateReceivers;
  Result := FReceivers.Bind;
  if Result then
    FReadThread := TMulticastReadThread.Create(Self);
end;

procedure TMulticastReceiver.Stop;
begin
  if FReadThread <> nil then
  begin
    FReadThread.Terminate;
    FReadThread.WaitFor;
  end;
  FReadThread.Free;
  FReadThread := nil;
end;

procedure TMulticastReceiver.ReadData;
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
end;

function TMulticastReceiver.AddAnySocket: TSocket;
var
  LLocalAddress: TIPAddress;
  LIndex: Integer;
begin
  LIndex := FReceivers.IndexOf(TIPVersion.IPv4);
  if LIndex = -1 then
  begin
    LLocalAddress.IP := '0.0.0.0';
    LLocalAddress.IPVersion := TIPVersion.IPv4;
    LLocalAddress.InterfaceIndex := 0;
    Result := FReceivers.AddSocket(LLocalAddress, FPort);
  end
  else
    Result := FReceivers[LIndex];
end;

procedure TMulticastReceiver.UpdateReceivers;
var
  LLocalAddress: TIPAddress;
  LSocket: TSocket;
begin
  FReceivers.ClearSockets;
  for LLocalAddress in NetworkProvider.GetLocalAddresses do
  begin
    // It appears macOS/iOS cannot listen on individual IPv4 addresses - sucky!
    if (LLocalAddress.IPVersion = TIPVersion.IPv4) and (TOSVersion.Platform in [TOSVersion.TPlatform.pfMacOS, TOSVersion.TPlatform.pfIOS]) then
      LSocket := AddAnySocket
    else
      LSocket := FReceivers.AddSocket(LLocalAddress, FPort);
    UpdateSocketGroup(LSocket);
  end;
end;

{ TCustomMulticastSender }

procedure TCustomMulticastSender.Broadcast(const AData: string; const APort: Integer);
begin
  Broadcast(TEncoding.Default.GetBytes(AData), APort);
end;

procedure TCustomMulticastSender.Broadcast(const AData: TBytes; const APort: Integer);
begin
  //
end;

{ TMulticastSender }

constructor TMulticastSender.Create;
begin
  inherited;
  FSenders := TSockets.Create;
end;

destructor TMulticastSender.Destroy;
begin
  FSenders.Free;
  inherited;
end;

procedure TMulticastSender.LocalAddressesChange;
begin
  UpdateSenders;
end;

procedure TMulticastSender.Broadcast(const AData: TBytes; const APort: Integer);
begin
  if FSenders.Count = 0 then
    UpdateSenders;
  FSenders.Broadcast(AData, APort);
end;

procedure TMulticastSender.UpdateSenders;
var
  LLocalAddress: TIPAddress;
  LSocket: TSocket;
begin
  FSenders.ClearSockets;
  for LLocalAddress in NetworkProvider.GetLocalAddresses do
  begin
    LSocket := FSenders.AddSocket(LLocalAddress, 0);
    UpdateSocketGroup(LSocket);
  end;
end;

{ TMulticastProvider }

function TMulticastProvider.CreateReceiver: IMulticastReceiver;
begin
  {$IF Defined(ANDROID)}
  Result := TPlatformMulticastReceiver.Create;
  {$ELSE}
  Result := TMulticastReceiver.Create;
  {$ENDIF}
end;

function TMulticastProvider.CreateSender: IMulticastSender;
begin
  {$IF Defined(ANDROID)}
  Result := TPlatformMulticastSender.Create;
  {$ELSE}
  Result := TMulticastSender.Create;
  {$ENDIF}
end;

initialization
  MulticastProvider := TMulticastProvider.Create;

end.

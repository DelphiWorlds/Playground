unit DW.Network.Socket;

interface

uses
  System.SysUtils, System.Net.Socket, System.Generics.Collections,
  DW.Network.Types;

type
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
    constructor Create(const AAddress: TIPAddress; const APort: Integer);
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
    function AddSocket(const AAddress: TIPAddress; const APort: Integer): TSocket;
    function Bind: Boolean;
    function Broadcast(const AData: TBytes; const APort: Integer): Boolean;
    procedure ClearSockets;
    function IndexOf(const AIPVersion: TIPVersion): Integer;
  end;

implementation

uses
{$IF Defined(MacOS)}
  DW.Network.Socket.Mac,
{$ENDIF}
{$IF Defined(MSWINDOWS)}
  DW.Network.Socket.Win,
{$ENDIF}
  DW.OSLog;

{$IF Defined(ANDROID) or Defined(ANDROID64)}
type
  TPlatformSocket = class(TSocket);
{$ENDIF}

{ TSocket }

constructor TSocket.Create(const AAddress: TIPAddress; const APort: Integer);
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
//    if not FIsBound then
//      TNetwork.Error('TSocket.Bind')
//    else
//      TOSLog.d('Bound %s to %d', [IP, Port]);
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
//    if not Result then
//      TNetwork.Error('TSocket.AllocateHandle');
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
      Result := SendTo(AData, APort);
//    else
//      TNetwork.Error('TSocket.Broadcast');
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
//  if not Result then
//    TNetwork.Error('TSocket.SendTo');
end;

procedure TSocket.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

{ TSockets }

function TSockets.AddSocket(const AAddress: TIPAddress; const APort: Integer): TSocket;
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

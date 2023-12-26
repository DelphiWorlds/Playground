unit DW.Network.Socket.Win;

interface

uses
  System.SysUtils, System.Net.Socket,
  DW.Network.Socket;

type
  TPlatformSocket = class(TSocket)
  private
    FMarshaller: TMarshaller;
    function InternalRead(var AIP: string; var AData: TBytes): Boolean;
  protected
    function ApplyBroadcastEnabled: Boolean; override;
    function ApplyLoopback: Boolean; override;
    function ApplyTimeToLive: Boolean; override;
    procedure CloseHandle; override;
    function CreateHandle: TSocketHandle; override;
    function DoBind: Boolean; override;
    function DoRead(var AIP: string; var AData: TBytes): Boolean; override;
    function JoinGroup: Boolean; override;
    function SendTo(const AIP: string; const AData: TBytes; const APort: Integer): Boolean; override;
  end;

implementation

uses
  DW.OSLog,
  DW.Network.Types, DW.Network.Provider,
  Winapi.ShellAPI, Winapi.IpHlpApi, Winapi.IpTypes, Winapi.IpExport, Winapi.Windows, Winapi.WinSock, Winapi.Winsock2;

const
  ntdll_dll ='ntdll.dll';
  ws2_32 = 'ws2_32.dll';

  // Because the Winapi.Winsock2 unit has it wrong :-(
  IP_MULTICAST_TTL = 10;
  IP_MULTICAST_LOOP = 11;
  IP_ADD_MEMBERSHIP = 12;
  IPV6_MULTICAST_IF = 9;

  IF_TYPE_SOFTWARE_LOOPBACK = 24;

  cIPFamily: array[TIPVersion] of Integer = (AF_INET, AF_INET6);
  cIPProtoValues: array[TIPVersion] of Integer = (IPPROTO_IP, IPPROTO_IPV6);

type
  ssize_t = Integer;
  in_addr_t = u_long;

  ip_mreq = packed record
    imr_multiaddr: in_addr;
    imr_interface: in_addr;
  end;

  ip_mreq6 = packed record
    ipv6mr_multiaddr: in6_addr;
    ipv6mr_interface: UInt32;
  end;

  psockaddr_in6 = ^sockaddr_in6;
  pin6_addr = ^in6_addr;

  sockaddr_in_multi = packed record
    ss_family: short;
    pad: array[0..125] of Byte;
  end;

  TPlatformNetworkProvider = class(TCustomPlatformNetworkProvider)
  protected
    function GetLocalAddresses: TIPAddresses; override;
  public
    constructor Create;
  end;

function bindsocket(s: TSocket; name: PSockAddr; namelen: Integer): Integer; stdcall;
  external ws2_32 name 'bind';
function recvfrom(const s: TSocket; var Buf; len, flags: Integer; from: PSOCKADDR; fromlen: PInteger): Integer; stdcall;
  external ws2_32 name 'recvfrom';

function RtlIpv4AddressToString(addr: pin_addr; dst: PChar): Pointer; stdcall; external ntdll_dll name 'RtlIpv4AddressToStringW';
function RtlIpv6AddressToString(addr: pin6_addr; dst: PChar): Pointer; stdcall; external ntdll_dll name 'RtlIpv6AddressToStringW';

// https://docs.microsoft.com/en-us/windows/win32/api/iphlpapi/nf-iphlpapi-getadaptersaddresses?redirectedfrom=MSDN

{ TPlatformNetworkProvider }

constructor TPlatformNetworkProvider.Create;
var
  LData: WSAData;
begin
  inherited;
  WSAStartup(WINSOCK_VERSION, LData);
end;

function TPlatformNetworkProvider.GetLocalAddresses: TIPAddresses;
var
  LFlags: Cardinal;
  LLength: u_long;
  LAddresses, LAddress: PIP_ADAPTER_ADDRESSES;
  LUnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
  LLocalAddress: TIPAddress;
  LLocalAddresses: TIPAddressesList;
  LIP: string;
begin
  SetLength(LIP, 128);
  LFlags := GAA_FLAG_SKIP_ANYCAST or GAA_FLAG_SKIP_MULTICAST or GAA_FLAG_SKIP_DNS_SERVER or GAA_FLAG_SKIP_FRIENDLY_NAME;
  LLength := 1024 * 15;
  GetMem(LAddresses, LLength);
  try
    if GetAdaptersAddresses(AF_UNSPEC, LFlags, nil, LAddresses, @LLength) = ERROR_SUCCESS then
    begin
      LAddress := LAddresses;
      repeat
        if (LAddress^.IfType <> IF_TYPE_SOFTWARE_LOOPBACK) and ((LAddress^.Flags and IP_ADAPTER_RECEIVE_ONLY) = 0) then
        begin
          LUnicastAddress := LAddress^.FirstUnicastAddress;
          while LUnicastAddress <> nil do
          begin
            if LUnicastAddress^.DadState = IpDadStatePreferred then
            begin
              case LUnicastAddress^.Address.lpSockaddr.sin_family of
                AF_INET:
                begin
                  LLocalAddress.IPVersion := TIPVersion.IPv4;
                  RtlIpv4AddressToString(@(PSockAddrIn(LUnicastAddress^.Address.lpSockaddr)^.sin_addr), PChar(LIP));
                  LLocalAddress.InterfaceIndex := LAddress^.Union.IfIndex;
                end;
                AF_INET6:
                begin
                  LLocalAddress.IPVersion := TIPVersion.IPv6;
                  RtlIpv6AddressToString(@(psockaddr_in6(LUnicastAddress^.Address.lpSockaddr)^.sin6_addr), PChar(LIP));
                  LLocalAddress.InterfaceIndex := LAddress^.Ipv6IfIndex;
                end;
              end;
              LLocalAddress.IP := LIP.Substring(0, Pos(#0, LIP) - 1);
              LLocalAddress.InterfaceName := string(LAddress^.FriendlyName);
              LLocalAddresses.Add(LLocalAddress);
            end;
            LUnicastAddress := LUnicastAddress^.Next;
          end;
        end;
        LAddress := LAddress^.Next;
      until LAddress = nil;
    end;
  finally
    FreeMem(LAddresses);
  end;
  Result := LLocalAddresses.Items;
end;

{ TPlatformSocket }

function TPlatformSocket.ApplyBroadcastEnabled: Boolean;
var
  LOption: Integer;
begin
  LOption := 1; //!!!!! Needs property
  Result := setsockopt(Handle, SOL_SOCKET, SO_BROADCAST, PAnsiChar(@LOption), SizeOf(LOption)) <> -1 ;
end;

function TPlatformSocket.ApplyLoopback: Boolean;
var
  LOption: Integer;
begin
  LOption := 0;  //!!!!! Needs property
  Result := setsockopt(Handle, cIPProtoValues[IPVersion], IP_MULTICAST_LOOP, PAnsiChar(@LOption), SizeOf(LOption)) <> -1;
end;

function TPlatformSocket.ApplyTimeToLive: Boolean;
var
  LOption: Integer;
begin
  LOption := 1;  //!!!!! Needs property
  Result := setsockopt(Handle, cIPProtoValues[IPVersion], IP_MULTICAST_TTL, PAnsiChar(@LOption), SizeOf(LOption)) <> -1;
end;

procedure TPlatformSocket.CloseHandle;
begin
  closesocket(Handle);
end;

function TPlatformSocket.CreateHandle: TSocketHandle;
begin
  Result := socket(cIPFamily[IPVersion], SOCK_DGRAM, 0); //!!!!! SocketType
end;

function TPlatformSocket.DoBind: Boolean;
var
  LSockAddrIPv4: sockaddr_in;
  LNetAddrIPv4: in_addr_t;
  LSockAddrIPv6: sockaddr_in6;
  LNetAddrIPv6: in6_addr;
  LOption: Integer;
begin
  Result := False;
  LOption := 0;
  setsockopt(Handle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@LOption), SizeOf(LOption));
  case IPVersion of
    TIPVersion.IPv4:
    begin
      inet_pton(AF_INET, FMarshaller.AsAnsi(IP).ToPointer, @LNetAddrIPv4);
      FillChar(LSockAddrIPv4, SizeOf(LSockAddrIPv4), 0);
      LSockAddrIPv4.sin_family := AF_INET;
      LSockAddrIPv4.sin_addr.s_addr := LNetAddrIPv4;
      LSockAddrIPv4.sin_port := htons(Port);
      Result := bindsocket(Handle, PSockAddr(@LSockAddrIPv4), SizeOf(LSockAddrIPv4)) <> -1;
    end;
    TIPVersion.IPv6:
    begin
      inet_pton(AF_INET6, FMarshaller.AsAnsi(IP).ToPointer, @LNetAddrIPv6);
      FillChar(LSockAddrIPv6, SizeOf(LSockAddrIPv6), 0);
      LSockAddrIPv6.sin6_family := AF_INET6;
      LSockAddrIPv6.sin6_addr := LNetAddrIPv6;
      LSockAddrIPv6.sin6_port := htons(Port);
      Result := bindsocket(Handle, PSockAddr(@LSockAddrIPv6), SizeOf(LSockAddrIPv6)) <> -1;
    end;
  end;
end;

function TPlatformSocket.DoRead(var AIP: string; var AData: TBytes): Boolean;
var
  LSet: fd_set;
  LResult, LTimeout: Integer;
  // LBytesRead, LLength: Integer;
  LTime: TTimeVal;
  LTimePtr: PTimeVal;
  LMsg: string;
begin
  Result := False;
  LTimeout := 20; // ms
  LTime.tv_sec := LTimeout div 1000;
  LTime.tv_usec := (LTimeout mod 1000) * 1000;
  LTimePtr := @LTime;
  FD_ZERO(LSet);
  _FD_SET(Handle, LSet);
  LResult := select(FD_SETSIZE, @LSet, nil, nil, LTimePtr);
  if LResult > 0 then
    Result := InternalRead(AIP, AData)
  else if LResult <> 0 then
  begin
    LMsg := SysErrorMessage(GetLastError);
    TOSLog.d('Read error: ' + LMsg);
  end;
end;

function TPlatformSocket.InternalRead(var AIP: string; var AData: TBytes): Boolean;
var
  LBytesRead, LLength: Integer;
  LSockAddrMulti: sockaddr_in_multi;
begin
  Result := False;
  AIP := '';
  LLength := SizeOf(LSockAddrMulti);
  FillChar(LSockAddrMulti, SizeOf(LSockAddrMulti), 0);
  LBytesRead := recvfrom(Handle, Buffer[0], Length(Buffer), 0, PSockAddr(@LSockAddrMulti), @LLength);
  if LBytesRead > 0 then
  begin
    SetLength(AIP, 64);
    case LSockAddrMulti.ss_family of
      AF_INET:
        RtlIpv4AddressToString(@(PSockAddrIn(@LSockAddrMulti)^.sin_addr), PChar(AIP));
      AF_INET6:
        RtlIpv6AddressToString(@(psockaddr_in6(@LSockAddrMulti)^.sin6_addr), PChar(AIP));
    end;
    AIP := AIP.Substring(0, Pos(#0, AIP) - 1);
    SetLength(AData, LBytesRead);
    Move(Buffer[0], AData[0], LBytesRead);
    Result := True;
  end;
end;

function TPlatformSocket.JoinGroup: Boolean;
var
  LMembershipIPv4: ip_mreq;
  LGroupNetIPv4: in_addr_t;
  LLocalNetIPv4: in_addr_t;
  LMembershipIPv6: ip_mreq6;
  LGroupNetIPv6: in6_addr;
  LMsg: string;
begin
  Result := True;
  if not Group.IsEmpty and (Port > 0) then
  begin
    Result := False;
    case IPVersion of
      TIPVersion.IPv4:
      begin
        inet_pton(AF_INET, FMarshaller.AsAnsi(Group).ToPointer, @LGroupNetIPv4);
        inet_pton(AF_INET, FMarshaller.AsAnsi(IP).ToPointer, @LLocalNetIPv4);
        FillChar(LMembershipIPv4, SizeOf(LMembershipIPv4), 0);
        LMembershipIPv4.imr_multiaddr.s_addr := LGroupNetIPv4;
        LMembershipIPv4.imr_interface.s_addr := LLocalNetIPv4;
        Result := setsockopt(Handle, IPPROTO_IP, IP_ADD_MEMBERSHIP, PAnsiChar(@LMembershipIPv4), SizeOf(LMembershipIPv4)) <> -1;
      end;
      TIPVersion.IPv6:
      begin
        inet_pton(AF_INET6, FMarshaller.AsAnsi(Group).ToPointer, @LGroupNetIPv6);
        FillChar(LMembershipIPv6, SizeOf(LMembershipIPv6), 0);
        LMembershipIPv6.ipv6mr_multiaddr := LGroupNetIPv6;
        LMembershipIPv6.ipv6mr_interface := InterfaceIndex;
        Result := setsockopt(Handle, IPPROTO_IPV6, IP_ADD_MEMBERSHIP, PAnsiChar(@LMembershipIPv6), SizeOf(LMembershipIPv6)) <> -1;
        if not Result then
        begin
          LMsg := SysErrorMessage(GetLastError);
          Sleep(0);
        end;
      end;
    end;
    if not Result then
      TOSLog.e('TPlatformSocket.JoinGroup for receiver');
  end;
  if not Group.IsEmpty and (Port = 0) and (IPVersion = TIPVersion.IPv6) then
  begin
    Result := setsockopt(Handle, IPPROTO_IPV6, IPV6_MULTICAST_IF, PAnsiChar(@InterfaceIndex), SizeOf(InterfaceIndex)) <> -1;
    if not Result then
      TOSLog.e('TPlatformSocket.JoinGroup for IPv6 sender');
  end;
end;

function TPlatformSocket.SendTo(const AIP: string; const AData: TBytes; const APort: Integer): Boolean;
var
  LSockAddrIPv4: sockaddr_in;
  LNetAddrIPv4: in_addr_t;
  LSockAddrIPv6: sockaddr_in6;
  LNetAddrIPv6: in6_addr;
  LSendAddr: psockaddr;
  LBytesSent: ssize_t;
  // LOption: Integer;
  // LMsg: string;
begin
  LBytesSent := -1;
  case IPVersion of
    TIPVersion.IPv4:
    begin
      ApplyBroadcastEnabled;
      inet_pton(AF_INET, FMarshaller.AsAnsi(AIP).ToPointer, @LNetAddrIPv4);
      FillChar(LSockAddrIPv4, SizeOf(LSockAddrIPv4), 0);
      LSockAddrIPv4.sin_family := AF_INET;
      LSockAddrIPv4.sin_addr.s_addr := LNetAddrIPv4;
      LSockAddrIPv4.sin_port := htons(APort);
      LSendAddr := @LSockAddrIPv4;
      LBytesSent := Winapi.Winsock2.sendto(Handle, AData[0], Length(AData), 0, LSendAddr, SizeOf(LSockAddrIPv4));
    end;
    TIPVersion.IPv6:
    begin
      inet_pton(AF_INET6, FMarshaller.AsAnsi(AIP).ToPointer, @LNetAddrIPv6);
      FillChar(LSockAddrIPv6, SizeOf(LSockAddrIPv6), 0);
      LSockAddrIPv6.sin6_family := AF_INET6;
      LSockAddrIPv6.sin6_addr := LNetAddrIPv6;
      LSockAddrIPv6.sin6_port := htons(APort);
      LSockAddrIPv6.sin6_scope_id := InterfaceIndex;
      LSendAddr := @LSockAddrIPv6;
      LBytesSent := Winapi.Winsock2.sendto(Handle, AData[0], Length(AData), 0, LSendAddr, SizeOf(LSockAddrIPv6));
    end;
  end;
  Result := LBytesSent <> -1;
end;

initialization
  NetworkProvider := TPlatformNetworkProvider.Create;

end.

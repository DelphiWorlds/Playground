unit DW.Network.Socket.Mac;

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
  Posix.SysTypes, Posix.SysTime, Posix.SysSocket, Posix.NetinetIn, Posix.NetIf, Posix.Base, Posix.ArpaInet, Posix.Unistd, Posix.SysSelect;

const
  cIPFamily: array[TIPVersion] of Integer = (AF_INET, AF_INET6);
  cIPProtoValues: array[TIPVersion] of Integer = (IPPROTO_IP, IPPROTO_IPV6);
  {$WARN SYMBOL_PLATFORM OFF}
  cInterfaceFlagValues: array[TInterfaceFlag] of Integer = (
    IFF_UP, IFF_BROADCAST, IFF_DEBUG, IFF_LOOPBACK, IFF_POINTOPOINT, IFF_NOTRAILERS, IFF_RUNNING, IFF_NOARP, IFF_PROMISC, IFF_ALLMULTI,
    IFF_OACTIVE, IFF_SIMPLEX, IFF_LINK0, IFF_LINK1, IFF_LINK2, IFF_ALTPHYS, IFF_MULTICAST
  );
  {$WARN SYMBOL_PLATFORM ON}

type
  TUInt32Bytes = packed array[0..3] of Byte;

  TInAddr = packed record
    Bytes: TUInt32Bytes;
  end;

  ip_mreq6 = packed record
    ipv6mr_multiaddr: in6_addr;
    ipv6mr_interface: UInt32;
  end;

  sockaddr_in_multi = packed record
    ss_len: UInt8;
    ss_family: sa_family_t;
    pad: array[0..125] of Byte;
  end;
  psockaddr_in_multi = ^sockaddr_in_multi;

  TPlatformNetworkProvider = class(TCustomPlatformNetworkProvider)
  private
    class function GetInterfaceFlags(const AFlags: Integer): TInterfaceFlags;
  protected
    function GetLocalAddresses: TIPAddresses; override;
  public
    constructor Create;
  end;

function getifaddrs(var ifap: pifaddrs): Integer; cdecl;
  external libc name _PU + 'getifaddrs';
procedure freeifaddrs(ifap: pifaddrs); cdecl;
  external libc name _PU + 'freeifaddrs';

function InAddrToString(const AInAddr: in_addr): string;
var
  LInAddr: TInAddr;
begin
  LInAddr := TInAddr(AInAddr);
  Result := IntToStr(LInAddr.Bytes[0]) + '.' + IntToStr(LInAddr.Bytes[1]) + '.' + IntToStr(LInAddr.Bytes[2]) + '.' + IntToStr(LInAddr.Bytes[3]);
end;

function In6AddrToString(const AIn6Addr: in6_addr): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to 7 do
    Result := Result + IntToHex(ntohs(AIn6Addr.__s6_addr16[I]), 1) + ':';
  SetLength(Result, Length(Result) - 1);
end;

{ TPlatformSocket }

function TPlatformSocket.ApplyBroadcastEnabled: Boolean;
var
  LOption: Integer;
begin
  LOption := 1; //!!!!! Needs property
  Result := setsockopt(Handle, SOL_SOCKET, SO_BROADCAST, LOption, SizeOf(LOption)) <> -1 ;
end;

function TPlatformSocket.ApplyLoopback: Boolean;
var
  LOption: Integer;
begin
  LOption := 0;  //!!!!! Needs property
  {$WARN SYMBOL_PLATFORM OFF}
  Result := setsockopt(Handle, cIPProtoValues[IPVersion], IP_MULTICAST_LOOP, LOption, SizeOf(LOption)) <> -1;
  {$WARN SYMBOL_PLATFORM ON}
end;

function TPlatformSocket.ApplyTimeToLive: Boolean;
var
  LOption: Integer;
begin
  LOption := 1;  //!!!!! Needs property
  {$WARN SYMBOL_PLATFORM OFF}
  Result := setsockopt(Handle, cIPProtoValues[IPVersion], IP_MULTICAST_TTL, LOption, SizeOf(LOption)) <> -1;
  {$WARN SYMBOL_PLATFORM ON}
end;

procedure TPlatformSocket.CloseHandle;
begin
  __close(Handle);
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
  LSockAddr: psockaddr;
  // LMsg, LTest: string;
  LOption: Integer;
begin
  Result := False;
  LOption := 1;
  setsockopt(Handle, SOL_SOCKET, SO_REUSEADDR, LOption, SizeOf(LOption));
  case IPVersion of
    TIPVersion.IPv4:
    begin
      inet_pton(AF_INET, FMarshaller.AsAnsi(IP).ToPointer, @LNetAddrIPv4);
      FillChar(LSockAddrIPv4, SizeOf(LSockAddrIPv4), 0);
      LSockAddrIPv4.sin_family := AF_INET;
      LSockAddrIPv4.sin_addr.s_addr := LNetAddrIPv4;
      LSockAddrIPv4.sin_port := htons(Port);
      LSockAddr := PSockAddr(@LSockAddrIPv4);
      Result := Posix.SysSocket.bind(Handle, LSockAddr^, SizeOf(LSockAddrIPv4)) <> -1;
    end;
    TIPVersion.IPv6:
    begin
      inet_pton(AF_INET6, FMarshaller.AsAnsi(IP).ToPointer, @LNetAddrIPv6);
      FillChar(LSockAddrIPv6, SizeOf(LSockAddrIPv6), 0);
      LSockAddrIPv6.sin6_family := AF_INET6;
      LSockAddrIPv6.sin6_addr := LNetAddrIPv6;
      LSockAddrIPv6.sin6_port := htons(Port);
      LSockAddrIPv6.sin6_scope_id := InterfaceIndex;
      LSockAddr := PSockAddr(@LSockAddrIPv6);
      Result := Posix.SysSocket.bind(Handle, LSockAddr^, SizeOf(LSockAddrIPv6)) <> -1;
   end;
  end;
end;

function TPlatformSocket.DoRead(var AIP: string; var AData: TBytes): Boolean;
var
  LSet: fd_set;
  LResult: Integer;
  // LLength: socklen_t;
  LTimeout: Integer;
  LTime: timeval;
  LTimePtr: Ptimeval;
  LMsg: string;
  // LError: Integer;
  // LCount: Integer;
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
  LLength: socklen_t;
  LSockAddrMulti: sockaddr_in_multi;
  LSockAddr: sockaddr;
  LBytesRead: Int64;
begin
  Result := False;
  AIP := '';
  LLength := SizeOf(LSockAddrMulti);
  LSockAddr := PSockAddr(@LSockAddrMulti)^;
  LBytesRead := recvfrom(Handle, Buffer[0], Length(Buffer), 0, LSockAddr, LLength);
  if LBytesRead > 0 then
  begin
    LSockAddrMulti := psockaddr_in_multi(@LSockAddr)^;
    case LSockAddrMulti.ss_family of
      AF_INET:
        AIP := InAddrToString(PSockAddr_In(@LSockAddrMulti)^.sin_addr);
      AF_INET6:
        AIP := In6AddrToString(PSockAddr_In6(@LSockAddrMulti)^.sin6_addr);
    end;
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
        {$WARN SYMBOL_PLATFORM OFF}
        Result := setsockopt(Handle, IPPROTO_IP, IP_ADD_MEMBERSHIP, LMembershipIPv4, SizeOf(LMembershipIPv4)) <> -1;
        {$WARN SYMBOL_PLATFORM ON}
        if not Result then
        begin
          LMsg := SysErrorMessage(GetLastError);
          Sleep(0);
        end;
      end;
      TIPVersion.IPv6:
      begin
        inet_pton(AF_INET6, FMarshaller.AsAnsi(Group).ToPointer, @LGroupNetIPv6);
        FillChar(LMembershipIPv6, SizeOf(LMembershipIPv6), 0);
        LMembershipIPv6.ipv6mr_multiaddr := LGroupNetIPv6;
        LMembershipIPv6.ipv6mr_interface := InterfaceIndex;
        {$WARN SYMBOL_PLATFORM OFF}
        Result := setsockopt(Handle, IPPROTO_IPV6, IP_ADD_MEMBERSHIP, LMembershipIPv6, SizeOf(LMembershipIPv6)) <> -1;
        {$WARN SYMBOL_PLATFORM ON}
      end;
    end;
    if not Result then
      TOSLog.e('TPlatformSocket.JoinGroup for receiver');
  end;
  if not Group.IsEmpty and (Port = 0) and (IPVersion = TIPVersion.IPv6) then
  begin
    Result := setsockopt(Handle, IPPROTO_IPV6, IPV6_MULTICAST_IF, InterfaceIndex, SizeOf(InterfaceIndex)) <> -1;
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
  LSockAddr: PSockAddr;
  LBytesSent: ssize_t;
  LOption: Integer;
  // LMsg: string;
begin
  LBytesSent := -1;
  case IPVersion of
    TIPVersion.IPv4:
    begin
      LOption := 1;
      setsockopt(Handle, SOL_SOCKET, SO_BROADCAST, LOption, SizeOf(LOption));
      inet_pton(AF_INET, FMarshaller.AsAnsi(AIP).ToPointer, @LNetAddrIPv4);
      FillChar(LSockAddrIPv4, SizeOf(LSockAddrIPv4), 0);
      LSockAddrIPv4.sin_family := AF_INET;
      LSockAddrIPv4.sin_addr.s_addr := LNetAddrIPv4;
      LSockAddrIPv4.sin_port := htons(APort);
      LSockAddr := PSockAddr(@LSockAddrIPv4);
      LBytesSent := Posix.SysSocket.sendto(Handle, AData[0], Length(AData), 0, LSockAddr^, SizeOf(LSockAddrIPv4));
    end;
    TIPVersion.IPv6:
    begin
      inet_pton(AF_INET6, FMarshaller.AsAnsi(AIP).ToPointer, @LNetAddrIPv6);
      FillChar(LSockAddrIPv6, SizeOf(LSockAddrIPv6), 0);
      LSockAddrIPv6.sin6_family := AF_INET6;
      LSockAddrIPv6.sin6_addr := LNetAddrIPv6;
      LSockAddrIPv6.sin6_port := htons(APort);
      LSockAddrIPv6.sin6_scope_id := InterfaceIndex;
      LSockAddr := PSockAddr(@LSockAddrIPv6);
      LBytesSent := Posix.SysSocket.sendto(Handle, AData[0], Length(AData), 0, LSockAddr^, SizeOf(LSockAddrIPv6));
    end;
  end;
  Result := LBytesSent <> -1;
end;

{ TPlatformNetworkProvider }

constructor TPlatformNetworkProvider.Create;
begin
  inherited;
  InterfaceFlagsFilter := [TInterfaceFlag.PointToPoint];
end;

class function TPlatformNetworkProvider.GetInterfaceFlags(const AFlags: Integer): TInterfaceFlags;
var
  LInterfaceFlag: TInterfaceFlag;
begin
  Result := [];
  for LInterfaceFlag := Low(TInterfaceFlag) to High(TInterfaceFlag) do
  begin
    if (AFlags and cInterfaceFlagValues[LInterfaceFlag]) > 0 then
      Result := Result + [LInterfaceFlag];
  end;
end;

function TPlatformNetworkProvider.GetLocalAddresses: TIPAddresses;
var
  LAddrList, LAddrInfo: pifaddrs;
  LSockAddr: sockaddr;
  LLocalAddress: TIPAddress;
  LLocalAddresses: TIPAddressesList;
begin
  if getifaddrs(LAddrList) = 0 then
  try
    LAddrInfo := LAddrList;
    repeat
      {$WARN SYMBOL_PLATFORM OFF}
      if (LAddrInfo^.ifa_addr <> nil) and ((LAddrInfo^.ifa_flags and IFF_LOOPBACK) = 0) then
      {$WARN SYMBOL_PLATFORM ON}
      begin
        LSockAddr := LAddrInfo^.ifa_addr^;
        if LSockAddr.sa_family in [AF_INET, AF_INET6] then
        begin
          LLocalAddress.InterfaceIndex := if_nametoindex(LAddrInfo^.ifa_name);
          LLocalAddress.InterfaceName := string(LAddrInfo^.ifa_name);
          LLocalAddress.InterfaceFlags := GetInterfaceFlags(LAddrInfo^.ifa_flags);
          case LSockAddr.sa_family of
            AF_INET:
            begin
              LLocalAddress.IP := InAddrToString(PSockAddr_In(LAddrInfo^.ifa_addr)^.sin_addr);
              LLocalAddress.IPVersion := TIPVersion.IPv4;
            end;
            AF_INET6:
            begin
              LLocalAddress.IP := In6AddrToString(PSockAddr_In6(LAddrInfo^.ifa_addr)^.sin6_addr);
              LLocalAddress.IPVersion := TIPVersion.IPv6;
            end;
          end;
          // Exclusive check i.e. address interface does not have flags of the filter
          if (LLocalAddress.InterfaceFlags * InterfaceFlagsFilter) = [] then
            LLocalAddresses.Add(LLocalAddress);
        end;
      end;
      LAddrInfo := LAddrInfo^.ifa_next;
    until LAddrInfo = nil;
  finally
    freeifaddrs(LAddrList);
  end;
  Result := LLocalAddresses.Items;
end;

initialization
  NetworkProvider := TPlatformNetworkProvider.Create;

end.

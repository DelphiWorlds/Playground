unit DW.Network.Android;

interface

uses
  DW.Network;

type
  TPlatformNetwork = class(TNetwork)
  protected
    function DoGetLocalAddresses: TLocalAddresses; override;
  end;

implementation

uses
  System.SysUtils,
  Androidapi.JNI.Java.Net, Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNIBridge;

{ TPlatformNetwork }

function TPlatformNetwork.DoGetLocalAddresses: TLocalAddresses;
var
  LInterfaces, LAddresses: JEnumeration;
  LInterface: JNetworkInterface;
  LAddress: JInetAddress;
  LName, LHostAddress: string;
  LLocalAddress: TLocalAddress;
  LLocalAddresses: TLocalAddresses;
begin
  LInterfaces := TJNetworkInterface.JavaClass.getNetworkInterfaces;
  while LInterfaces.hasMoreElements do
  begin
    LInterface := TJNetworkInterface.Wrap(LInterfaces.nextElement);
    LLocalAddress.InterfaceIndex := LInterface.getIndex;
    if LInterface.isUp then
      LLocalAddress.InterfaceFlags := LLocalAddress.InterfaceFlags + [TInterfaceFlag.Up];
    if LInterface.supportsMulticast then
      LLocalAddress.InterfaceFlags := LLocalAddress.InterfaceFlags + [TInterfaceFlag.Multicast];
    if LInterface.isPointToPoint then
      LLocalAddress.InterfaceFlags := LLocalAddress.InterfaceFlags + [TInterfaceFlag.PointToPoint];
    if LInterface.isLoopback then
      LLocalAddress.InterfaceFlags := LLocalAddress.InterfaceFlags + [TInterfaceFlag.Loopback];
    LLocalAddress.InterfaceName := JStringToString(LInterface.getName);
    LAddresses := LInterface.getInetAddresses;
    while LAddresses.hasMoreElements do
    begin
      LAddress := TJInetAddress.Wrap(LAddresses.nextElement);
      if LAddress.isLoopbackAddress then
        Continue;
      // Hack until I can find out how to check properly
      LName := JStringToString(LAddress.getClass.getName);
      LHostAddress := JStringToString(LAddress.getHostAddress);
      // Trim excess stuff
      if LHostAddress.IndexOf('%') > -1 then
        LHostAddress := LHostAddress.Substring(0, LHostAddress.IndexOf('%'));
      LLocalAddress.IP := LHostAddress;
      if LName.Contains('Inet4Address') then
        LLocalAddress.IPVersion := TIPVersion.IPv4
      else if LName.Contains('Inet6Address') then
        LLocalAddress.IPVersion := TIPVersion.IPv6;
      if (TInterfaceFlag.Up in LLocalAddress.InterfaceFlags) and ((LLocalAddress.InterfaceFlags * InterfaceFlagsFilter) = []) then
        LLocalAddresses.Add(LLocalAddress);
    end;
  end;
  Result := LLocalAddresses;
end;

end.

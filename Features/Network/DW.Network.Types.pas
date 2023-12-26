unit DW.Network.Types;

interface

type
  TIPVersion = (IPv4, IPv6);

  TInterfaceFlag = (Up, Broadcast, Debug, Loopback, PointToPoint, NoTrailers, Running, NoARP, Promiscuous, AllMulti, OActive, Simplex, Link0, Link1,
    Link2, AltPhys, Multicast);

  TInterfaceFlags = set of TInterfaceFlag;

  TIPAddress = record
    InterfaceFlags: TInterfaceFlags;
    InterfaceIndex: Integer;
    InterfaceName: string;
    IP: string;
    IPVersion: TIPVersion;
    function Equals(const AAddress: TIPAddress): Boolean;
  end;

  TIPAddresses = TArray<TIPAddress>;

  TIPAddressesList = record
  public
    Items: TIPAddresses;
    function Add(const AAddress: TIPAddress): Boolean;
    procedure Clear;
    function Count: Integer;
    function Exists(const AAddress: TIPAddress): Boolean;
    function IndexOfIP(const AIP: string): Integer;
    function Update(const AAddresses: TIPAddresses): Boolean;
  end;

implementation

uses
  System.SysUtils;

{ TIPAddress }

function TIPAddress.Equals(const AAddress: TIPAddress): Boolean;
begin
  Result := IP.Equals(AAddress.IP) and (IPVersion = AAddress.IPVersion);
end;

{ TIPAddressesList }

function TIPAddressesList.Add(const AAddress: TIPAddress): Boolean;
begin
  Result := False;
  if not Exists(AAddress) then
  begin
    Items := Items + [AAddress];
    Result := True;
  end;
end;

procedure TIPAddressesList.Clear;
begin
  SetLength(Items, 0);
end;

function TIPAddressesList.Count: Integer;
begin
  Result := Length(Items);
end;

function TIPAddressesList.Exists(const AAddress: TIPAddress): Boolean;
var
  LAddress: TIPAddress;
begin
  for LAddress in Items do
  begin
    if LAddress.Equals(AAddress) then
      Exit(True);
  end;
  Result := False;
end;

function TIPAddressesList.IndexOfIP(const AIP: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].IP.ToLower.Equals(AIP.ToLower) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TIPAddressesList.Update(const AAddresses: TIPAddresses): Boolean;
var
  LAddress: TIPAddress;
  LList: TIPAddressesList;
  I: Integer;
begin
  Result := False;
  for LAddress in AAddresses do
  begin
    if Add(LAddress) then
      Result := True;
  end;
  LList.Items := AAddresses;
  for I := Count - 1 downto 0 do
  begin
    if not LList.Exists(Items[I]) then
    begin
      Delete(Items, I, 1);
      Result := True;
    end;
  end;
end;

end.

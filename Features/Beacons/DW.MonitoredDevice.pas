unit DW.MonitoredDevice;

interface

uses
  System.JSON;

type
  TMonitoredDevice = record
    Address: string;
    Seen: TDateTime;
    Distance: Double;
    procedure FromJSON(const AJSON: TJSONValue);
    function SeenMoreThan(const ASeconds: Int64): Boolean;
    function ToJSON: TJSONValue;
  end;

  TMonitoredDevices = TArray<TMonitoredDevice>;

  TMonitoredDevicesStore = record
  private
    procedure ReadDevices(const AJSON: TJSONArray);
    function WriteDevices: TJSONArray;
  public
    MonitoredDevices: TMonitoredDevices;
    procedure AddDevice(const ADevice: TMonitoredDevice);
    function FindDevice(const AAddress: string; out ADevice: TMonitoredDevice): Boolean;
    procedure FromJSON(const AJSON: string);
    function ToJSON: string;
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

{ TMonitoredDevice }

procedure TMonitoredDevice.FromJSON(const AJSON: TJSONValue);
var
  LSeen: string;
begin
  AJSON.TryGetValue('address', Address);
  if AJSON.TryGetValue('seen', LSeen) then
    Seen := ISO8601ToDate(LSeen);
  AJSON.TryGetValue('distance', Distance);
end;

function TMonitoredDevice.SeenMoreThan(const ASeconds: Int64): Boolean;
begin
  Result := SecondsBetween(Now, Seen) > ASeconds;
end;

function TMonitoredDevice.ToJSON: TJSONValue;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  LJSON.AddPair('address', Address);
  LJSON.AddPair('seen', DateToISO8601(Seen));
  LJSON.AddPair('distance', TJSONNumber.Create(Distance));
  Result := LJSON;
end;

{ TMonitoredDevicesStore }

procedure TMonitoredDevicesStore.AddDevice(const ADevice: TMonitoredDevice);
begin
  MonitoredDevices := MonitoredDevices + [ADevice];
end;

function TMonitoredDevicesStore.FindDevice(const AAddress: string; out ADevice: TMonitoredDevice): Boolean;
var
  LDevice: TMonitoredDevice;
begin
  Result := False;
  for LDevice in MonitoredDevices do
  begin
    if LDevice.Address.Equals(AAddress) then
    begin
      ADevice := LDevice;
      Result := True;
      Break;
    end;
  end;
end;

procedure TMonitoredDevicesStore.FromJSON(const AJSON: string);
var
  LJSON: TJSONValue;
  LDevices: TJSONArray;
begin
  LJSON := TJSONObject.ParseJSONValue(AJSON);
  if LJSON <> nil then
  try
    if LJSON.TryGetValue('devices', LDevices) then
      ReadDevices(LDevices);
  finally
    LJSON.Free;
  end;
end;

procedure TMonitoredDevicesStore.ReadDevices(const AJSON: TJSONArray);
var
  LValue: TJSONValue;
  LDevice: TMonitoredDevice;
begin
  MonitoredDevices := [];
  for LValue in AJSON do
  begin
    LDevice.FromJSON(LValue);
    MonitoredDevices := MonitoredDevices + [LDevice];
  end;
end;

function TMonitoredDevicesStore.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('devices', WriteDevices);
    Result := LJSON.ToString;
  finally
    LJSON.Free;
  end;
end;

function TMonitoredDevicesStore.WriteDevices: TJSONArray;
var
  LDevice: TMonitoredDevice;
begin
  Result := TJSONArray.Create;
  for LDevice in MonitoredDevices do
    Result.AddElement(LDevice.ToJSON);
end;

end.

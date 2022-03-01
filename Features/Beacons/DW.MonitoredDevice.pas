unit DW.MonitoredDevice;

interface

uses
  System.JSON,
  DW.BluetoothLE.Types;

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
    procedure ReadFilters(const AJSON: TJSONArray);
    function WriteDevices: TJSONArray;
    function WriteFilters: TJSONArray;
  public
    Filters: TBluetoothLEScanFilters;
    HasAlarm: Boolean;
    MonitoredDevices: TMonitoredDevices;
    ScanResultDateTime: TDateTime;
    procedure AddDevice(const ADevice: TMonitoredDevice);
    function FindDevice(const AAddress: string; out ADevice: TMonitoredDevice): Boolean;
    procedure FromJSON(const AJSON: string);
    function SecondsSinceScan: Integer;
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
  LArray: TJSONArray;
  LValue: string;
begin
  ScanResultDateTime := 0;
  HasAlarm := False;
  LJSON := TJSONObject.ParseJSONValue(AJSON);
  if LJSON <> nil then
  try
    if LJSON.TryGetValue('devices', LArray) then
      ReadDevices(LArray);
    if LJSON.TryGetValue('filters', LArray) then
      ReadFilters(LArray);
    LJSON.TryGetValue('hasAlarm', HasAlarm);
    if LJSON.TryGetValue('scanResultDateTime', LValue) then
      ScanResultDateTime := ISO8601ToDate(LValue);
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

procedure TMonitoredDevicesStore.ReadFilters(const AJSON: TJSONArray);
var
  LValue: TJSONValue;
  LFilter: TBluetoothLEScanFilter;
begin
  Filters := [];
  for LValue in AJSON do
  begin
    LFilter.FromJSON(LValue);
    Filters := Filters + [LFilter];
  end;
end;

function TMonitoredDevicesStore.SecondsSinceScan: Integer;
begin
  if ScanResultDateTime > 0 then
    Result := SecondsBetween(Now, ScanResultDateTime)
  else
    Result := MaxInt;
end;

function TMonitoredDevicesStore.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('devices', WriteDevices);
    LJSON.AddPair('filters', WriteFilters);
    LJSON.AddPair('hasAlarm', TJSONBool.Create(HasAlarm));
    if ScanResultDateTime > 0 then
      LJSON.AddPair('scanResultDateTime', DateToISO8601(ScanResultDateTime));
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

function TMonitoredDevicesStore.WriteFilters: TJSONArray;
var
  LFilter: TBluetoothLEScanFilter;
begin
  Result := TJSONArray.Create;
  for LFilter in Filters do
    Result.AddElement(LFilter.ToJSON);
end;

end.

unit DW.Bluetooth;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections;

const
  cProximityImmediate = 0.5; // Metres
  cProximityNear = 3;
  cProximityFar = 30;
  cUUIDWith16Bit = '0000%s-0000-1000-8000-00805F9B34FB';

type
  TServiceDataRawData = record
    Key: TGUID;
    Value: TBytes;
  end;

  TBluetoothLEScanFilter = record
    LocalName: string;
    DeviceAddress: string;
    ManufacturerId: Integer;
    ManufacturerData: TBytes;
    ManufacturerDataMask: TBytes;
    ServiceUUID: TGUID;
    ServiceUUIDMask: TGUID;
    ServiceData: TServiceDataRawData;
    ServiceDataMask: TBytes;
    constructor Create(const AServiceUUID: string);
    procedure FromJSON(const AJSON: TJSONValue);
    function ToJSON: TJSONValue;
  end;

  TBluetoothLEScanFilters = TArray<TBluetoothLEScanFilter>;

  TBluetoothLEDeviceKind = (Unknown, iBeacon);

  TCustomBluetoothService = class(TObject)
  private
    FUUID: TGUID;
    FData: TBytes;
    FIsOverflow: Boolean;
  protected
    procedure SetData(const AValue: TBytes);
  public
    constructor Create(const AUUID: TGUID; const AIsOverflow: Boolean = False); virtual;
    property UUID: TGUID read FUUID;
  end;

  TBluetoothServices = TObjectDictionary<string, TCustomBluetoothService>;

  TCustomBluetoothLEDevice = class;

  TBluetoothLEDevices = TObjectDictionary<string, TCustomBluetoothLEDevice>;

  TBluetoothScanner = class;

  TCustomPlatformBluetoothScanner = class(TObject)
  private
    FAndroidServiceName: string;
    FBluetoothScanner: TBluetoothScanner;
    FDevices: TBluetoothLEDevices;
    FExpiry: Integer;
    FFilters: TBluetoothLEScanFilters;
  protected
    procedure AddFilter(const AFilter: TBluetoothLEScanFilter);
    procedure ClearFilters;
    procedure DiscoveredDevice(const ADevice: TCustomBluetoothLEDevice);
    procedure HandleIntent(const AIntentID: Pointer); virtual;
    function HasBluetoothPermission: Boolean; virtual;
    function IsBluetoothEnabled: Boolean; virtual;
    procedure ScanFinish;
    procedure Scan; virtual;
    procedure ScanStart;
    procedure Stop; virtual;
    procedure ScanWithIntent; virtual;
    procedure ScanWithIntentStart;
    property AndroidServiceName: string read FAndroidServiceName write FAndroidServiceName;
    property BluetoothScanner: TBluetoothScanner read FBluetoothScanner;
    property Devices: TBluetoothLEDevices read FDevices;
    property Expiry: Integer read FExpiry write FExpiry;
    property Filters: TBluetoothLEScanFilters read FFilters;
  public
    constructor Create(const ABluetoothScanner: TBluetoothScanner); virtual;
    destructor Destroy; override;
  end;

  TDiscoveredDeviceEvent = procedure(Sender: TObject; const Device: TCustomBluetoothLEDevice) of object;

  TBluetoothScanner = class(TObject)
  private
    FPlatformBluetoothScanner: TCustomPlatformBluetoothScanner;
    FOnDiscoveredDevice: TDiscoveredDeviceEvent;
    FOnScanFinish: TNotifyEvent;
    function GetFilters: TBluetoothLEScanFilters;
    function GetDevices: TBluetoothLEDevices;
    function GetExpiry: Integer;
    procedure SetExpiry(const Value: Integer);
    function GetAndroidServiceName: string;
    procedure SetAndroidServiceName(const Value: string);
  protected
    procedure DoDiscoveredDevice(const ADevice: TCustomBluetoothLEDevice);
    procedure DoScanFinish;
    property Devices: TBluetoothLEDevices read GetDevices;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFilter(const AFilter: TBluetoothLEScanFilter);
    procedure ClearFilters;
    procedure HandleIntent(const AIntentID: Pointer);
    procedure Scan;
    procedure ScanWithIntent;
    procedure Stop;
    property AndroidServiceName: string read GetAndroidServiceName write SetAndroidServiceName;
    property Expiry: Integer read GetExpiry write SetExpiry;
    property Filters: TBluetoothLEScanFilters read GetFilters;
    property OnDiscoveredDevice: TDiscoveredDeviceEvent read FOnDiscoveredDevice write FOnDiscoveredDevice;
    property OnScanFinish: TNotifyEvent read FOnScanFinish write FOnScanFinish;
  end;

  TCharacteristicReadEvent = procedure(Sender: TObject; const ID: string; const Value: TBytes) of object;
  TRemoteRssiEvent = procedure(Sender: TObject; const Rssi: Integer) of object;
  TServicesDiscoveredEvent = procedure(Sender: TObject; const Success: Boolean) of object;

  TCustomBluetoothLEDevice = class(TObject)
  private
    FAddress: string;
    FData: TBytes;
    FDeviceType: Integer;
    FIsConnectable: Boolean;
    FKey: string;
    FKind: TBluetoothLEDeviceKind;
    FName: string;
    FRSSI: Integer;
    FServices: TBluetoothServices;
    FTxPower: Integer;
    FOnCharacteristicRead: TCharacteristicReadEvent;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnRemoteRssi: TRemoteRssiEvent;
    FOnServicesDiscovered: TServicesDiscoveredEvent;
    function BytesMatch(const ABytes: TBytes; const AOffset: Integer = 0): Boolean;
    function GetDistance: Double;
    function GetServices: TBluetoothServices;
    procedure SetData(const Value: TBytes);
  protected
    function Connect: Boolean; virtual;
    procedure DoCharacteristicRead(const AID: string; const AValue: TBytes);
    procedure DoConnected;
    procedure DoDisconnected;
    procedure DoRemoteRssi(const ARssi: Integer);
    procedure DoServicesDiscovered(const ASuccess: Boolean);
    procedure ReadCharacteristic(const AServiceUUID, ACharacteristicUUID: string); virtual;
  public
    function DiscoverServices: Boolean; virtual;
    function DisplayName: string;
    function FindService(const AUUIDString: string; out AService: TCustomBluetoothService): Boolean;
    function HasService(const AUUID: TGUID): Boolean; overload;
    function HasService(const AUUIDString: string): Boolean; overload;
    procedure SetServiceData(const AUUIDString: string; const AData: TBytes);
    property Address: string read FAddress write FAddress;
    property Data: TBytes read FData write SetData;
    property DeviceType: Integer read FDeviceType write FDeviceType;
    property Distance: Double read GetDistance;
    property IsConnectable: Boolean read FIsConnectable write FIsConnectable;
    property Key: string read FKey write FKey;
    property Kind: TBluetoothLEDeviceKind read FKind;
    property Name: string read FName write FName;
    property RSSI: Integer read FRSSI write FRSSI;
    property Services: TBluetoothServices read GetServices;
    property TxPower: Integer read FTxPower write FTxPower;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnCharacteristicRead: TCharacteristicReadEvent read FOnCharacteristicRead write FOnCharacteristicRead;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnRemoteRssi: TRemoteRssiEvent read FOnRemoteRssi write FOnRemoteRssi;
    property OnServicesDiscovered: TServicesDiscoveredEvent read FOnServicesDiscovered write FOnServicesDiscovered;
  end;

implementation

uses
  System.Math,
  {$IF Defined(MACOS)}
  DW.Bluetooth.Mac;
  {$ELSEIF Defined(ANDROID)}
  DW.Bluetooth.Android;
  {$ELSE}
  DW.Bluetooth.Default;
  {$ENDIF}

const
  cSignalConst = 2;
  cBLEDataIBeaconTxPower = 29;

{ TBluetoothLEScanFilter }

constructor TBluetoothLEScanFilter.Create(const AServiceUUID: string);
begin
  ServiceUUID := TGUID.Create(AServiceUUID);
  ServiceUUIDMask := TGUID.Create('{FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF}');
end;

procedure TBluetoothLEScanFilter.FromJSON(const AJSON: TJSONValue);
var
  LValue: string;
begin
  AJSON.TryGetValue('localName', LocalName);
  AJSON.TryGetValue('deviceAddress', DeviceAddress);
  if AJSON.TryGetValue('serviceUUID', LValue) then
    ServiceUUID := TGUID.Create(LValue);
  if AJSON.TryGetValue('serviceUUIDMask', LValue) then
    ServiceUUIDMask := TGUID.Create(LValue);
end;

function TBluetoothLEScanFilter.ToJSON: TJSONValue;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  LJSON.AddPair('localName', TJSONString.Create(LocalName));
  LJSON.AddPair('deviceAddress', TJSONString.Create(DeviceAddress));
  LJSON.AddPair('serviceUUID', TJSONString.Create(ServiceUUID.ToString));
  LJSON.AddPair('serviceUUIDMask', TJSONString.Create(ServiceUUIDMask.ToString));
  Result := LJSON;
end;

{ TCustomPlatformBluetoothScanner }

constructor TCustomPlatformBluetoothScanner.Create(const ABluetoothScanner: TBluetoothScanner);
begin
  inherited Create;
  FBluetoothScanner := ABluetoothScanner;
  FDevices := TBluetoothLEDevices.Create([doOwnsValues]);
  FExpiry := 15000;
end;

destructor TCustomPlatformBluetoothScanner.Destroy;
begin
  FDevices.Free;
  inherited;
end;

procedure TCustomPlatformBluetoothScanner.AddFilter(const AFilter: TBluetoothLEScanFilter);
begin
  FFilters := FFilters + [AFilter];
end;

procedure TCustomPlatformBluetoothScanner.ClearFilters;
begin
  FFilters := [];
end;

procedure TCustomPlatformBluetoothScanner.DiscoveredDevice(const ADevice: TCustomBluetoothLEDevice);
begin
  FBluetoothScanner.DoDiscoveredDevice(ADevice);
end;

procedure TCustomPlatformBluetoothScanner.HandleIntent(const AIntentID: Pointer);
begin
  //
end;

function TCustomPlatformBluetoothScanner.HasBluetoothPermission: Boolean;
begin
  Result := False;
end;

function TCustomPlatformBluetoothScanner.IsBluetoothEnabled: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformBluetoothScanner.ScanFinish;
begin
  FBluetoothScanner.DoScanFinish;
end;

procedure TCustomPlatformBluetoothScanner.ScanStart;
begin
  FDevices.Clear;
  Scan;
end;

procedure TCustomPlatformBluetoothScanner.Stop;
begin
  //
end;

procedure TCustomPlatformBluetoothScanner.ScanWithIntent;
begin
  //
end;

procedure TCustomPlatformBluetoothScanner.ScanWithIntentStart;
begin
  FDevices.Clear;
  ScanWithIntent;
end;

procedure TCustomPlatformBluetoothScanner.Scan;
begin
  //
end;

{ TBluetoothScanner }

constructor TBluetoothScanner.Create;
begin
  inherited;
  FPlatformBluetoothScanner := TPlatformBluetoothScanner.Create(Self);
end;

destructor TBluetoothScanner.Destroy;
begin
  FPlatformBluetoothScanner.Free;
  inherited;
end;

procedure TBluetoothScanner.AddFilter(const AFilter: TBluetoothLEScanFilter);
begin
  FPlatformBluetoothScanner.AddFilter(AFilter);
end;

procedure TBluetoothScanner.ClearFilters;
begin
  FPlatformBluetoothScanner.ClearFilters;
end;

procedure TBluetoothScanner.DoDiscoveredDevice(const ADevice: TCustomBluetoothLEDevice);
begin
  if Assigned(FOnDiscoveredDevice) then
    FOnDiscoveredDevice(Self, ADevice);
end;

procedure TBluetoothScanner.DoScanFinish;
begin
  if Assigned(FOnScanFinish) then
    FOnScanFinish(Self);
end;

function TBluetoothScanner.GetAndroidServiceName: string;
begin
  Result := FPlatformBluetoothScanner.AndroidServiceName;
end;

function TBluetoothScanner.GetDevices: TBluetoothLEDevices;
begin
  Result := FPlatformBluetoothScanner.Devices;
end;

function TBluetoothScanner.GetExpiry: Integer;
begin
  Result := FPlatformBluetoothScanner.Expiry;
end;

function TBluetoothScanner.GetFilters: TBluetoothLEScanFilters;
begin
  Result := FPlatformBluetoothScanner.Filters;
end;

procedure TBluetoothScanner.HandleIntent(const AIntentID: Pointer);
begin
  FPlatformBluetoothScanner.HandleIntent(AIntentID);
end;

procedure TBluetoothScanner.Scan;
begin
  FPlatformBluetoothScanner.ScanStart;
end;

procedure TBluetoothScanner.ScanWithIntent;
begin
  FPlatformBluetoothScanner.ScanWithIntentStart;
end;

procedure TBluetoothScanner.SetAndroidServiceName(const Value: string);
begin
  FPlatformBluetoothScanner.AndroidServiceName := Value;
end;

procedure TBluetoothScanner.SetExpiry(const Value: Integer);
begin
  FPlatformBluetoothScanner.Expiry := Value;
end;

procedure TBluetoothScanner.Stop;
begin
  FPlatformBluetoothScanner.Stop;
end;

{ TCustomBluetoothService }

constructor TCustomBluetoothService.Create(const AUUID: TGUID; const AIsOverflow: Boolean);
begin
  inherited Create;
  FUUID := AUUID;
  FIsOverflow := AIsOverflow;
end;

procedure TCustomBluetoothService.SetData(const AValue: TBytes);
begin
  FData := AValue;
end;

{ TCustomBluetoothLEDevice }

function TCustomBluetoothLEDevice.BytesMatch(const ABytes: TBytes; const AOffset: Integer = 0): Boolean;
var
  I: Integer;
begin
  Result := False;
  if  Length(FData) > Length(ABytes) + AOffset then
  begin
    Result := True;
    for I := 0 to High(ABytes) do
    begin
      if ABytes[I] <> FData[I + AOffset] then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TCustomBluetoothLEDevice.DisplayName: string;
var
  LKey: string;
begin
  if Name.IsEmpty then
  begin
    if Key.IsEmpty then
      Result := '(unknown)'
    else
      Result := Key;
  end
  else
  begin
    Result := Name;
    if not Key.IsEmpty then
    begin
      LKey := Key;
      if Length(LKey) > 15 then
        LKey := LKey.Substring(0, 7) + '..' + LKey.Substring(Length(LKey) - 7);
      Result := Result + ' (' + LKey + ')';
    end;
  end;
end;

function TCustomBluetoothLEDevice.GetDistance: Double;
begin
  Result := Power(10, (TxPower - RSSI) / (10 * cSignalConst));
end;

function TCustomBluetoothLEDevice.GetServices: TBluetoothServices;
begin
  if FServices = nil then
    FServices := TBluetoothServices.Create([doOwnsValues]);
  Result := FServices;
end;

function TCustomBluetoothLEDevice.FindService(const AUUIDString: string; out AService: TCustomBluetoothService): Boolean;
begin
  Result := Services.TryGetValue(AUUIDString, AService);
end;

function TCustomBluetoothLEDevice.HasService(const AUUIDString: string): Boolean;
begin
  Result := Services.ContainsKey(AUUIDString);
end;

function TCustomBluetoothLEDevice.HasService(const AUUID: TGUID): Boolean;
begin
  Result := HasService(AUUID.ToString);
end;

procedure TCustomBluetoothLEDevice.SetServiceData(const AUUIDString: string; const AData: TBytes);
var
  LService: TCustomBluetoothService;
begin
  if FindService(AUUIDString, LService) then
    LService.SetData(AData);
end;

procedure TCustomBluetoothLEDevice.SetData(const Value: TBytes);
begin
  FKind := TBluetoothLEDeviceKind.Unknown;
  FData := Value;
  if BytesMatch(TBytes.Create($02, $01, $06)) then
    FKind := TBluetoothLEDeviceKind.iBeacon;
  case FKind of
    TBluetoothLEDeviceKind.iBeacon:
    begin
      TxPower := ShortInt(FData[cBLEDataIBeaconTxPower]);
      Sleep(0);
    end;
  end;
end;

function TCustomBluetoothLEDevice.DiscoverServices: Boolean;
begin
  Result := False;
end;

function TCustomBluetoothLEDevice.Connect: Boolean;
begin
  Result := False;
end;

procedure TCustomBluetoothLEDevice.DoCharacteristicRead(const AID: string; const AValue: TBytes);
begin
  if Assigned(FOnCharacteristicRead) then
    FOnCharacteristicRead(Self, AID, AValue);
end;

procedure TCustomBluetoothLEDevice.DoConnected;
begin
  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TCustomBluetoothLEDevice.DoDisconnected;
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

procedure TCustomBluetoothLEDevice.DoRemoteRssi(const ARssi: Integer);
begin
  if Assigned(FOnRemoteRssi) then
    FOnRemoteRssi(Self, ARssi);
end;

procedure TCustomBluetoothLEDevice.DoServicesDiscovered(const ASuccess: Boolean);
begin
  if Assigned(FOnServicesDiscovered) then
    FOnServicesDiscovered(Self, ASuccess);
end;

procedure TCustomBluetoothLEDevice.ReadCharacteristic(const AServiceUUID, ACharacteristicUUID: string);
begin
  //
end;

end.

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
  System.SysUtils, System.Classes,
  // DW
  DW.BluetoothLE.Types;

const
  cProximityImmediate = 0.5; // Metres
  cProximityNear = 3;
  cProximityFar = 30;
  cServiceUUIDWith16Bit = '0000%s-0000-1000-8000-00805F9B34FB';

type
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

  TBluetoothDevice = class;

  TCustomPlatformBluetoothDevice = class(TObject)
  private
    FBluetoothDevice: TBluetoothDevice;
  protected
    procedure Connect(const AAddress: string); virtual;
    function DiscoverServices: Boolean; virtual;
    procedure DoCharacteristicRead(const AID: string; const AValue: TBytes);
    procedure DoConnected;
    procedure DoDisconnected;
    procedure DoRemoteRssi(const ARssi: Integer);
    procedure DoServicesDiscovered;
    procedure ReadCharacteristic(const AServiceUUID, ACharacteristicUUID: string); virtual;
  public
    constructor Create(const ABluetoothDevice: TBluetoothDevice); virtual;
    destructor Destroy; override;
  end;

  TBluetoothDevice = class(TObject)
  private
    FPlatformBluetoothDevice: TCustomPlatformBluetoothDevice;
    FOnCharacteristicRead: TCharacteristicReadEvent;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnRemoteRssi: TRemoteRssiEvent;
    FOnServicesDiscovered: TNotifyEvent;
  protected
    procedure DoCharacteristicRead(const AID: string; const AValue: TBytes);
    procedure DoConnected;
    procedure DoDisconnected;
    procedure DoRemoteRssi(const ARssi: Integer);
    procedure DoServicesDiscovered;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const AAddress: string);
    function DiscoverServices: Boolean;
    procedure ReadCharacteristic(const AServiceUUID, ACharacteristicUUID: string);
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnCharacteristicRead: TCharacteristicReadEvent read FOnCharacteristicRead write FOnCharacteristicRead;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnRemoteRssi: TRemoteRssiEvent read FOnRemoteRssi write FOnRemoteRssi;
    property OnServicesDiscovered: TNotifyEvent read FOnServicesDiscovered write FOnServicesDiscovered;
  end;

implementation

uses
  System.Generics.Collections,
  {$IF Defined(MACOS)}
  DW.Bluetooth.Mac;
  {$ELSEIF Defined(ANDROID)}
  DW.Bluetooth.Android;
  {$ELSE}
  DW.Bluetooth.Default;
  {$ENDIF}

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

{ TCustomPlatformBluetoothDevice }

constructor TCustomPlatformBluetoothDevice.Create(const ABluetoothDevice: TBluetoothDevice);
begin
  inherited Create;
  FBluetoothDevice := ABluetoothDevice;
end;

destructor TCustomPlatformBluetoothDevice.Destroy;
begin
  //
  inherited;
end;

function TCustomPlatformBluetoothDevice.DiscoverServices: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformBluetoothDevice.Connect(const AAddress: string);
begin
  //
end;

procedure TCustomPlatformBluetoothDevice.DoCharacteristicRead(const AID: string; const AValue: TBytes);
begin
  FBluetoothDevice.DoCharacteristicRead(AID, AValue);
end;

procedure TCustomPlatformBluetoothDevice.DoConnected;
begin
  FBluetoothDevice.DoConnected;
end;

procedure TCustomPlatformBluetoothDevice.DoDisconnected;
begin
  FBluetoothDevice.DoDisconnected;
end;

procedure TCustomPlatformBluetoothDevice.DoRemoteRssi(const ARssi: Integer);
begin
  FBluetoothDevice.DoRemoteRssi(ARssi);
end;

procedure TCustomPlatformBluetoothDevice.DoServicesDiscovered;
begin

end;

procedure TCustomPlatformBluetoothDevice.ReadCharacteristic(const AServiceUUID, ACharacteristicUUID: string);
begin
  //
end;

{ TBluetoothDevice }

constructor TBluetoothDevice.Create;
begin
  inherited;
  FPlatformBluetoothDevice := TPlatformBluetoothDevice.Create(Self);
end;

destructor TBluetoothDevice.Destroy;
begin
  FPlatformBluetoothDevice.Free;
  inherited;
end;

function TBluetoothDevice.DiscoverServices: Boolean;
begin
  Result := FPlatformBluetoothDevice.DiscoverServices;
end;

procedure TBluetoothDevice.Connect(const AAddress: string);
begin
  FPlatformBluetoothDevice.Connect(AAddress);
end;

procedure TBluetoothDevice.DoCharacteristicRead(const AID: string; const AValue: TBytes);
begin
  if Assigned(FOnCharacteristicRead) then
    FOnCharacteristicRead(Self, AID, AValue);
end;

procedure TBluetoothDevice.DoConnected;
begin
  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TBluetoothDevice.DoDisconnected;
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

procedure TBluetoothDevice.DoRemoteRssi(const ARssi: Integer);
begin
  if Assigned(FOnRemoteRssi) then
    FOnRemoteRssi(Self, ARssi);
end;

procedure TBluetoothDevice.DoServicesDiscovered;
begin
  if Assigned(FOnServicesDiscovered) then
    FOnServicesDiscovered(Self);
end;

procedure TBluetoothDevice.ReadCharacteristic(const AServiceUUID, ACharacteristicUUID: string);
begin
  FPlatformBluetoothDevice.ReadCharacteristic(AServiceUUID, ACharacteristicUUID);
end;

end.

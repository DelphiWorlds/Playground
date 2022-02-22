unit DW.Beacons;

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
  System.SysUtils, System.Generics.Collections, System.Classes;

const
  cBeaconServiceUUIDBluecats = '{0000FEC4-0000-1000-8000-00805F9B34FB}';
  cSignalPropagationConst = 0.5;
  cProximityImmediate = 0.5; // Metres
  cProximityNear = 3;
  cProximityFar = 30;

type
  TServiceDataRawData = record
    Key: TGUID;
    Value: TBytes;
    //!!!! constructor create(const AKey: TGUID; const AValue: TBytes);
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
  end;

  TBluetoothLEScanFilters = TArray<TBluetoothLEScanFilter>;

  TCustomBluetoothLEDevice = class(TObject)
  private
    FAddress: string;
    FDeviceType: Integer;
    FName: string;
    FRSSI: Integer;
    FServiceUUIDs: TArray<TGUID>;
    FTxPower: Integer;
    function GetDistance: Double;
  public
    property Address: string read FAddress write FAddress;
    property DeviceType: Integer read FDeviceType write FDeviceType;
    property Distance: Double read GetDistance;
    property Name: string read FName write FName;
    property RSSI: Integer read FRSSI write FRSSI;
    property ServiceUUIDs: TArray<TGUID> read FServiceUUIDs write FServiceUUIDs;
    property TxPower: Integer read FTxPower write FTxPower;
  end;

  TBluetoothLEDevices = TObjectDictionary<string, TCustomBluetoothLEDevice>;

  TBeacons = class;

  TCustomPlatformBeacons = class(TObject)
  private
    FBeacons: TBeacons;
    FDevices: TBluetoothLEDevices;
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
    procedure ScanWithIntent; virtual;
    procedure ScanWithIntentStart;
    property Devices: TBluetoothLEDevices read FDevices;
    property Beacons: TBeacons read FBeacons;
    property Filters: TBluetoothLEScanFilters read FFilters;
  public
    constructor Create(const ABeacons: TBeacons); virtual;
    destructor Destroy; override;
  end;

  TDiscoveredDeviceEvent = procedure(Sender: TObject; const ADevice: TCustomBluetoothLEDevice) of object;

  TBeacons = class(TObject)
  private
    FPlatformBeacons: TCustomPlatformBeacons;
    FOnDiscoveredDevice: TDiscoveredDeviceEvent;
    FOnScanFinish: TNotifyEvent;
    function GetFilters: TBluetoothLEScanFilters;
    function GetDevices: TBluetoothLEDevices;
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
    property Filters: TBluetoothLEScanFilters read GetFilters;
    property OnDiscoveredDevice: TDiscoveredDeviceEvent read FOnDiscoveredDevice write FOnDiscoveredDevice;
    property OnScanFinish: TNotifyEvent read FOnScanFinish write FOnScanFinish;
  end;

implementation

uses
  System.Math,
  {$IF Defined(IOS)}
  DW.Beacons.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Beacons.Android;
  {$ELSE}
  DW.Beacons.Default;
  {$ENDIF}

{ TBluetoothLEScanFilter }

constructor TBluetoothLEScanFilter.Create(const AServiceUUID: string);
begin
  ServiceUUID := TGUID.Create(AServiceUUID);
  ServiceUUIDMask := TGUID.Create('{FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF}');
end;

{ TCustomBluetoothLEDevice }

function TCustomBluetoothLEDevice.GetDistance: Double;
begin
  Result := Power(10, (RSSI - TxPower) / -10 * cSignalPropagationConst);
end;

{ TCustomPlatformBeacons }

constructor TCustomPlatformBeacons.Create(const ABeacons: TBeacons);
begin
  inherited Create;
  FBeacons := ABeacons;
  FDevices := TBluetoothLEDevices.Create([doOwnsValues]);
end;

destructor TCustomPlatformBeacons.Destroy;
begin
  FDevices.Free;
  inherited;
end;

procedure TCustomPlatformBeacons.AddFilter(const AFilter: TBluetoothLEScanFilter);
begin
  FFilters := FFilters + [AFilter];
end;

procedure TCustomPlatformBeacons.ClearFilters;
begin
  FFilters := [];
end;

procedure TCustomPlatformBeacons.DiscoveredDevice(const ADevice: TCustomBluetoothLEDevice);
begin
  FBeacons.DoDiscoveredDevice(ADevice);
end;

procedure TCustomPlatformBeacons.HandleIntent(const AIntentID: Pointer);
begin
  //
end;

function TCustomPlatformBeacons.HasBluetoothPermission: Boolean;
begin
  Result := False;
end;

function TCustomPlatformBeacons.IsBluetoothEnabled: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformBeacons.ScanFinish;
begin
  FBeacons.DoScanFinish;
end;

procedure TCustomPlatformBeacons.ScanStart;
begin
  FDevices.Clear;
  Scan;
end;

procedure TCustomPlatformBeacons.ScanWithIntent;
begin
  //
end;

procedure TCustomPlatformBeacons.ScanWithIntentStart;
begin
  FDevices.Clear;
  ScanWithIntent;
end;

procedure TCustomPlatformBeacons.Scan;
begin
  //
end;

{ TBeacons }

constructor TBeacons.Create;
begin
  inherited;
  FPlatformBeacons := TPlatformBeacons.Create(Self);
end;

destructor TBeacons.Destroy;
begin
  FPlatformBeacons.Free;
  inherited;
end;

procedure TBeacons.AddFilter(const AFilter: TBluetoothLEScanFilter);
begin
  FPlatformBeacons.AddFilter(AFilter);
end;

procedure TBeacons.ClearFilters;
begin
  FPlatformBeacons.ClearFilters;
end;

procedure TBeacons.DoDiscoveredDevice(const ADevice: TCustomBluetoothLEDevice);
begin
  if Assigned(FOnDiscoveredDevice) then
    FOnDiscoveredDevice(Self, ADevice);
end;

procedure TBeacons.DoScanFinish;
begin
  if Assigned(FOnScanFinish) then
    FOnScanFinish(Self);
end;

function TBeacons.GetDevices: TBluetoothLEDevices;
begin
  Result := FPlatformBeacons.Devices;
end;

function TBeacons.GetFilters: TBluetoothLEScanFilters;
begin
  Result := FPlatformBeacons.Filters;
end;

procedure TBeacons.HandleIntent(const AIntentID: Pointer);
begin
  FPlatformBeacons.HandleIntent(AIntentID);
end;

procedure TBeacons.Scan;
begin
  FPlatformBeacons.ScanStart;
end;

procedure TBeacons.ScanWithIntent;
begin
  FPlatformBeacons.ScanWithIntentStart;
end;

end.

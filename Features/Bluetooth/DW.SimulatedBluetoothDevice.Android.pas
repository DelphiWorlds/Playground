unit DW.SimulatedBluetoothDevice.Android;

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
  System.SysUtils,
  // Android
  Androidapi.JNI.Bluetooth, Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.DWBluetooth, DW.SimulatedBluetoothDevice;

type
  TPlatformSimulatedBluetoothDevice = class;

  TAdvertiseCallbackDelegate = class(TJavaLocal, JDWAdvertiseCallbackDelegate)
  private
    FDevice: TPlatformSimulatedBluetoothDevice;
  public
    { JDWAdvertiseCallbackDelegate }
    procedure onStartFailure(errorCode: Integer); cdecl;
    procedure onStartSuccess(settingsInEffect: JAdvertiseSettings); cdecl;
  public
    constructor Create(const ADevice: TPlatformSimulatedBluetoothDevice);
  end;

  TBluetoothGattServerCallbackDelegate = class(TJavaLocal, JDWBluetoothGattServerCallbackDelegate)
  private
    FDevice: TPlatformSimulatedBluetoothDevice;
  public
    { JDWBluetoothGattServerCallbackDelegate }
    procedure onCharacteristicReadRequest(device: JBluetoothDevice; requestId: Integer; offset: Integer;
      characteristic: JBluetoothGattCharacteristic); cdecl;
    procedure onCharacteristicWriteRequest(device: JBluetoothDevice; requestId: Integer; characteristic: JBluetoothGattCharacteristic;
      preparedWrite: Boolean; responseNeeded: Boolean; offset: Integer; value: TJavaArray<Byte>); cdecl;
    procedure onConnectionStateChange(device: JBluetoothDevice; status: Integer; newState: Integer); cdecl;
    procedure onDescriptorReadRequest(device: JBluetoothDevice; requestId: Integer; offset: Integer; descriptor: JBluetoothGattDescriptor); cdecl;
    procedure onDescriptorWriteRequest(device: JBluetoothDevice; requestId: Integer; descriptor: JBluetoothGattDescriptor; preparedWrite: Boolean;
      responseNeeded: Boolean; offset: Integer; value: TJavaArray<Byte>); cdecl;
    procedure onExecuteWrite(device: JBluetoothDevice; requestId: Integer; execute: Boolean); cdecl;
    procedure onServiceAdded(status: Integer; service: JBluetoothGattService); cdecl;
  public
    constructor Create(const ADevice: TPlatformSimulatedBluetoothDevice);
  end;

  TServiceItem = record
    Service: JBluetoothGattService;
    AdvertisingData: TBytes;
    constructor Create(const AService: JBluetoothGattService; const AAdvertisingData: TBytes);
  end;

  TServiceItems = TArray<TServiceItem>;

  TBluetoothServices = record
    Items: TServiceItems;
    procedure AddService(const AService: JBluetoothGattService; const AAdvertisingData: TBytes);
  end;

  TPlatformSimulatedBluetoothDevice = class(TCustomPlatformSimulatedBluetoothDevice)
  private
    FAdapter: JBluetoothAdapter;
    FAdvertiser: JBluetoothLeAdvertiser;
    FAdvertiseCallback: JDWAdvertiseCallback;
    FAdvertiseCallbackDelegate: JDWAdvertiseCallbackDelegate;
    FGattServer: JBluetoothGattServer;
    FGattServerCallback: JDWBluetoothGattServerCallback;
    FGattServerCallbackDelegate: JDWBluetoothGattServerCallbackDelegate;
    FManager: JBluetoothManager;
    FServices: TBluetoothServices;
    FServicesIndex: Integer;
    procedure AddNextServerService;
    procedure AddServerService;
  protected
    procedure AdvertiserStartFailure(const AErrorCode: Integer);
    procedure AdvertiserStartSuccess(const ASettingsInEffect: JAdvertiseSettings);
    procedure CharacteristicReadRequest(const ADevice: JBluetoothDevice; const ARequestId, AOffset: Integer;
      const ACharacteristic: JBluetoothGattCharacteristic);
    procedure CharacteristicWriteRequest(const ADevice: JBluetoothDevice; const ARequestId: Integer;
      const ACharacteristic: JBluetoothGattCharacteristic; const APreparedWrite, AResponseNeeded: Boolean; const AOffset: Integer;
      const AValue: TJavaArray<Byte>);
    procedure ConnectionStateChange(const ADevice: JBluetoothDevice; const AStatus: Integer; const ANewState: Integer);
    procedure DescriptorReadRequest(const ADevice: JBluetoothDevice; const ARequestId, AOffset: Integer; const ADescriptor: JBluetoothGattDescriptor);
    procedure DescriptorWriteRequest(const ADevice: JBluetoothDevice; const ARequestId: Integer; const ADescriptor: JBluetoothGattDescriptor;
      const APreparedWrite, AResponseNeeded: Boolean; const AOffset: Integer; const AValue: TJavaArray<Byte>);
    procedure ServiceAdded(const status: Integer; const service: JBluetoothGattService);
  protected
    procedure ActiveChanging(const Value: Boolean); override;
    procedure AddService(const AUUID: string; const ACharacteristics: TArray<JBluetoothGattCharacteristic>; const AAdvertisingData: TBytes;
      const AIsPrimary: Boolean);
    procedure StartAdvertising; override;
    procedure StartServer; override;
    procedure StopAdvertising; override;
    procedure StopServer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  // Android
  Androidapi.Helpers, Androidapi.Jni.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,
  // DW
  DW.OSLog;

function StringToJUUID(const AValue: string): JUUID;
begin
  Result := TJUUID.JavaClass.fromString(StringToJString(AValue));
end;

function StringToServiceUUID(const AValue: string): string;
begin
  Result := AValue;
  if Length(Result) = 4 then
    Result := Format(cUUIDWith16Bit, [Result]);
end;

function StringToServiceJUUID(const AValue: string): JUUID;
begin
  Result := TJUUID.JavaClass.fromString(StringToJString(StringToServiceUUID(AValue)));
end;

{ TServiceItem }

constructor TServiceItem.Create(const AService: JBluetoothGattService; const AAdvertisingData: TBytes);
begin
  Service := AService;
  AdvertisingData := AAdvertisingData;
end;

{ TBluetoothServices }

procedure TBluetoothServices.AddService(const AService: JBluetoothGattService; const AAdvertisingData: TBytes);
begin
  Items := Items + [TServiceItem.Create(AService, AAdvertisingData)];
end;

{ TAdvertiseCallbackDelegate }

constructor TAdvertiseCallbackDelegate.Create(const ADevice: TPlatformSimulatedBluetoothDevice);
begin
  inherited Create;
  FDevice := ADevice;
end;

procedure TAdvertiseCallbackDelegate.onStartFailure(errorCode: Integer);
begin
  FDevice.AdvertiserStartFailure(errorCode);
end;

procedure TAdvertiseCallbackDelegate.onStartSuccess(settingsInEffect: JAdvertiseSettings);
begin
  FDevice.AdvertiserStartSuccess(settingsInEffect);
end;

{ TBluetoothGattServerCallbackDelegate }

constructor TBluetoothGattServerCallbackDelegate.Create(const ADevice: TPlatformSimulatedBluetoothDevice);
begin
  inherited Create;
  FDevice := ADevice;
end;

procedure TBluetoothGattServerCallbackDelegate.onCharacteristicReadRequest(device: JBluetoothDevice; requestId, offset: Integer;
  characteristic: JBluetoothGattCharacteristic);
begin
  FDevice.CharacteristicReadRequest(device, requestId, offset, characteristic);
end;

procedure TBluetoothGattServerCallbackDelegate.onCharacteristicWriteRequest(device: JBluetoothDevice; requestId: Integer;
  characteristic: JBluetoothGattCharacteristic; preparedWrite, responseNeeded: Boolean; offset: Integer; value: TJavaArray<Byte>);
begin
  FDevice.CharacteristicWriteRequest(device, requestId, characteristic, preparedWrite, responseNeeded, offset, value);
end;

procedure TBluetoothGattServerCallbackDelegate.onConnectionStateChange(device: JBluetoothDevice; status, newState: Integer);
begin
  FDevice.ConnectionStateChange(device, status, newState);
end;

procedure TBluetoothGattServerCallbackDelegate.onDescriptorReadRequest(device: JBluetoothDevice; requestId, offset: Integer;
  descriptor: JBluetoothGattDescriptor);
begin
  FDevice.DescriptorReadRequest(device, requestId, offset, descriptor);
end;

procedure TBluetoothGattServerCallbackDelegate.onDescriptorWriteRequest(device: JBluetoothDevice; requestId: Integer;
  descriptor: JBluetoothGattDescriptor; preparedWrite, responseNeeded: Boolean; offset: Integer; value: TJavaArray<Byte>);
begin
  FDevice.DescriptorWriteRequest(device, requestId, descriptor, preparedWrite, responseNeeded, offset, value);
end;

procedure TBluetoothGattServerCallbackDelegate.onExecuteWrite(device: JBluetoothDevice; requestId: Integer; execute: Boolean);
begin
  //
end;

procedure TBluetoothGattServerCallbackDelegate.onServiceAdded(status: Integer; service: JBluetoothGattService);
begin
  FDevice.ServiceAdded(status, service);
end;

{ TPlatformSimulatedBluetoothDevice }

constructor TPlatformSimulatedBluetoothDevice.Create;
begin
  inherited;
  FManager := TJBluetoothManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.BLUETOOTH_SERVICE));
  if FManager <> nil then
  begin
    FAdapter := FManager.getAdapter;
    if FAdapter.isMultipleAdvertisementSupported then
      FAdvertiser := FAdapter.getBluetoothLeAdvertiser;
  end;
end;

destructor TPlatformSimulatedBluetoothDevice.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformSimulatedBluetoothDevice.StartAdvertising;
var
  LServiceDataBytes: TJavaArray<Byte>;
  LDataBuilder: JAdvertiseData_Builder;
  LSettingsBuilder: JAdvertiseSettings_Builder;
  LUuid: JParcelUuid;
  LServiceItem: TServiceItem;
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.StartAdvertising');
  if FAdvertiser <> nil then
  begin
    if FAdvertiseCallbackDelegate = nil then
      FAdvertiseCallbackDelegate := TAdvertiseCallbackDelegate.Create(Self);
    if FAdvertiseCallback = nil then
      FAdvertiseCallback := TJDWAdvertiseCallback.JavaClass.init(FAdvertiseCallbackDelegate);
    LDataBuilder := TJAdvertiseData_Builder.JavaClass.init;
    for LServiceItem in FServices.Items do
    begin
      LUuid := TJParcelUuid.JavaClass.init(LServiceItem.Service.getUuid);
      if Length(LServiceItem.AdvertisingData) > 0 then
      begin
        LServiceDataBytes := TBytesToTJavaArray(LServiceItem.AdvertisingData);
        try
          LDataBuilder.addServiceData(LUuid, LServiceDataBytes);
        finally
          LServiceDataBytes.Free;
        end;
      end;
      LDataBuilder.addServiceUuid(LUuid);
    end;
    LDataBuilder.setIncludeDeviceName(False)
      .setIncludeTxPowerLevel(False);
    LSettingsBuilder := TJAdvertiseSettings_Builder.JavaClass.init
     .setAdvertiseMode(TJAdvertiseSettings.JavaClass.ADVERTISE_MODE_LOW_LATENCY)
     .setTxPowerLevel(TJAdvertiseSettings.JavaClass.ADVERTISE_TX_POWER_HIGH)
     .setConnectable(False);
    FAdvertiser.startAdvertising(LSettingsBuilder.build, LDataBuilder.build, FAdvertiseCallback);
  end;
  // else cannot advertise
end;

procedure TPlatformSimulatedBluetoothDevice.StopAdvertising;
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.StopAdvertising');
  if FAdvertiseCallback <> nil then
    FAdvertiser.stopAdvertising(FAdvertiseCallback);
end;

procedure TPlatformSimulatedBluetoothDevice.ActiveChanging(const Value: Boolean);
begin
  if Value then
    StartServer
  else
    StopServer;
end;

procedure TPlatformSimulatedBluetoothDevice.AdvertiserStartFailure(const AErrorCode: Integer);
begin
  FIsAdvertising := False;
  TOSLog.d('TBluetoothGattServer.AdvertiserStartFailure > errorCode: %d', [AErrorCode]);
end;

procedure TPlatformSimulatedBluetoothDevice.AdvertiserStartSuccess(const ASettingsInEffect: JAdvertiseSettings);
begin
  FIsAdvertising := True;
  TOSLog.d('TBluetoothGattServer.AdvertiserStartSuccess > settings: %s', [JStringToString(ASettingsInEffect.toString)]);
end;

procedure TPlatformSimulatedBluetoothDevice.ServiceAdded(const status: Integer; const service: JBluetoothGattService);
begin
  AddServerService;
end;

procedure TPlatformSimulatedBluetoothDevice.StartServer;
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.StartServer');
  if FGattServerCallbackDelegate = nil then
    FGattServerCallbackDelegate := TBluetoothGattServerCallbackDelegate.Create(Self);
  if FGattServerCallback = nil then
    FGattServerCallback := TJDWBluetoothGattServerCallback.JavaClass.init(FGattServerCallbackDelegate);
  FGattServer := FManager.openGattServer(TAndroidHelper.Context, FGattServerCallback);
  FServicesIndex := 0;
  AddServerService;
end;

procedure TPlatformSimulatedBluetoothDevice.AddServerService;
begin
  if FServicesIndex = Length(FServices.Items) - 1 then
  begin
    FIsActive := True;
    ServicesAdded;
  end
  else
    AddNextServerService;
end;

procedure TPlatformSimulatedBluetoothDevice.AddNextServerService;
begin
  Inc(FServicesIndex);
  FGattServer.addService(FServices.Items[FServicesIndex].Service);
end;

procedure TPlatformSimulatedBluetoothDevice.AddService(const AUUID: string; const ACharacteristics: TArray<JBluetoothGattCharacteristic>;
  const AAdvertisingData: TBytes; const AIsPrimary: Boolean);
var
  LServiceType: Integer;
  LCharacteristic: JBluetoothGattCharacteristic;
  LService: JBluetoothGattService;
begin
  if AIsPrimary then
    LServiceType := TJBluetoothGattService.JavaClass.SERVICE_TYPE_PRIMARY
  else
    LServiceType := TJBluetoothGattService.JavaClass.SERVICE_TYPE_SECONDARY;
  LService := TJBluetoothGattService.JavaClass.init(StringToServiceJUUID(AUUID), LServiceType);
  for LCharacteristic in ACharacteristics do
    LService.addCharacteristic(LCharacteristic);
  FServices.AddService(LService, AAdvertisingData);
end;

procedure TPlatformSimulatedBluetoothDevice.StopServer;
begin
  TOSLog.d('TBluetoothGattServer.StopServer');
  FGattServer.close;
  FGattServer := nil;
  FIsActive := False;
end;

procedure TPlatformSimulatedBluetoothDevice.CharacteristicReadRequest(const ADevice: JBluetoothDevice; const ARequestId, AOffset: Integer;
  const ACharacteristic: JBluetoothGattCharacteristic);
begin
  TOSLog.d('TBluetoothGattServer.CharacteristicReadRequest');
end;

procedure TPlatformSimulatedBluetoothDevice.CharacteristicWriteRequest(const ADevice: JBluetoothDevice; const ARequestId: Integer;
  const ACharacteristic: JBluetoothGattCharacteristic; const APreparedWrite, AResponseNeeded: Boolean; const AOffset: Integer;
  const AValue: TJavaArray<Byte>);
begin
  TOSLog.d('TBluetoothGattServer.CharacteristicWriteRequest');
end;

procedure TPlatformSimulatedBluetoothDevice.ConnectionStateChange(const ADevice: JBluetoothDevice; const AStatus, ANewState: Integer);
begin
  TOSLog.d('TBluetoothGattServer.ConnectionStateChange > Device: %s, Status: %d, NewState: %d', [JStringToString(ADevice.getName), AStatus, ANewState]);
end;

procedure TPlatformSimulatedBluetoothDevice.DescriptorReadRequest(const ADevice: JBluetoothDevice; const ARequestId, AOffset: Integer;
  const ADescriptor: JBluetoothGattDescriptor);
begin
  TOSLog.d('TBluetoothGattServer.DescriptorReadRequest');
end;

procedure TPlatformSimulatedBluetoothDevice.DescriptorWriteRequest(const ADevice: JBluetoothDevice; const ARequestId: Integer;
  const ADescriptor: JBluetoothGattDescriptor; const APreparedWrite, AResponseNeeded: Boolean; const AOffset: Integer;
  const AValue: TJavaArray<Byte>);
begin
  TOSLog.d('TBluetoothGattServer.DescriptorWriteRequest');
end;

end.

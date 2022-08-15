unit DW.SimulatedBluetoothLEDevice.Android;

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
  DW.Androidapi.JNI.DWBluetooth, DW.SimulatedBluetoothLEDevice;

type
  TPlatformSimulatedBluetoothLEDevice = class;

  TAdvertiseCallbackDelegate = class(TJavaLocal, JDWAdvertiseCallbackDelegate)
  private
    FDevice: TPlatformSimulatedBluetoothLEDevice;
  public
    { JDWAdvertiseCallbackDelegate }
    procedure onStartFailure(errorCode: Integer); cdecl;
    procedure onStartSuccess(settingsInEffect: JAdvertiseSettings); cdecl;
  public
    constructor Create(const ADevice: TPlatformSimulatedBluetoothLEDevice);
  end;

  TBluetoothGattServerCallbackDelegate = class(TJavaLocal, JDWBluetoothGattServerCallbackDelegate)
  private
    FDevice: TPlatformSimulatedBluetoothLEDevice;
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
    constructor Create(const ADevice: TPlatformSimulatedBluetoothLEDevice);
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

  TPlatformSimulatedBluetoothLEDevice = class(TCustomPlatformSimulatedBluetoothLEDevice)
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
    FCanIncludeName: Boolean;
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
    function CreateCharacteristic(const AUUID, AName: string; const AProperties: Integer): JBluetoothGattCharacteristic;
    function GetCharacteristicValue(const ACharacteristic: JBluetoothGattCharacteristic): TBytes;
    function GetDescriptorValue(const ADescriptor: JBluetoothGattDescriptor): TBytes;
    procedure NotifyCharacteristicChanged(const ACharacteristic: JBluetoothGattCharacteristic);
    function SendResponse(const ADevice: JBluetoothDevice; const ARequestId, AStatus, AOffset: Integer; const AValue: TBytes): Boolean;
    procedure SetCharacteristicValue(const ACharacteristic: JBluetoothGattCharacteristic; const AValue: TBytes);
    procedure SetDescriptorValue(const ADescriptor: JBluetoothGattDescriptor; const AValue: TBytes);
    procedure StartAdvertising; override;
    procedure StartServer; override;
    procedure StopAdvertising; override;
    procedure StopServer; override;
    property Server: JBluetoothGattServer read FGattServer;
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

function StringToUUID(const AValue: string): string;
begin
  Result := AValue;
  if Length(Result) = 4 then
    Result := Format(cUUIDWith16Bit, [Result]);
end;

function StringToJUUID(const AValue: string): JUUID;
begin
  Result := TJUUID.JavaClass.fromString(StringToJString(StringToUUID(AValue)));
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

constructor TAdvertiseCallbackDelegate.Create(const ADevice: TPlatformSimulatedBluetoothLEDevice);
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

constructor TBluetoothGattServerCallbackDelegate.Create(const ADevice: TPlatformSimulatedBluetoothLEDevice);
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

{ TPlatformSimulatedBluetoothLEDevice }

constructor TPlatformSimulatedBluetoothLEDevice.Create;
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

destructor TPlatformSimulatedBluetoothLEDevice.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformSimulatedBluetoothLEDevice.NotifyCharacteristicChanged(const ACharacteristic: JBluetoothGattCharacteristic);
var
  I: Integer;
  LDevices: JList;
begin
  LDevices := FManager.getConnectedDevices(TJBluetoothProfile.JavaClass.GATT);
  for I := 0 to LDevices.size -1 do
    FGattServer.notifyCharacteristicChanged(TJBluetoothDevice.Wrap(LDevices.get(I)), ACharacteristic, False);
end;

function TPlatformSimulatedBluetoothLEDevice.CreateCharacteristic(const AUUID, AName: string; const AProperties: Integer): JBluetoothGattCharacteristic;
begin
  Result := TJBluetoothGattCharacteristic.JavaClass.init(StringToJUUID(AUUID), AProperties,
    TJBluetoothGattCharacteristic.JavaClass.PERMISSION_READ or TJBluetoothGattCharacteristic.JavaClass.PERMISSION_WRITE);
end;

procedure TPlatformSimulatedBluetoothLEDevice.StartAdvertising;
var
  LServiceDataBytes: TJavaArray<Byte>;
  LDataBuilder: JAdvertiseData_Builder;
  LSettingsBuilder: JAdvertiseSettings_Builder;
  LUuid: JParcelUuid;
  LServiceItem: TServiceItem;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StartAdvertising');
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
    LDataBuilder.setIncludeDeviceName(FCanIncludeName)
      .setIncludeTxPowerLevel(False);
    LSettingsBuilder := TJAdvertiseSettings_Builder.JavaClass.init
     .setAdvertiseMode(TJAdvertiseSettings.JavaClass.ADVERTISE_MODE_LOW_LATENCY)
     .setTxPowerLevel(TJAdvertiseSettings.JavaClass.ADVERTISE_TX_POWER_HIGH)
     .setConnectable(True);
    FAdvertiser.startAdvertising(LSettingsBuilder.build, LDataBuilder.build, FAdvertiseCallback);
  end;
  // else cannot advertise
end;

procedure TPlatformSimulatedBluetoothLEDevice.StopAdvertising;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StopAdvertising');
  if FAdvertiseCallback <> nil then
    FAdvertiser.stopAdvertising(FAdvertiseCallback);
end;

procedure TPlatformSimulatedBluetoothLEDevice.ActiveChanging(const Value: Boolean);
begin
  if Value then
    StartServer
  else
    StopServer;
end;

procedure TPlatformSimulatedBluetoothLEDevice.AdvertiserStartFailure(const AErrorCode: Integer);
begin
  FIsAdvertising := False;
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.AdvertiserStartFailure > errorCode: %d', [AErrorCode]);
end;

procedure TPlatformSimulatedBluetoothLEDevice.AdvertiserStartSuccess(const ASettingsInEffect: JAdvertiseSettings);
begin
  FIsAdvertising := True;
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.AdvertiserStartSuccess > settings: %s', [JStringToString(ASettingsInEffect.toString)]);
end;

procedure TPlatformSimulatedBluetoothLEDevice.ServiceAdded(const status: Integer; const service: JBluetoothGattService);
begin
  AddServerService;
end;

procedure TPlatformSimulatedBluetoothLEDevice.StartServer;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StartServer');
  if FGattServerCallbackDelegate = nil then
    FGattServerCallbackDelegate := TBluetoothGattServerCallbackDelegate.Create(Self);
  if FGattServerCallback = nil then
    FGattServerCallback := TJDWBluetoothGattServerCallback.JavaClass.init(FGattServerCallbackDelegate);
  FGattServer := FManager.openGattServer(TAndroidHelper.Context, FGattServerCallback);
  FServicesIndex := 0;
  AddServerService;
end;

procedure TPlatformSimulatedBluetoothLEDevice.AddServerService;
begin
  if FServicesIndex = Length(FServices.Items) - 1 then
  begin
    FIsActive := True;
    ServicesAdded;
  end
  else
    AddNextServerService;
end;

procedure TPlatformSimulatedBluetoothLEDevice.AddNextServerService;
begin
  Inc(FServicesIndex);
  FGattServer.addService(FServices.Items[FServicesIndex].Service);
end;

procedure TPlatformSimulatedBluetoothLEDevice.AddService(const AUUID: string; const ACharacteristics: TArray<JBluetoothGattCharacteristic>;
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
  LService := TJBluetoothGattService.JavaClass.init(StringToJUUID(AUUID), LServiceType);
  for LCharacteristic in ACharacteristics do
    LService.addCharacteristic(LCharacteristic);
  FServices.AddService(LService, AAdvertisingData);
end;

procedure TPlatformSimulatedBluetoothLEDevice.StopServer;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StopServer');
  FGattServer.close;
  FGattServer := nil;
  FIsActive := False;
end;

function TPlatformSimulatedBluetoothLEDevice.GetCharacteristicValue(const ACharacteristic: JBluetoothGattCharacteristic): TBytes;
var
  LValue: TJavaArray<Byte>;
begin
  LValue := ACharacteristic.getValue;
  try
    Result := TJavaArrayToTBytes(LValue);
  finally
    LValue.Free;
  end;
end;

procedure TPlatformSimulatedBluetoothLEDevice.SetCharacteristicValue(const ACharacteristic: JBluetoothGattCharacteristic; const AValue: TBytes);
var
  LValue: TJavaArray<Byte>;
begin
  LValue := TBytesToTJavaArray(AValue);
  try
    ACharacteristic.setValue(LValue);
  finally
    LValue.Free;
  end;
end;

function TPlatformSimulatedBluetoothLEDevice.GetDescriptorValue(const ADescriptor: JBluetoothGattDescriptor): TBytes;
var
  LValue: TJavaArray<Byte>;
begin
  LValue := ADescriptor.getValue;
  try
    Result := TJavaArrayToTBytes(LValue);
  finally
    LValue.Free;
  end;
end;

procedure TPlatformSimulatedBluetoothLEDevice.SetDescriptorValue(const ADescriptor: JBluetoothGattDescriptor; const AValue: TBytes);
var
  LValue: TJavaArray<Byte>;
begin
  LValue := TBytesToTJavaArray(AValue);
  try
    ADescriptor.setValue(LValue);
  finally
    LValue.Free;
  end;
end;

function TPlatformSimulatedBluetoothLEDevice.SendResponse(const ADevice: JBluetoothDevice; const ARequestId, AStatus, AOffset: Integer;
  const AValue: TBytes): Boolean;
var
  LValue: TJavaArray<Byte>;
  LName, LAddress: string;
begin
  LValue := TBytesToTJavaArray(AValue);
  try
    Result := FGattServer.sendResponse(ADevice, ARequestId, AStatus, AOffset, LValue);
  finally
    LValue.Free;
  end;
  if not Result then
  begin
    LName := JStringToString(ADevice.getName);
    LAddress := JStringToString(ADevice.getAddress);
    TOSLog.e('Cannot send response to device "%s" at "%s" - Status: %d', [LName , LAddress, AStatus]);
  end;
end;

procedure TPlatformSimulatedBluetoothLEDevice.CharacteristicReadRequest(const ADevice: JBluetoothDevice; const ARequestId, AOffset: Integer;
  const ACharacteristic: JBluetoothGattCharacteristic);
var
  LStatus, LOffset: Integer;
  LValue: TBytes;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.CharacteristicReadRequest');
  LStatus := TJBluetoothGatt.JavaClass.GATT_SUCCESS;
  LValue := GetCharacteristicValue(ACharacteristic);
  LOffset := Length(LValue) - AOffset;
  if LOffset <= 0 then
  begin
    SetLength(LValue, 0);
    SendResponse(ADevice, ARequestId, LStatus, LOffset, LValue);
  end
  else
  begin
    if AOffset > 0 then
    begin
      Move(LValue[AOffset], LValue[0], LOffset);
      SetLength(LValue, LOffset);
    end;
    SendResponse(ADevice, ARequestId, LStatus, LOffset, LValue);
  end;
end;

procedure TPlatformSimulatedBluetoothLEDevice.CharacteristicWriteRequest(const ADevice: JBluetoothDevice; const ARequestId: Integer;
  const ACharacteristic: JBluetoothGattCharacteristic; const APreparedWrite, AResponseNeeded: Boolean; const AOffset: Integer;
  const AValue: TJavaArray<Byte>);
var
  LStatus, LOffset, LTotal: Integer;
  LNewValue, LOldValue: TBytes;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.CharacteristicWriteRequest');
  LNewValue := TJavaArrayToTBytes(AValue);
  LStatus := TJBluetoothGatt.JavaClass.GATT_SUCCESS;
  if AOffset > 0 then
  begin
    LTotal := Length(LNewValue);
    LOffset := AOffset + LTotal;
    LOldValue := GetCharacteristicValue(ACharacteristic);
    SetLength(LOldValue, LOffset);
    Move(LNewValue[0], LOldValue[AOffset], LTotal);
    SetCharacteristicValue(ACharacteristic, LNewValue);
  end
  else
    SetCharacteristicValue(ACharacteristic, LNewValue);
  if AResponseNeeded then
    SendResponse(ADevice, ARequestId, LStatus, AOffset, LNewValue);
end;


procedure TPlatformSimulatedBluetoothLEDevice.ConnectionStateChange(const ADevice: JBluetoothDevice; const AStatus, ANewState: Integer);
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.ConnectionStateChange > Device: %s, Status: %d, NewState: %d',
    [JStringToString(ADevice.getName), AStatus, ANewState]);
end;

procedure TPlatformSimulatedBluetoothLEDevice.DescriptorReadRequest(const ADevice: JBluetoothDevice; const ARequestId, AOffset: Integer;
  const ADescriptor: JBluetoothGattDescriptor);
var
  LStatus, LOffset: Integer;
  LValue: TBytes;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.DescriptorReadRequest');
  LStatus := TJBluetoothGatt.JavaClass.GATT_SUCCESS;
  LValue := GetDescriptorValue(ADescriptor);
  LOffset := Length(LValue) - AOffset;
  if LOffset <= 0 then
  begin
    SetLength(LValue, 0);
    SendResponse(ADevice, ARequestId, LStatus, LOffset, LValue);
  end
  else
  begin
    if AOffset > 0 then
    begin
      Move(LValue[AOffset], LValue[0], LOffset);
      SetLength(LValue, LOffset);
    end;
    SendResponse(ADevice, ARequestId, LStatus, LOffset, LValue);
  end;
end;

procedure TPlatformSimulatedBluetoothLEDevice.DescriptorWriteRequest(const ADevice: JBluetoothDevice; const ARequestId: Integer;
  const ADescriptor: JBluetoothGattDescriptor; const APreparedWrite, AResponseNeeded: Boolean; const AOffset: Integer;
  const AValue: TJavaArray<Byte>);
var
  LStatus, LOffset, LTotal: Integer;
  LNewValue, LOldValue: TBytes;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.DescriptorWriteRequest');
  LNewValue := TJavaArrayToTBytes(AValue);
  LStatus := TJBluetoothGatt.JavaClass.GATT_SUCCESS;
  if AOffset > 0 then
  begin
    LTotal := Length(LNewValue);
    LOffset := AOffset + LTotal;
    LOldValue := GetDescriptorValue(ADescriptor);
    SetLength(LOldValue, LOffset);
    Move(LNewValue[0], LOldValue[AOffset], LTotal);
    SetDescriptorValue(ADescriptor, LNewValue);
  end
  else
    SetDescriptorValue(ADescriptor, LNewValue);
  if AResponseNeeded then
    SendResponse(ADevice, ARequestId, LStatus, AOffset, LNewValue);
end;

end.

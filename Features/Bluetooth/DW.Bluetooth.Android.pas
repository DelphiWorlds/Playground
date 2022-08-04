unit DW.Bluetooth.Android;

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
  System.Generics.Collections, System.SysUtils, System.Classes,
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Bluetooth, Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.DWBluetooth, DW.Bluetooth, DW.BluetoothLE.Types;

const
  cBluetoothBroadcastReceiver = 'com.delphiworlds.kastri.DWBluetoothBroadcastReceiver';
  cBluetoothBroadcastReceiverActionAlarm = cBluetoothBroadcastReceiver + '.ACTION_ALARM';
  cBluetoothBroadcastReceiverActionFound = cBluetoothBroadcastReceiver + '.ACTION_FOUND';

type
  TPlatformBluetoothScanner = class;

  TBluetoothScanCallbackDelegate = class(TJavaLocal, JDWScanCallbackDelegate)
  private
    FCallback: JScanCallback;
    FPlatformBluetoothScanner: TPlatformBluetoothScanner;
  public
    { JDWScanCallbackDelegate }
    procedure onBatchScanResults(results: JList); cdecl;
    procedure onScanFailed(errorCode: Integer); cdecl;
    procedure onScanResult(callbackType: Integer; result: Jle_ScanResult); cdecl;
  public
    constructor Create(const APlatformBluetoothScanner: TPlatformBluetoothScanner);
    property Callback: JScanCallback read FCallback;
  end;

  TPlatformBluetoothScanner = class(TCustomPlatformBluetoothScanner)
  private
    FBluetoothManager: JBluetoothManager;
    FScanCallbackDelegate: TBluetoothScanCallbackDelegate;
    FScanTimer: JRunnable;
    function GetFilters: JList;
    function GetRequiredPermissions: TArray<string>;
    function GetScanSettings: JScanSettings;
    procedure InternalScan;
    procedure InternalScanWithIntent;
    procedure ScanTimeExpired;
    procedure ScanWithPermission(const AScanMethod: TProc);
  protected
    procedure HandleIntent(const AIntentID: Pointer); override;
    function IsBluetoothEnabled: Boolean; override;
    procedure ScanFailed(const AErrorCode: Integer);
    procedure Scan; override;
    procedure ScanResult(const AScanResult: Jle_ScanResult);
    procedure ScanWithIntent; override;
  public
    constructor Create(const ABluetoothScanner: TBluetoothScanner); override;
    destructor Destroy; override;
  end;

  TPlatformBluetoothDevice = class;

  TBluetoothGattCallbackDelegate = class(TJavaLocal, JDWBluetoothGattCallbackDelegate)
  private
    FBluetoothGattCallback: JBluetoothGattCallback;
    FPlatformBluetoothDevice: TPlatformBluetoothDevice;
  public
    { JDWBluetoothGattCallbackDelegate }
    procedure onCharacteristicChanged(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic); cdecl;
    procedure onCharacteristicRead(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic; status: Integer); cdecl;
    procedure onCharacteristicWrite(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic; status: Integer); cdecl;
    procedure onConnectionStateChange(gatt: JBluetoothGatt; status: Integer; newState: Integer); cdecl;
    procedure onDescriptorRead(gatt: JBluetoothGatt; descriptor: JBluetoothGattDescriptor; status: Integer); cdecl;
    procedure onDescriptorWrite(gatt: JBluetoothGatt; descriptor: JBluetoothGattDescriptor; status: Integer); cdecl;
    procedure onReadRemoteRssi(gatt: JBluetoothGatt; rssi: Integer; status: Integer); cdecl;
    procedure onReliableWriteCompleted(gatt: JBluetoothGatt; status: Integer); cdecl;
    procedure onServicesDiscovered(gatt: JBluetoothGatt; status: Integer); cdecl;
  public
    constructor Create(const APlatformBluetoothDevice: TPlatformBluetoothDevice);
    property Callback: JBluetoothGattCallback read FBluetoothGattCallback;
  end;

  TPlatformBluetoothDevice = class(TCustomPlatformBluetoothDevice)
  private
    FBluetoothAdapter: JBluetoothAdapter;
    FBluetoothDevice: JBluetoothDevice;
    FBluetoothGatt: JBluetoothGatt;
    FBluetoothGattCallbackDelegate: TBluetoothGattCallbackDelegate;
    FBluetoothManager: JBluetoothManager;
    FIsConnected: Boolean;
    function GetCharacteristic(const AServiceUUID, ACharacteristicUUID: string): JBluetoothGattCharacteristic; overload;
    function GetCharacteristic(const AService: JBluetoothGattService; const ACharacteristicUUID: string): JBluetoothGattCharacteristic; overload;
    function GetCharacteristicValue(const ACharacteristic: JBluetoothGattCharacteristic): TBytes;
    function GetService(const AServiceUUID: string): JBluetoothGattService;
  protected
    procedure CharacteristicChanged(const AGatt: JBluetoothGatt; const ACharacteristic: JBluetoothGattCharacteristic);
    procedure CharacteristicRead(const AGatt: JBluetoothGatt; const ACharacteristic: JBluetoothGattCharacteristic; const AStatus: Integer);
    procedure CharacteristicWrite(const AGatt: JBluetoothGatt; const ACharacteristic: JBluetoothGattCharacteristic; const AStatus: Integer);
    procedure ConnectionStateChange(const AGatt: JBluetoothGatt; const AStatus: Integer; const ANewState: Integer);
    procedure DescriptorRead(const AGatt: JBluetoothGatt; const ADescriptor: JBluetoothGattDescriptor; const AStatus: Integer);
    procedure DescriptorWrite(const AGatt: JBluetoothGatt; const ADescriptor: JBluetoothGattDescriptor; const AStatus: Integer);
    procedure ReadRemoteRssi(const AGatt: JBluetoothGatt; const ARssi: Integer; const AStatus: Integer);
    procedure ReliableWriteCompleted(const AGatt: JBluetoothGatt; const AStatus: Integer);
    procedure ServicesDiscovered(const AGatt: JBluetoothGatt; const AStatus: Integer);
  protected
    procedure Connect(const AAddress: string); override;
    function DiscoverServices: Boolean; override;
    procedure ReadCharacteristic(const AServiceUUID, ACharacteristicUUID: string); override;
  public
    constructor Create(const ABluetoothDevice: TBluetoothDevice); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Permissions,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.OSLog,
  DW.Android.Helpers, DW.Consts.Android, DW.Permissions.Helpers;

type
  [JavaSignature('android/bluetooth/le/BluetoothLeScanner')]
  JBluetoothLeScannerEx = interface(JBluetoothLeScanner)
    ['{A03F64D2-5E1F-4772-9FEA-0920B826B68F}']
    function startScan(filters: JList; settings: JScanSettings; intent: JPendingIntent): Integer; cdecl; overload;
    procedure stopScan(intent: JPendingIntent); cdecl; overload;
  end;
  TJBluetoothLeScannerEx = class(TJavaGenericImport<JBluetoothLeScannerClass, JBluetoothLeScannerEx>) end;

  [JavaSignature('android/bluetooth/le/ScanRecord')]
  JScanRecordEx = interface(JObject)
    ['{75E54FC6-6BC5-47A9-893C-68981FB9E529}']
    function getServiceData: JMap; cdecl;
  end;
  TJScanRecordEx = class(TJavaGenericImport<JScanRecordClass, JScanRecordEx>) end;

  TPlatformBluetoothLEDevice = class(TCustomBluetoothLEDevice)
  private
    FBluetoothDevice: JBluetoothDevice;
  public
    property BluetoothDevice: JBluetoothDevice read FBluetoothDevice write FBluetoothDevice;
  end;

  TScanRunnable = class(THandlerRunnable, JRunnable)
  private
    FRunHandler: TThreadProcedure;
  protected
    procedure DoRun; override;
  public
    constructor Create(const ARunHandler: TThreadProcedure; const ADelay: Cardinal);
  end;

function GUIDToJParcelUuid(const AGUID: TGUID): JParcelUuid;
var
  LGUIDString: string;
begin
  LGUIDString := AGUID.ToString;
  LGUIDString := LGUIDString.Substring(1, LGUIDString.Length - 2);
  Result := TJParcelUuid.JavaClass.fromString(StringToJString(LGUIDString));
end;

function JParcelUuidToGUID(const AJParcelUuid: JParcelUuid): TGUID;
begin
  Result := TGUID.Create('{' + JStringToString(AJParcelUuid.getUuid.toString) + '}');
end;

{ TScanRunnable }

constructor TScanRunnable.Create(const ARunHandler: TThreadProcedure; const ADelay: Cardinal);
begin
  inherited Create(ADelay);
  FRunHandler := ARunHandler;
  Execute;
end;

procedure TScanRunnable.DoRun;
begin
  TThread.Queue(nil, FRunHandler);
end;

{ TBluetoothScanCallbackDelegate }

constructor TBluetoothScanCallbackDelegate.Create(const APlatformBluetoothScanner: TPlatformBluetoothScanner);
begin
  inherited Create;
  FPlatformBluetoothScanner := APlatformBluetoothScanner;
  FCallback := TJDWScanCallback.JavaClass.init(Self);
end;

procedure TBluetoothScanCallbackDelegate.onBatchScanResults(results: JList);
begin
  // Nothing?
end;

procedure TBluetoothScanCallbackDelegate.onScanFailed(errorCode: Integer);
begin
  FPlatformBluetoothScanner.ScanFailed(errorCode);
end;

procedure TBluetoothScanCallbackDelegate.onScanResult(callbackType: Integer; result: Jle_ScanResult);
begin
  FPlatformBluetoothScanner.ScanResult(result);
end;

{ TPlatformBluetoothScanner }

constructor TPlatformBluetoothScanner.Create(const ABluetoothScanner: TBluetoothScanner);
var
  LService: JObject;
begin
  inherited;
  LService := TAndroidHelper.Context.getApplicationContext.getSystemService(TJContext.JavaClass.BLUETOOTH_SERVICE);
  FBluetoothManager := TJBluetoothManager.Wrap(LService);
  FScanCallbackDelegate := TBluetoothScanCallbackDelegate.Create(Self);
end;

destructor TPlatformBluetoothScanner.Destroy;
begin
  FScanTimer := nil;
  FBluetoothManager := nil;
  FScanCallbackDelegate.Free;
  FScanCallbackDelegate := nil;
  inherited;
end;

procedure TPlatformBluetoothScanner.ScanFailed(const AErrorCode: Integer);
begin
  //!!!!!
end;

procedure TPlatformBluetoothScanner.ScanResult(const AScanResult: Jle_ScanResult);
var
  LDeviceAddress: string;
  LCustomDevice: TCustomBluetoothLEDevice;
  LDevice: TPlatformBluetoothLEDevice;
  LIsNew: Boolean;
  LBluetoothDevice: JBluetoothDevice;
  LUuids: JList;
  I: Integer;
  LData: TJavaArray<Byte>;
  LService: TDeviceService;
  LParcelUuid: JParcelUuid;
begin
  // TOSLog.d('TPlatformBluetoothScanner.ScanResult');
  LIsNew := False;
  LBluetoothDevice := AScanResult.getDevice;
  LDeviceAddress := JStringToString(LBluetoothDevice.getAddress);
  // TOSLog.d('> Device Address: %s', [LDeviceAddress]);
  if not Devices.TryGetValue(LDeviceAddress, LCustomDevice) then
  begin
    LDevice := TPlatformBluetoothLEDevice.Create;
    LDevice.Key := LDeviceAddress;
    LDevice.Address := LDeviceAddress;
    LDevice.Name := JStringToString(LBluetoothDevice.getName);
    if not LDevice.Name.IsEmpty then
      TOSLog.w('> Device Name: %s', [LDevice.Name]);
    TOSLog.d('> Device Address: %s', [LDevice.Address]);
    LDevice.DeviceType := LBluetoothDevice.getType;
    LUuids := AScanResult.getScanRecord.getServiceUuids;
    if LUuids <> nil then
    begin
      if LUuids.size > 1 then
        TOSLog.d('> Service uuids size: %d', [LUuids.size]);
      for I := 0 to LUuids.size - 1 do
      begin
        LParcelUuid := TJParcelUuid.Wrap(LUuids.get(I));
        LService.UUID := JParcelUuidToGUID(LParcelUuid);
        TOSLog.w('> Service UUID: %s', [LService.UUID.ToString]);
        LData := AScanResult.getScanRecord.getServiceData(LParcelUuid);
        if LData <> nil then
        try
          LService.Data := TJavaArrayToTBytes(LData);
        finally
          LData.Free;
        end;
        LDevice.Services := LDevice.Services + [LService];
      end;
    end;
    LDevice.BluetoothDevice := LBluetoothDevice;
    Devices.AddOrSetValue(LDevice.Key, LDevice);
    LIsNew := True;
  end
  else
    LDevice := TPlatformBluetoothLEDevice(LCustomDevice);
  LData := AScanResult.getScanRecord.getBytes;
  if LData <> nil then
  try
    LDevice.Data := TJavaArrayToTBytes(LData);
  finally
    LData.Free;
  end;
  LDevice.RSSI := AScanResult.getRssi;
  // LDevice.TxPower :=
  TOSLog.d('> Device RSSI: %d, TxPower: %d', [LDevice.RSSI, LDevice.TxPower]);
  TOSLog.d('> Device Distance: %.2f', [LDevice.Distance]);
  if LIsNew then
    DiscoveredDevice(LDevice);
  // TOSLog.d('-TPlatformBluetoothScanner.ScanResult');
end;

procedure TPlatformBluetoothScanner.ScanTimeExpired;
var
  LScanner: JBluetoothLeScannerEx;
begin
  FScanTimer := nil;
  if (FBluetoothManager <> nil) and (FScanCallbackDelegate <> nil) then
  begin
    LScanner := TJBluetoothLeScannerEx.Wrap(FBluetoothManager.getAdapter.getBluetoothLeScanner);
    LScanner.stopScan(FScanCallbackDelegate.Callback);
    ScanFinish;
  end;
end;

// Based on:
//   https://github.com/AltBeacon/android-beacon-library/blob/40fd635a003dc2f5ef671e2c48d0dba2e55a6189/lib/src/main/java/org/altbeacon/beacon/service/scanner/ScanFilterUtils.java#L123
// Add 128 bit support if needed?
function TPlatformBluetoothScanner.GetFilters: JList;
var
  LFilters: JArrayList;
  LBuilder: JScanFilter_Builder;
  LFilter: TBluetoothLEScanFilter;
  LServiceUUID, LServiceUUIDMask: JParcelUuid;
  LManufacturerData, LManufacturerDataMask: TJavaArray<Byte>;
begin
  LFilters := TJArrayList.JavaClass.init;
  for LFilter in Filters do
  begin
    if (LFilter.ServiceUUID <> TGUID.Empty) or (Length(LFilter.ManufacturerData) > 0) then
    begin
      LBuilder := TJScanFilter_Builder.JavaClass.init;
      if LFilter.ServiceUUID <> TGUID.Empty then
      begin
        LServiceUUID := GUIDToJParcelUuid(LFilter.ServiceUUID);
        LServiceUUIDMask := nil;
        if LFilter.ServiceUUIDMask <> TGUID.Empty then
          LServiceUUIDMask := GUIDToJParcelUuid(LFilter.ServiceUUIDMask);
        if LServiceUUIDMask <> nil then
        begin
          // TOSLog.d('Set ServiceUUID of %s on builder', [LFilter.ServiceUUID.ToString]);
          LBuilder.setServiceUuid(LServiceUUID, LServiceUUIDMask);
        end
        else
          LBuilder.setServiceUuid(LServiceUUID);
      end
      else
      begin
        LBuilder.setServiceUuid(nil);
        LManufacturerData := TBytesToTJavaArray(LFilter.ManufacturerData);
        try
          LManufacturerDataMask := TBytesToTJavaArray(LFilter.ManufacturerDataMask);
          try
            LBuilder.setManufacturerData(LFilter.ManufacturerId, LManufacturerData, LManufacturerDataMask);
          finally
            LManufacturerDataMask.Free;
          end;
        finally
          LManufacturerData.Free;
        end;
      end;
      LFilters.add(LBuilder.build);
      // TOSLog.d('Added filter to array');
    end;
  end;
  if LFilters.size > 0 then
  begin
    // TOSLog.d('%d filters in array', [LFilters.size]);
    Result := TJList.Wrap(LFilters);
  end
  else
    Result := nil;
end;

function TPlatformBluetoothScanner.GetScanSettings: JScanSettings;
begin
  Result := TJScanSettings_Builder.JavaClass.init
    .setScanMode(TJScanSettings.JavaClass.SCAN_MODE_LOW_POWER)
    .build;
end;

// http://www.davidgyoungtech.com/2017/08/07/beacon-detection-with-android-8
procedure TPlatformBluetoothScanner.HandleIntent(const AIntentID: Pointer);
var
  LBLECallbackType: Integer;
  LScanResults: JArrayList;
  I: Integer;
  LIntent: JIntent;
begin
  LIntent := TJIntent.Wrap(AIntentID);
  LBLECallbackType := LIntent.getIntExtra(TJBluetoothLeScanner.JavaClass.EXTRA_CALLBACK_TYPE, -1);
  if LBLECallbackType <> -1 then
  begin
    LScanResults := LIntent.getParcelableArrayListExtra(TJBluetoothLeScanner.JavaClass.EXTRA_LIST_SCAN_RESULT);
    if LScanResults <> nil then
    begin
      for I := 0 to LScanResults.size - 1 do
        ScanResult(TJle_ScanResult.Wrap(LScanResults.get(I)));
    end;
  end;
end;

function TPlatformBluetoothScanner.GetRequiredPermissions: TArray<string>;
begin
  if not PermissionsService.IsPermissionGranted(cPermissionAccessFineLocation) then
    Result := Result + [cPermissionAccessFineLocation];
  if not PermissionsService.IsPermissionGranted(cPermissionBluetooth) then
    Result := Result + [cPermissionBluetooth];
  if TJBuild_VERSION.JavaClass.SDK_INT > 31 then
  begin
    if not PermissionsService.IsPermissionGranted(cPermissionBluetoothConnect) then
      Result := Result + [cPermissionBluetoothConnect];
    if not PermissionsService.IsPermissionGranted(cPermissionBluetoothScan) then
      Result := Result + [cPermissionBluetoothScan];
  end;
end;

function TPlatformBluetoothScanner.IsBluetoothEnabled: Boolean;
begin
  Result := TJBluetoothAdapter.JavaClass.getDefaultAdapter.isEnabled;
end;

procedure TPlatformBluetoothScanner.InternalScan;
var
  LScanner: JBluetoothLeScannerEx;
begin
  TOSLog.d('Starting plain scan..');
  LScanner := TJBluetoothLeScannerEx.Wrap(FBluetoothManager.getAdapter.getBluetoothLeScanner);
  LScanner.startScan(GetFilters, GetScanSettings, FScanCallbackDelegate.Callback);
  // LScanner.startScan(nil, GetScanSettings, FScanCallbackDelegate.Callback);
  FScanTimer := TScanRunnable.Create(ScanTimeExpired, Expiry);
end;

procedure TPlatformBluetoothScanner.Scan;
begin
  ScanWithPermission(InternalScan);
end;

procedure TPlatformBluetoothScanner.InternalScanWithIntent;
var
  LScanner: JBluetoothLeScannerEx;
  LIntent: JIntent;
  LPendingIntent: JPendingIntent;
  LFlags: Integer;
  LServiceName: string;
begin
  // Using receiver:
  // LIntent := TJIntent.JavaClass.init(StringToJString(cBluetoothReceiverActionFound));
  // LIntent.setClassName(TAndroidHelper.Context, StringToJString(cBluetoothReceiver));
  // Using service:
  LServiceName := AndroidServiceName;
  if not LServiceName.StartsWith(cEMBTJavaServicePrefix) then
    LServiceName := cEMBTJavaServicePrefix + AndroidServiceName;
  LIntent := TJIntent.JavaClass.init;
  LIntent.setClassName(TAndroidHelper.Context, StringToJString(LServiceName));
  LFlags := TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT;
  if TJBuild_VERSION.JavaClass.SDK_INT > 31 then
    LFlags := LFlags or $02000000; // https://developer.android.com/reference/android/app/PendingIntent#FLAG_MUTABLE
  // Using receiver:
  // LPendingIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LIntent, LFlags);
  // Using service:
  LPendingIntent := TJPendingIntent.JavaClass.getService(TAndroidHelper.Context, 0, LIntent, LFlags);
  LScanner := TJBluetoothLeScannerEx.Wrap(FBluetoothManager.getAdapter.getBluetoothLeScanner);
  TOSLog.d('Starting intent scan..');
  LScanner.startScan(GetFilters, GetScanSettings, LPendingIntent);
end;

procedure TPlatformBluetoothScanner.ScanWithIntent;
begin
  ScanWithPermission(InternalScanWithIntent);
end;

procedure TPlatformBluetoothScanner.ScanWithPermission(const AScanMethod: TProc);
var
  LPermissions: TArray<string>;
begin
  if IsBluetoothEnabled then
  begin
    LPermissions := GetRequiredPermissions;
    if Length(LPermissions) > 0 then
    begin
      PermissionsService.RequestPermissions(LPermissions,
        procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
        begin
          if AGrantResults.AreAllGranted then
            AScanMethod
          else
            TOSLog.d('Has not granted all required permissions');
        end
      );
    end
    else
      AScanMethod;
  end
  else
    TOSLog.d('Bluetooth NOT enabled');
end;

{ TBluetoothGattCallbackDelegate }

constructor TBluetoothGattCallbackDelegate.Create(const APlatformBluetoothDevice: TPlatformBluetoothDevice);
begin
  inherited Create;
  FPlatformBluetoothDevice := APlatformBluetoothDevice;
  FBluetoothGattCallback := TJDWBluetoothGattCallback.JavaClass.init(Self);
end;

procedure TBluetoothGattCallbackDelegate.onCharacteristicChanged(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic);
begin
  FPlatformBluetoothDevice.CharacteristicChanged(gatt, characteristic);
end;

procedure TBluetoothGattCallbackDelegate.onCharacteristicRead(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic; status: Integer);
begin
  FPlatformBluetoothDevice.CharacteristicRead(gatt, characteristic, status);
end;

procedure TBluetoothGattCallbackDelegate.onCharacteristicWrite(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic; status: Integer);
begin
  FPlatformBluetoothDevice.CharacteristicWrite(gatt, characteristic, status);
end;

procedure TBluetoothGattCallbackDelegate.onConnectionStateChange(gatt: JBluetoothGatt; status, newState: Integer);
begin
  FPlatformBluetoothDevice.ConnectionStateChange(gatt, status, newState);
end;

procedure TBluetoothGattCallbackDelegate.onDescriptorRead(gatt: JBluetoothGatt; descriptor: JBluetoothGattDescriptor; status: Integer);
begin
  FPlatformBluetoothDevice.DescriptorRead(gatt, descriptor, status);
end;

procedure TBluetoothGattCallbackDelegate.onDescriptorWrite(gatt: JBluetoothGatt; descriptor: JBluetoothGattDescriptor; status: Integer);
begin
  FPlatformBluetoothDevice.DescriptorWrite(gatt, descriptor, status);
end;

procedure TBluetoothGattCallbackDelegate.onReadRemoteRssi(gatt: JBluetoothGatt; rssi, status: Integer);
begin
  FPlatformBluetoothDevice.ReadRemoteRssi(gatt, rssi, status);
end;

procedure TBluetoothGattCallbackDelegate.onReliableWriteCompleted(gatt: JBluetoothGatt; status: Integer);
begin
  FPlatformBluetoothDevice.ReliableWriteCompleted(gatt, status);
end;

procedure TBluetoothGattCallbackDelegate.onServicesDiscovered(gatt: JBluetoothGatt; status: Integer);
begin
  FPlatformBluetoothDevice.ServicesDiscovered(gatt, status);
end;

{ TPlatformBluetoothDevice }

constructor TPlatformBluetoothDevice.Create;
var
  LService: JObject;
begin
  inherited;
  LService := TAndroidHelper.Context.getApplicationContext.getSystemService(TJContext.JavaClass.BLUETOOTH_SERVICE);
  FBluetoothManager := TJBluetoothManager.Wrap(LService);
  FBluetoothAdapter := FBluetoothManager.getAdapter;
  FBluetoothGattCallbackDelegate := TBluetoothGattCallbackDelegate.Create(Self);
end;

destructor TPlatformBluetoothDevice.Destroy;
begin
  FBluetoothGattCallbackDelegate.Free;
  inherited;
end;

function TPlatformBluetoothDevice.DiscoverServices: Boolean;
begin
  Result := False;
  if FIsConnected then
    Result := FBluetoothGatt.discoverServices;
end;

procedure TPlatformBluetoothDevice.Connect(const AAddress: string);
begin
  FBluetoothDevice := nil;
  FBluetoothGatt := nil;
  FBluetoothDevice := FBluetoothAdapter.getRemoteDevice(StringToJString(AAddress));
  if FBluetoothDevice <> nil then
  begin
    FBluetoothGatt := FBluetoothDevice.connectGatt(TAndroidHelper.Context, False, FBluetoothGattCallbackDelegate.Callback);
    TOSLog.d('Connecting..');
  end;
end;

function TPlatformBluetoothDevice.GetCharacteristic(const AService: JBluetoothGattService;
  const ACharacteristicUUID: string): JBluetoothGattCharacteristic;
var
  LCharacteristics: JList;
  LCharacteristic: JBluetoothGattCharacteristic;
  I: Integer;
begin
  Result := nil;
  LCharacteristics := AService.getCharacteristics;
  for I := 0 to LCharacteristics.size - 1 do
  begin
    LCharacteristic := TJBluetoothGattCharacteristic.Wrap(LCharacteristics.get(I));
    if JStringToString(LCharacteristic.getUuid.toString).Equals(ACharacteristicUUID) then
    begin
      Result := LCharacteristic;
      Break;
    end;
  end;
end;

function TPlatformBluetoothDevice.GetCharacteristic(const AServiceUUID, ACharacteristicUUID: string): JBluetoothGattCharacteristic;
var
  LService: JBluetoothGattService;
begin
  Result := nil;
  LService := GetService(AServiceUUID);
  if LService <> nil then
    Result := GetCharacteristic(LService, ACharacteristicUUID);
end;

function TPlatformBluetoothDevice.GetService(const AServiceUUID: string): JBluetoothGattService;
var
  LServices: JList;
  LService: JBluetoothGattService;
  I: Integer;
begin
  Result := nil;
  if FBluetoothGatt <> nil then
  begin
    LServices := FBluetoothGatt.getServices;
    for I := 0 to LServices.size - 1 do
    begin
      LService := TJBluetoothGattService.Wrap(LServices.get(I));
      if JStringToString(LService.getUuid.toString).Equals(AServiceUUID) then
      begin
        Result := LService;
        Break;
      end;
    end;
  end;
end;

function TPlatformBluetoothDevice.GetCharacteristicValue(const ACharacteristic: JBluetoothGattCharacteristic): TBytes;
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

procedure TPlatformBluetoothDevice.CharacteristicChanged(const AGatt: JBluetoothGatt; const ACharacteristic: JBluetoothGattCharacteristic);
begin
  DoCharacteristicRead(JStringToString(ACharacteristic.getUuid.toString), GetCharacteristicValue(ACharacteristic));
end;

procedure TPlatformBluetoothDevice.CharacteristicRead(const AGatt: JBluetoothGatt; const ACharacteristic: JBluetoothGattCharacteristic; const AStatus: Integer);
begin
  // ACharacteristic.
  if AStatus = TJBluetoothGatt.JavaClass.GATT_SUCCESS then
    CharacteristicChanged(AGatt, ACharacteristic);
  // else did not succeed - duh
end;

procedure TPlatformBluetoothDevice.CharacteristicWrite(const AGatt: JBluetoothGatt; const ACharacteristic: JBluetoothGattCharacteristic; const AStatus: Integer);
begin
  //
end;

procedure TPlatformBluetoothDevice.ConnectionStateChange(const AGatt: JBluetoothGatt; const AStatus: Integer; const ANewState: Integer);
begin
  if ANewState = TJBluetoothProfile.JavaClass.STATE_CONNECTED then
  begin
    TOSLog.d('Connected');
    FIsConnected := True;
  end
  else if ANewState = TJBluetoothProfile.JavaClass.STATE_DISCONNECTED then
  begin
    TOSLog.d('Disconnected');
    FIsConnected := False;
  end;
end;

procedure TPlatformBluetoothDevice.DescriptorRead(const AGatt: JBluetoothGatt; const ADescriptor: JBluetoothGattDescriptor; const AStatus: Integer);
begin
  //
end;

procedure TPlatformBluetoothDevice.DescriptorWrite(const AGatt: JBluetoothGatt; const ADescriptor: JBluetoothGattDescriptor; const AStatus: Integer);
begin
  //
end;

procedure TPlatformBluetoothDevice.ReadCharacteristic(const AServiceUUID, ACharacteristicUUID: string);
var
  LCharacteristic: JBluetoothGattCharacteristic;
begin
  LCharacteristic := GetCharacteristic(AServiceUUID, ACharacteristicUUID);
  if LCharacteristic <> nil then
    FBluetoothGatt.readCharacteristic(LCharacteristic);
end;

procedure TPlatformBluetoothDevice.ReadRemoteRssi(const AGatt: JBluetoothGatt; const ARssi: Integer; const AStatus: Integer);
begin
  if AStatus = TJBluetoothGatt.JavaClass.GATT_SUCCESS then
    DoRemoteRssi(ARssi);
end;

procedure TPlatformBluetoothDevice.ReliableWriteCompleted(const AGatt: JBluetoothGatt; const AStatus: Integer);
begin

end;

procedure TPlatformBluetoothDevice.ServicesDiscovered(const AGatt: JBluetoothGatt; const AStatus: Integer);
begin
  TOSLog.d('TPlatformBluetoothDevice.ServicesDiscovered');
  DoServicesDiscovered;
end;

end.

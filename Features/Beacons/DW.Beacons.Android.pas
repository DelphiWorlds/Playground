unit DW.Beacons.Android;

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
  System.Generics.Collections, System.SysUtils,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Bluetooth, Androidapi.JNIBridge,
  DW.Beacons;

type
  // Move out to general Android bluetooth unit?
  JDWScanCallback = interface;
  JDWScanCallbackDelegate = interface;

  JDWScanCallbackClass = interface(JScanCallbackClass)
    ['{B07B8889-6ADE-4F00-B825-8037C3EDB6E8}']
    {class} function init(delegate: JDWScanCallbackDelegate): JDWScanCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/le/DWScanCallback')]
  JDWScanCallback = interface(JScanCallback)
    ['{B4A61D43-EFD7-4FC7-8EF2-F6C825234E19}']
    procedure onBatchScanResults(results: JList); cdecl;
    procedure onScanFailed(errorCode: Integer); cdecl;
    procedure onScanResult(callbackType: Integer; result: Jle_ScanResult); cdecl;
  end;
  TJDWScanCallback = class(TJavaGenericImport<JDWScanCallbackClass, JDWScanCallback>) end;

  JDWScanCallbackDelegateClass = interface(IJavaClass)
    ['{6FE79FAD-0366-40B9-A896-F38CB6917528}']
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/le/DWScanCallbackDelegate')]
  JDWScanCallbackDelegate = interface(IJavaInstance)
    ['{FD3CB01F-1079-4738-84C1-99DEEB711095}']
    procedure onBatchScanResults(results: JList); cdecl;
    procedure onScanFailed(errorCode: Integer); cdecl;
    procedure onScanResult(callbackType: Integer; result: Jle_ScanResult); cdecl;
  end;
  TJDWScanCallbackDelegate = class(TJavaGenericImport<JDWScanCallbackDelegateClass, JDWScanCallbackDelegate>) end;

  TPlatformBeacons = class;

  TBeaconsScanCallbackDelegate = class(TJavaLocal, JDWScanCallbackDelegate)
  private
    FCallback: JScanCallback;
    FPlatformBeacons: TPlatformBeacons;
  public
    { JDWScanCallbackDelegate }
    procedure onBatchScanResults(results: JList); cdecl;
    procedure onScanFailed(errorCode: Integer); cdecl;
    procedure onScanResult(callbackType: Integer; result: Jle_ScanResult); cdecl;
  public
    constructor Create(const APlatformBeacons: TPlatformBeacons);
    property Callback: JScanCallback read FCallback;
  end;

  TPlatformBeacons = class(TCustomPlatformBeacons)
  private
    FBluetoothManager: JBluetoothManager;
    FScanCallbackDelegate: TBeaconsScanCallbackDelegate;
    FScanTimer: JRunnable;
    function GetFilters: JList;
    function GetScanSettings: JScanSettings;
    procedure InternalScan;
    procedure InternalScanWithIntent;
    procedure ScanTimeExpired;
    procedure ScanWithPermission(const AScanMethod: TProc);
  protected
    procedure HandleIntent(const AIntentID: Pointer); override;
    function HasBluetoothPermission: Boolean; override;
    function HasLocationPermission: Boolean;
    function IsBluetoothEnabled: Boolean; override;
    procedure ScanFailed(const AErrorCode: Integer);
    procedure Scan; override;
    procedure ScanResult(const AScanResult: Jle_ScanResult);
    procedure ScanWithIntent; override;
  public
    constructor Create(const ABeacons: TBeacons); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Permissions, System.Classes,
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI,
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

{ TBeaconsScanCallbackDelegate }

constructor TBeaconsScanCallbackDelegate.Create(const APlatformBeacons: TPlatformBeacons);
begin
  inherited Create;
  FPlatformBeacons := APlatformBeacons;
  FCallback := TJDWScanCallback.JavaClass.init(Self);
end;

procedure TBeaconsScanCallbackDelegate.onBatchScanResults(results: JList);
begin
  // Nothing?
end;

procedure TBeaconsScanCallbackDelegate.onScanFailed(errorCode: Integer);
begin
  FPlatformBeacons.ScanFailed(errorCode);
end;

procedure TBeaconsScanCallbackDelegate.onScanResult(callbackType: Integer; result: Jle_ScanResult);
begin
  FPlatformBeacons.ScanResult(result);
end;

{ TPlatformBeacons }

constructor TPlatformBeacons.Create(const ABeacons: TBeacons);
var
  LService: JObject;
begin
  inherited;
  LService := TAndroidHelper.Context.getApplicationContext.getSystemService(TJContext.JavaClass.BLUETOOTH_SERVICE);
  FBluetoothManager := TJBluetoothManager.Wrap(LService);
  FScanCallbackDelegate := TBeaconsScanCallbackDelegate.Create(Self);
end;

destructor TPlatformBeacons.Destroy;
begin
  FScanTimer := nil;
  FBluetoothManager := nil;
  FScanCallbackDelegate.Free;
  FScanCallbackDelegate := nil;
  inherited;
end;

procedure TPlatformBeacons.ScanFailed(const AErrorCode: Integer);
begin
  //!!!!!
end;

procedure TPlatformBeacons.ScanResult(const AScanResult: Jle_ScanResult);
var
  LDeviceAddress: string;
  LCustomDevice: TCustomBluetoothLEDevice;
  LDevice: TPlatformBluetoothLEDevice;
  LIsNew: Boolean;
  LBluetoothDevice: JBluetoothDevice;
  LUuids: JList;
  I: Integer;
  LGUID: TGUID;
begin
  // TOSLog.d('TPlatformBeacons.ScanResult');
  LIsNew := False;
  LBluetoothDevice := AScanResult.getDevice;
  LDeviceAddress := JStringToString(LBluetoothDevice.getAddress);
  // TOSLog.d('> Device Address: %s', [LDeviceAddress]);
  if not Devices.TryGetValue(LDeviceAddress, LCustomDevice) then
  begin
    LDevice := TPlatformBluetoothLEDevice.Create;
    LDevice.Address := LDeviceAddress;
    LDevice.Name := JStringToString(LBluetoothDevice.getName);
    if not LDevice.Name.IsEmpty then
      TOSLog.d('> Device Name: %s', [LDevice.Name]);
    LDevice.DeviceType := LBluetoothDevice.getType;
    LUuids := AScanResult.getScanRecord.getServiceUuids;
    if LUuids <> nil then
    begin
      for I := 0 to LUuids.size - 1 do
      begin
        LGUID := JParcelUuidToGUID(TJParcelUuid.Wrap(LUuids.get(I)));
        TOSLog.d('> Service UUID: %s', [LGUID.ToString]);
        LDevice.ServiceUUIDs := LDevice.ServiceUUIDs + [LGUID];
      end;
    end;
    LDevice.BluetoothDevice := LBluetoothDevice;
    Devices.AddOrSetValue(LDevice.Address, LDevice);
    LIsNew := True;
  end
  else
    LDevice := TPlatformBluetoothLEDevice(LCustomDevice);
  LDevice.RSSI := AScanResult.getRssi;
  LDevice.TxPower := AScanResult.getTxPower;
  // LDevice.FAdvertisedData := ScanRecordToTScanResponse(result.getScanRecord.getBytes, LDevice.FAdvertisedData);
  if LIsNew then
    DiscoveredDevice(LDevice);
  // TOSLog.d('-TPlatformBeacons.ScanResult');
end;

procedure TPlatformBeacons.ScanTimeExpired;
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
function TPlatformBeacons.GetFilters: JList;
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

function TPlatformBeacons.GetScanSettings: JScanSettings;
begin
  Result := TJScanSettings_Builder.JavaClass.init
    .setScanMode(TJScanSettings.JavaClass.SCAN_MODE_LOW_POWER)
    .build;
end;

// http://www.davidgyoungtech.com/2017/08/07/beacon-detection-with-android-8
procedure TPlatformBeacons.HandleIntent(const AIntentID: Pointer);
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

function TPlatformBeacons.HasBluetoothPermission: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(cPermissionBluetooth);
end;

function TPlatformBeacons.HasLocationPermission: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(cPermissionAccessFineLocation);
end;

function TPlatformBeacons.IsBluetoothEnabled: Boolean;
begin
  Result := HasBluetoothPermission and TJBluetoothAdapter.JavaClass.getDefaultAdapter.isEnabled;
end;

procedure TPlatformBeacons.InternalScan;
var
  LScanner: JBluetoothLeScannerEx;
begin
  LScanner := TJBluetoothLeScannerEx.Wrap(FBluetoothManager.getAdapter.getBluetoothLeScanner);
  LScanner.startScan(GetFilters, GetScanSettings, FScanCallbackDelegate.Callback);
  // LScanner.startScan(nil, GetScanSettings, FScanCallbackDelegate.Callback);
  FScanTimer := TScanRunnable.Create(ScanTimeExpired, 30000); // Use a property
end;

procedure TPlatformBeacons.Scan;
begin
  ScanWithPermission(InternalScan);
end;

procedure TPlatformBeacons.InternalScanWithIntent;
var
  LScanner: JBluetoothLeScannerEx;
  LIntent: JIntent;
  LPendingIntent: JPendingIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString('com.delphiworlds.kastri.DWBeaconsReceiver.ACTION_FOUND'));
  LIntent.setClassName(TAndroidHelper.Context, StringToJString('com.delphiworlds.kastri.DWBeaconsReceiver'));
  // LIntent.putExtra(StringToJString('o-scan'), True); // <--- No idea what this is
  LPendingIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LIntent, TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
  LScanner := TJBluetoothLeScannerEx.Wrap(FBluetoothManager.getAdapter.getBluetoothLeScanner);
  LScanner.startScan(GetFilters, GetScanSettings, LPendingIntent);
end;

procedure TPlatformBeacons.ScanWithIntent;
begin
  ScanWithPermission(InternalScanWithIntent);
end;

procedure TPlatformBeacons.ScanWithPermission(const AScanMethod: TProc);
begin
  if IsBluetoothEnabled then
  begin
    if not HasLocationPermission then
    begin
      PermissionsService.RequestPermissions([cPermissionAccessFineLocation],
        procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
        begin
          if AGrantResults.AreAllGranted then
            AScanMethod;
        end
      );
    end
    else
      AScanMethod;
  end;
end;

end.

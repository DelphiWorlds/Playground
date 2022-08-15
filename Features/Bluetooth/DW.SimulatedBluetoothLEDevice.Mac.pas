unit DW.SimulatedBluetoothLEDevice.Mac;

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
  // macOS
  Macapi.Bluetooth, Macapi.ObjectiveC,
  {$IF Defined(IOS)}
  iOSapi.Foundation,
  {$ELSE}
  Macapi.Foundation,
  {$ENDIF}
  // DW
  DW.SimulatedBluetoothLEDevice;

type
  TNSObjects = TArray<NSObject>;

  CBUUIDEx = interface(CBUUID)
    ['{640038CE-482F-4307-B2FF-6F21FB3BEA3F}']
    function UUIDString: NSString; cdecl;
  end;
  TCBUUIDEx = class(TOCGenericImport<CBUUIDClass, CBUUIDEx>) end;

  TPlatformSimulatedBluetoothLEDevice = class;

  TPeripheralManagerDelegate = class(TOCLocal, CBPeripheralManagerDelegate)
  private
    FDevice: TPlatformSimulatedBluetoothLEDevice;
  public
    { CBPeripheralManagerDelegate }
    procedure peripheralManagerDidUpdateState(peripheral: CBPeripheralManager); cdecl;
    [MethodName('peripheralManager:willRestoreState:')]
    procedure peripheralManagerWillRestoreState(peripheral: CBPeripheralManager; dict: NSDictionary); cdecl;
    [MethodName('peripheralManagerDidStartAdvertising:error:')]
    procedure peripheralManagerDidStartAdvertising(peripheral: CBPeripheralManager; error: NSError); cdecl;
    [MethodName('peripheralManager:didAddService:error:')]
    procedure peripheralManagerDidAddService(peripheral: CBPeripheral; service: CBService; error: NSError); cdecl;
    [MethodName('peripheralManager:central:didSubscribeToCharacteristic:')]
    procedure peripheralManagerDidSubscribetoCharacteristic(peripheral: CBPeripheral; central: CBCentral;
      characteristic: CBCharacteristic); cdecl;
    [MethodName('peripheralManager:central:didUnsubscribeFromCharacteristic:')]
    procedure peripheralManagerDidUnsubscribeFromCharacteristic(peripheral: CBPeripheralManager; central: CBCentral;
      characteristic: CBCharacteristic); cdecl;
    [MethodName('peripheralManager:didReceiveReadRequest:')]
    procedure peripheralManagerDidReceiveReadRequests(peripheral: CBPeripheralManager; request: CBATTRequest); cdecl;
    [MethodName('peripheralManager:didReceiveWriteRequests:')]
    procedure peripheralManagerDidReceiveWriteRequests(peripheral: CBPeripheralManager; requests: NSArray); cdecl;
    procedure peripheralManagerIsReadyToUpdateSubscribers(peripheral: CBPeripheralManager); cdecl;
  public
    constructor Create(const ADevice: TPlatformSimulatedBluetoothLEDevice);
  end;

  TPlatformSimulatedBluetoothLEDevice = class(TCustomPlatformSimulatedBluetoothLEDevice)
  private
    FDelegate: TPeripheralManagerDelegate;
    FPeripheralManager: CBPeripheralManager;
    FServices: TArray<CBMutableService>;
    FServicesAdded: TArray<CBService>;
    FServicesIndex: Integer;
    procedure AddServerService;
    procedure StartServices;
  protected
    procedure DidAddService(const AService: CBService; const AError: NSError);
    procedure DidStartAdvertising(const APeripheral: CBPeripheralManager; const AError: NSError);
    procedure DidSubscribetoCharacteristic(const APeripheral: CBPeripheral; const ACentral: CBCentral;
      const ACharacteristic: CBCharacteristic); virtual;
    procedure DidUpdateState(const APeripheral: CBPeripheralManager);
    function GetCharacteristicValue(const ACharacteristic: CBCharacteristic): NSData; virtual;
    function IsSameCharacteristic(const ACharacteristic1, ACharacteristic2: CBCharacteristic): Boolean;
  protected
    procedure AddService(const AUUID: string; const ACharacteristics: TNSObjects);
    procedure ActiveChanging(const Value: Boolean); override;
    function CreateCharacteristic(const AUUID, AName: string; const AProps: CBCharacteristicProperties): CBMutableCharacteristic;
    function CreateNameDescriptor(const AName: string): Pointer;
    procedure StartAdvertising; override;
    procedure StartServer; override;
    procedure StopAdvertising; override;
    procedure StopServer; override;
    property PeripheralManager: CBPeripheralManager read FPeripheralManager;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  // macOS
  Macapi.Helpers,
  // iOS
  {$IF Defined(IOS)}
  iOSapi.Helpers,
  {$ENDIF}
  // DW
  DW.OSLog,
  DW.Macapi.Helpers;

function GetDeviceName: NSString;
begin
  {$IF Defined(IOS)}
  Result := TiOSHelper.CurrentDevice.name;
  {$ELSE}
  Result := TNSHost.Wrap(TNSHost.OCClass.currentHost).localizedName;
  {$ENDIF}
end;

function CBUUIDCharacteristicUserDescriptionString: NSString;
begin
  Result := CocoaNSStringConst(libCoreBluetooth, 'CBUUIDCharacteristicUserDescriptionString');
end;

{ TPeripheralManagerDelegate }

constructor TPeripheralManagerDelegate.Create(const ADevice: TPlatformSimulatedBluetoothLEDevice);
begin
  inherited Create;
  FDevice := ADevice;
end;

procedure TPeripheralManagerDelegate.peripheralManagerDidAddService(peripheral: CBPeripheral; service: CBService; error: NSError);
begin
  FDevice.DidAddService(service, error);
end;

procedure TPeripheralManagerDelegate.peripheralManagerDidReceiveReadRequests(peripheral: CBPeripheralManager; request: CBATTRequest);
begin
  request.setValue(FDevice.GetCharacteristicValue(request.characteristic));
  peripheral.respondToRequest(request, CBATTErrorSuccess);
end;

procedure TPeripheralManagerDelegate.peripheralManagerDidReceiveWriteRequests(peripheral: CBPeripheralManager; requests: NSArray);
begin

end;

procedure TPeripheralManagerDelegate.peripheralManagerDidStartAdvertising(peripheral: CBPeripheralManager; error: NSError);
begin
  FDevice.DidStartAdvertising(peripheral, error);
end;

procedure TPeripheralManagerDelegate.peripheralManagerDidSubscribetoCharacteristic(peripheral: CBPeripheral; central: CBCentral;
  characteristic: CBCharacteristic);
begin
  FDevice.DidSubscribetoCharacteristic(peripheral, central, characteristic);
end;

procedure TPeripheralManagerDelegate.peripheralManagerDidUnsubscribeFromCharacteristic(peripheral: CBPeripheralManager; central: CBCentral;
  characteristic: CBCharacteristic);
begin

end;

procedure TPeripheralManagerDelegate.peripheralManagerDidUpdateState(peripheral: CBPeripheralManager);
begin
  FDevice.DidUpdateState(peripheral);
end;

procedure TPeripheralManagerDelegate.peripheralManagerIsReadyToUpdateSubscribers(peripheral: CBPeripheralManager);
begin

end;

procedure TPeripheralManagerDelegate.peripheralManagerWillRestoreState(peripheral: CBPeripheralManager; dict: NSDictionary);
begin

end;

{ TPlatformSimulatedBluetoothLEDevice }

constructor TPlatformSimulatedBluetoothLEDevice.Create;
begin
  inherited;
  FDelegate := TPeripheralManagerDelegate.Create(Self);
end;

destructor TPlatformSimulatedBluetoothLEDevice.Destroy;
begin
  //
  inherited;
end;

function TPlatformSimulatedBluetoothLEDevice.CreateCharacteristic(const AUUID, AName: string;
  const AProps: CBCharacteristicProperties): CBMutableCharacteristic;
var
  LUUID: CBUUID;
  LPointer: Pointer;
begin
  LUUID := TCBUUID.OCClass.UUIDWithString(StrToNSStr(AUUID));
  LPointer := TCBMutableCharacteristic.Alloc.initWithType(LUUID, AProps, nil, CBAttributePermissionsReadable);
  Result := TCBMutableCharacteristic.Wrap(LPointer);
  Result.setDescriptors(TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(CreateNameDescriptor(AName))));
end;

function TPlatformSimulatedBluetoothLEDevice.CreateNameDescriptor(const AName: string): Pointer;
var
  LUUID: CBUUID;
begin
  LUUID := TCBUUID.OCClass.UUIDWithString(CBUUIDCharacteristicUserDescriptionString);
  Result := TCBMutableDescriptor.Alloc.initWithType(LUUID, StringToID(AName));
end;

procedure TPlatformSimulatedBluetoothLEDevice.AddService(const AUUID: string; const ACharacteristics: TNSObjects);
var
  LUUID: CBUUID;
  LService: CBMutableService;
begin
  LUUID := TCBUUID.OCClass.UUIDWithString(StrToNSStr(AUUID));
  LService := TCBMutableService.Alloc;
  LService := TCBMutableService.Wrap(LService.initWithType(LUUID, True));
  LService.setCharacteristics(TNSArrayHelper.FromNSObjects(ACharacteristics));
  FServices := FServices + [LService];
end;

procedure TPlatformSimulatedBluetoothLEDevice.DidAddService(const AService: CBService; const AError: NSError);
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.DidAddService');
  if (AError = nil) or (AError.code = 0) then
    FServicesAdded := FServicesAdded + [AService]
  else
    TOSLog.d(NSErrorToStr(AError));
  Inc(FServicesIndex);
  AddServerService;
end;

procedure TPlatformSimulatedBluetoothLEDevice.StartAdvertising;
var
  LAdvertisingData: NSMutableDictionary;
  LUUIDs: NSMutableArray;
  LService: CBService;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StartAdvertising');
  LUUIDs := TNSMutableArray.Create;
  for LService in FServicesAdded do
    LUUIDs.addObject(NSObjectToID(LService.UUID));
  LAdvertisingData := TNSMutableDictionary.Create;
  LAdvertisingData.setValueForKey(NSObjectToID(GetDeviceName), CBAdvertisementDataLocalNameKey);
  LAdvertisingData.setValueForKey(NSObjectToID(LUUIDs), CBAdvertisementDataServiceUUIDsKey);
  FPeripheralManager.startAdvertising(LAdvertisingData);
end;

procedure TPlatformSimulatedBluetoothLEDevice.DidStartAdvertising(const APeripheral: CBPeripheralManager; const AError: NSError);
begin
  FIsAdvertising := (AError = nil) or (AError.code = 0);
  if FIsAdvertising then
    TOSLog.d('TPlatformSimulatedBluetoothLEDevice.DidStartAdvertising')
  else
    TOSLog.d('> %s', [NSErrorToStr(AError)]);
end;

procedure TPlatformSimulatedBluetoothLEDevice.DidSubscribetoCharacteristic(const APeripheral: CBPeripheral; const ACentral: CBCentral;
  const ACharacteristic: CBCharacteristic);
begin
  //
end;

procedure TPlatformSimulatedBluetoothLEDevice.DidUpdateState(const APeripheral: CBPeripheralManager);
begin
  FIsActive := FPeripheralManager.state = CBPeripheralManagerStatePoweredOn;
  if FIsActive then
    StartServices;
end;

function TPlatformSimulatedBluetoothLEDevice.GetCharacteristicValue(const ACharacteristic: CBCharacteristic): NSData;
begin
  Result := nil;
end;

function TPlatformSimulatedBluetoothLEDevice.IsSameCharacteristic(const ACharacteristic1, ACharacteristic2: CBCharacteristic): Boolean;
var
  LUUIDString1, LUUIDString2: NSString;
begin
  LUUIDString1 := TCBUUIDEx.Wrap(NSObjectToID(ACharacteristic1.UUID)).UUIDString;
  LUUIDString2 := TCBUUIDEx.Wrap(NSObjectToID(ACharacteristic2.UUID)).UUIDString;
  Result := LUUIDString1.isEqualToString(LUUIDString2);
end;

procedure TPlatformSimulatedBluetoothLEDevice.StartServices;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StartServices');
  if FIsActive then
  begin
    FServicesIndex := 0;
    FServicesAdded := [];
    AddServerService;
  end;
end;

procedure TPlatformSimulatedBluetoothLEDevice.AddServerService;
begin
  if FServicesIndex = Length(FServices) then
  begin
    FIsActive := True;
    ServicesAdded;
  end
  else
    FPeripheralManager.addService(FServices[FServicesIndex]);
end;

procedure TPlatformSimulatedBluetoothLEDevice.StopAdvertising;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StopAdvertising');
  if FPeripheralManager <> nil then
    FPeripheralManager.stopAdvertising;
end;

procedure TPlatformSimulatedBluetoothLEDevice.ActiveChanging(const Value: Boolean);
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.ActiveChanging');
  if Value then
    StartServer
  else
    StopServer;
end;

procedure TPlatformSimulatedBluetoothLEDevice.StartServer;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StartServer');
  FPeripheralManager := TCBPeripheralManager.Alloc;
  FPeripheralManager := TCBPeripheralManager.Wrap(FPeripheralManager.initWithDelegate(FDelegate.GetObjectID, 0));
end;

procedure TPlatformSimulatedBluetoothLEDevice.StopServer;
begin
  TOSLog.d('TPlatformSimulatedBluetoothLEDevice.StopServer');
  FPeripheralManager := nil;
  FIsActive := False;
end;

end.

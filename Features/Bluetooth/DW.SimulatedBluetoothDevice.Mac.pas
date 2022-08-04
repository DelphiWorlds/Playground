unit DW.SimulatedBluetoothDevice.Mac;

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
  DW.SimulatedBluetoothDevice;

type
  TNSObjects = TArray<NSObject>;

  CBUUIDEx = interface(CBUUID)
    ['{640038CE-482F-4307-B2FF-6F21FB3BEA3F}']
    function UUIDString: NSString; cdecl;
  end;
  TCBUUIDEx = class(TOCGenericImport<CBUUIDClass, CBUUIDEx>) end;

  TPlatformSimulatedBluetoothDevice = class;

  TPeripheralManagerDelegate = class(TOCLocal, CBPeripheralManagerDelegate)
  private
    FDevice: TPlatformSimulatedBluetoothDevice;
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
    constructor Create(const ADevice: TPlatformSimulatedBluetoothDevice);
  end;

  TBluetoothServices = TArray<CBMutableService>;

  TPlatformSimulatedBluetoothDevice = class(TCustomPlatformSimulatedBluetoothDevice)
  private
    FDelegate: TPeripheralManagerDelegate;
    FPeripheralManager: CBPeripheralManager;
    FServices: TBluetoothServices;
    FServicesIndex: Integer;
    procedure AddServerService;
    procedure StartServices;
  protected
    procedure DidAddService(const AService: CBService; const AError: NSError);
    procedure DidStartAdvertising(const APeripheral: CBPeripheralManager; const AError: NSError);
    procedure DidUpdateState(const APeripheral: CBPeripheralManager);
  protected
    procedure AddService(const AUUID: string; const ACharacteristics: TNSObjects);
    procedure ActiveChanging(const Value: Boolean); override;
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

{ TPeripheralManagerDelegate }

constructor TPeripheralManagerDelegate.Create(const ADevice: TPlatformSimulatedBluetoothDevice);
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
  //
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
  //
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

{ TPlatformSimulatedBluetoothDevice }

constructor TPlatformSimulatedBluetoothDevice.Create;
begin
  inherited;
  FDelegate := TPeripheralManagerDelegate.Create(Self);
end;

destructor TPlatformSimulatedBluetoothDevice.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformSimulatedBluetoothDevice.AddService(const AUUID: string; const ACharacteristics: TNSObjects);
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

procedure TPlatformSimulatedBluetoothDevice.DidAddService(const AService: CBService; const AError: NSError);
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.DidAddService');
  if AError <> nil then
    TOSLog.d(NSErrorToStr(AError));
  AddServerService;
end;

procedure TPlatformSimulatedBluetoothDevice.StartAdvertising;
var
  LAdvertisingData: NSMutableDictionary;
  LUUIDs: NSMutableArray;
  LService: CBMutableService;
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.StartAdvertising');
  LUUIDs := TNSMutableArray.Create;
  for LService in FServices do
    LUUIDs.addObject(NSObjectToID(LService.UUID));
  LAdvertisingData := TNSMutableDictionary.Create;
  LAdvertisingData.setValueForKey(NSObjectToID(GetDeviceName), CBAdvertisementDataLocalNameKey);
  LAdvertisingData.setValueForKey(NSObjectToID(LUUIDs), CBAdvertisementDataServiceUUIDsKey);
  FPeripheralManager.startAdvertising(LAdvertisingData);
end;

procedure TPlatformSimulatedBluetoothDevice.DidStartAdvertising(const APeripheral: CBPeripheralManager; const AError: NSError);
begin
  FIsAdvertising := (AError = nil) or (AError.code = 0);
  if FIsAdvertising then
    TOSLog.d('TPlatformSimulatedBluetoothDevice.DidStartAdvertising')
  else
    TOSLog.d('> %s', [NSErrorToStr(AError)]);
end;

procedure TPlatformSimulatedBluetoothDevice.DidUpdateState(const APeripheral: CBPeripheralManager);
begin
  FIsActive := FPeripheralManager.state = CBPeripheralManagerStatePoweredOn;
  if FIsActive then
    StartServices;
end;

procedure TPlatformSimulatedBluetoothDevice.StartServices;
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.StartServices');
  if FIsActive then
  begin
    FServicesIndex := 0;
    AddServerService;
  end;
end;

procedure TPlatformSimulatedBluetoothDevice.AddServerService;
begin
  if FServicesIndex = Length(FServices) - 1 then
  begin
    FIsActive := True;
    ServicesAdded;
  end
  else
    FPeripheralManager.addService(FServices[FServicesIndex]);
end;

procedure TPlatformSimulatedBluetoothDevice.StopAdvertising;
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.StopAdvertising');
  if FPeripheralManager <> nil then
    FPeripheralManager.stopAdvertising;
end;

procedure TPlatformSimulatedBluetoothDevice.ActiveChanging(const Value: Boolean);
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.ActiveChanging');
  if Value then
    StartServer
  else
    StopServer;
end;

procedure TPlatformSimulatedBluetoothDevice.StartServer;
begin
  TOSLog.d('TPlatformSimulatedBluetoothDevice.StartServer');
  FPeripheralManager := TCBPeripheralManager.Alloc;
  FPeripheralManager := TCBPeripheralManager.Wrap(FPeripheralManager.initWithDelegate(FDelegate.GetObjectID, 0));
end;

procedure TPlatformSimulatedBluetoothDevice.StopServer;
begin
  TOSLog.d('TBluetoothGattServer.StopServer');
  FPeripheralManager := nil;
  FIsActive := False;
end;

end.

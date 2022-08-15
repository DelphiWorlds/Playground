unit DW.Bluetooth.Mac;

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
  Macapi.ObjectiveC, Macapi.Bluetooth,
  {$IF Defined(OSX)}
  Macapi.Foundation, Macapi.CocoaTypes,
  {$ENDIF}
  {$IF Defined(IOS)}
  iOSapi.Foundation, iOSapi.CocoaTypes,
  {$ENDIF}
  DW.Bluetooth, DW.OSTimer;

type
  CBPeer = interface;
  CBL2CAPChannel = interface;

  CBConnectionEvent = NSInteger;
  CBL2CAPPSM = UInt16;

  CBPeerClass = interface(NSObjectClass)
    ['{238D6F2F-1CCF-4BF4-95E2-88181CD47B51}']
  end;

  CBPeer = interface(NSObject)
    ['{D2D3CBDD-75B6-42DB-B517-4F98FB5CE9D9}']
    function identifier: NSUUID; cdecl;
  end;
  TCBPeer = class(TOCGenericImport<CBPeerClass, CBPeer>) end;

  CBL2CAPChannelClass = interface(NSObjectClass)
    ['{2DB7FD22-85AD-463C-ADAC-B48CF2113CD7}']
  end;

  CBL2CAPChannel = interface(NSObject)
    ['{3F6E3E5E-CF38-410C-8A73-E7D56BC66947}']
    function inputStream: NSInputStream; cdecl;
    function outputStream: NSOutputStream; cdecl;
    function peer: CBPeer; cdecl;
    function PSM: CBL2CAPPSM; cdecl;
  end;
  TCBL2CAPChannel = class(TOCGenericImport<CBL2CAPChannelClass, CBL2CAPChannel>) end;

  CBCentralManagerDelegate = interface(IObjectiveC)
    ['{3A116076-D6AB-4437-BD82-0326D057DBC5}']
    [MethodName('centralManager:connectionEventDidOccur:forPeripheral:')]
    procedure centralManagerConnectionEventDidOccur(central: CBCentralManager; connectionEventDidOccur: CBConnectionEvent; forPeripheral: CBPeripheral); cdecl;
    [MethodName('centralManager:didConnectPeripheral:')]
    procedure centralManagerDidConnectPeripheral(central: CBCentralManager; didConnectPeripheral: CBPeripheral); cdecl;
    [MethodName('centralManager:didDisconnectPeripheral:error:')]
    procedure centralManagerDidDisconnectPeripheral(central: CBCentralManager; didDisconnectPeripheral: CBPeripheral; error: NSError); cdecl;
    [MethodName('centralManager:didDiscoverPeripheral:advertisementData:RSSI:')]
    procedure centralManagerDidDiscoverPeripheral(central: CBCentralManager; didDiscoverPeripheral: CBPeripheral; advertisementData: NSDictionary; RSSI: NSNumber); cdecl;
    [MethodName('centralManager:didFailToConnectPeripheral:error:')]
    procedure centralManagerDidFailToConnectPeripheral(central: CBCentralManager; didFailToConnectPeripheral: CBPeripheral; error: NSError); cdecl;
    [MethodName('centralManager:didUpdateANCSAuthorizationForPeripheral:')]
    procedure centralManagerDidUpdateANCSAuthorizationForPeripheral(central: CBCentralManager; didUpdateANCSAuthorizationForPeripheral: CBPeripheral); cdecl;
    procedure centralManagerDidUpdateState(central: CBCentralManager); cdecl;
    [MethodName('centralManager:willRestoreState:')]
    procedure centralManagerWillRestoreState(central: CBCentralManager; willRestoreState: NSDictionary); cdecl;
  end;

  CBPeripheralDelegate = interface(IObjectiveC)
    ['{F48AEA9A-19C4-405F-81BF-EACC7AED8D03}']
    [MethodName('peripheral:didDiscoverCharacteristicsForService:error:')]
    procedure peripheralDidDiscoverCharacteristicsForService(peripheral: CBPeripheral; didDiscoverCharacteristicsForService: CBService;
      error: NSError); cdecl;
    [MethodName('peripheral:didDiscoverDescriptorsForCharacteristic:error:')]
    procedure peripheralDidDiscoverDescriptorsForCharacteristic(peripheral: CBPeripheral; didDiscoverDescriptorsForCharacteristic: CBCharacteristic;
      error: NSError); cdecl;
    [MethodName('peripheral:didDiscoverIncludedServicesForService:error:')]
    procedure peripheralDidDiscoverIncludedServicesForService(peripheral: CBPeripheral; didDiscoverIncludedServicesForService: CBService;
      error: NSError); cdecl;
    [MethodName('peripheral:didDiscoverServices:')]
    procedure peripheralDidDiscoverServices(peripheral: CBPeripheral; didDiscoverServices: NSError); cdecl;
    [MethodName('peripheral:didModifyServices:')]
    procedure peripheralDidModifyServices(peripheral: CBPeripheral; didModifyServices: NSArray); cdecl;
    [MethodName('peripheral:didOpenL2CAPChannel:error:')]
    procedure peripheralDidOpenL2CAPChannel(peripheral: CBPeripheral; didOpenL2CAPChannel: CBL2CAPChannel; error: NSError); cdecl;
    [MethodName('peripheral:didReadRSSI:error:')]
    procedure peripheralDidReadRSSI(peripheral: CBPeripheral; didReadRSSI: NSNumber; error: NSError); cdecl;
    procedure peripheralDidUpdateName(peripheral: CBPeripheral); cdecl;
    [MethodName('peripheral:didUpdateNotificationStateForCharacteristic:error:')]
    procedure peripheralDidUpdateNotificationStateForCharacteristic(peripheral: CBPeripheral;
      didUpdateNotificationStateForCharacteristic: CBCharacteristic; error: NSError); cdecl;
    procedure peripheralDidUpdateRSSI(peripheral: CBPeripheral; error: NSError); cdecl;
    [MethodName('peripheral:didUpdateValueForCharacteristic:error:')]
    procedure peripheralDidUpdateValueForCharacteristic(peripheral: CBPeripheral; didUpdateValueForCharacteristic: CBCharacteristic;
      error: NSError); cdecl;
    [MethodName('peripheral:didUpdateValueForDescriptor:error:')]
    procedure peripheralDidUpdateValueForDescriptor(peripheral: CBPeripheral; didUpdateValueForDescriptor: CBDescriptor; error: NSError); cdecl;
    [MethodName('peripheral:didWriteValueForCharacteristic:error:')]
    procedure peripheralDidWriteValueForCharacteristic(peripheral: CBPeripheral; didWriteValueForCharacteristic: CBCharacteristic;
      error: NSError); cdecl;
    [MethodName('peripheral:didWriteValueForDescriptor:error:')]
    procedure peripheralDidWriteValueForDescriptor(peripheral: CBPeripheral; didWriteValueForDescriptor: CBDescriptor; error: NSError); cdecl;
    procedure peripheralIsReadyToSendWriteWithoutResponse(peripheral: CBPeripheral); cdecl;
  end;

  TPlatformBluetoothScanner = class;

  TScannerCBCentralManagerDelegate = class(TOCLocal, CBCentralManagerDelegate)
  private
    FPlatformBluetoothScanner: TPlatformBluetoothScanner;
  public
    { CBCentralManagerDelegate }
    [MethodName('centralManager:connectionEventDidOccur:forPeripheral:')]
    procedure centralManagerConnectionEventDidOccur(central: CBCentralManager; connectionEventDidOccur: CBConnectionEvent;
      forPeripheral: CBPeripheral); cdecl;
    [MethodName('centralManager:didConnectPeripheral:')]
    procedure centralManagerDidConnectPeripheral(central: CBCentralManager; didConnectPeripheral: CBPeripheral); cdecl;
    [MethodName('centralManager:didDisconnectPeripheral:error:')]
    procedure centralManagerDidDisconnectPeripheral(central: CBCentralManager; didDisconnectPeripheral: CBPeripheral; error: NSError); cdecl;
    [MethodName('centralManager:didDiscoverPeripheral:advertisementData:RSSI:')]
    procedure centralManagerDidDiscoverPeripheral(central: CBCentralManager; didDiscoverPeripheral: CBPeripheral; advertisementData: NSDictionary;
      RSSI: NSNumber); cdecl;
    [MethodName('centralManager:didFailToConnectPeripheral:error:')]
    procedure centralManagerDidFailToConnectPeripheral(central: CBCentralManager; didFailToConnectPeripheral: CBPeripheral; error: NSError); cdecl;
    [MethodName('centralManager:didUpdateANCSAuthorizationForPeripheral:')]
    procedure centralManagerDidUpdateANCSAuthorizationForPeripheral(central: CBCentralManager;
      didUpdateANCSAuthorizationForPeripheral: CBPeripheral); cdecl;
    procedure centralManagerDidUpdateState(central: CBCentralManager); cdecl;
    [MethodName('centralManager:willRestoreState:')]
    procedure centralManagerWillRestoreState(central: CBCentralManager; willRestoreState: NSDictionary); cdecl;
  public
    constructor Create(const APlatformBluetoothScanner: TPlatformBluetoothScanner);
  end;

  TPlatformBluetoothScanner = class(TCustomPlatformBluetoothScanner)
  private
    FCentralManager: CBCentralManager;
    FCentralManagerDelegate: TScannerCBCentralManagerDelegate;
    FExpiryTimer: TOSTimer;
    FIsScanPending: Boolean;
    FIsStarting: Boolean;
    FScanTimer: TOSTimer;
    function IsScanning: Boolean;
    procedure ExpiryTimerIntervalHandler(Sender: TObject);
    procedure ScanTimerIntervalHandler(Sender: TObject);
    procedure ScanStop;
  protected
    procedure DidConnectPeripheral(const APeripheral: CBPeripheral);
    procedure DidDisconnectPeripheral(const APeripheral: CBPeripheral; const AError: NSError);
    procedure DidDiscoverPeripheral(const APeripheral: CBPeripheral; const AAdvertisementData: NSDictionary; const ARSSI: NSNumber);
    procedure DidUpdateState;
    function IsBluetoothEnabled: Boolean; override;
    procedure Scan; override;
    procedure Stop; override;
  public
    constructor Create(const ABluetoothScanner: TBluetoothScanner); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Macapi.Helpers,
  DW.OSLog,
  DW.Macapi.Helpers;

type
  CBUUIDEx = interface(CBUUID)
    ['{640038CE-482F-4307-B2FF-6F21FB3BEA3F}']
    function UUIDString: NSString; cdecl;
  end;
  TCBUUIDEx = class(TOCGenericImport<CBUUIDClass, CBUUIDEx>) end;

  CBCentralManagerEx = interface(CBCentralManager)
    ['{BF94D7A5-B665-4AD2-9527-5F0BB7710754}']
    function isScanning: Boolean; cdecl;
  end;
  TCBCentralManagerEx = class(TOCGenericImport<CBCentralManagerClass, CBCentralManagerEx>) end;

  TPlatformBluetoothLEDevice = class;

  TCBPeripheralDelegate = class(TOCLocal, CBPeripheralDelegate)
  private
    FPlatformBluetoothLEDevice: TPlatformBluetoothLEDevice;
  public
    { CBPeripheralDelegate }
    [MethodName('peripheral:didDiscoverCharacteristicsForService:error:')]
    procedure peripheralDidDiscoverCharacteristicsForService(peripheral: CBPeripheral; didDiscoverCharacteristicsForService: CBService;
      error: NSError); cdecl;
    [MethodName('peripheral:didDiscoverDescriptorsForCharacteristic:error:')]
    procedure peripheralDidDiscoverDescriptorsForCharacteristic(peripheral: CBPeripheral; didDiscoverDescriptorsForCharacteristic: CBCharacteristic;
      error: NSError); cdecl;
    [MethodName('peripheral:didDiscoverIncludedServicesForService:error:')]
    procedure peripheralDidDiscoverIncludedServicesForService(peripheral: CBPeripheral; didDiscoverIncludedServicesForService: CBService;
      error: NSError); cdecl;
    [MethodName('peripheral:didDiscoverServices:')]
    procedure peripheralDidDiscoverServices(peripheral: CBPeripheral; didDiscoverServices: NSError); cdecl;
    [MethodName('peripheral:didModifyServices:')]
    procedure peripheralDidModifyServices(peripheral: CBPeripheral; didModifyServices: NSArray); cdecl;
    [MethodName('peripheral:didOpenL2CAPChannel:error:')]
    procedure peripheralDidOpenL2CAPChannel(peripheral: CBPeripheral; didOpenL2CAPChannel: CBL2CAPChannel; error: NSError); cdecl;
    [MethodName('peripheral:didReadRSSI:error:')]
    procedure peripheralDidReadRSSI(peripheral: CBPeripheral; didReadRSSI: NSNumber; error: NSError); cdecl;
    procedure peripheralDidUpdateName(peripheral: CBPeripheral); cdecl;
    [MethodName('peripheral:didUpdateNotificationStateForCharacteristic:error:')]
    procedure peripheralDidUpdateNotificationStateForCharacteristic(peripheral: CBPeripheral;
      didUpdateNotificationStateForCharacteristic: CBCharacteristic; error: NSError); cdecl;
    procedure peripheralDidUpdateRSSI(peripheral: CBPeripheral; error: NSError); cdecl;
    [MethodName('peripheral:didUpdateValueForCharacteristic:error:')]
    procedure peripheralDidUpdateValueForCharacteristic(peripheral: CBPeripheral; didUpdateValueForCharacteristic: CBCharacteristic;
      error: NSError); cdecl;
    [MethodName('peripheral:didUpdateValueForDescriptor:error:')]
    procedure peripheralDidUpdateValueForDescriptor(peripheral: CBPeripheral; didUpdateValueForDescriptor: CBDescriptor; error: NSError); cdecl;
    [MethodName('peripheral:didWriteValueForCharacteristic:error:')]
    procedure peripheralDidWriteValueForCharacteristic(peripheral: CBPeripheral; didWriteValueForCharacteristic: CBCharacteristic;
      error: NSError); cdecl;
    [MethodName('peripheral:didWriteValueForDescriptor:error:')]
    procedure peripheralDidWriteValueForDescriptor(peripheral: CBPeripheral; didWriteValueForDescriptor: CBDescriptor; error: NSError); cdecl;
    procedure peripheralIsReadyToSendWriteWithoutResponse(peripheral: CBPeripheral); cdecl;
  public
    constructor Create(const APlatformBluetoothLEDevice: TPlatformBluetoothLEDevice);
  end;

  TDeviceCBCentralManagerDelegate = class(TOCLocal, CBCentralManagerDelegate)
  private
    FPlatformBluetoothDevice: TPlatformBluetoothLEDevice;
  public
    { CBCentralManagerDelegate }
    [MethodName('centralManager:connectionEventDidOccur:forPeripheral:')]
    procedure centralManagerConnectionEventDidOccur(central: CBCentralManager; connectionEventDidOccur: CBConnectionEvent;
      forPeripheral: CBPeripheral); cdecl;
    [MethodName('centralManager:didConnectPeripheral:')]
    procedure centralManagerDidConnectPeripheral(central: CBCentralManager; didConnectPeripheral: CBPeripheral); cdecl;
    [MethodName('centralManager:didDisconnectPeripheral:error:')]
    procedure centralManagerDidDisconnectPeripheral(central: CBCentralManager; didDisconnectPeripheral: CBPeripheral; error: NSError); cdecl;
    [MethodName('centralManager:didDiscoverPeripheral:advertisementData:RSSI:')]
    procedure centralManagerDidDiscoverPeripheral(central: CBCentralManager; didDiscoverPeripheral: CBPeripheral; advertisementData: NSDictionary;
      RSSI: NSNumber); cdecl;
    [MethodName('centralManager:didFailToConnectPeripheral:error:')]
    procedure centralManagerDidFailToConnectPeripheral(central: CBCentralManager; didFailToConnectPeripheral: CBPeripheral; error: NSError); cdecl;
    [MethodName('centralManager:didUpdateANCSAuthorizationForPeripheral:')]
    procedure centralManagerDidUpdateANCSAuthorizationForPeripheral(central: CBCentralManager;
      didUpdateANCSAuthorizationForPeripheral: CBPeripheral); cdecl;
    procedure centralManagerDidUpdateState(central: CBCentralManager); cdecl;
    [MethodName('centralManager:willRestoreState:')]
    procedure centralManagerWillRestoreState(central: CBCentralManager; willRestoreState: NSDictionary); cdecl;
  public
    constructor Create(const APlatformBluetoothLEDevice: TPlatformBluetoothLEDevice);
  end;

  TPlatformBluetoothLEDevice = class(TCustomBluetoothLEDevice)
  private
    FCentralManager: CBCentralManager;
    FCentralManagerDelegate: TDeviceCBCentralManagerDelegate;
    FIsConnected: Boolean;
    FNeedsDiscover: Boolean;
    FPeripheral: CBPeripheral;
    FPeripheralDelegate: TCBPeripheralDelegate;
  private
    procedure CheckNeedsDiscover;
  protected
    procedure DidConnectPeripheral(const ACentralManager: CBCentralManager; const APeripheral: CBPeripheral);
    procedure DidDisconnectPeripheral(const ACentralManager: CBCentralManager; const APeripheral: CBPeripheral; const AError: NSError);
    procedure DidDiscoverServices(const APeripheral: CBPeripheral; AError: NSError);
    procedure DidDiscoverIncludedServicesForService(const APeripheral: CBPeripheral; const AService: CBService; const AError: NSError);
    procedure DidFailToConnectPeripheral(const ACentralManager: CBCentralManager; const APeripheral: CBPeripheral; const AError: NSError);
  protected
    function Connect: Boolean; override;
  public
    constructor Create(const APeripheral: CBPeripheral);
    destructor Destroy; override;
    function DiscoverServices: Boolean; override;
    property Peripheral: CBPeripheral read FPeripheral;
  end;

  TPlatformBluetoothService = class(TCustomBluetoothService)
  private
    FService: CBService;
  protected
    property Service: CBService read FService write FService;
  end;

function CBUUIDAsString(const AUUID: CBUUIDEx): string;
begin
  Result := NSStrToStr(AUUID.UUIDString);
  if Length(Result) = 4 then
    Result := Format(cUUIDWith16Bit, [Result]);
  Result := '{' + Result + '}';
end;

{ TCBPeripheralDelegate }

constructor TCBPeripheralDelegate.Create(const APlatformBluetoothLEDevice: TPlatformBluetoothLEDevice);
begin
  inherited Create;
  FPlatformBluetoothLEDevice := APlatformBluetoothLEDevice;
end;

procedure TCBPeripheralDelegate.peripheralDidDiscoverCharacteristicsForService(peripheral: CBPeripheral;
  didDiscoverCharacteristicsForService: CBService; error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidDiscoverDescriptorsForCharacteristic(peripheral: CBPeripheral;
  didDiscoverDescriptorsForCharacteristic: CBCharacteristic; error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidDiscoverIncludedServicesForService(peripheral: CBPeripheral;
  didDiscoverIncludedServicesForService: CBService; error: NSError);
begin
  FPlatformBluetoothLEDevice.DidDiscoverIncludedServicesForService(peripheral, didDiscoverIncludedServicesForService, error);
end;

procedure TCBPeripheralDelegate.peripheralDidDiscoverServices(peripheral: CBPeripheral; didDiscoverServices: NSError);
begin
  FPlatformBluetoothLEDevice.DidDiscoverServices(peripheral, didDiscoverServices);
end;

procedure TCBPeripheralDelegate.peripheralDidModifyServices(peripheral: CBPeripheral; didModifyServices: NSArray);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidOpenL2CAPChannel(peripheral: CBPeripheral; didOpenL2CAPChannel: CBL2CAPChannel; error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidReadRSSI(peripheral: CBPeripheral; didReadRSSI: NSNumber; error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidUpdateName(peripheral: CBPeripheral);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidUpdateNotificationStateForCharacteristic(peripheral: CBPeripheral;
  didUpdateNotificationStateForCharacteristic: CBCharacteristic; error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidUpdateRSSI(peripheral: CBPeripheral; error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidUpdateValueForCharacteristic(peripheral: CBPeripheral; didUpdateValueForCharacteristic: CBCharacteristic;
  error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidUpdateValueForDescriptor(peripheral: CBPeripheral; didUpdateValueForDescriptor: CBDescriptor;
  error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidWriteValueForCharacteristic(peripheral: CBPeripheral; didWriteValueForCharacteristic: CBCharacteristic;
  error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralDidWriteValueForDescriptor(peripheral: CBPeripheral; didWriteValueForDescriptor: CBDescriptor;
  error: NSError);
begin

end;

procedure TCBPeripheralDelegate.peripheralIsReadyToSendWriteWithoutResponse(peripheral: CBPeripheral);
begin

end;

{ TDeviceCBCentralManagerDelegate }

constructor TDeviceCBCentralManagerDelegate.Create(const APlatformBluetoothLEDevice: TPlatformBluetoothLEDevice);
begin
  inherited Create;
  FPlatformBluetoothDevice := APlatformBluetoothLEDevice;
end;

procedure TDeviceCBCentralManagerDelegate.centralManagerConnectionEventDidOccur(central: CBCentralManager; connectionEventDidOccur: CBConnectionEvent;
  forPeripheral: CBPeripheral);
begin
  // FPlatformBluetoothDevice.ConnectionEventDidOccur(central, connectionEventDidOccur, forPeripheral);
end;

procedure TDeviceCBCentralManagerDelegate.centralManagerDidConnectPeripheral(central: CBCentralManager; didConnectPeripheral: CBPeripheral);
begin
  FPlatformBluetoothDevice.DidConnectPeripheral(central, didConnectPeripheral);
end;

procedure TDeviceCBCentralManagerDelegate.centralManagerDidDisconnectPeripheral(central: CBCentralManager; didDisconnectPeripheral: CBPeripheral;
  error: NSError);
begin
  FPlatformBluetoothDevice.DidDisconnectPeripheral(central, didDisconnectPeripheral, error);
end;

procedure TDeviceCBCentralManagerDelegate.centralManagerDidDiscoverPeripheral(central: CBCentralManager; didDiscoverPeripheral: CBPeripheral;
  advertisementData: NSDictionary; RSSI: NSNumber);
begin
  //
end;

procedure TDeviceCBCentralManagerDelegate.centralManagerDidFailToConnectPeripheral(central: CBCentralManager;
  didFailToConnectPeripheral: CBPeripheral; error: NSError);
begin
  FPlatformBluetoothDevice.DidFailToConnectPeripheral(central, didFailToConnectPeripheral, error);
end;

procedure TDeviceCBCentralManagerDelegate.centralManagerDidUpdateANCSAuthorizationForPeripheral(central: CBCentralManager;
  didUpdateANCSAuthorizationForPeripheral: CBPeripheral);
begin
  //
end;

procedure TDeviceCBCentralManagerDelegate.centralManagerDidUpdateState(central: CBCentralManager);
begin
  //
end;

procedure TDeviceCBCentralManagerDelegate.centralManagerWillRestoreState(central: CBCentralManager; willRestoreState: NSDictionary);
begin
  //
end;

{ TPlatformBluetoothLEDevice }

constructor TPlatformBluetoothLEDevice.Create(const APeripheral: CBPeripheral);
begin
  inherited Create;
  FCentralManagerDelegate := TDeviceCBCentralManagerDelegate.Create(Self);
  FCentralManager := TCBCentralManager.Wrap(TCBCentralManager.Alloc.initWithDelegate(FCentralManagerDelegate.GetObjectID, 0));
  FPeripheralDelegate := TCBPeripheralDelegate.Create(Self);
  FPeripheral := APeripheral;
  FPeripheral.setDelegate(FPeripheralDelegate.GetObjectID);
end;

destructor TPlatformBluetoothLEDevice.Destroy;
begin
  FCentralManagerDelegate.Free;
  FPeripheralDelegate.Free;
  inherited;
end;

procedure TPlatformBluetoothLEDevice.CheckNeedsDiscover;
begin
  if FNeedsDiscover then
  begin
    FNeedsDiscover := False;
    if FIsConnected then
      DiscoverServices
    else
      DoServicesDiscovered(False);
  end;
end;

function TPlatformBluetoothLEDevice.Connect: Boolean;
begin
  FCentralManager.connectPeripheral(FPeripheral, nil);
  Result := True;
end;

procedure TPlatformBluetoothLEDevice.DidConnectPeripheral(const ACentralManager: CBCentralManager; const APeripheral: CBPeripheral);
begin
  FIsConnected := True;
  CheckNeedsDiscover;
end;

procedure TPlatformBluetoothLEDevice.DidDisconnectPeripheral(const ACentralManager: CBCentralManager; const APeripheral: CBPeripheral;
  const AError: NSError);
begin
  FIsConnected := False;
  CheckNeedsDiscover;
end;

procedure TPlatformBluetoothLEDevice.DidDiscoverIncludedServicesForService(const APeripheral: CBPeripheral; const AService: CBService;
  const AError: NSError);
begin
  // Future
end;

procedure TPlatformBluetoothLEDevice.DidDiscoverServices(const APeripheral: CBPeripheral; AError: NSError);
var
  LSuccess: Boolean;
  I: Integer;
  LService: CBService;
  LCustomDeviceService: TCustomBluetoothService;
  LDeviceService: TPlatformBluetoothService;
  LGUID: TGUID;
begin
  LSuccess := (AError = nil) or (AError.code = 0);
  if LSuccess then
  begin
    for I := 0 to FPeripheral.services.count - 1 do
    begin
      LService := TCBService.Wrap(FPeripheral.services.objectAtIndex(I));
      LGUID := TGUID.Create(CBUUIDAsString(TCBUUIDEx.Wrap(LService.UUID)));
      if Services.TryGetValue(LGUID.ToString, LCustomDeviceService) then
        LDeviceService := TPlatformBluetoothService(LCustomDeviceService)
      else
        LDeviceService := TPlatformBluetoothService.Create(LGUID, False);
      LDeviceService.Service := LService;
    end;
  end;
  DoServicesDiscovered(LSuccess);
end;

procedure TPlatformBluetoothLEDevice.DidFailToConnectPeripheral(const ACentralManager: CBCentralManager; const APeripheral: CBPeripheral;
  const AError: NSError);
begin
  FIsConnected := False;
  CheckNeedsDiscover;
end;

function TPlatformBluetoothLEDevice.DiscoverServices: Boolean;
begin
  if FIsConnected then
  begin
    Result := True;
    TOSLog.d('Discovering..');
    FPeripheral.discoverServices(nil);
  end
  else
  begin
    FNeedsDiscover := True;
    Result := Connect;
    if not Result then
      FNeedsDiscover := False;
  end;
end;

{ TScannerCBCentralManagerDelegate }

constructor TScannerCBCentralManagerDelegate.Create(const APlatformBluetoothScanner: TPlatformBluetoothScanner);
begin
  inherited Create;
  FPlatformBluetoothScanner := APlatformBluetoothScanner;
end;

procedure TScannerCBCentralManagerDelegate.centralManagerConnectionEventDidOccur(central: CBCentralManager; connectionEventDidOccur: CBConnectionEvent;
  forPeripheral: CBPeripheral);
begin
  //
end;

procedure TScannerCBCentralManagerDelegate.centralManagerDidConnectPeripheral(central: CBCentralManager; didConnectPeripheral: CBPeripheral);
begin
  FPlatformBluetoothScanner.DidConnectPeripheral(didConnectPeripheral);
end;

procedure TScannerCBCentralManagerDelegate.centralManagerDidDisconnectPeripheral(central: CBCentralManager; didDisconnectPeripheral: CBPeripheral; error: NSError);
begin
  FPlatformBluetoothScanner.DidDisconnectPeripheral(didDisconnectPeripheral, error);
end;

procedure TScannerCBCentralManagerDelegate.centralManagerDidDiscoverPeripheral(central: CBCentralManager; didDiscoverPeripheral: CBPeripheral;
  advertisementData: NSDictionary; RSSI: NSNumber);
begin
  FPlatformBluetoothScanner.DidDiscoverPeripheral(didDiscoverPeripheral, advertisementData, RSSI);
end;

procedure TScannerCBCentralManagerDelegate.centralManagerDidFailToConnectPeripheral(central: CBCentralManager; didFailToConnectPeripheral: CBPeripheral;
  error: NSError);
begin
  // Might need it?
end;

procedure TScannerCBCentralManagerDelegate.centralManagerDidUpdateANCSAuthorizationForPeripheral(central: CBCentralManager;
  didUpdateANCSAuthorizationForPeripheral: CBPeripheral);
begin
  //
end;

procedure TScannerCBCentralManagerDelegate.centralManagerDidUpdateState(central: CBCentralManager);
begin
  FPlatformBluetoothScanner.DidUpdateState;
end;

procedure TScannerCBCentralManagerDelegate.centralManagerWillRestoreState(central: CBCentralManager; willRestoreState: NSDictionary);
begin
  //
end;

{ TPlatformBluetoothScanner }

constructor TPlatformBluetoothScanner.Create(const ABluetoothScanner: TBluetoothScanner);
begin
  inherited;
  FExpiryTimer := TOSTimer.Create;
  FExpiryTimer.OnInterval := ExpiryTimerIntervalHandler;
  FScanTimer := TOSTimer.Create;
  FScanTimer.OnInterval := ScanTimerIntervalHandler;
  FScanTimer.Interval := 100;
  FIsStarting := True;
  FCentralManagerDelegate := TScannerCBCentralManagerDelegate.Create(Self);
  FCentralManager := TCBCentralManager.Wrap(TCBCentralManager.Alloc.initWithDelegate(FCentralManagerDelegate.GetObjectID, 0));
end;

destructor TPlatformBluetoothScanner.Destroy;
begin
  FExpiryTimer.Free;
  FScanTimer.Free;
  FCentralManagerDelegate.Free;
  inherited;
end;

procedure TPlatformBluetoothScanner.DidConnectPeripheral(const APeripheral: CBPeripheral);
begin

end;

procedure TPlatformBluetoothScanner.DidDisconnectPeripheral(const APeripheral: CBPeripheral; const AError: NSError);
begin

end;

procedure TPlatformBluetoothScanner.DidDiscoverPeripheral(const APeripheral: CBPeripheral; const AAdvertisementData: NSDictionary;
  const ARSSI: NSNumber);
var
  LKey: string;
  LCustomDevice: TCustomBluetoothLEDevice;
  LDevice: TPlatformBluetoothLEDevice;
  LPointer: Pointer;
  LUUIDs: NSArray;
  I: Integer;
  LIsNew: Boolean;
  LService: TCustomBluetoothService; // Platform
  LServiceData: NSDictionary;
  LServiceUUID: string;
  LGUID: TGUID;
begin
  LIsNew := False;
  LKey := NSStrToStr(APeripheral.identifier.UUIDString);
  if not Devices.TryGetValue(LKey, LCustomDevice) then
  begin
    LDevice := TPlatformBluetoothLEDevice.Create(APeripheral);
    LDevice.Key := LKey;
    LPointer := AAdvertisementData.valueForKey(CBAdvertisementDataManufacturerDataKey);
    if LPointer <> nil then
      LDevice.Data := TNSDataHelper.ToBytes(TNSData.Wrap(LPointer));
    LPointer := AAdvertisementData.valueForKey(CBAdvertisementDataLocalNameKey);
    if LPointer <> nil then
      LDevice.Name := NSStrToStr(TNSString.Wrap(LPointer));
    LPointer := AAdvertisementData.valueForKey(CBAdvertisementDataServiceUUIDsKey);
    if LPointer <> nil then
    begin
      LUUIDs := TNSArray.Wrap(LPointer);
      for I := 0 to LUUIDs.count - 1 do
      begin
        LGUID := TGUID.Create(CBUUIDAsString(TCBUUIDEx.Wrap(LUUIDs.objectAtIndex(I))));
        LService := TPlatformBluetoothService.Create(LGUID);
        LDevice.Services.Add(LGUID.ToString, LService);
      end;
    end;
    LPointer := AAdvertisementData.valueForKey(CBAdvertisementDataOverflowServiceUUIDsKey);
    if LPointer <> nil then
    begin
      LUUIDs := TNSArray.Wrap(LPointer);
      for I := 0 to LUUIDs.count - 1 do
      begin
        LGUID := TGUID.Create(CBUUIDAsString(TCBUUIDEx.Wrap(LUUIDs.objectAtIndex(I))));
        LService := TPlatformBluetoothService.Create(LGUID, True);
        LDevice.Services.Add(LGUID.ToString, LService);
      end;
    end;
    LPointer := AAdvertisementData.valueForKey(CBAdvertisementDataServiceDataKey);
    if LPointer <> nil then
    begin
      LServiceData := TNSDictionary.Wrap(LPointer);
      for I := 0 to LServiceData.count - 1 do
      begin
        LPointer := LServiceData.allKeys.objectAtIndex(I);
        if LPointer <> nil then
        begin
          LServiceUUID := CBUUIDAsString(TCBUUIDEx.Wrap(LPointer));
          LPointer := LServiceData.allValues.objectAtIndex(I);
          if LPointer <> nil then
            LDevice.SetServiceData(LServiceUUID, TNSDataHelper.ToBytes(TNSData.Wrap(LPointer)));
        end;
      end;
    end;
    Devices.AddOrSetValue(LDevice.Key, LDevice);
    LIsNew := True;
  end
  else
    LDevice := TPlatformBluetoothLEDevice(LCustomDevice);
  LPointer := AAdvertisementData.valueForKey(CBAdvertisementDataTxPowerLevelKey);
  if LPointer <> nil then
    LDevice.TxPower := TNSNumber.Wrap(LPointer).intValue;
  LPointer := AAdvertisementData.valueForKey(CBAdvertisementDataIsConnectable);
  if LPointer <> nil then
    LDevice.IsConnectable := TNSNumber.Wrap(LPointer).boolValue;
  LDevice.RSSI := ARSSI.intValue;
  if LIsNew then
    DiscoveredDevice(LDevice);
end;

// CBManagerStateUnknown = 0, CBManagerStateResetting, CBManagerStateUnsupported,
//    CBManagerStateUnauthorized, CBManagerStatePoweredOff, CBManagerStatePoweredOn

procedure TPlatformBluetoothScanner.DidUpdateState;
begin
  FIsStarting := False;
  if FIsScanPending then
    Scan;
end;

function TPlatformBluetoothScanner.IsBluetoothEnabled: Boolean;
begin
  Result := (FCentralManager <> nil) and (FCentralManager.state = CBManagerStatePoweredOn);
end;

function TPlatformBluetoothScanner.IsScanning: Boolean;
begin
  Result := (FCentralManager <> nil) and TCBCentralManagerEx.Wrap(NSObjectToID(FCentralManager)).isScanning;
end;

procedure TPlatformBluetoothScanner.Scan;
begin
  if FIsStarting then
    FIsScanPending := True
  else if IsBluetoothEnabled and not IsScanning then
  begin
    FIsScanPending := False;
    FExpiryTimer.Interval := Expiry;
    FExpiryTimer.Enabled := True;
    FScanTimer.Enabled := True;
    // UUIDs, Options
    FCentralManager.scanForPeripheralsWithServices(nil, nil);
    // Results of scan come via DidDiscoverPeripheral
  end;
  // else cannot scan
end;

procedure TPlatformBluetoothScanner.ScanStop;
begin
  FExpiryTimer.Enabled := False;
  FScanTimer.Enabled := False;
  ScanFinish;
end;

procedure TPlatformBluetoothScanner.ExpiryTimerIntervalHandler(Sender: TObject);
begin
  if IsScanning then
    FCentralManager.stopScan;
  ScanStop;
end;

procedure TPlatformBluetoothScanner.ScanTimerIntervalHandler(Sender: TObject);
begin
  if not IsScanning then
    ScanStop;
end;

procedure TPlatformBluetoothScanner.Stop;
begin
  FCentralManager.stopScan;
end;

end.

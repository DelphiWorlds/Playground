unit DW.BluetoothLE.Types;

interface

uses
  System.SysUtils, System.Generics.Collections, System.JSON;

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
    procedure FromJSON(const AJSON: TJSONValue);
    function ToJSON: TJSONValue;
  end;

  TBluetoothLEScanFilters = TArray<TBluetoothLEScanFilter>;

  TBluetoothLEDeviceKind = (Unknown, iBeacon);

  TCustomBluetoothLEDevice = class(TObject)
  private
    FAddress: string;
    FData: TBytes;
    FDeviceType: Integer;
    FKind: TBluetoothLEDeviceKind;
    FName: string;
    FRSSI: Integer;
    FServiceUUIDs: TArray<TGUID>;
    FTxPower: Integer;
    function BytesMatch(const ABytes: TBytes; const AOffset: Integer = 0): Boolean;
    function GetDistance: Double;
    procedure SetData(const Value: TBytes);
  public
    property Address: string read FAddress write FAddress;
    property Data: TBytes read FData write SetData;
    property DeviceType: Integer read FDeviceType write FDeviceType;
    property Distance: Double read GetDistance;
    property Kind: TBluetoothLEDeviceKind read FKind;
    property Name: string read FName write FName;
    property RSSI: Integer read FRSSI write FRSSI;
    property ServiceUUIDs: TArray<TGUID> read FServiceUUIDs write FServiceUUIDs;
    property TxPower: Integer read FTxPower write FTxPower;
  end;

  TBluetoothLEDevices = TObjectDictionary<string, TCustomBluetoothLEDevice>;

implementation

uses
  System.Math;

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

function TCustomBluetoothLEDevice.GetDistance: Double;
var
  LRatio: Double;
begin
  // https://github.com/kevindigi/android-iot-samples/blob/7fb4b91eb769a3dba06891286f4f2f3249dab2a6/app/src/main/java/com/digicorp/helper/DistanceManager.java#L199
{
  LRatio := RSSI / TxPower; //  -61;
  if LRatio < 1 then
    Result := Power(10, LRatio)
  else
    Result := (0.42093 * Power(6.9476, LRatio)) + 0.54992;
}
  Result := Power(10, (TxPower - RSSI) / (10 * cSignalConst));
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

end.

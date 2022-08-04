unit DW.BluetoothLE.Types;

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
  System.SysUtils, System.Generics.Collections, System.JSON;

const
  cUUIDWith16Bit = '0000%s-0000-1000-8000-00805F9B34FB';

type
  TServiceDataRawData = record
    Key: TGUID;
    Value: TBytes;
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

  TDeviceService = record
    UUID: TGUID;
    Data: TBytes;
    IsOverflow: Boolean;
  end;

  TDeviceServices = TArray<TDeviceService>;

  TCustomBluetoothLEDevice = class(TObject)
  private
    FAddress: string;
    FData: TBytes;
    FDeviceType: Integer;
    FIsConnectable: Boolean;
    FKey: string;
    FKind: TBluetoothLEDeviceKind;
    FName: string;
    FRSSI: Integer;
    FServices: TDeviceServices;
    FTxPower: Integer;
    function BytesMatch(const ABytes: TBytes; const AOffset: Integer = 0): Boolean;
    function GetDistance: Double;
    function IndexOfService(const AUUIDString: string): Integer;
    procedure SetData(const Value: TBytes);
  public
    function DisplayName: string;
    function FindService(const AUUIDString: string; out AService: TDeviceService): Boolean;
    function HasService(const AUUID: TGUID): Boolean; overload;
    function HasService(const AUUIDString: string): Boolean; overload;
    procedure SetServiceData(const AUUIDString: string; const AData: TBytes);
    property Address: string read FAddress write FAddress;
    property Data: TBytes read FData write SetData;
    property DeviceType: Integer read FDeviceType write FDeviceType;
    property Distance: Double read GetDistance;
    property IsConnectable: Boolean read FIsConnectable write FIsConnectable;
    property Key: string read FKey write FKey;
    property Kind: TBluetoothLEDeviceKind read FKind;
    property Name: string read FName write FName;
    property RSSI: Integer read FRSSI write FRSSI;
    property Services: TDeviceServices read FServices write FServices;
    property TxPower: Integer read FTxPower write FTxPower;
  end;

  TBluetoothLEDevices = TObjectDictionary<string, TCustomBluetoothLEDevice>;

implementation

uses
  // RTL
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

function TCustomBluetoothLEDevice.DisplayName: string;
var
  LKey: string;
begin
  if Name.IsEmpty then
  begin
    if Key.IsEmpty then
      Result := '(unknown)'
    else
      Result := Key;
  end
  else
  begin
    Result := Name;
    if not Key.IsEmpty then
    begin
      LKey := Key;
      if Length(LKey) > 15 then
        LKey := LKey.Substring(0, 7) + '..' + LKey.Substring(Length(LKey) - 7);
      Result := Result + ' (' + LKey + ')';
    end;
  end;
end;

function TCustomBluetoothLEDevice.GetDistance: Double;
begin
  Result := Power(10, (TxPower - RSSI) / (10 * cSignalConst));
end;

function TCustomBluetoothLEDevice.FindService(const AUUIDString: string; out AService: TDeviceService): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOfService(AUUIDString);
  if LIndex > - 1 then
  begin
    AService := Services[LIndex];
    Result := True;
  end;
end;

function TCustomBluetoothLEDevice.HasService(const AUUIDString: string): Boolean;
begin
  Result := IndexOfService(AUUIDString) > -1;
end;

function TCustomBluetoothLEDevice.IndexOfService(const AUUIDString: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(Services) - 1 do
  begin
    if Services[I].UUID.ToString.Equals(AUUIDString) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TCustomBluetoothLEDevice.HasService(const AUUID: TGUID): Boolean;
begin
  Result := HasService(AUUID.ToString);
end;

procedure TCustomBluetoothLEDevice.SetServiceData(const AUUIDString: string; const AData: TBytes);
var
  LIndex: Integer;
begin
  LIndex := IndexOfService(AUUIDString);
  if LIndex > - 1 then
    Services[LIndex].Data := AData;
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

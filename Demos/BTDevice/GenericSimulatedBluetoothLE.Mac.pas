unit GenericSimulatedBluetoothLE.Mac;

interface

uses
  DW.SimulatedBluetoothLEDevice.Mac;

type
  TPlatformGenericSimulatedBluetoothLEDevice = class(TPlatformSimulatedBluetoothLEDevice)
  public
    constructor Create; override;
  end;

implementation

uses
  GenericSimulatedBluetoothLE.Consts;

{ TPlatformGenericSimulatedBluetoothLEDevice }

constructor TPlatformGenericSimulatedBluetoothLEDevice.Create;
begin
  inherited;
  AddService(USER_DATA_SERVICE_UUID, []);
end;

end.

unit GenericSimulatedBluetoothLE.Android;

interface

uses
  DW.SimulatedBluetoothLEDevice.Android;

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
  AddService(USER_DATA_SERVICE_UUID, [], [], True);
end;

end.

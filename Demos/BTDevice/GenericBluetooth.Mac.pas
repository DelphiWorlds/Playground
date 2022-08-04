unit GenericBluetooth.Mac;

interface

uses
  DW.SimulatedBluetoothDevice.Mac;

type
  TPlatformGenericBluetoothDevice = class(TPlatformSimulatedBluetoothDevice)
  public
    constructor Create; override;
  end;

implementation

uses
  GenericBluetooth.Consts;

{ TPlatformGenericBluetoothDevice }

constructor TPlatformGenericBluetoothDevice.Create;
begin
  inherited;
  AddService(USER_DATA_SERVICE_UUID, []);
end;

end.

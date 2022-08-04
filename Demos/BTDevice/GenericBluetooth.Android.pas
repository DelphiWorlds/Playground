unit GenericBluetooth.Android;

interface

uses
  DW.SimulatedBluetoothDevice.Android;

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
  AddService(USER_DATA_SERVICE_UUID, [], [], True);
end;

end.

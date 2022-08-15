unit GenericSimulatedBluetoothLE;

interface

uses
{$IF Defined(MACOS)}
  GenericSimulatedBluetoothLE.Mac;
{$ELSEIF Defined(ANDROID)}
  GenericSimulatedBluetoothLE.Android;
{$ELSE}
  GenericSimulatedBluetoothLE.Default;
{$ENDIF}

type
  TGenericSimulatedBluetoothLEDevice = class(TPlatformGenericSimulatedBluetoothLEDevice);

implementation

end.

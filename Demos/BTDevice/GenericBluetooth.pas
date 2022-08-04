unit GenericBluetooth;

interface

uses
{$IF Defined(MACOS)}
  GenericBluetooth.Mac;
{$ELSEIF Defined(ANDROID)}
  GenericBluetooth.Android;
{$ELSE}
  GenericBluetooth.Default;
{$ENDIF}

type
  TGenericBluetoothDevice = class(TPlatformGenericBluetoothDevice);

implementation

end.

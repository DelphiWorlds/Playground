unit DW.Bluetooth.Default;

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
  // DW
  DW.Bluetooth;

type
  TPlatformBluetoothDevice = class(TCustomPlatformBluetoothDevice);
  TPlatformBluetoothScanner = class(TCustomPlatformBluetoothScanner);

implementation

end.

unit DW.Androidapi.JNI.DWBluetooth;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2022 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.Bluetooth, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes;

type
  JDWAdvertiseCallback = interface;
  JDWAdvertiseCallbackDelegate = interface;
  JDWBluetoothGattCallback = interface;
  JDWBluetoothGattCallbackDelegate = interface;
  JDWBluetoothGattServerCallback = interface;
  JDWBluetoothGattServerCallbackDelegate = interface;
  JDWBluetoothBroadcastReceiver = interface;
  JDWScanCallback = interface;
  JDWScanCallbackDelegate = interface;
  JDWBluetoothSimulatedDevice = interface;
  JDWBluetoothSimulatedDeviceDelegate = interface;

  // Actually derived from DWBroadcastReceiver, but it doesn't matter here
  JDWBluetoothBroadcastReceiverClass = interface(JBroadcastReceiverClass)
    ['{30588A2E-1E07-4820-A3CB-15B69595397B}']
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/DWBluetoothBroadcastReceiver')]
  JDWBluetoothBroadcastReceiver = interface(JBroadcastReceiver)
    ['{475C099F-8888-4F25-B20F-C154258E17CA}']
    procedure startAlarm(context: JContext; interval: Int64); cdecl;
    procedure cancelAlarm(context: JContext); cdecl;
  end;
  TJDWBluetoothReceiver = class(TJavaGenericImport<JDWBluetoothBroadcastReceiverClass, JDWBluetoothBroadcastReceiver>) end;

  JDWAdvertiseCallbackClass = interface(JAdvertiseCallbackClass)
    ['{A803242F-2D33-45B0-9F2E-0A576E3372B8}']
    {class} function init(delegate: JDWAdvertiseCallbackDelegate): JDWAdvertiseCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/le/DWAdvertiseCallback')]
  JDWAdvertiseCallback = interface(JAdvertiseCallback)
    ['{88B62E31-9F09-424F-AD5A-95EB9B1C25BA}']
  end;
  TJDWAdvertiseCallback = class(TJavaGenericImport<JDWAdvertiseCallbackClass, JDWAdvertiseCallback>) end;

  JDWAdvertiseCallbackDelegateClass = interface(IJavaClass)
    ['{C64EADB9-3ECC-43BB-94D7-86FF8D8B3ACB}']
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/le/DWAdvertiseCallbackDelegate')]
  JDWAdvertiseCallbackDelegate = interface(IJavaInstance)
    ['{EF9556BC-487D-4C25-B61E-B38EE9DBA85D}']
    procedure onStartFailure(errorCode: Integer); cdecl;
    procedure onStartSuccess(settingsInEffect: JAdvertiseSettings); cdecl;
  end;
  TJDWAdvertiseCallbackDelegate = class(TJavaGenericImport<JDWAdvertiseCallbackDelegateClass, JDWAdvertiseCallbackDelegate>) end;

  JDWBluetoothGattCallbackClass = interface(JBluetoothGattCallbackClass)
    ['{006B6E19-45CD-4FF7-AD11-6C3A6AFE9FA5}']
    {class} function init(delegate: JDWBluetoothGattCallbackDelegate): JDWBluetoothGattCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/DWBluetoothGattCallback')]
  JDWBluetoothGattCallback = interface(JBluetoothGattCallback)
    ['{027ED87C-A961-4828-93BC-A9A8151DC8BA}']
  end;
  TJDWBluetoothGattCallback = class(TJavaGenericImport<JDWBluetoothGattCallbackClass, JDWBluetoothGattCallback>) end;

  JDWBluetoothGattCallbackDelegateClass = interface(IJavaClass)
    ['{35E2243F-822E-4F3B-931B-558C2BE1C0F2}']
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/DWBluetoothGattCallbackDelegate')]
  JDWBluetoothGattCallbackDelegate = interface(IJavaInstance)
    ['{C85D43CB-E913-480F-9DFA-B7415318501F}']
    procedure onCharacteristicChanged(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic); cdecl;
    procedure onCharacteristicRead(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic; status: Integer); cdecl;
    procedure onCharacteristicWrite(gatt: JBluetoothGatt; characteristic: JBluetoothGattCharacteristic; status: Integer); cdecl;
    procedure onConnectionStateChange(gatt: JBluetoothGatt; status: Integer; newState: Integer); cdecl;
    procedure onDescriptorRead(gatt: JBluetoothGatt; descriptor: JBluetoothGattDescriptor; status: Integer); cdecl;
    procedure onDescriptorWrite(gatt: JBluetoothGatt; descriptor: JBluetoothGattDescriptor; status: Integer); cdecl;
    procedure onReadRemoteRssi(gatt: JBluetoothGatt; rssi: Integer; status: Integer); cdecl;
    procedure onReliableWriteCompleted(gatt: JBluetoothGatt; status: Integer); cdecl;
    procedure onServicesDiscovered(gatt: JBluetoothGatt; status: Integer); cdecl;
  end;
  TJDWBluetoothGattCallbackDelegate = class(TJavaGenericImport<JDWBluetoothGattCallbackDelegateClass, JDWBluetoothGattCallbackDelegate>) end;

  JDWBluetoothGattServerCallbackClass = interface(JBluetoothGattServerCallbackClass)
    ['{17C055AB-2A4B-4604-93C7-DDA9D15DCAA0}']
    {class} function init(delegate: JDWBluetoothGattServerCallbackDelegate): JDWBluetoothGattServerCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/DWBluetoothGattServerCallback')]
  JDWBluetoothGattServerCallback = interface(JBluetoothGattServerCallback)
    ['{59546494-E6BD-486C-ABCD-5F912FD4ECCF}']
  end;
  TJDWBluetoothGattServerCallback = class(TJavaGenericImport<JDWBluetoothGattServerCallbackClass, JDWBluetoothGattServerCallback>) end;

  JDWBluetoothGattServerCallbackDelegateClass = interface(IJavaClass)
    ['{65E1889F-AD5A-4177-BB17-48E0754A0CC7}']
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/DWBluetoothGattServerCallbackDelegate')]
  JDWBluetoothGattServerCallbackDelegate = interface(IJavaInstance)
    ['{0B312F78-1F56-4B23-9442-0CE3E53A1776}']
    procedure onCharacteristicReadRequest(device: JBluetoothDevice; requestId: Integer; offset: Integer;
      characteristic: JBluetoothGattCharacteristic); cdecl;
    procedure onCharacteristicWriteRequest(device: JBluetoothDevice; requestId: Integer; characteristic: JBluetoothGattCharacteristic;
      preparedWrite: Boolean; responseNeeded: Boolean; offset: Integer; value: TJavaArray<Byte>); cdecl;
    procedure onConnectionStateChange(device: JBluetoothDevice; status: Integer; newState: Integer); cdecl;
    procedure onDescriptorReadRequest(device: JBluetoothDevice; requestId: Integer; offset: Integer; descriptor: JBluetoothGattDescriptor); cdecl;
    procedure onDescriptorWriteRequest(device: JBluetoothDevice; requestId: Integer; descriptor: JBluetoothGattDescriptor; preparedWrite: Boolean;
      responseNeeded: Boolean; offset: Integer; value: TJavaArray<Byte>); cdecl;
    procedure onExecuteWrite(device: JBluetoothDevice; requestId: Integer; execute: Boolean); cdecl;
    procedure onServiceAdded(status: Integer; service: JBluetoothGattService); cdecl;
  end;
  TJDWBluetoothGattServerCallbackDelegate = class(TJavaGenericImport<JDWBluetoothGattServerCallbackDelegateClass,
    JDWBluetoothGattServerCallbackDelegate>) end;

  JDWScanCallbackClass = interface(JScanCallbackClass)
    ['{B07B8889-6ADE-4F00-B825-8037C3EDB6E8}']
    {class} function init(delegate: JDWScanCallbackDelegate): JDWScanCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/le/DWScanCallback')]
  JDWScanCallback = interface(JScanCallback)
    ['{B4A61D43-EFD7-4FC7-8EF2-F6C825234E19}']
    procedure onBatchScanResults(results: JList); cdecl;
    procedure onScanFailed(errorCode: Integer); cdecl;
    procedure onScanResult(callbackType: Integer; result: Jle_ScanResult); cdecl;
  end;
  TJDWScanCallback = class(TJavaGenericImport<JDWScanCallbackClass, JDWScanCallback>) end;

  JDWScanCallbackDelegateClass = interface(IJavaClass)
    ['{6FE79FAD-0366-40B9-A896-F38CB6917528}']
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/le/DWScanCallbackDelegate')]
  JDWScanCallbackDelegate = interface(IJavaInstance)
    ['{FD3CB01F-1079-4738-84C1-99DEEB711095}']
    procedure onBatchScanResults(results: JList); cdecl;
    procedure onScanFailed(errorCode: Integer); cdecl;
    procedure onScanResult(callbackType: Integer; result: Jle_ScanResult); cdecl;
  end;
  TJDWScanCallbackDelegate = class(TJavaGenericImport<JDWScanCallbackDelegateClass, JDWScanCallbackDelegate>) end;


  JDWBluetoothSimulatedDeviceClass = interface(JObjectClass)
    ['{4583861C-F9E6-4EF6-B6BB-5ED382590FF4}']
    {class} function init(context: JContext; serviceUuid: JString; delegate: JDWBluetoothSimulatedDeviceDelegate): JDWBluetoothSimulatedDevice; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/DWBluetoothSimulatedDevice')]
  JDWBluetoothSimulatedDevice = interface(JObject)
    ['{D13B6507-83BC-4D2B-8199-FA4701E2C6E4}']
    procedure startAdvertising(advertiseData: JString); cdecl;
    procedure startServer; cdecl;
    procedure stopAdvertising; cdecl;
  end;
  TJDWBluetoothSimulatedDevice = class(TJavaGenericImport<JDWBluetoothSimulatedDeviceClass, JDWBluetoothSimulatedDevice>) end;

  JDWBluetoothSimulatedDeviceDelegateClass = interface(IJavaClass)
    ['{C64EADB9-3ECC-43BB-94D7-86FF8D8B3ACB}']
  end;

  [JavaSignature('com/delphiworlds/kastri/bluetooth/DWBluetoothSimulatedDeviceDelegate')]
  JDWBluetoothSimulatedDeviceDelegate = interface(IJavaInstance)
    ['{EF9556BC-487D-4C25-B61E-B38EE9DBA85D}']
    procedure onAdvertiserStartFailure(errorCode: Integer); cdecl;
    procedure onAdvertiserStartSuccess(settingsInEffect: JAdvertiseSettings); cdecl;
    procedure onServerServicesAdded; cdecl;
  end;
  TJDWBluetoothSimulatedDeviceDelegate = class(TJavaGenericImport<JDWBluetoothSimulatedDeviceDelegateClass, JDWBluetoothSimulatedDeviceDelegate>) end;

implementation

end.

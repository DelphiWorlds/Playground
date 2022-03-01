unit BD.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
  DW.MonitoredDevice, DW.BluetoothLE.Types;

type
  TServiceModule = class(TAndroidIntentService)
    procedure AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
  private
    FStore: TMonitoredDevicesStore;
    procedure CheckDevice(const ADevice: TCustomBluetoothLEDevice);
    function DevicesStoreFileName: string;
  public
    { Public declarations }
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  DW.OSLog, DW.Beacons, DW.Beacons.Android,
  System.Generics.Collections, System.IOUtils,
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

const
  cAlarmInterval = 15000; // Milliseconds

type
  TOpenBeacons = class(TBeacons)
  public
    property Devices;
  end;

function TServiceModule.DevicesStoreFileName: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'devices.json');
end;

procedure TServiceModule.AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
var
  LBeacons: TOpenBeacons;
  LDevicePair: TPair<string, TCustomBluetoothLEDevice>;
  LIsAlarm: Boolean;
  LFilter: TBluetoothLEScanFilter;
begin
  TOSLog.d('+TServiceModule.AndroidIntentServiceHandleIntent');
  LIsAlarm := (AnIntent.getAction <> nil) and AnIntent.getAction.equals(StringToJString(cBeaconsReceiverActionAlarm));
  if TFile.Exists(DevicesStoreFileName) then
    FStore.FromJSON(TFile.ReadAllText(DevicesStoreFileName));
//  if not FStore.HasAlarm and not LIsAlarm then
//  begin
//    TJDWBeaconsReceiver.JavaClass.startAlarm(TAndroidHelper.Context, cAlarmInterval);
//    FStore.HasAlarm := True;
//  end;
  LBeacons := TOpenBeacons.Create;
  try
    if LIsAlarm then
    begin
      TOSLog.d('> From alarm');
      if FStore.SecondsSinceScan > cAlarmInterval then
      begin
        // Using warning here to make it stand out more
        TOSLog.w('> *** Last scan result more than %d seconds ago ***', [Round(cAlarmInterval / 1000)]);
        for LFilter in FStore.Filters do
          LBeacons.AddFilter(LFilter);
        LBeacons.ScanWithIntent;
      end;
    end
    else
    begin
      FStore.ScanResultDateTime := Now;
      LBeacons.HandleIntent(TAndroidHelper.JObjectToID(AnIntent));
    end;
    for LDevicePair in LBeacons.Devices do
      CheckDevice(LDevicePair.Value);
  finally
    LBeacons.Free;
  end;
  //!!!! "Age" off devices not seen for some time??
  TFile.WriteAllText(DevicesStoreFileName, FStore.ToJSON);
  TOSLog.d('-TServiceModule.AndroidIntentServiceHandleIntent');
end;

procedure TServiceModule.CheckDevice(const ADevice: TCustomBluetoothLEDevice);
var
  LMonitoredDevice: TMonitoredDevice;
  LIsNew: Boolean;
begin
  // TOSLog.d('Checking device - Address: %s', [ADevice.Address]);
  LIsNew := True;
  if FStore.FindDevice(ADevice.Address, LMonitoredDevice) then
  begin
    LIsNew := False;
    // Check last seen, distance?
  end;
  LMonitoredDevice.Address := ADevice.Address;
  LMonitoredDevice.Seen := Now;
  LMonitoredDevice.Distance := ADevice.Distance;
  if LIsNew then
    FStore.AddDevice(LMonitoredDevice);
end;

end.

unit BD.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
  DW.Beacons, DW.MonitoredDevice;

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
  DW.OSLog,
  System.Generics.Collections, System.IOUtils,
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

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
begin
  TOSLog.d('+TServiceModule.AndroidIntentServiceHandleIntent');
  if TFile.Exists(DevicesStoreFileName) then
    FStore.FromJSON(TFile.ReadAllText(DevicesStoreFileName));
  LBeacons := TOpenBeacons.Create;
  try
    LBeacons.HandleIntent(TAndroidHelper.JObjectToID(AnIntent));
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
  TOSLog.d('Checking device - Address: %s', [ADevice.Address]);
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

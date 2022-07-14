unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  DW.Beacons, DW.BluetoothLE.Types, DW.MonitoredDevice;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    BottomLayout: TLayout;
    ScanButton: TButton;
    IntentButton: TButton;
    procedure ScanButtonClick(Sender: TObject);
    procedure IntentButtonClick(Sender: TObject);
  private
    FBeacons: TBeacons;
    FStore: TMonitoredDevicesStore;
    procedure BeaconsDiscoveredDeviceHandler(Sender: TObject; const ADevice: TCustomBluetoothLEDevice);
    procedure BeaconsScanFinishHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FBeacons := TBeacons.Create;
  // FBeacons.AddFilter(TBluetoothLEScanFilter.Create(cBeaconServiceUUIDBluecats));
  FBeacons.OnDiscoveredDevice := BeaconsDiscoveredDeviceHandler;
  FBeacons.OnScanFinish := BeaconsScanFinishHandler;
end;

procedure TForm1.IntentButtonClick(Sender: TObject);
begin
  FStore.Filters := Copy(FBeacons.Filters);
  TFile.WriteAllText(TPath.Combine(TPath.GetDocumentsPath, 'devices.json'), FStore.ToJSON);
  FBeacons.ScanWithIntent;
  Memo1.Lines.Add('Created intent');
end;

procedure TForm1.ScanButtonClick(Sender: TObject);
begin
  FBeacons.Scan;
end;

procedure TForm1.BeaconsDiscoveredDeviceHandler(Sender: TObject; const ADevice: TCustomBluetoothLEDevice);
begin
  //
end;

procedure TForm1.BeaconsScanFinishHandler(Sender: TObject);
begin
  //
end;

end.

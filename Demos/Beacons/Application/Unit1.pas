unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  DW.Beacons, FMX.Layouts;

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
    procedure BeaconsDiscoveredDeviceHandler(Sender: TObject; const ADevice: TCustomBluetoothLEDevice);
    procedure BeaconsScanFinishHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

type
  TCustomBluetoothLEDeviceHelper = class helper for TCustomBluetoothLEDevice
  public
    function GetDisplayValue: string;
  end;

{ TCustomBluetoothLEDeviceHelper }

function TCustomBluetoothLEDeviceHelper.GetDisplayValue: string;
begin
  Result := Format('Name: %s, Address: %s, Type: %d', [Name, Address, DeviceType]);
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FBeacons := TBeacons.Create;
  FBeacons.AddFilter(TBluetoothLEScanFilter.Create(cBeaconServiceUUIDBluecats));
  FBeacons.OnDiscoveredDevice := BeaconsDiscoveredDeviceHandler;
  FBeacons.OnScanFinish := BeaconsScanFinishHandler;
end;

procedure TForm1.IntentButtonClick(Sender: TObject);
begin
  FBeacons.ScanWithIntent;
  Memo1.Lines.Add('Created intent');
end;

procedure TForm1.ScanButtonClick(Sender: TObject);
begin
  FBeacons.Scan;
end;

procedure TForm1.BeaconsDiscoveredDeviceHandler(Sender: TObject; const ADevice: TCustomBluetoothLEDevice);
begin
  // Memo1.Lines.Add(ADevice.GetDisplayValue);
end;

procedure TForm1.BeaconsScanFinishHandler(Sender: TObject);
begin
  // Memo1.Lines.Add('Scan done');
end;

end.

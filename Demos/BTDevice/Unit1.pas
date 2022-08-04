unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Edit,
  DW.Bluetooth, DW.BluetoothLE.Types,
  GenericBluetooth;

type
  TForm1 = class(TForm)
    DetectedDevicesLabel: TLabel;
    DevicesListBox: TListBox;
    BottomLayout: TLayout;
    ScanButton: TButton;
    DeviceGridPanelLayout: TGridPanelLayout;
    DeviceIDLabel: TLabel;
    DeviceIDValueLabel: TLabel;
    DeviceRSSILabel: TLabel;
    DeviceRSSIValueLabel: TLabel;
    DeviceDistanceLabel: TLabel;
    DeviceDistanceValueLabel: TLabel;
    SwitchLayout: TLayout;
    ActiveSwitch: TSwitch;
    procedure ScanButtonClick(Sender: TObject);
    procedure DevicesListBoxItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure ActiveSwitchSwitch(Sender: TObject);
  private
    FGenericDevice: TGenericBluetoothDevice;
    FScanner: TBluetoothScanner;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure ClearDeviceDetails;
    procedure CreateDevice;
    function GetPermissions: TArray<string>;
    procedure ScannerDiscoveredDeviceHandler(Sender: TObject; const ADevice: TCustomBluetoothLEDevice);
    procedure ScannerScanFinishHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Permissions,
  FMX.Platform,
  DW.Permissions.Helpers, DW.Consts.Android,
  DW.SimulatedBluetoothDevice,
  GenericBluetooth.Consts;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  FScanner := TBluetoothScanner.Create;
  FScanner.Expiry := 5000; // milliseconds
  FScanner.OnScanFinish := ScannerScanFinishHandler;
  FScanner.OnDiscoveredDevice := ScannerDiscoveredDeviceHandler;
  ClearDeviceDetails;
  {$IF Defined(OSX)}
  CreateDevice;
  {$ENDIF}
end;

destructor TForm1.Destroy;
begin
  FGenericDevice.Free;
  FScanner.Free;
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TForm1.ClearDeviceDetails;
begin
  DeviceIDValueLabel.Text := '';
  DeviceRSSIValueLabel.Text := '';
  DeviceDistanceValueLabel.Text := '';
end;

procedure TForm1.DevicesListBoxItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
var
  LDevice: TCustomBluetoothLEDevice;
begin
  LDevice := TCustomBluetoothLEDevice(Item.TagObject);
  DeviceIDValueLabel.Text := LDevice.Key;
  DeviceRSSIValueLabel.Text := LDevice.RSSI.ToString;
  DeviceDistanceValueLabel.Text := Format('%.2f', [LDevice.Distance]);
end;

function TForm1.GetPermissions: TArray<string>;
begin
  if TOSVersion.Check(12) then
    Result := [cPermissionBluetoothScan, cPermissionBluetoothAdvertise, cPermissionAccessFineLocation]
  else
    Result := [cPermissionAccessFineLocation];
end;

procedure TForm1.ScanButtonClick(Sender: TObject);
begin
  ScanButton.Enabled := False;
  DevicesListBox.Items.Clear;
  ClearDeviceDetails;
  FScanner.Scan;
end;

procedure TForm1.ScannerDiscoveredDeviceHandler(Sender: TObject; const ADevice: TCustomBluetoothLEDevice);
var
  LService: TDeviceService;
begin
  if ADevice.FindService('{' + StringToServiceUUID(USER_DATA_SERVICE_UUID) + '}', LService) then
  begin
    DevicesListBox.Items.Add(Format('Name: %s, RSSI: %s', [ADevice.DisplayName, ADevice.RSSI.ToString]));
    DevicesListBox.ListItems[DevicesListBox.Items.Count - 1].TagObject := ADevice;
  end;
end;

procedure TForm1.ScannerScanFinishHandler(Sender: TObject);
begin
  ScanButton.Enabled := True;
end;

procedure TForm1.ActiveSwitchSwitch(Sender: TObject);
begin
  FGenericDevice.IsActive := ActiveSwitch.IsChecked;
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      if FGenericDevice = nil then
      begin
        {$IF Defined(ANDROID)}
        PermissionsService.RequestPermissions(GetPermissions,
          procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
          begin
            if AGrantResults.AreAllGranted then
              CreateDevice;
          end
        );
        {$ELSE}
        CreateDevice;
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TForm1.CreateDevice;
begin
  FGenericDevice := TGenericBluetoothDevice.Create;
  ActiveSwitch.IsChecked := True;
end;

end.

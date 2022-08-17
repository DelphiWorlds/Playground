unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Edit,
  DW.Bluetooth,
  GenericSimulatedBluetoothLE;

type
  TAniIndicator = class(FMX.StdCtrls.TAniIndicator)
  public
    procedure Animate(const AAnimate: Boolean);
  end;

  TForm1 = class(TForm)
    DetectedDevicesLabel: TLabel;
    DevicesListBox: TListBox;
    BottomLayout: TLayout;
    ScanButton: TButton;
    SwitchLayout: TLayout;
    ActiveSwitch: TSwitch;
    DeviceServicesLabel: TLabel;
    ServicesListBox: TListBox;
    ServicesAniIndicator: TAniIndicator;
    DevicesAniIndicator: TAniIndicator;
    procedure ScanButtonClick(Sender: TObject);
    procedure DevicesListBoxItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure ActiveSwitchSwitch(Sender: TObject);
  private
    FSelectedDevice: TCustomBluetoothLEDevice;
    FGenericDevice: TGenericSimulatedBluetoothLEDevice;
    FScanner: TBluetoothScanner;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure CreateDevice;
    procedure DeviceServicesDiscoveredHandler(Sender: TObject; const ASuccess: Boolean);
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
  DW.Permissions.Helpers, DW.Consts.Android, DW.SimulatedBluetoothLEDevice,
  GenericSimulatedBluetoothLE.Consts;

{ TAniIndicator }

procedure TAniIndicator.Animate(const AAnimate: Boolean);
begin
  Visible := AAnimate;
  Enabled := AAnimate;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  DevicesAniIndicator.Visible := False;
  ServicesAniIndicator.Visible := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  FScanner := TBluetoothScanner.Create;
  FScanner.Expiry := 5000; // milliseconds
  FScanner.OnScanFinish := ScannerScanFinishHandler;
  FScanner.OnDiscoveredDevice := ScannerDiscoveredDeviceHandler;
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

procedure TForm1.DeviceServicesDiscoveredHandler(Sender: TObject; const ASuccess: Boolean);
var
  LService: TCustomBluetoothService;
begin
  ServicesAniIndicator.Animate(False);
  for LService in FSelectedDevice.Services.Values do
    ServicesListBox.Items.Add(LService.UUID.ToString);
end;

procedure TForm1.DevicesListBoxItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  ServicesListBox.Items.Clear;
  if FSelectedDevice <> nil then
    FSelectedDevice.OnServicesDiscovered := nil;
  FSelectedDevice := TCustomBluetoothLEDevice(Item.TagObject);
  FSelectedDevice.OnServicesDiscovered := DeviceServicesDiscoveredHandler;
  if FSelectedDevice.DiscoverServices then
    ServicesAniIndicator.Animate(True);
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
  DevicesAniIndicator.Animate(True);
  FScanner.Scan;
end;

procedure TForm1.ScannerDiscoveredDeviceHandler(Sender: TObject; const ADevice: TCustomBluetoothLEDevice);
begin
  DevicesListBox.Items.Add(Format('Name: %s, RSSI: %s', [ADevice.DisplayName, ADevice.RSSI.ToString]));
  DevicesListBox.ListItems[DevicesListBox.Items.Count - 1].TagObject := ADevice;
end;

procedure TForm1.ScannerScanFinishHandler(Sender: TObject);
begin
  DevicesAniIndicator.Animate(False);
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
  FGenericDevice := TGenericSimulatedBluetoothLEDevice.Create;
  ActiveSwitch.IsChecked := True;
end;

end.

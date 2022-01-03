unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Sensors,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  FMX.Memo.Types,
  DW.LocationMonitor, DW.Location.Types;

type
  TMainView = class(TForm)
    Memo: TMemo;
    ClearButton: TButton;
    ContentLayout: TLayout;
    ButtonsLayout: TLayout;
    ChangeStateButton: TButton;
    procedure ClearButtonClick(Sender: TObject);
    procedure ChangeStateButtonClick(Sender: TObject);
  private
    FLocationMonitor: TLocationMonitor;
    procedure DoRequestBackgroundLocationPermission;
    function GetBasePermissions: TArray<string>;
    function HasBasePermissions: Boolean;
    procedure LocationMonitorLocationChangedHandler(Sender: TObject; const AData: TLocationData);
    procedure LocationMonitorStateChangedHandler(Sender: TObject);
    procedure RequestBackgroundLocationPermission;
    procedure RequestForegroundLocationPermission;
    procedure RequestPermissions;
    procedure ShowBackgroundPermissionRationale(const APostRationaleProc: TProc);
    procedure ToggleLocationActive;
    procedure UpdateChangeStateButton;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainView: TMainView;

implementation

{$R *.fmx}

uses
  System.Permissions,
  FMX.DialogService.Async,
  DW.OSLog, DW.OSDevice,
  DW.Sensors, DW.Consts.Android, DW.UIHelper, DW.Permissions.Helpers,
  LD.LocationUpdater;

const
  cBackgroundPermissionsMessage = 'This application requires access to location updates in the background'#13#10#13#10 +
    'When prompted, please tap the "Allow in settings" option and select "Allow all the time"';

{ TMainView }

constructor TMainView.Create(AOwner: TComponent);
begin
  inherited;
  FLocationMonitor := TLocationMonitor.Create;
  FLocationMonitor.OnStateChanged := LocationMonitorStateChangedHandler;
  FLocationMonitor.OnLocationChanged := LocationMonitorLocationChangedHandler;
  FLocationMonitor.UsageAuthorization := TLocationUsageAuthorization.Always;
  FLocationMonitor.ActivityType := TLocationActivityType.Navigation;
  FLocationMonitor.AlarmInterval := 60000; // Once per minute
  UpdateChangeStateButton;
end;

destructor TMainView.Destroy;
begin
  FLocationMonitor.Free;
  inherited;
end;

procedure TMainView.Resize;
begin
  inherited;
  ContentLayout.Padding.Rect := TUIHelper.GetOffsetRect;
end;

function TMainView.GetBasePermissions: TArray<string>;
begin
  Result := [cPermissionAccessCoarseLocation, cPermissionAccessFineLocation];
end;

function TMainView.HasBasePermissions: Boolean;
begin
  Result := PermissionsService.IsEveryPermissionGranted(GetBasePermissions);
end;

procedure TMainView.RequestPermissions;
begin
  if HasBasePermissions then
    RequestBackgroundLocationPermission
  else
    RequestForegroundLocationPermission;
end;

procedure TMainView.RequestForegroundLocationPermission;
begin
  PermissionsService.RequestPermissions(GetBasePermissions,
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        RequestBackgroundLocationPermission;
      // else Show location updates will not work message
    end
  );
end;

procedure TMainView.RequestBackgroundLocationPermission;
begin
  if TOSVersion.Check(10) then
    ShowBackgroundPermissionRationale(DoRequestBackgroundLocationPermission)
  else
    ToggleLocationActive;
end;

procedure TMainView.ShowBackgroundPermissionRationale(const APostRationaleProc: TProc);
begin
  if not PermissionsService.IsPermissionGranted(cPermissionAccessBackgroundLocation) then
  begin
    TDialogServiceAsync.MessageDialog(cBackgroundPermissionsMessage, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        APostRationaleProc;
      end
    );
  end
  else
    APostRationaleProc;
end;

procedure TMainView.DoRequestBackgroundLocationPermission;
begin
  PermissionsService.RequestPermissions([cPermissionAccessBackgroundLocation],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        ToggleLocationActive;
      // else show that location updates will not occur in the background
    end
  );
end;

procedure TMainView.UpdateChangeStateButton;
begin
  if FLocationMonitor.IsActive then
    ChangeStateButton.Text := 'Pause'
  else
    ChangeStateButton.Text := 'Resume';
end;

procedure TMainView.LocationMonitorLocationChangedHandler(Sender: TObject; const AData: TLocationData);
var
  LTimestamp: string;
begin
  // This event is where the data needs to be handled on iOS. On Android, the data needs to be handled in the service
  if TOSVersion.Platform = TOSVersion.TPlatform.pfiOS then
    TLocationUpdater.HandleLocationData(AData);
  LTimestamp := FormatDateTime('hh:nn:ss.zzz', Now);
  Memo.Lines.Add(Format('%s - Location: %2.6f, %2.6f', [LTimestamp, AData.Location.Latitude, AData.Location.Longitude]));
  Memo.Lines.Add(Format('%s - Speed: %.1f, Altitude: %.1f, Bearing: %.1f', [LTimestamp, AData.Speed, AData.Altitude, AData.Bearing]));
end;

procedure TMainView.LocationMonitorStateChangedHandler(Sender: TObject);
begin
  UpdateChangeStateButton;
end;

procedure TMainView.ChangeStateButtonClick(Sender: TObject);
begin
  if HasBasePermissions or FLocationMonitor.IsActive then
    ToggleLocationActive
  else if not FLocationMonitor.IsActive then
    RequestPermissions;
end;

procedure TMainView.ToggleLocationActive;
begin
  FLocationMonitor.IsActive := not FLocationMonitor.IsActive;
end;

procedure TMainView.ClearButtonClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

end.

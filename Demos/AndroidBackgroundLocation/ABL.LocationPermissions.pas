unit ABL.LocationPermissions;

interface

type
  TPermissionsCompleteProc = reference to procedure(const ACanProceed: Boolean);

  ILocationPermissions = interface(IInterface)
    ['{6DF2A4C1-92AB-4D68-BC82-275F59EA8B79}']
    function CanStartForeground: Boolean;
    function IsRequesting: Boolean;
    procedure Request(const ACompletion: TPermissionsCompleteProc);
    procedure RequestBackground(const ACompletion: TPermissionsCompleteProc);
  end;

var
  LocationPermissions: ILocationPermissions;

implementation

uses
  System.Permissions, System.SysUtils, System.UITypes,
  FMX.DialogService.Async,
  DW.Consts.Android, DW.Permissions.Helpers;

const
  cBackgroundPermissionsMessage = 'This application requires access to location updates in the background'#13#10#13#10 +
    'When prompted, please tap the "Allow in settings" option and select "Allow all the time"';

type
  TLocationPermissions = class(TInterfacedObject, ILocationPermissions)
  private
    FCompletion: TPermissionsCompleteProc;
    FIsRequesting: Boolean;
    procedure DoRequestBackgroundLocationPermission;
    function GetBasePermissions: TArray<string>;
    procedure RequestBackgroundLocationPermission;
    procedure RequestForegroundLocationPermission;
    procedure RequestPermissionsComplete(const ACanStart: Boolean);
    procedure ShowBackgroundPermissionRationale(const APostRationaleProc: TProc);
  public
    { ILocationPermissions }
    function CanStartForeground: Boolean;
    function IsRequesting: Boolean;
    procedure Request(const ACompletion: TPermissionsCompleteProc);
    procedure RequestBackground(const ACompletion: TPermissionsCompleteProc);
  end;

{ TLocationPermissions }

function TLocationPermissions.GetBasePermissions: TArray<string>;
begin
  Result := [cPermissionAccessCoarseLocation, cPermissionAccessFineLocation];
end;

function TLocationPermissions.CanStartForeground: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(cPermissionAccessBackgroundLocation) and not FIsRequesting;
end;

function TLocationPermissions.IsRequesting: Boolean;
begin
  Result := FIsRequesting;
end;

procedure TLocationPermissions.Request(const ACompletion: TPermissionsCompleteProc);
begin
  FCompletion := ACompletion;
  FIsRequesting := True;
  RequestForegroundLocationPermission;
end;

procedure TLocationPermissions.RequestBackground(const ACompletion: TPermissionsCompleteProc);
begin
  FCompletion := ACompletion;
  FIsRequesting := True;
  if PermissionsService.IsPermissionGranted(cPermissionAccessFineLocation) then
    RequestBackgroundLocationPermission
  else
    RequestForegroundLocationPermission;
end;

procedure TLocationPermissions.RequestBackgroundLocationPermission;
begin
  if TOSVersion.Check(10) then
    ShowBackgroundPermissionRationale(DoRequestBackgroundLocationPermission)
  else
    RequestPermissionsComplete(True);
end;

procedure TLocationPermissions.DoRequestBackgroundLocationPermission;
begin
  PermissionsService.RequestPermissions([cPermissionAccessBackgroundLocation],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        RequestPermissionsComplete(True);
      // else show that location updates will not occur in the background
    end
  );
end;

procedure TLocationPermissions.RequestForegroundLocationPermission;
begin
  PermissionsService.RequestPermissions(GetBasePermissions,
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        RequestBackgroundLocationPermission
      else
        RequestPermissionsComplete(False); // Show location updates will not work message
    end
  );
end;

procedure TLocationPermissions.RequestPermissionsComplete(const ACanStart: Boolean);
begin
  FIsRequesting := False;
  if Assigned(FCompletion) then
    FCompletion(ACanStart);
end;

procedure TLocationPermissions.ShowBackgroundPermissionRationale(const APostRationaleProc: TProc);
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

initialization
  LocationPermissions := TLocationPermissions.Create;

end.

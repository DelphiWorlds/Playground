unit DW.LocationMonitor.Android;

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

// *** NOTE ***: Requires dw-fusedlocation.jar (in the Lib folder) to be added to the Libraries node under the Android platform in Project Manager

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.Location, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.Androidapi.JNI.DWFusedLocation, DW.MultiReceiver.Android, DW.LocationMonitor, DW.Location.Types;

type
  TPlatformLocationMonitor = class;

  TFusedLocationClientDelegate = class(TJavaLocal, JDWFusedLocationClientDelegate)
  private
    FMonitor: TPlatformLocationMonitor;
  public
    { JDWFusedLocationClientDelegate }
    procedure onLocation(location: JLocation); cdecl;
    procedure onLocationUpdatesChange(active: Boolean); cdecl;
    procedure onSetMockLocationResult(location: JLocation); cdecl;
    procedure onSetMockModeResult(success: Boolean); cdecl;
  public
    constructor Create(const AMonitor: TPlatformLocationMonitor);
  end;

  TLocationReceiver = class(TMultiReceiver)
  private
    FMonitor: TPlatformLocationMonitor;
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    constructor Create(const AMonitor: TPlatformLocationMonitor);
  end;

  TPlatformLocationMonitor = class(TCustomPlatformLocationMonitor)
  private
    FClient: JDWFusedLocationClient;
    FDelegate: JDWFusedLocationClientDelegate;
    FLastData: TLocationData;
    FLocationReceiver: TLocationReceiver;
    FRequestedActiveChange: Boolean;
    function GetLocationData(const ALocation: JLocation): TLocationData;
    function HasPermissions: Boolean;
    procedure UpdateClient;
  protected
    procedure DoLocationChanged(const AData: TLocationData); override;
    function GetIsActive: Boolean; override;
    procedure LocationChange(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AActive: Boolean);
    procedure SetIsActive(const AValue: Boolean); override;
  public
    constructor Create(const ALocationMonitor: TLocationMonitor); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Permissions, System.SysUtils, System.DateUtils, System.Sensors,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Provider,
  // DW
  DW.OSLog, DW.Consts.Android, DW.Geodetic, DW.LocationHelpers.Android;

{ TFusedLocationClientDelegate }

constructor TFusedLocationClientDelegate.Create(const AMonitor: TPlatformLocationMonitor);
begin
  inherited Create;
  FMonitor := AMonitor;
end;

procedure TFusedLocationClientDelegate.onLocation(location: JLocation);
begin
  //
end;

procedure TFusedLocationClientDelegate.onLocationUpdatesChange(active: Boolean);
begin
  FMonitor.LocationUpdatesChange(active);
end;

procedure TFusedLocationClientDelegate.onSetMockLocationResult(location: JLocation);
begin
  //
end;

procedure TFusedLocationClientDelegate.onSetMockModeResult(success: Boolean);
begin
  //
end;

{ TLocationReceiver }

constructor TLocationReceiver.Create(const AMonitor: TPlatformLocationMonitor);
begin
  inherited Create(True);
  FMonitor := AMonitor;
end;

procedure TLocationReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cActionLocation));
end;

procedure TLocationReceiver.Receive(context: JContext; intent: JIntent);
var
  LData: TLocationData;
begin
  LData.FromJSON(JStringToString(intent.getStringExtra(StringToJString(cExtraLocationData))));
  FMonitor.DoLocationChanged(LData);
end;

{ TPlatformLocationMonitor }

constructor TPlatformLocationMonitor.Create(const ALocationMonitor: TLocationMonitor);
begin
  inherited;
  FLastData.DateTime := 0;
  FLastData.Location := TLocationCoord2D.Create(cInvalidLatitude, cInvalidLongitude);
  FDelegate := TFusedLocationClientDelegate.Create(Self);
  FClient := TJDWFusedLocationClient.JavaClass.init(TAndroidHelper.Context, FDelegate);
  FLocationReceiver := TLocationReceiver.Create(Self);
  FastestInterval := FClient.getFastestInterval;
  Interval := FClient.getInterval;
  Priority := FClient.getPriority;
  Distance := FClient.getSmallestDisplacement;
end;

destructor TPlatformLocationMonitor.Destroy;
begin
  FLocationReceiver.Free;
  inherited;
end;

procedure TPlatformLocationMonitor.DoLocationChanged(const AData: TLocationData);
begin
  inherited;
end;

function TPlatformLocationMonitor.HasPermissions: Boolean;
begin
  if TOSVersion.Check(10) and NeedsBackgroundAccess then
    Result := PermissionsService.IsEveryPermissionGranted([cPermissionAccessBackgroundLocation, cPermissionAccessCoarseLocation, cPermissionAccessFineLocation])
  else
    Result := PermissionsService.IsEveryPermissionGranted([cPermissionAccessCoarseLocation, cPermissionAccessFineLocation]);
end;

{
function TLocation.GetLocationMode: Integer;
begin
  Result := TJSettings_Secure.JavaClass.getInt(TAndroidHelper.ContentResolver, TJSettings_Secure.JavaClass.LOCATION_MODE);
end;
}

procedure TPlatformLocationMonitor.LocationUpdatesChange(const AActive: Boolean);
begin
  FRequestedActiveChange := False;
  DoStateChanged;
end;

function TPlatformLocationMonitor.GetIsActive: Boolean;
begin
  Result := (FClient <> nil) and FClient.getIsActive;
end;

procedure TPlatformLocationMonitor.SetIsActive(const AValue: Boolean);
begin
  if not FRequestedActiveChange and (AValue <> IsActive) then
  begin
    if IsActive then
    begin
      FRequestedActiveChange := True;
      FClient.stopLocationUpdates;
    end
    else if HasPermissions then
    begin
      FRequestedActiveChange := True;
      UpdateClient;
      FClient.startLocationUpdates;
    end
    // else invoke some error thingy
  end;
end;

procedure TPlatformLocationMonitor.UpdateClient;
begin
  FClient.setInterval(Interval);
  FClient.setFastestInterval(FastestInterval);
  FClient.setPriority(Priority);
  FClient.setSmallestDisplacement(Distance);
end;

function TPlatformLocationMonitor.GetLocationData(const ALocation: JLocation): TLocationData;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Location := TLocationCoord2D.Create(ALocation.getLatitude, ALocation.getLongitude);
  if ALocation.hasAccuracy then
  begin
    Include(Result.Flags, TLocationDataFlag.Accuracy);
    Result.Accuracy := ALocation.getAccuracy;
  end;
  if ALocation.hasAltitude then
  begin
    Include(Result.Flags, TLocationDataFlag.Altitude);
    Result.Altitude := ALocation.getAltitude;
  end;
  if ALocation.hasBearing then
  begin
    Include(Result.Flags, TLocationDataFlag.Bearing);
    Result.Bearing := ALocation.getBearing;
  end;
  if ALocation.hasSpeed then
  begin
    Include(Result.Flags, TLocationDataFlag.Speed);
    Result.Speed := ALocation.getSpeed;
  end
  else if (FLastData.DateTime > 0) and (FLastData.Location.Latitude <> cInvalidLatitude) then
  begin
    Include(Result.Flags, TLocationDataFlag.Speed);
    Result.Speed := TGeodetic.DistanceBetween(FLastData.Location, Result.Location) / SecondsBetween(Now, FLastData.DateTime);
  end;
end;

procedure TPlatformLocationMonitor.LocationChange(const ALocation: JLocation);
begin
  DoLocationChanged(GetLocationData(ALocation));
end;

end.

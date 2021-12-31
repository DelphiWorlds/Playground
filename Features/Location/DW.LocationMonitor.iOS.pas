unit DW.LocationMonitor.iOS;

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

interface

uses
  // RTL
  System.Sensors, System.Classes, System.TypInfo,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CoreLocation, iOSapi.Foundation,
  // DW
  DW.LocationMonitor;

type
  IApplicationActiveNotifications = interface(NSObject)
    ['{BCEDAA3F-B9AB-4A77-B7BC-9457C3A304BD}']
    procedure willEnterForeground(notification: Pointer); cdecl;
    procedure didEnterBackground(notification: Pointer); cdecl;
  end;

  TApplicationActiveNotifier = class(TOCLocal)
  private
    class var FIsLaunched: Boolean;
  private
    FIsActive: Boolean;
    FOnChange: TNotifyEvent;
    procedure DoChange;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  protected
    class property IsLaunched: Boolean read FIsLaunched;
  public
    { IApplicationActiveNotifications }
    procedure willEnterForeground(notification: Pointer); cdecl;
    procedure didEnterBackground(notification: Pointer); cdecl;
  public
    constructor Create;
    destructor Destroy; override;
    property IsActive: Boolean read FIsActive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TPlatformLocationMonitor = class;

  TLocationManagerDelegate = class(TOCLocal, CLLocationManagerDelegate)
  private
    FMonitor: TPlatformLocationMonitor;
    // FPreviousLocation: TLocationCoord2D;
  public
    { CLLocationManagerDelegate }
    procedure locationManager(manager: CLLocationManager; didFailWithError: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateHeading: CLHeading); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateToLocation: CLLocation; fromLocation: CLLocation); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; monitoringDidFailForRegion: CLRegion; withError: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didChangeAuthorizationStatus: CLAuthorizationStatus); overload; cdecl;
    [MethodName('locationManager:didUpdateLocations:')]
    procedure locationManagerDidUpdateLocations(manager: CLLocationManager; locations: NSArray); cdecl;
    function locationManagerShouldDisplayHeadingCalibration(manager: CLLocationManager): Boolean; cdecl;
    [MethodName('locationManager:didDetermineState:forRegion:')]
    procedure locationManagerDidDetermineStateForRegion(manager: CLLocationManager; state: CLRegionState; region: CLRegion); cdecl;
    [MethodName('locationManager:didRangeBeacons:inRegion:')]
    procedure locationManagerDidRangeBeaconsInRegion(manager: CLLocationManager; beacons: NSArray; region: CLBeaconRegion); cdecl;
    [MethodName('locationManager:rangingBeaconsDidFailForRegion:withError:')]
    procedure locationManagerRangingBeaconsDidFailForRegionWithError(manager: CLLocationManager; region: CLBeaconRegion; error: NSError); cdecl;
    [MethodName('locationManager:didEnterRegion:')]
    procedure locationManagerDidEnterRegion(manager: CLLocationManager; region: CLRegion); cdecl;
    [MethodName('locationManager:didExitRegion:')]
    procedure locationManagerDidExitRegion(manager: CLLocationManager; region: CLRegion); cdecl;
    [MethodName('locationManager:didStartMonitoringForRegion:')]
    procedure locationManagerDidStartMonitoringForRegion(manager: CLLocationManager; region: CLRegion); cdecl;
    procedure locationManagerDidPauseLocationUpdates(manager: CLLocationManager); cdecl;
    procedure locationManagerDidResumeLocationUpdates(manager: CLLocationManager); cdecl;
    [MethodName('locationManager:didFinishDeferredUpdatesWithError:')]
    procedure locationManagerDidFinishDeferredUpdatesWithError(manager: CLLocationManager; error: NSError); cdecl;
  public
    constructor Create(const AMonitor: TPlatformLocationMonitor);
  end;

  TLocationChangeMonitoring = (Normal, Significant);

  TPlatformLocationMonitor = class(TCustomPlatformLocationMonitor)
  private
    const
      cActivityValues: array[TLocationActivityType] of CLActivityType = (
        CLActivityTypeOther, CLActivityTypeAutomotiveNavigation, CLActivityTypeFitness, CLActivityTypeOtherNavigation
      );
  private
    FActivityType: TLocationActivityType;
    FApplicationActiveNotifier: TApplicationActiveNotifier;
    FIsActive: Boolean;
    FLocationChangeMonitoring: TLocationChangeMonitoring;
    FLocationDelegate: TLocationManagerDelegate;
    FLocationManager: CLLocationManager;
    FRequestedActiveChange: Boolean;
    FUsageAuthorization: TLocationUsageAuthorization;
    function CheckPermission: Boolean;
    procedure DoApplicationActiveChanged(Sender: TObject);
    function HasAlwaysAuthorization: Boolean;
    function HasSignificantChangeMonitoring: Boolean;
    procedure RequestPermission;
    procedure Start(const APermissionCheck: Boolean);
    procedure StartMonitoring;
    procedure Stop;
    procedure StopMonitoring;
  protected
    procedure DidChangeAuthorizationStatus(const AAuthorizationStatus: CLAuthorizationStatus);
    procedure DidUpdateLocation(const ALocation: CLLocation);
    function GetAccuracy: Double; override;
    function GetActivityType: TLocationActivityType; override;
    function GetDistance: Double; override;
    function GetUsageAuthorization: TLocationUsageAuthorization; override;
    function GetIsActive: Boolean; override;
    procedure SetAccuracy(const Value: Double); override;
    procedure SetActivityType(const Value: TLocationActivityType); override;
    procedure SetDistance(const Value: Double); override;
    procedure SetIsActive(const AValue: Boolean); override;
    procedure SetUsageAuthorization(const Value: TLocationUsageAuthorization); override;
  public
    constructor Create(const ALocationMonitor: TLocationMonitor); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers,
  // iOS
  iOSapi.Helpers, iOSapi.UIKit,
  // DW
  DW.Location.Types;

{ TApplicationActiveNotifier }

constructor TApplicationActiveNotifier.Create;
begin
  inherited;
  FIsActive := TiOSHelper.SharedApplication.applicationState <> UIApplicationStateBackground;
  TiOSHelper.DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('willEnterForeground:'),
    NSObjectToID(UIApplicationWillEnterForegroundNotification), nil);
  TiOSHelper.DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('didEnterBackground:'),
    NSObjectToID(UIApplicationDidEnterBackgroundNotification), nil);
end;

destructor TApplicationActiveNotifier.Destroy;
begin
  TiOSHelper.DefaultNotificationCenter.removeObserver(GetObjectID);
  inherited;
end;

function TApplicationActiveNotifier.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IApplicationActiveNotifications);
end;

procedure TApplicationActiveNotifier.didEnterBackground(notification: Pointer);
begin
  FIsActive := False;
  DoChange;
end;

procedure TApplicationActiveNotifier.willEnterForeground(notification: Pointer);
begin
  FIsActive := True;
  FIsLaunched := True;
  DoChange;
end;

procedure TApplicationActiveNotifier.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TLocationManagerDelegate }

constructor TLocationManagerDelegate.Create(const AMonitor: TPlatformLocationMonitor);
begin
  inherited Create;
  FMonitor := AMonitor;
end;

procedure TLocationManagerDelegate.locationManager(manager: CLLocationManager; didChangeAuthorizationStatus: CLAuthorizationStatus);
begin
  FMonitor.DidChangeAuthorizationStatus(didChangeAuthorizationStatus);
end;

procedure TLocationManagerDelegate.locationManager(manager: CLLocationManager; monitoringDidFailForRegion: CLRegion; withError: NSError);
begin
  // TODO?
end;

procedure TLocationManagerDelegate.locationManager(manager: CLLocationManager; didUpdateToLocation, fromLocation: CLLocation);
begin
  // Handled in locationManagerDidUpdateLocations
end;

procedure TLocationManagerDelegate.locationManager(manager: CLLocationManager; didUpdateHeading: CLHeading);
begin
  // Possible future implementation
end;

procedure TLocationManagerDelegate.locationManager(manager: CLLocationManager; didFailWithError: NSError);
begin
  // TODO?
end;

procedure TLocationManagerDelegate.locationManagerDidDetermineStateForRegion(manager: CLLocationManager; state: CLRegionState; region: CLRegion);
begin
  // TODO?
end;

procedure TLocationManagerDelegate.locationManagerDidEnterRegion(manager: CLLocationManager; region: CLRegion);
begin
  // TODO
end;

procedure TLocationManagerDelegate.locationManagerDidExitRegion(manager: CLLocationManager; region: CLRegion);
begin
  // TODO
end;

procedure TLocationManagerDelegate.locationManagerDidFinishDeferredUpdatesWithError(manager: CLLocationManager; error: NSError);
begin
  // Not implemented
end;

procedure TLocationManagerDelegate.locationManagerDidPauseLocationUpdates(manager: CLLocationManager);
begin
  // TODO
end;

procedure TLocationManagerDelegate.locationManagerDidRangeBeaconsInRegion(manager: CLLocationManager; beacons: NSArray; region: CLBeaconRegion);
begin
  // TODO
end;

procedure TLocationManagerDelegate.locationManagerDidResumeLocationUpdates(manager: CLLocationManager);
begin
  // TODO
end;

procedure TLocationManagerDelegate.locationManagerDidStartMonitoringForRegion(manager: CLLocationManager; region: CLRegion);
begin
  // TODO
end;

procedure TLocationManagerDelegate.locationManagerDidUpdateLocations(manager: CLLocationManager; locations: NSArray);
begin
  FMonitor.DidUpdateLocation(TCLLocation.Wrap(locations.lastObject));
end;

procedure TLocationManagerDelegate.locationManagerRangingBeaconsDidFailForRegionWithError(manager: CLLocationManager; region: CLBeaconRegion;
  error: NSError);
begin
  // Not implemented
end;

function TLocationManagerDelegate.locationManagerShouldDisplayHeadingCalibration(manager: CLLocationManager): Boolean;
begin
  Result := False;
end;

{ TPlatformLocationMonitor }

constructor TPlatformLocationMonitor.Create(const ALocationMonitor: TLocationMonitor);
begin
  inherited;
  FApplicationActiveNotifier := TApplicationActiveNotifier.Create;
  FApplicationActiveNotifier.OnChange := DoApplicationActiveChanged;
  FLocationDelegate := TLocationManagerDelegate.Create(Self);
  FLocationManager := TCLLocationManager.Create;
  FLocationManager.retain;
  FLocationManager.setDelegate(FLocationDelegate.GetObjectID);
end;

destructor TPlatformLocationMonitor.Destroy;
begin
  FLocationManager.setDelegate(nil);
  FLocationManager.release;
  FLocationDelegate.Free;
  inherited;
end;

function TPlatformLocationMonitor.CheckPermission: Boolean;
begin
  Result := False;
  case TCLLocationManager.OCClass.authorizationStatus of
    kCLAuthorizationStatusNotDetermined:
      RequestPermission;
    kCLAuthorizationStatusAuthorized, kCLAuthorizationStatusAuthorizedWhenInUse:
      Result := True;
  end;
end;

procedure TPlatformLocationMonitor.RequestPermission;
begin
  case FUsageAuthorization of
    TLocationUsageAuthorization.WhenInUse:
      FLocationManager.requestWhenInUseAuthorization;
    TLocationUsageAuthorization.Always:
      FLocationManager.requestAlwaysAuthorization;
  end;
end;

procedure TPlatformLocationMonitor.DidChangeAuthorizationStatus(const AAuthorizationStatus: CLAuthorizationStatus);
begin
  case AAuthorizationStatus of
    kCLAuthorizationStatusAuthorized, kCLAuthorizationStatusAuthorizedWhenInUse:
    begin
      if FRequestedActiveChange then
        Start(False);
    end;
    kCLAuthorizationStatusDenied, kCLAuthorizationStatusRestricted:
      Stop;
  end;
end;

procedure TPlatformLocationMonitor.DoApplicationActiveChanged(Sender: TObject);
begin
  // Changes level of monitoring depending on the application state.
  if FIsActive then
    StartMonitoring;
end;

procedure TPlatformLocationMonitor.Start(const APermissionCheck: Boolean);
begin
  if not APermissionCheck or CheckPermission then
  begin
    if TOSVersion.Check(9) and TiOSHelper.HasBackgroundMode('location') then
      FLocationManager.setAllowsBackgroundLocationUpdates(True);
    StartMonitoring;
    FRequestedActiveChange := False;
    FIsActive := True;
  end;
end;

procedure TPlatformLocationMonitor.StartMonitoring;
begin
  FLocationManager.setPausesLocationUpdatesAutomatically(FApplicationActiveNotifier.IsActive);
  if HasSignificantChangeMonitoring and HasAlwaysAuthorization and not FApplicationActiveNotifier.IsActive then
  begin
    FLocationManager.startMonitoringSignificantLocationChanges;
    FLocationChangeMonitoring := TLocationChangeMonitoring.Significant;
  end
  else
  begin
    FLocationManager.startUpdatingLocation;
    FLocationChangeMonitoring := TLocationChangeMonitoring.Normal;
  end;
end;

procedure TPlatformLocationMonitor.Stop;
begin
  StopMonitoring;
  FIsActive := False;
  DoStateChanged;
end;

procedure TPlatformLocationMonitor.StopMonitoring;
begin
  if FLocationChangeMonitoring = TLocationChangeMonitoring.Significant then
    FLocationManager.stopMonitoringSignificantLocationChanges
  else
    FLocationManager.stopUpdatingLocation;
end;

function TPlatformLocationMonitor.GetAccuracy: Double;
begin
  Result := FLocationManager.desiredAccuracy;
end;

function TPlatformLocationMonitor.GetActivityType: TLocationActivityType;
begin
  Result := FActivityType;
end;

function TPlatformLocationMonitor.GetDistance: Double;
begin
  Result := FLocationManager.distanceFilter;
end;

function TPlatformLocationMonitor.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

function TPlatformLocationMonitor.GetUsageAuthorization: TLocationUsageAuthorization;
begin
  Result := FUsageAuthorization;
end;

function TPlatformLocationMonitor.HasAlwaysAuthorization: Boolean;
begin
  Result := TCLLocationManager.OCClass.authorizationStatus = kCLAuthorizationStatusAuthorizedAlways;
end;

function TPlatformLocationMonitor.HasSignificantChangeMonitoring: Boolean;
begin
  Result := TCLLocationManager.OCClass.significantLocationChangeMonitoringAvailable;
end;

procedure TPlatformLocationMonitor.SetAccuracy(const Value: Double);
begin
  FLocationManager.setDesiredAccuracy(Value);
end;

procedure TPlatformLocationMonitor.SetActivityType(const Value: TLocationActivityType);
begin
  FActivityType := Value;
  FLocationManager.setActivityType(cActivityValues[FActivityType]);
end;

procedure TPlatformLocationMonitor.SetDistance(const Value: Double);
begin
  FLocationManager.setDistanceFilter(Value);
end;

procedure TPlatformLocationMonitor.SetIsActive(const AValue: Boolean);
begin
  if not FRequestedActiveChange and (AValue <> FIsActive) then
  begin
    if FIsActive then
      Stop
    else
    begin
      FRequestedActiveChange := True;
      Start(True);
    end;
  end;
end;

procedure TPlatformLocationMonitor.SetUsageAuthorization(const Value: TLocationUsageAuthorization);
begin
  if Value <> FUsageAuthorization then
  begin
    FUsageAuthorization := Value;
    if FIsActive then
      RequestPermission;
  end;
end;

procedure TPlatformLocationMonitor.DidUpdateLocation(const ALocation: CLLocation);
var
  LData: TLocationData;
begin
  LData.Accuracy := ALocation.horizontalAccuracy;
  LData.Altitude := ALocation.altitude;
  LData.Bearing := ALocation.course;
  LData.Location := TLocationCoord2D.Create(ALocation.coordinate.latitude, ALocation.coordinate.longitude);
  LData.DateTime := NSDateToDate(ALocation.timestamp);
  LData.Speed := ALocation.speed;
  LData.Flags := [TLocationDataFlag.Accuracy, TLocationDataFlag.Altitude, TLocationDataFlag.Bearing, TLocationDataFlag.Speed];
  if not FApplicationActiveNotifier.IsActive then
    LData.ApplicationState := TLocationApplicationState.Background;
  // This means that iOS launched the application into the background, but it is not shown
  if not TApplicationActiveNotifier.IsLaunched then
    LData.ApplicationState := TLocationApplicationState.Hidden;
  DoLocationChanged(LData);
end;

end.

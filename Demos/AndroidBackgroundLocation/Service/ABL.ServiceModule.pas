unit ABL.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service, System.JSON,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.Location, Androidapi.JNI.JavaTypes,
  DW.FusedLocation,
  DW.LocationService.Common, ABL.LocationSender;

type
  TServiceModule = class(TAndroidService, IFusedLocationOwner)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceCreate(Sender: TObject);
  private
    FFusedLocation: IFusedLocation;
    FLocationSender: ILocationSender;
    function CheckServiceInfo(const AInfo: TLocationServiceInfo): Boolean;
    function CanStartForeground: Boolean;
  public
    { IFusedLocationOwner }
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.Sensors, System.DateUtils, System.Permissions,
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.Helpers,
  DW.OSLog,
  DW.Android.Helpers, DW.ForegroundServiceHelper, DW.Location.Types, DW.Consts.Android,
  DW.BroadcastMessage.Sender;

type
  TLocationDataHelper = record helper for TLocationData
    procedure FromLocation(const ALocation: JLocation);
  end;

{ TLocationDataHelper }

procedure TLocationDataHelper.FromLocation(const ALocation: JLocation);
begin
  Self := Default(TLocationData);
  // For some weird reason, ALocation.getTime is returning a value millenia into the future?? Or UnixToDateTime is broken.
  // DateTime := UnixToDateTime(ALocation.getTime); // UTC
  DateTime := TTimeZone.Local.ToUniversalTime(Now);
  Location := TLocationCoord2D.Create(ALocation.getLatitude, ALocation.getLongitude);
  IsMocked := ALocation.isFromMockProvider;
  if ALocation.hasAccuracy then
  begin
    Include(Flags, TLocationDataFlag.Accuracy);
    Accuracy := ALocation.getAccuracy;
  end;
  if ALocation.hasAltitude then
  begin
    Include(Flags, TLocationDataFlag.Altitude);
    Altitude := ALocation.getAltitude;
  end;
  if ALocation.hasBearing then
  begin
    Include(Flags, TLocationDataFlag.Bearing);
    Bearing := ALocation.getBearing;
  end;
  if ALocation.hasSpeed then
  begin
    Include(Flags, TLocationDataFlag.Speed);
    Speed := ALocation.getSpeed;
  end;
end;

// TODO:
//   Start service from boot

// Highest priority:
//   Remain running for long periods
//   Avoid app "hangs"

{ TServiceModule }

procedure TServiceModule.AndroidServiceCreate(Sender: TObject);
begin
  FFusedLocation := TFusedLocation.Create(Self);
  FLocationSender := TLocationSender.Create;
  // Set a URL that points to a REST server that can store location data.
  FLocationSender.URL := 'https://radsoft.com.au:8086/locations/add';
end;

function TServiceModule.CanStartForeground: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(cPermissionAccessBackgroundLocation);
end;

function TServiceModule.CheckServiceInfo(const AInfo: TLocationServiceInfo): Boolean;
begin
  Result := AInfo.IsServiceInfo;
  if Result then
  begin
    if AInfo.IsActive then
      FFusedLocation.Start(AInfo.Options)
    else
      FFusedLocation.Stop;
  end;
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
var
  LInfo: TLocationServiceInfo;
begin
  LInfo := TLocationServiceInfo.Create(Intent);
  // If the service was *not* started from the app, just start it in the foreground.
  // There is no RELIABLE way to know whether the app is in the foreground - the system could kill it and not be able to persist its state anywhere
  if CanStartForeground and not LInfo.IsServiceInfo then
    TForegroundServiceHelper.StartForeground(JavaService, 'ABL Demo', 'Service in foreground');
  CheckServiceInfo(LInfo);
  FFusedLocation.CheckIntent(Intent);
  // START_NOT_STICKY is returned, as the service does not need to continue running if the application is.
  Result := TJService.JavaClass.START_NOT_STICKY;
end;

procedure TServiceModule.LocationReceived(const ALocation: JLocation);
var
  LData: TLocationData;
begin
  LData.FromLocation(ALocation);
  TOSLog.d('Received: %.5f, %.5f', [LData.Location.Latitude, LData.Location.Longitude]);
  // Uncomment this line when using a REST server that can update location data
  // FLocationSender.SendLocation(LData);
  // Message type 0 is a notification of a location update
  TBroadcastMessageSender.Send(0, LData.ToJSON);
end;

procedure TServiceModule.LocationUpdatesChange(const AIsActive: Boolean);
begin
  // Message type 1 is a notification that the location updates have started/stopped
  TOSLog.d('TServiceModule.LocationUpdatesChange - AIsActive: %s', [BoolToStr(AIsActive, True)]);
  TBroadcastMessageSender.Send(1, '');
end;

end.

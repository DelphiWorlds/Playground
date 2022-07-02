unit DW.LocationRequester;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2022 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

// LocationRequester provides "one-shot" location requests, i.e. the location services are used only when required,
// as opposed to continuous location updates.
//
// Permissions requests for location services are made only at the time they are needed
// This code uses a workaround for an issue where location updates could otherwise start before the user has authorized them

interface

uses
  System.Sensors, System.Sensors.Components;

const
  cLocationStatePermissionDenied = 'location services permission was denied';
  cLocationStateServicesDisabled = 'location services has been disabled at the device level';
  cLocationStateSensorTimeout = 'location services did not respond in time';
  cLocationStateServicesFailed = 'location services could not obtain a location';

  cLocationSinceDefault = 30000;
  cSamplesTimeoutDefault = 5000;

type
  TLocationCoord2D = System.Sensors.TLocationCoord2D;

  TLocationResponseState = (OK, AccessDenied, LocationDisabled, LocationTimeout, Error);
  TLocationResponseMethod = reference to procedure(const State: TLocationResponseState; const Location: TLocationCoord2D);

  TLocationRequestParams = record
  public
    LocationSince: Integer; // Milliseconds
    LocationTimeout: Integer; // Milliseconds
    MaxSamples: Integer;
    MinDistance: Integer; // Metres
    SamplesTimeout: Integer; // Milliseconds
    class function CreateMultiple(const AMaxSamples: Integer = 5; const ASamplesTimeout: Integer = cSamplesTimeoutDefault;
      const AMinDistance: Integer = 5): TLocationRequestParams; static;
    class function CreateSingle(const ALocationSince: Integer = 0; const ALocationTimeout: Integer = 0): TLocationRequestParams; static;
  end;

  ILocationRequester = interface(IInterface)
    ['{AD709426-D4DD-4EF9-9495-64948369EEAC}']
    function GetLocations: TArray<TLocationCoord2D>;
    /// <summary>
    ///   Request a location that is optimized for the given parameters, or respond with an error where applicable
    /// </summary>
    /// <remarks>
    ///   The method will return when the number of samples gathered has reached the MaxSamples value, or a timeout has occurred
    ///   The MinDistance value specifies the minimum distance between one sample and the next, to be considered a viable sample
    /// </remarks>
    procedure Request(const AParams: TLocationRequestParams; const AResponseMethod: TLocationResponseMethod);
    property Locations: TArray<TLocationCoord2D> read GetLocations;
  end;

  TLocationCoord2DHelper = record helper for TLocationCoord2D
    function AsString(const ADecimals: Integer = 6): string;
  end;

const
  cLocationStateErrorMessage: array[TLocationResponseState] of string = (
    '',
    cLocationStatePermissionDenied,
    cLocationStateServicesDisabled,
    cLocationStateSensorTimeout,
    cLocationStateServicesFailed
  );

var
  LocationRequester: ILocationRequester;

implementation

{$IF not (Defined(IOS) or Defined(ANDROID))}
  {$DEFINE SIMULATED}
{$ENDIF}

uses
  System.Math,
  System.SysUtils, System.Messaging, System.DateUtils, System.Permissions, System.Types,
  {$IF Defined(IOS)}
  iOSapi.CoreLocation,
  {$ENDIF}
  FMX.Platform, FMX.Types,
  DW.OSLog,
  DW.SlackWebHook,
  DW.Permissions.Helpers, DW.Geodetic, DW.OSDevice;

const
  cPermissionAccessCoarseLocation = 'android.permission.ACCESS_COARSE_LOCATION';
  cPermissionAccessFineLocation = 'android.permission.ACCESS_FINE_LOCATION';

  cMinimumLocationDistanceChangeDefault = 5;
  cLocationTimeoutDefault = 10000;

type
  TLocationRequester = class(TInterfacedObject, ILocationRequester)
  private
    FIsAuthorizing: Boolean;
    FLocation: TLocationCoord2D;
    FLocations: TArray<TLocationCoord2D>;
    FLocationAcquired: TDateTime;
    FRequestParams: TLocationRequestParams;
    FResponseMethod: TLocationResponseMethod;
    FSensor: TLocationSensor;
    FSamplesTimer: TTimer;
    FSensorTimer: TTimer;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure CreateSensor;
    procedure ExecuteResponse(const AState: TLocationResponseState; const ALocation: TLocationCoord2D);
    function GetLocations: TArray<TLocationCoord2D>;
    function GetOptimalLocation: TLocationCoord2D;
    procedure SensorLocationChangedHandler(Sender: TObject; const AOldLocation, ANewLocation: TLocationCoord2D);
    procedure SensorStateChangedHandler(Sender: TObject);
    procedure SamplesTimerHandler(Sender: TObject);
    procedure SensorTimerHandler(Sender: TObject);
    procedure Start;
    procedure StartSensor;
    procedure Stop;
  public
    { ILocationService }
    procedure Request(const AParams: TLocationRequestParams; const AResponseMethod: TLocationResponseMethod);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TLocationRequestParams }

class function TLocationRequestParams.CreateMultiple(const AMaxSamples: Integer = 5; const ASamplesTimeout: Integer = 5000;
  const AMinDistance: Integer = 5): TLocationRequestParams;
begin
  Result.LocationSince := 0; // Not applicable
  Result.MaxSamples := AMaxSamples;
  Result.MinDistance := AMinDistance;
  Result.SamplesTimeout := ASamplesTimeout;
  Result.LocationTimeout := 0; // Not applicable
end;

class function TLocationRequestParams.CreateSingle(const ALocationSince: Integer = 0; const ALocationTimeout: Integer = 0): TLocationRequestParams;
begin
  Result.LocationSince := ALocationSince;
  Result.MaxSamples := 1;
  Result.MinDistance := 0; // Not applicable
  Result.SamplesTimeout := 0; // Not applicable
  Result.LocationTimeout := ALocationTimeout;
end;

{ TLocationRequester }

constructor TLocationRequester.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  // Used for a scenario of when the sensor *never* errors, but does not provide a location within a reasonable amount of time
  FSensorTimer := TTimer.Create(nil);
  FSensorTimer.Enabled := False;
  FSensorTimer.OnTimer := SensorTimerHandler;
  // Used as a timeout for gathering multiple location samples
  FSamplesTimer := TTimer.Create(nil);
  FSamplesTimer.Enabled := False;
  FSamplesTimer.OnTimer := SamplesTimerHandler;
end;

destructor TLocationRequester.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FSensor.Free;
  FSensorTimer.Free;
  inherited;
end;

procedure TLocationRequester.SamplesTimerHandler(Sender: TObject);
begin
  TOSLog.d('Samples timeout');
  Stop;
  if Length(FLocations) > 0 then
    ExecuteResponse(TLocationResponseState.OK, GetOptimalLocation)
  else
    ExecuteResponse(TLocationResponseState.Error, TLocationCoord2D.Create(0, 0));
end;

procedure TLocationRequester.SensorTimerHandler(Sender: TObject);
begin
  Stop;
  ExecuteResponse(TLocationResponseState.LocationTimeout, TLocationCoord2D.Create(0, 0));
end;

procedure TLocationRequester.CreateSensor;
begin
  if FSensor = nil then
  begin
    FSensor := TLocationSensor.Create(nil);
    FSensor.OnStateChanged := SensorStateChangedHandler;
    FSensor.OnLocationChanged := SensorLocationChangedHandler;
  end;
end;

procedure TLocationRequester.Start;
begin
  {$IF Defined(IOS)}
  // The following code works around an issue where location updates start before the user has authorized them
  // See also the ApplicationEventMessageHandler method
  case FSensor.Authorized of
    TAuthorizationType.atNotSpecified:
    begin
      FIsAuthorizing := True;
      // Requesting authorization causes the app to become inactive while the prompt shows, then active once they have responded
      if FSensor.UsageAuthorization = TLocationUsageAuthorization.WhenInUse then
        TCLLocationManager.Create.requestWhenInUseAuthorization
      else
        TCLLocationManager.Create.requestAlwaysAuthorization;
    end;
    TAuthorizationType.atAuthorized:
      StartSensor;
  end;
  {$ELSEIF Defined(ANDROID)}
  PermissionsService.RequestPermissions([cPermissionAccessCoarseLocation, cPermissionAccessFineLocation],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        StartSensor
      else
        ExecuteResponse(TLocationResponseState.AccessDenied, TLocationCoord2D.Create(0, 0));
    end
  );
  {$ENDIF}
end;

procedure TLocationRequester.StartSensor;
begin
  if not FSensor.Active then
  begin
    FSamplesTimer.Interval := FRequestParams.SamplesTimeout;
    FSamplesTimer.Enabled := FSamplesTimer.Interval > 0;
    FSensorTimer.Interval := FRequestParams.LocationTimeout;
    FSensorTimer.Enabled := FSensorTimer.Interval > 0;
    TOSLog.d('Starting sensor');
    FSensor.Active := True;
  end;
end;

procedure TLocationRequester.Stop;
begin
  FSensorTimer.Enabled := False;
  FSamplesTimer.Enabled := False;
  FSensor.Active := False;
end;

procedure TLocationRequester.SensorLocationChangedHandler(Sender: TObject; const AOldLocation, ANewLocation: TLocationCoord2D);
var
  LCount: Integer;
begin
  if (ANewLocation.Latitude <> 0) or (ANewLocation.Longitude <> 0) then
  begin
    FSensorTimer.Enabled := False;
    LCount := Length(FLocations);
    if (LCount = 0) or (TGeodetic.DistanceBetween(ANewLocation, FLocations[LCount - 1]) > FRequestParams.MinDistance) then
    begin
      FLocation := ANewLocation;
      FLocations := FLocations + [FLocation];
      TOSLog.d('Received location #%d of %d: %s', [LCount + 1, FRequestParams.MaxSamples, FLocation.AsString]);
      FLocationAcquired := Now;
      if Length(FLocations) = FRequestParams.MaxSamples then
      begin
        Stop;
        ExecuteResponse(TLocationResponseState.OK, GetOptimalLocation);
      end;
    end;
  end
  else
    TOSLog.d('Received 0, 0 location');
end;

procedure TLocationRequester.SensorStateChangedHandler(Sender: TObject);
var
  LState: TLocationResponseState;
begin
  if FSensor.Sensor <> nil then
  begin
    LState := TLocationResponseState.OK;
    case FSensor.Sensor.State of
      TSensorState.AccessDenied:
        LState := TLocationResponseState.AccessDenied;
      TSensorState.Error:
        LState := TLocationResponseState.Error;
    end;
    if LState <> TLocationResponseState.OK then
    begin
      FSensor.Active := False;
      ExecuteResponse(LState, TLocationCoord2D.Create(0, 0));
    end;
  end;
end;

procedure TLocationRequester.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      // The following code works around an issue where location updates start before the user has authorized them
      // See also the Start method
      if FIsAuthorizing then
      begin
        FIsAuthorizing := False;
        if FSensor.Authorized = TAuthorizationType.atAuthorized then
          FSensor.Active := True
        else
          ExecuteResponse(TLocationResponseState.AccessDenied, TLocationCoord2D.Create(0, 0));
      end
    end;
  end;
end;

procedure TLocationRequester.ExecuteResponse(const AState: TLocationResponseState; const ALocation: TLocationCoord2D);
begin
  if Assigned(FResponseMethod) then
  try
    FResponseMethod(AState, ALocation);
  finally
    FResponseMethod := nil;
  end;
end;

function TLocationRequester.GetLocations: TArray<TLocationCoord2D>;
begin
  Result := FLocations;
end;

function TLocationRequester.GetOptimalLocation: TLocationCoord2D;
begin
  {$IF not Defined(SIMULATED)}
  Result := FLocations[Length(FLocations) - 1];
  {$ELSE}
  Result := TLocationCoord2D.Create(0, 0);
  {$ENDIF}
end;

procedure TLocationRequester.Request(const AParams: TLocationRequestParams; const AResponseMethod: TLocationResponseMethod);
begin
  // Prevent additional requests while one is pending
  if not Assigned(FResponseMethod) then
  begin
    {$IF not Defined(SIMULATED)}
    FRequestParams := AParams;
    if FRequestParams.MaxSamples > 0 then
      FRequestParams.MaxSamples := AParams.MaxSamples
    else
      FRequestParams.MaxSamples := 1;
    FRequestParams.MinDistance := Max(FRequestParams.MinDistance, cMinimumLocationDistanceChangeDefault);
    FLocations := [];
    if TOSDevice.IsLocationServiceEnabled then
    begin
      CreateSensor;
      if FSensor.Authorized <> TAuthorizationType.atUnauthorized then
      begin
        if (FLocationAcquired = 0) or (MilliSecondsBetween(Now, FLocationAcquired) > FRequestParams.LocationSince) then
        begin
          FResponseMethod := AResponseMethod;
          Start;
        end
        else
          AResponseMethod(TLocationResponseState.OK, FLocation);
      end
      else
        AResponseMethod(TLocationResponseState.AccessDenied, TLocationCoord2D.Create(0, 0));
    end
    else
      AResponseMethod(TLocationResponseState.LocationDisabled, TLocationCoord2D.Create(0, 0));
    {$ELSE}
    AResponseMethod(TLocationResponseState.OK, TLocationCoord2D.Create(30.3976043, -97.7317599));
    {$ENDIF}
  end
  else
    TOSLog.d('Request in progress');
end;

{ TLocationCoord2DHelper }

function TLocationCoord2DHelper.AsString(const ADecimals: Integer = 6): string;
begin
  Result := Format('%.' + ADecimals.ToString + 'f, %.' + ADecimals.ToString + 'f', [Latitude, Longitude]);
end;

initialization
  LocationRequester := TLocationRequester.Create;

end.

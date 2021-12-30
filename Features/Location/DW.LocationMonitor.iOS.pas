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

// *** NOTE ***: Requires dw-fusedlocation.jar (in the Lib folder) to be added to the Libraries node under the Android platform in Project Manager

interface

uses
  // RTL
  System.Sensors, System.Sensors.Components,
  // DW
  DW.LocationMonitor, DW.Sensors;

type
  TPlatformLocationMonitor = class(TCustomPlatformLocationMonitor)
  private
    FSensor: TLocationSensor;
    procedure SensorLocationChangedHandler(Sender: TObject; const AOldLocation, ANewLocation: TLocationCoord2D);
  protected
    function GetAccuracy: Double; override;
    function GetActivityType: TLocationActivityType; override;
    function GetDistance: Double; override;
    function GetUsageAuthorization: TLocationUsageAuthorization; override;
    function GetIsActive: Boolean; override;
    procedure LocationUpdatesChange(const AActive: Boolean);
    procedure SetAccuracy(const Value: Double); override;
    procedure SetActivityType(const Value: TLocationActivityType); override;
    procedure SetDistance(const Value: Double); override;
    procedure SetUsageAuthorization(const Value: TLocationUsageAuthorization); override;
    procedure SetIsActive(const AValue: Boolean); override;
  public
    constructor Create(const ALocationMonitor: TLocationMonitor); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.Location.Types;

{ TPlatformLocationMonitor }

constructor TPlatformLocationMonitor.Create(const ALocationMonitor: TLocationMonitor);
begin
  inherited;
  FSensor := TLocationSensor.Create(nil);
  FSensor.OnLocationChanged := SensorLocationChangedHandler;
end;

destructor TPlatformLocationMonitor.Destroy;
begin
  FSensor.Free;
  inherited;
end;

procedure TPlatformLocationMonitor.LocationUpdatesChange(const AActive: Boolean);
begin
  // FRequestedActiveChange := False;
  DoStateChanged;
end;

function TPlatformLocationMonitor.GetAccuracy: Double;
begin
  Result := FSensor.Accuracy;
end;

function TPlatformLocationMonitor.GetActivityType: TLocationActivityType;
begin
  Result := FSensor.ActivityType;
end;

function TPlatformLocationMonitor.GetDistance: Double;
begin
  Result := FSensor.Distance;
end;

function TPlatformLocationMonitor.GetIsActive: Boolean;
begin
  Result := FSensor.Active;
end;

function TPlatformLocationMonitor.GetUsageAuthorization: TLocationUsageAuthorization;
begin
  Result := FSensor.UsageAuthorization;
end;

procedure TPlatformLocationMonitor.SetAccuracy(const Value: Double);
begin
  FSensor.Accuracy := Value;
end;

procedure TPlatformLocationMonitor.SetActivityType(const Value: TLocationActivityType);
begin
  FSensor.ActivityType := Value;
end;

procedure TPlatformLocationMonitor.SetDistance(const Value: Double);
begin
  FSensor.Distance := Value;
end;

procedure TPlatformLocationMonitor.SetIsActive(const AValue: Boolean);
begin
{
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
}
end;

procedure TPlatformLocationMonitor.SetUsageAuthorization(const Value: TLocationUsageAuthorization);
begin
  FSensor.UsageAuthorization := Value;
end;

procedure TPlatformLocationMonitor.SensorLocationChangedHandler(Sender: TObject; const AOldLocation, ANewLocation: TLocationCoord2D);
var
  LData: TLocationData;
begin
  LData.Location := ANewLocation;
  if FSensor.Sensor <> nil then
  begin
    Include(LData.Flags, TLocationDataFlag.Accuracy);
    LData.Accuracy := FSensor.Sensor.Accuracy;
    if TCustomLocationSensor.TProperty.Altitude in FSensor.Sensor.AvailableProperties then
    begin
      Include(LData.Flags, TLocationDataFlag.Altitude);
      LData.Altitude := FSensor.Sensor.Altitude;
    end;
    if TCustomLocationSensor.TProperty.TrueHeading in FSensor.Sensor.AvailableProperties then
    begin
      Include(LData.Flags, TLocationDataFlag.Bearing);
      LData.Bearing := FSensor.Sensor.TrueHeading;
    end;
    if TCustomLocationSensor.TProperty.Speed in FSensor.Sensor.AvailableProperties then
    begin
      Include(LData.Flags, TLocationDataFlag.Speed);
      LData.Speed := FSensor.Sensor.Speed;
    end;
  end;
  DoLocationChanged(LData);
end;

end.

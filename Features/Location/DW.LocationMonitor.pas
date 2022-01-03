unit DW.LocationMonitor;

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
  System.Sensors, System.Classes,
  // DW
  DW.Location.Types;

type
  TLocationMonitor = class;

  TCustomPlatformLocationMonitor = class(TObject)
  private
    FAlarmInterval: Int64;
    FLocationMonitor: TLocationMonitor;
    FFastestInterval: Int64;
    FInterval: Int64;
    FNeedsBackgroundAccess: Boolean;
    FPriority: Integer;
  protected
    procedure DoLocationChanged(const AData: TLocationData); virtual;
    procedure DoStateChanged;
    function GetAccuracy: Double; virtual;
    function GetActivityType: TLocationActivityType; virtual;
    function GetDistance: Double; virtual;
    function GetUsageAuthorization: TLocationUsageAuthorization; virtual;
    function GetIsActive: Boolean; virtual;
    procedure SetAccuracy(const Value: Double); virtual;
    procedure SetActivityType(const Value: TLocationActivityType); virtual;
    procedure SetAlarmInterval(const Value: Int64); virtual;
    procedure SetDistance(const Value: Double); virtual;
    procedure SetUsageAuthorization(const Value: TLocationUsageAuthorization); virtual;
    procedure SetIsActive(const AValue: Boolean); virtual;
    property Accuracy: Double read GetAccuracy write SetAccuracy;
    property ActivityType: TLocationActivityType read GetActivityType write SetActivityType;
    property AlarmInterval: Int64 read FAlarmInterval write SetAlarmInterval;
    property Distance: Double read GetDistance write SetDistance;
    property FastestInterval: Int64 read FFastestInterval write FFastestInterval;
    property Interval: Int64 read FInterval write FInterval;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property NeedsBackgroundAccess: Boolean read FNeedsBackgroundAccess write FNeedsBackgroundAccess;
    property Priority: Integer read FPriority write FPriority;
    property UsageAuthorization: TLocationUsageAuthorization read GetUsageAuthorization write SetUsageAuthorization;
  public
    constructor Create(const ALocationMonitor: TLocationMonitor); virtual;
  end;

  TLocationMonitor = class(TObject)
  private
    FPlatformLocationMonitor: TCustomPlatformLocationMonitor;
    FOnLocationChanged: TLocationChangedEvent;
    FOnStateChanged: TNotifyEvent;
    FNeedsBackgroundAccess: Boolean;
    function GetAccuracy: Double;
    function GetActivityType: TLocationActivityType;
    function GetDistance: Double;
    function GetFastestInterval: Int64;
    function GetInterval: Int64;
    function GetIsActive: Boolean;
    function GetPriority: Integer;
    function GetUsageAuthorization: TLocationUsageAuthorization;
    procedure SetAccuracy(const Value: Double);
    procedure SetActivityType(const Value: TLocationActivityType);
    procedure SetDistance(const Value: Double);
    procedure SetFastestInterval(const Value: Int64);
    procedure SetInterval(const Value: Int64);
    procedure SetIsActive(const Value: Boolean);
    procedure SetPriority(const Value: Integer);
    procedure SetUsageAuthorization(const Value: TLocationUsageAuthorization);
    function GetAlarmInterval: Int64;
    procedure SetAlarmInterval(const Value: Int64);
  protected
    procedure DoLocationChanged(const AData: TLocationData);
    procedure DoStateChanged;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Accuracy property on iOS
    /// </summary>
    property Accuracy: Double read GetAccuracy write SetAccuracy;
    /// <summary>
    ///   ActivityType property on iOS
    /// </summary>
    property ActivityType: TLocationActivityType read GetActivityType write SetActivityType;
    property AlarmInterval: Int64 read GetAlarmInterval write SetAlarmInterval;
    /// <summary>
    ///   Distance property on iOS
    /// </summary>
    property Distance: Double read GetDistance write SetDistance;
    property FastestInterval: Int64 read GetFastestInterval write SetFastestInterval;
    property Interval: Int64 read GetInterval write SetInterval;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    /// <summary>
    ///   Determines whether or not Access Background Location is required on Android
    /// </summary>
    property NeedsBackgroundAccess: Boolean read FNeedsBackgroundAccess write FNeedsBackgroundAccess;
    property Priority: Integer read GetPriority write SetPriority;
    /// <summary>
    ///   UsageAuthorization property on iOS
    /// </summary>
    property UsageAuthorization: TLocationUsageAuthorization read GetUsageAuthorization write SetUsageAuthorization;
    property OnLocationChanged: TLocationChangedEvent read FOnLocationChanged write FOnLocationChanged;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

implementation

{$IF Defined(IOS)}
uses
  DW.LocationMonitor.iOS;
{$ELSEIF Defined(ANDROID)}
uses
  DW.LocationMonitor.Android;
{$ELSE}
type
  TPlatformLocation = class(TCustomPlatformLocationMonitor);
{$ENDIF}

{ TCustomPlatformLocationMonitor }

constructor TCustomPlatformLocationMonitor.Create(const ALocationMonitor: TLocationMonitor);
begin
  inherited Create;
  FLocationMonitor := ALocationMonitor;
end;

procedure TCustomPlatformLocationMonitor.DoLocationChanged(const AData: TLocationData);
begin
  FLocationMonitor.DoLocationChanged(AData);
end;

procedure TCustomPlatformLocationMonitor.DoStateChanged;
begin
  FLocationMonitor.DoStateChanged;
end;

function TCustomPlatformLocationMonitor.GetAccuracy: Double;
begin
  Result := 0;
end;

function TCustomPlatformLocationMonitor.GetActivityType: TLocationActivityType;
begin
  Result := TLocationActivityType.Other;
end;

function TCustomPlatformLocationMonitor.GetDistance: Double;
begin
  Result := 0;
end;

function TCustomPlatformLocationMonitor.GetIsActive: Boolean;
begin
  Result := False;
end;

function TCustomPlatformLocationMonitor.GetUsageAuthorization: TLocationUsageAuthorization;
begin
  Result := TLocationUsageAuthorization.WhenInUse;
end;

procedure TCustomPlatformLocationMonitor.SetAccuracy(const Value: Double);
begin
  //
end;

procedure TCustomPlatformLocationMonitor.SetActivityType(const Value: TLocationActivityType);
begin
  //
end;

procedure TCustomPlatformLocationMonitor.SetAlarmInterval(const Value: Int64);
begin
  FAlarmInterval := Value;
end;

procedure TCustomPlatformLocationMonitor.SetDistance(const Value: Double);
begin
  //
end;

procedure TCustomPlatformLocationMonitor.SetIsActive(const AValue: Boolean);
begin
  //
end;

procedure TCustomPlatformLocationMonitor.SetUsageAuthorization(const Value: TLocationUsageAuthorization);
begin
  //
end;

{ TLocationMonitor }

constructor TLocationMonitor.Create;
begin
  inherited;
  FPlatformLocationMonitor := TPlatformLocationMonitor.Create(Self);
end;

destructor TLocationMonitor.Destroy;
begin
  FPlatformLocationMonitor.Free;
  inherited;
end;

procedure TLocationMonitor.DoLocationChanged(const AData: TLocationData);
begin
  if Assigned(FOnLocationChanged) then
    FOnLocationChanged(Self, AData);
end;

procedure TLocationMonitor.DoStateChanged;
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

function TLocationMonitor.GetAccuracy: Double;
begin
  Result := FPlatformLocationMonitor.Accuracy;
end;

function TLocationMonitor.GetActivityType: TLocationActivityType;
begin
  Result := FPlatformLocationMonitor.ActivityType;
end;

function TLocationMonitor.GetAlarmInterval: Int64;
begin
  Result := FPlatformLocationMonitor.AlarmInterval;
end;

function TLocationMonitor.GetDistance: Double;
begin
  Result := FPlatformLocationMonitor.Distance;
end;

function TLocationMonitor.GetFastestInterval: Int64;
begin
  Result := FPlatformLocationMonitor.FastestInterval;
end;

function TLocationMonitor.GetInterval: Int64;
begin
  Result := FPlatformLocationMonitor.Interval;
end;

function TLocationMonitor.GetIsActive: Boolean;
begin
  Result := FPlatformLocationMonitor.IsActive;
end;

function TLocationMonitor.GetPriority: Integer;
begin
  Result := FPlatformLocationMonitor.Priority;
end;

function TLocationMonitor.GetUsageAuthorization: TLocationUsageAuthorization;
begin
  Result := FPlatformLocationMonitor.UsageAuthorization;
end;

procedure TLocationMonitor.SetAccuracy(const Value: Double);
begin
  FPlatformLocationMonitor.Accuracy := Value;
end;

procedure TLocationMonitor.SetActivityType(const Value: TLocationActivityType);
begin
  FPlatformLocationMonitor.ActivityType := Value;
end;

procedure TLocationMonitor.SetAlarmInterval(const Value: Int64);
begin
  FPlatformLocationMonitor.AlarmInterval := Value;
end;

procedure TLocationMonitor.SetDistance(const Value: Double);
begin
  FPlatformLocationMonitor.Distance := Value;
end;

procedure TLocationMonitor.SetFastestInterval(const Value: Int64);
begin
  FPlatformLocationMonitor.FastestInterval := Value;
end;

procedure TLocationMonitor.SetInterval(const Value: Int64);
begin
  FPlatformLocationMonitor.Interval := Value;
end;

procedure TLocationMonitor.SetIsActive(const Value: Boolean);
begin
  FPlatformLocationMonitor.IsActive := Value;
end;

procedure TLocationMonitor.SetPriority(const Value: Integer);
begin
  FPlatformLocationMonitor.Priority := Value;
end;

procedure TLocationMonitor.SetUsageAuthorization(const Value: TLocationUsageAuthorization);
begin
  FPlatformLocationMonitor.UsageAuthorization := Value;
end;

end.

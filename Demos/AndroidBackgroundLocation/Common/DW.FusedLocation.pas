unit DW.FusedLocation;

interface

uses
  Androidapi.JNI.Location, Androidapi.JNI.GraphicsContentViewText,
  DW.Androidapi.JNI.DWFusedLocation;

type
  TFusedLocationOptions = record
    FastestInterval: Int64;
    Interval: Int64;
    Priority: Integer;
    SmallestDisplacement: Single;
    class function Defaults: TFusedLocationOptions; static;
  end;

  IFusedLocationOwner = interface(IInterface)
    ['{0173C546-5BFA-4384-A458-89D095ADA70A}']
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
  end;

  IFusedLocation = interface(IInterface)
    ['{0173C546-5BFA-4384-A458-89D095ADA70A}']
    function CheckIntent(const AIntent: JIntent): Boolean;
    function GetOptions: TFusedLocationOptions;
    function IsActive: Boolean;
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
    procedure RequestLastKnownLocation;
    procedure SetOptions(const Value: TFusedLocationOptions);
    procedure Start; overload;
    procedure Start(const AOptions: TFusedLocationOptions); overload;
    procedure Stop;
    function UseCallback: Boolean;
  end;

  TFusedLocation = class(TInterfacedObject, IFusedLocation)
  private
    FClient: JDWFusedLocationClient;
    FDelegate: JDWFusedLocationClientDelegate;
    FIsActive: Boolean;
    FOwner: IFusedLocationOwner;
    FUseCallback: Boolean;
  public
    { IFusedLocation }
    function CheckIntent(const AIntent: JIntent): Boolean;
    function GetOptions: TFusedLocationOptions;
    function IsActive: Boolean;
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
    procedure RequestLastKnownLocation;
    procedure SetOptions(const Value: TFusedLocationOptions);
    procedure Start; overload;
    procedure Start(const AOptions: TFusedLocationOptions); overload;
    procedure Stop;
    function UseCallback: Boolean;
  public
    constructor Create(const AOwner: IFusedLocationOwner; const AUseCallback: Boolean = False);
    destructor Destroy; override;
  end;

implementation

uses
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.Androidapi.JNI.Location;

const
  cDefaultLocationInterval = 10000;
  cDefaultLocationFastestInterval = 5000;

type
  TFusedLocationClientDelegate = class(TJavaLocal, JDWFusedLocationClientDelegate)
  private
    FFusedLocation: IFusedLocation;
  public
    { JDWFusedLocationClientDelegate }
    procedure onLocation(location: JLocation); cdecl;
    procedure onLocationUpdatesChange(active: Boolean); cdecl;
    procedure onSetMockLocationResult(location: JLocation); cdecl;
    procedure onSetMockModeResult(success: Boolean); cdecl;
    function useCallback: Boolean; cdecl;
  public
    constructor Create(const AFusedLocation: IFusedLocation);
  end;

{ TFusedLocationClientDelegate }

constructor TFusedLocationClientDelegate.Create(const AFusedLocation: IFusedLocation);
begin
  inherited Create;
  FFusedLocation := AFusedLocation;
end;

procedure TFusedLocationClientDelegate.onLocation(location: JLocation);
begin
  FFusedLocation.LocationReceived(location);
end;

procedure TFusedLocationClientDelegate.onLocationUpdatesChange(active: Boolean);
begin
  FFusedLocation.LocationUpdatesChange(active);
end;

procedure TFusedLocationClientDelegate.onSetMockLocationResult(location: JLocation);
begin
  //!!!!
end;

procedure TFusedLocationClientDelegate.onSetMockModeResult(success: Boolean);
begin
  //!!!!
end;

function TFusedLocationClientDelegate.useCallback: Boolean;
begin
  Result := FFusedLocation.UseCallback;
end;

{ TFusedLocation }

constructor TFusedLocation.Create(const AOwner: IFusedLocationOwner; const AUseCallback: Boolean = False);
begin
  inherited Create;
  FOwner := AOwner;
  FUseCallback := AUseCallback;
  FDelegate := TFusedLocationClientDelegate.Create(Self);
  FClient := TJDWFusedLocationClient.JavaClass.init(TAndroidHelper.Context, FDelegate);
  FClient.setInterval(cDefaultLocationInterval);
  FClient.setFastestInterval(cDefaultLocationFastestInterval);
end;

destructor TFusedLocation.Destroy;
begin
  //
  inherited;
end;

function TFusedLocation.GetOptions: TFusedLocationOptions;
begin
  Result.FastestInterval := FClient.getFastestInterval;
  Result.Interval := FClient.getInterval;
  Result.Priority := FClient.getPriority;
  Result.SmallestDisplacement := FClient.getSmallestDisplacement;
end;

procedure TFusedLocation.SetOptions(const Value: TFusedLocationOptions);
var
  LWasActive: Boolean;
begin
  LWasActive := FClient.getIsActive;
  try
    Stop;
    FClient.setFastestInterval(Value.FastestInterval);
    FClient.setInterval(Value.Interval);
    FClient.setPriority(Value.Priority);
    FClient.setSmallestDisplacement(Value.SmallestDisplacement);
  finally
    if LWasActive then
      FClient.startLocationUpdates;
  end;
end;

function TFusedLocation.CheckIntent(const AIntent: JIntent): Boolean;
var
  LResult: JLocationResult;
begin
  Result := False;
  if TJLocationResult.JavaClass.hasResult(AIntent) then
  begin
    LResult := TJLocationResult.JavaClass.extractResult(AIntent);
    LocationReceived(LResult.getLastLocation);
    Result := True;
  end;
end;

function TFusedLocation.IsActive: Boolean;
begin
  Result := FIsActive;
end;

procedure TFusedLocation.LocationReceived(const ALocation: JLocation);
begin
  FOwner.LocationReceived(ALocation);
end;

procedure TFusedLocation.LocationUpdatesChange(const AIsActive: Boolean);
begin
  FIsActive := AIsActive;
  FOwner.LocationUpdatesChange(FIsActive);
end;

procedure TFusedLocation.RequestLastKnownLocation;
begin
  FClient.requestLastKnownLocation;
end;

procedure TFusedLocation.Start;
begin
  if not FIsActive then
    FClient.startLocationUpdates;
end;

procedure TFusedLocation.Start(const AOptions: TFusedLocationOptions);
begin
  SetOptions(AOptions);
  Start;
end;

procedure TFusedLocation.Stop;
begin
  if FIsActive then
    FClient.stopLocationUpdates;
end;

function TFusedLocation.UseCallback: Boolean;
begin
  Result := FUseCallback;
end;

{ TFusedLocationOptions }

class function TFusedLocationOptions.Defaults: TFusedLocationOptions;
begin
  Result.Interval := cDefaultLocationInterval;
  Result.FastestInterval := cDefaultLocationFastestInterval;
  // Result.Priority := TJLocationRequest.JavaClass.PRIORITY_BALANCED_POWER_ACCURACY;
  Result.Priority := TJLocationRequest.JavaClass.PRIORITY_HIGH_ACCURACY;
  Result.SmallestDisplacement := 0;
end;

end.

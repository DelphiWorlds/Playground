unit DW.FusedLocation;

interface

uses
  Androidapi.JNI.Location,
  DW.Androidapi.JNI.DWFusedLocation;

type
  IFusedLocationOwner = interface(IInterface)
    ['{0173C546-5BFA-4384-A458-89D095ADA70A}']
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
  end;

  IFusedLocation = interface(IInterface)
    ['{0173C546-5BFA-4384-A458-89D095ADA70A}']
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
    procedure Start;
    procedure Stop;
  end;

  TFusedLocation = class(TInterfacedObject, IFusedLocation)
  private
    FClient: JDWFusedLocationClient;
    FDelegate: JDWFusedLocationClientDelegate;
    FOwner: IFusedLocationOwner;
  public
    { IFusedLocation }
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
    procedure Start;
    procedure Stop;
  public
    constructor Create(const AOwner: IFusedLocationOwner);
    destructor Destroy; override;
  end;

implementation

uses
  Androidapi.JNIBridge, Androidapi.Helpers;

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

{ TFusedLocation }

constructor TFusedLocation.Create(const AOwner: IFusedLocationOwner);
begin
  inherited Create;
  FOwner := AOwner;
//  FIsPaused := True;
//  FLastData.DateTime := 0;
//  FLastData.Location := TLocationCoord2D.Create(cInvalidLatitude, cInvalidLongitude);
  FDelegate := TFusedLocationClientDelegate.Create(Self);
  FClient := TJDWFusedLocationClient.JavaClass.init(TAndroidHelper.Context, FDelegate);
  FClient.setInterval(cDefaultLocationInterval);
  FClient.setFastestInterval(cDefaultLocationFastestInterval);
  //!!!!  FClient.setPriority(cLocationPriorityHighAccuracy);
end;

destructor TFusedLocation.Destroy;
begin
  //
  inherited;
end;

procedure TFusedLocation.LocationReceived(const ALocation: JLocation);
begin
  FOwner.LocationReceived(ALocation);
end;

procedure TFusedLocation.LocationUpdatesChange(const AIsActive: Boolean);
begin
  FOwner.LocationUpdatesChange(AIsActive);
end;

procedure TFusedLocation.Start;
begin
  FClient.startLocationUpdates;
end;

procedure TFusedLocation.Stop;
begin
  FClient.stopLocationUpdates;
end;

end.

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.PlayServices.Maps,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.Maps,
  DW.Location.Types,
  DW.BroadcastMessage.Receiver,
  DW.LocationService.Manager, DW.LocationService.Common;

type
  TForm1 = class(TForm)
    ButtonsLayout: TLayout;
    StartStopLocationButton: TButton;
    MessagesMemo: TMemo;
    TabControl: TTabControl;
    MessagesTab: TTabItem;
    MapTab: TTabItem;
    MapView: TMapView;
    procedure StartStopLocationButtonClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
  private
    FMap: JGoogleMap;
    FMapReadyCallback: JOnMapReadyCallback;
    FMessageReceiver: TJSONBroadcastMessageReceiver;
    FPoint: JLatLng;
    FPreferences: ILocationPreferences;
    FServiceManager: ILocationServiceManager;
    procedure AddLocationToMap(const AData: TLocationData);
    procedure ServiceMessageHandler(const AKind: Integer; const AMsg: TJSONValue);
    procedure UpdateGoogleMap;
    procedure UpdateStartStopLocationButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Sensors,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNIBridge,
  DW.OSLog, DW.JSON, DW.BroadcastMessage.Sender,
  DW.LocationPermissions;

type
  TLocationCoord2DHelper = record helper for TLocationCoord2D
    function GetLatLng: JLatLng;
  end;

  TMapReadyCallback = class(TJavaLocal, JOnMapReadyCallback)
  private
    FCallback: TProc<JGoogleMap>;
  public
    { JOnMapReadyCallback }
    procedure onMapReady(googleMap: JGoogleMap); cdecl;
  public
    constructor Create(const ACallback: TProc<JGoogleMap>);
  end;

function GetCircle(const AColor: TAlphaColor; const ASize: Integer): JBitmap;
var
  LPaint: JPaint;
  LCanvas: JCanvas;
begin
  LPaint := TJPaint.JavaClass.init;
  LPaint.setColor(TAndroidHelper.AlphaColorToJColor(AColor));
  Result := TJBitmap.JavaClass.createBitmap(ASize * 2, ASize * 2, TJBitmap_Config.JavaClass.ARGB_8888);
  LCanvas := TJCanvas.JavaClass.init(Result);
  LCanvas.drawCircle(ASize, ASize, ASize, LPaint);
end;

{ TLocationCoord2DHelper }

function TLocationCoord2DHelper.GetLatLng: JLatLng;
begin
  Result := TJLatLng.JavaClass.init(Latitude, Longitude);
end;

{ TMapReadyCallback }

constructor TMapReadyCallback.Create(const ACallback: TProc<JGoogleMap>);
begin
  inherited Create;
  FCallback := ACallback;
end;

procedure TMapReadyCallback.onMapReady(googleMap: JGoogleMap);
begin
  FCallback(googleMap);
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TabControl.ActiveTab := MessagesTab;
  FServiceManager := TLocationServiceManager.Create('ABLDemoService');
  FPreferences := TLocationPreferences.Create;
  FMessageReceiver := TJSONBroadcastMessageReceiver.Create(ServiceMessageHandler);
  UpdateStartStopLocationButton;
  UpdateGoogleMap;
end;

destructor TForm1.Destroy;
begin
  //
  inherited;
end;

procedure TForm1.UpdateGoogleMap;
var
  LView: JMapView;
begin
  if Supports(MapView, JMapView, LView) then
  begin
    FMapReadyCallback := TMapReadyCallback.Create(
      procedure(googleMap: JGoogleMap)
      begin
        FMap := googleMap;
      end
    );
    LView.getMapAsync(FMapReadyCallback);
  end;
end;

procedure TForm1.StartStopLocationButtonClick(Sender: TObject);
begin
  if not FPreferences.GetIsActive then
  begin
    LocationPermissions.RequestBackground(
      procedure(const ACanStart: Boolean)
      begin
        if ACanStart then
          FServiceManager.StartLocationUpdates
      end
    );
  end
  else
    FServiceManager.StopLocationUpdates;
end;

procedure TForm1.TabControlChange(Sender: TObject);
begin
  MapView.Visible := TabControl.ActiveTab = MapTab;
end;

procedure TForm1.UpdateStartStopLocationButton;
const
  cButtonCaptions: array[Boolean] of string = ('Start Location', 'Stop Location');
begin
  StartStopLocationButton.Text := cButtonCaptions[FPreferences.GetIsActive];
end;

procedure TForm1.ServiceMessageHandler(const AKind: Integer; const AMsg: TJSONValue);
const
  cActiveCaptions: array[Boolean] of string = ('Inactive', 'Active');
var
  LLocationData: TLocationData;
begin
  case AKind of
    0:
    begin
      TOSLog.d('> Location received in app');
      MessagesMemo.Lines.Add(TJSONHelper.Tidy(AMsg));
      LLocationData.FromJSONValue(AMsg);
      // Location properties can now be accessed from LLocationData
      AddLocationToMap(LLocationData);
    end;
    1:
    begin
      TOSLog.d('> Location updates changed in app');
      MessagesMemo.Lines.Add('Location updates changed to: ' + cActiveCaptions[FPreferences.GetIsActive]);
      UpdateStartStopLocationButton;
    end;
  end;
end;

procedure TForm1.AddLocationToMap(const AData: TLocationData);
var
  LPoint: JLatLng;
  LDescriptor: JBitmapDescriptor;
begin
  if FMap <> nil then
  begin
    LPoint := AData.Location.GetLatLng;
    // Add a marker
    LDescriptor := TJBitmapDescriptorFactory.JavaClass.fromBitmap(GetCircle(TAlphaColors.Blue, 16));
    FMap.addMarker(TJMarkerOptions.JavaClass.init
      .position(LPoint)
      .icon(LDescriptor)
      .anchor(0.5, 0.5)
    );
    // Draw a line between the last one and the new one
    if FPoint <> nil then
    begin
      FMap.addPolyline(TJPolylineOptions.JavaClass.init
        .add(FPoint)
        .add(LPoint)
        .width(16)
        .color(TAndroidHelper.AlphaColorToJColor(TAlphaColors.Blue))
      );
    end
    else
      FMap.moveCamera(TJCameraUpdateFactory.JavaClass.newLatLngZoom(LPoint, 17));
    FPoint := LPoint;
  end
  else
  begin
    MessagesMemo.Lines.Add('Cannot obtain GoogleMap reference?');
    TabControl.ActiveTab := MessagesTab;
  end;
end;

end.

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
  DW.LocationService.Manager, DW.LocationService.Common, FMX.ListBox, FMX.Edit;

type
  TForm1 = class(TForm)
    ConfigLayout: TLayout;
    UpdateButton: TButton;
    MessagesMemo: TMemo;
    TabControl: TTabControl;
    MessagesTab: TTabItem;
    MapTab: TTabItem;
    MapView: TMapView;
    PriorityLayout: TLayout;
    PriorityLabel: TLabel;
    PriorityComboBox: TComboBox;
    IntervalLayout: TLayout;
    IntervalLabel: TLabel;
    IntervalEdit: TEdit;
    SmallestDisplacementLayout: TLayout;
    SmallestDisplacementLabel: TLabel;
    SmallestDisplacementEdit: TEdit;
    ButtonLayout: TLayout;
    StopButton: TButton;
    ClearMessagesButton: TButton;
    procedure UpdateButtonClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure ClearMessagesButtonClick(Sender: TObject);
  private
    FMap: JGoogleMap;
    FMapReadyCallback: JOnMapReadyCallback;
    FMessageReceiver: TJSONBroadcastMessageReceiver;
    FPoint: JLatLng;
    FPreferences: ILocationPreferences;
    FServiceManager: ILocationServiceManager;
    procedure AddLocationToMap(const AData: TLocationData);
    function GetPriorityIndex(const AValue: Integer): Integer;
    function GetPriorityText(const AValue: Integer): string;
    procedure LocationUpdatesChanged;
    procedure ServiceMessageHandler(const AKind: Integer; const AMsg: TJSONValue);
    procedure UpdateButtons;
    procedure UpdateConfigControls;
    procedure UpdateGoogleMap;
    procedure UpdateLocationOptions;
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
  DW.LocationPermissions, DW.FusedLocation;

const
  cPriorityValues: array[0..3] of Integer = (
    102, // PRIORITY_BALANCED_POWER_ACCURACY
    100, // PRIORITY_HIGH_ACCURACY
    104, // PRIORITY_LOW_POWER
    105  // PRIORITY_PASSIVE
  );

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
  UpdateGoogleMap;
  if FPreferences.GetIsActive then
    UpdateConfigControls;
  LocationUpdatesChanged;
end;

destructor TForm1.Destroy;
begin
  //
  inherited;
end;

function TForm1.GetPriorityIndex(const AValue: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(cPriorityValues) to High(cPriorityValues) do
  begin
    if cPriorityValues[I] = AValue then
    begin
      Result := I - Low(cPriorityValues);
      Break;
    end;
  end;
end;

function TForm1.GetPriorityText(const AValue: Integer): string;
var
  I: Integer;
begin
  Result := 'Unknown';
  for I := Low(cPriorityValues) to High(cPriorityValues) do
  begin
    if cPriorityValues[I] = AValue then
    begin
      Result := PriorityComboBox.Items[I - Low(cPriorityValues)];
      Break;
    end;
  end;
end;

procedure TForm1.LocationUpdatesChanged;
const
  cActiveCaptions: array[Boolean] of string = ('Inactive', 'Active');
begin
  MessagesMemo.Lines.Add('Location updates are: ' + cActiveCaptions[FPreferences.GetIsActive]);
  if FPreferences.GetIsActive then
  begin
    MessagesMemo.Lines.Add(Format('Priority: %s', [GetPriorityText(FPreferences.GetOptions.Priority)]));
    MessagesMemo.Lines.Add(Format('Interval: %d ms', [FPreferences.GetOptions.Interval]));
    MessagesMemo.Lines.Add(Format('Smallest Displacement: %.0f m', [FPreferences.GetOptions.SmallestDisplacement]));
  end;
  UpdateButtons;
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

procedure TForm1.UpdateLocationOptions;
var
  LInfo: TLocationServiceInfo;
begin
  LInfo.Options := TFusedLocationOptions.Defaults;
  LInfo.Options.Priority := cPriorityValues[PriorityComboBox.ItemIndex];
  LInfo.Options.Interval := StrToInt(IntervalEdit.Text);
  LInfo.Options.SmallestDisplacement := StrToInt(SmallestDisplacementEdit.Text);
  FServiceManager.StartLocationUpdates(LInfo);
end;

procedure TForm1.UpdateButtonClick(Sender: TObject);
begin
  LocationPermissions.RequestBackground(
    procedure(const ACanStart: Boolean)
    begin
      if ACanStart then
        UpdateLocationOptions;
    end
  );
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  FServiceManager.StopLocationUpdates;
end;

procedure TForm1.ClearMessagesButtonClick(Sender: TObject);
begin
  MessagesMemo.Lines.Clear;
end;

procedure TForm1.TabControlChange(Sender: TObject);
begin
  MapView.Visible := TabControl.ActiveTab = MapTab;
end;

procedure TForm1.UpdateButtons;
const
  cUpdateButtonCaptions: array[Boolean] of string = ('Start', 'Update');
begin
  UpdateButton.Text := cUpdateButtonCaptions[FPreferences.GetIsActive];
  StopButton.Enabled := FPreferences.GetIsActive;
end;

procedure TForm1.UpdateConfigControls;
begin
  PriorityComboBox.ItemIndex := GetPriorityIndex(FPreferences.GetOptions.Priority);
  IntervalEdit.Text := FPreferences.GetOptions.Interval.ToString;
  SmallestDisplacementEdit.Text := FPreferences.GetOptions.SmallestDisplacement.ToString;
end;

procedure TForm1.ServiceMessageHandler(const AKind: Integer; const AMsg: TJSONValue);
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
      LocationUpdatesChanged;
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

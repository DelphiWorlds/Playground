unit ABL.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.Location, Androidapi.JNI.JavaTypes,
  DW.FusedLocation;

type
  TServiceModule = class(TAndroidService, IFusedLocationOwner)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceCreate(Sender: TObject);
    procedure AndroidServiceDestroy(Sender: TObject);
  private
    FFusedLocation: IFusedLocation;
    procedure SendMessage(const AKind: Integer; const AMsg: string);
  public
    { IFusedLocationOwner }
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.Sensors, System.DateUtils,
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.Helpers,
  DW.OSLog,
  DW.Android.Helpers, DW.ForegroundServiceHelper, DW.Location.Types, DW.Androidapi.JNI.AndroidX.LocalBroadcastManager, DW.MessageReceiver.Consts,
  ABL.Common;

// TODO: Refactor to elsewhere??
type
  TLocationDataHelper = record helper for TLocationData
    procedure FromLocation(const ALocation: JLocation);
  end;

{ TLocationDataHelper }

procedure TLocationDataHelper.FromLocation(const ALocation: JLocation);
begin
  Self := Default(TLocationData);
  DateTime := UnixToDateTime(ALocation.getTime); // UTC
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
//   Modifiable - update frequency, accuracy, priority, smallest distance change

// Highest priority:
//   Remain running for long periods
//   Avoid app "hangs"

{ TServiceModule }

procedure TServiceModule.AndroidServiceCreate(Sender: TObject);
begin
  FFusedLocation := TFusedLocation.Create(Self);
end;

procedure TServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  TOSLog.d('TServiceModule.AndroidServiceDestroy');
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
var
  LInfo: TABLServiceInfo;
begin
  LInfo.FromIntent(Intent);
  case LInfo.ForegroundMode of
    TActionMode.Start:
      TForegroundServiceHelper.StartForeground(JavaService, 'X', 'X');
    TActionMode.Stop:
      TForegroundServiceHelper.StopForeground(JavaService);
  end;
  case LInfo.LocationMode of
    TActionMode.Start:
      FFusedLocation.Start;
    TActionMode.Stop:
      FFusedLocation.Stop;
  end;
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TServiceModule.LocationReceived(const ALocation: JLocation);
var
  LData: TLocationData;
begin
  LData.FromLocation(ALocation);
  TOSLog.d('Received: %.5f, %.5f', [LData.Location.Latitude, LData.Location.Longitude]);
  SendMessage(0, LData.ToJSON);
end;

procedure TServiceModule.LocationUpdatesChange(const AIsActive: Boolean);
begin
  TOSLog.d('TServiceModule.LocationUpdatesChange > AIsActive: %s', [BoolToStr(AIsActive, True)]);
end;

procedure TServiceModule.SendMessage(const AKind: Integer; const AMsg: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(MessageReceiverConsts.ACTION_MESSAGE);
  LIntent.putExtra(MessageReceiverConsts.EXTRA_MESSAGE_KIND, AKind);
  LIntent.putExtra(MessageReceiverConsts.EXTRA_MESSAGE, StringToJString(AMsg));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

end.

unit RT.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  Androidapi.JNI.App, AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
  DW.RadioPlayer, DW.MultiReceiver.Android;

type
  TServiceModule = class;

  TLocalReceiver = class(TMultiReceiver)
  private
    FService: TServiceModule;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AService: TServiceModule);
  end;

  TServiceModule = class(TAndroidService)
    procedure AndroidServiceCreate(Sender: TObject);
    procedure AndroidServiceDestroy(Sender: TObject);
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    FNotificationChannel: JNotificationChannel;
    FRadio: TRadioPlayer;
    FReceiver: TLocalReceiver;
    procedure CreateNotificationChannel;
    function IsForeground: Boolean;
    procedure StartForeground(const AMustStartForeground: Boolean);
    procedure StopForeground(const AIsStarting: Boolean);
  protected
    procedure LocalReceiverReceive(intent: JIntent);
  public
    { Public declarations }
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  {$IF CompilerVersion < 35}
  DW.Androidapi.JNI.SupportV4,
  {$ELSE}
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager, DW.Androidapi.JNI.AndroidX.App,
  {$ENDIF}
  DW.OSLog,
  DW.Consts.Android, DW.Android.Helpers,
  DW.RadioPlayer.Common, DW.RadioPlayer.ServiceHelper;

const
  cNotificationChannelName = 'RadioService';
  cServiceNameFull = cEMBTJavaServicePrefix + 'RadioService';
  cServiceNotificationCaption = 'Radio Demo';
  cServiceNotificationText = 'Radio Demo Service';
  cServiceForegroundId = 4321; // Just a random number

{ TLocalReceiver }

constructor TLocalReceiver.Create(const AService: TServiceModule);
begin
  inherited Create(True);
  FService := AService;
end;

procedure TLocalReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceCommandAction));
  IntentFilter.addAction(StringToJString(cServiceCommandJSONAction));
end;

procedure TLocalReceiver.Receive(context: JContext; intent: JIntent);
begin
  FService.LocalReceiverReceive(intent);
end;

{ TServiceModule }

procedure TServiceModule.AndroidServiceCreate(Sender: TObject);
begin
  // The Create event can be called by the system when the app is "swipe closed"
  if TAndroidHelperEx.IsActivityForeground then
  begin
    FReceiver := TLocalReceiver.Create(Self);
    FRadio := TRadioPlayer.Create;
  end
  else
    JavaService.stopSelf;
end;

procedure TServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  FReceiver.Free;
  FRadio.Free;
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  if not TAndroidHelperEx.IsActivityForeground then
    StartForeground(False);
  TRadioServiceHelper.SendState(cServiceStateStarted);
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TServiceModule.CreateNotificationChannel;
begin
  FNotificationChannel := TJNotificationChannel.JavaClass.init(TAndroidHelper.Context.getPackageName, StrToJCharSequence(cNotificationChannelName),
    TJNotificationManager.JavaClass.IMPORTANCE_NONE);
  FNotificationChannel.setLightColor(TJColor.JavaClass.BLUE);
  FNotificationChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
  TAndroidHelperEx.NotificationManager.createNotificationChannel(FNotificationChannel);
end;

function TServiceModule.IsForeground: Boolean;
begin
  Result := TAndroidHelperEx.IsServiceForeground(cServiceNameFull);
end;

procedure TServiceModule.LocalReceiverReceive(intent: JIntent);
var
  LJSON: string;
  LRadioCommand: TRadioCommand;
  LCommand: Integer;
begin
  if intent.getAction.equals(StringToJString(cServiceCommandJSONAction)) then
  begin
    LJSON := JStringToString(intent.getStringExtra(StringToJString(cServiceBroadcastParamCommand)));
    TOSLog.d('Received: %s', [LJSON]);
    LRadioCommand.FromJSON(LJSON);
    // Commands from the app
    case LRadioCommand.Command of
      cServiceCommandRadioSetURL:
      begin
        if not LRadioCommand.URL.IsEmpty then
          FRadio.URL := LRadioCommand.URL;
      end;
      cServiceCommandRadioPlay:
      begin
        TOSLog.d('URL: %s', [LRadioCommand.URL]);
        if not LRadioCommand.URL.IsEmpty then
        begin
          FRadio.Stop;
          FRadio.URL := LRadioCommand.URL;
        end;
        FRadio.Play;
      end;
      cServiceCommandRadioPause:
        FRadio.Pause;
      cServiceCommandRadioStop:
        FRadio.Stop;
    end;
  end
  else if intent.getAction.equals(StringToJString(cServiceCommandAction)) then
  begin
    LCommand := intent.getIntExtra(StringToJString(cServiceBroadcastParamCommand), 0);
    // Commands from the app
    TOSLog.d('Received command: %d', [LCommand]);
    case LCommand of
      cServiceCommandAppBecameActive:
        StopForeground(False);
      // If the app is requesting permissions, the service *must* be put into the foreground (if Android requires it)
      cServiceCommandAppEnteredBackground, cServiceCommandAppIsRequestingPermissions:
        StartForeground(LCommand = cServiceCommandAppIsRequestingPermissions);
    end;
  end;
end;

procedure TServiceModule.StartForeground(const AMustStartForeground: Boolean);
var
  LBuilder: JNotificationCompat_Builder;
  LIsForegroundMandatory: Boolean;
  LIntent: JIntent;
  LFlags: Integer;
begin
  LIsForegroundMandatory := not TAndroidHelperEx.IsActivityForeground or AMustStartForeground;
  // Only allow the service to start in the foreground if it needs to. One case is where the Android permissions dialog is showing!
  if not IsForeground and TOSVersion.Check(8) and LIsForegroundMandatory then
  begin
    TOSLog.d('Starting foreground..');
    if FNotificationChannel = nil then
      CreateNotificationChannel;
    JavaService.stopForeground(True);
    // Create an intent for starting the app if the user taps the notification
    LIntent := TJIntent.Create;
    LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString('com.embarcadero.firemonkey.FMXNativeActivity'));
    LFlags := TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK or TJPendingIntent.JavaClass.FLAG_IMMUTABLE;
    LBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context, TAndroidHelper.Context.getPackageName);
    LBuilder.setAutoCancel(True);
    LBuilder.setContentTitle(StrToJCharSequence(cServiceNotificationCaption));
    LBuilder.setContentText(StrToJCharSequence(cServiceNotificationText));
    LBuilder.setSmallIcon(TAndroidHelperEx.GetDefaultIconID);
    LBuilder.setTicker(StrToJCharSequence(cServiceNotificationCaption));
    LBuilder.setPriority(TJNotification.JavaClass.PRIORITY_MIN);
    LBuilder.setContentIntent(TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, 0, LIntent, LFlags));
    JavaService.startForeground(cServiceForegroundId, LBuilder.build);
  end;
end;

procedure TServiceModule.StopForeground(const AIsStarting: Boolean);
begin
  // Stop foreground service if the app is in the foreground, or if the service is foreground already
  if TOSVersion.Check(8) and IsForeground then
  begin
    TOSLog.d('Stopping foreground..');
    JavaService.stopForeground(True);
  end;
end;

end.

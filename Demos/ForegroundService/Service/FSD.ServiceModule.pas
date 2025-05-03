unit FSD.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service, System.JSON,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.Location, Androidapi.JNI.JavaTypes,
  DW.OSTimer;

type
  TServiceModule = class(TAndroidService)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceCreate(Sender: TObject);
    procedure AndroidServiceDestroy(Sender: TObject);
  private
    FTimer: TOSTimer;
    procedure TimerIntervalHandler(Sender: TObject);
  public
    //
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
  DW.Android.Helpers, DW.ForegroundServiceHelper, DW.Consts.Android,
  DW.BroadcastMessage.Sender;

function IsAppForeground: Boolean;
var
  LAppInfo: JActivityManager_RunningAppProcessInfo;
begin
  LAppInfo := TJActivityManager_RunningAppProcessInfo.JavaClass.init;
  TJActivityManager.JavaClass.getMyMemoryState(LAppInfo);
  Result := (LAppInfo.importance = TJActivityManager_RunningAppProcessInfo.JavaClass.IMPORTANCE_FOREGROUND) or
    (LAppInfo.importance = TJActivityManager_RunningAppProcessInfo.JavaClass.IMPORTANCE_VISIBLE);
  TOSLog.d('IsAppForeground = %s', [BoolToStr(Result, True)]);
end;

{ TServiceModule }

procedure TServiceModule.AndroidServiceCreate(Sender: TObject);
begin
  FTimer := TOSTimer.Create;
  FTimer.OnInterval := TimerIntervalHandler;
  FTimer.Interval := 10000;
end;

procedure TServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  // Notify the app that the service has stopped running
  TBroadcastMessageSender.Send(0, '{"IsRunning": false}');
  FTimer.Free;
end;

procedure TServiceModule.TimerIntervalHandler(Sender: TObject);
begin
  TOSLog.d('TServiceModule.TimerIntervalHandler');
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  TOSLog.d('+TServiceModule.AndroidServiceStartCommand');
  // Some other action might need to be taken if the service is started at boot time
  if Intent.getBooleanExtra(StringToJString('startAtBoot'), False) then
    TOSLog.d('> Started at boot time');
  // If the app is not in the foreground, the service needs to be put into the foreground
  if not IsAppForeground then
    TForegroundServiceHelper.StartForeground(JavaService, 'FS Demo', 'Service in foreground', TJServiceInfo.JavaClass.FOREGROUND_SERVICE_TYPE_CONNECTED_DEVICE);
  if not FTimer.Enabled then
    FTimer.Enabled := True;
  // Notify the app that the service is now running
  TBroadcastMessageSender.Send(0, '{"IsRunning": true}');
  Result := TJService.JavaClass.START_STICKY;
  TOSLog.d('-TServiceModule.AndroidServiceStartCommand');
end;

end.

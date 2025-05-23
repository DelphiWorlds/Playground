unit DW.ForegroundServiceHelper;

interface

uses
  Androidapi.JNI.App, Androidapi.JNI.JavaTypes;

type
  TForegroundServiceHelper = record
  private
    class function CreateNotificationChannel: JString; static;
  public
    class procedure StartForeground(const AService: JService; const ANotificationCaption, ANotificationText: string; const AType: Integer;
      const AChannelId: string = ''); static;
    class procedure StopForeground(const AService: JService); static;
  end;

implementation

uses
  System.SysUtils,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  DW.Android.Helpers;

const
  cServiceForegroundId = 3988; // Just a random number
  cNotificationChannelId = 'ForegroundService';
  cNotificationChannelName = 'ForegroundService';

class function TForegroundServiceHelper.CreateNotificationChannel: JString;
var
  LChannel: JNotificationChannel;
begin
  Result := StringToJString(cNotificationChannelId);
  LChannel := TJNotificationChannel.JavaClass.init(Result, StrToJCharSequence(cNotificationChannelName),
    TJNotificationManager.JavaClass.IMPORTANCE_NONE);
  LChannel.setLightColor(TJColor.JavaClass.BLUE); //!!!!
  LChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
  TAndroidHelperEx.NotificationManager.createNotificationChannel(LChannel);
end;

class procedure TForegroundServiceHelper.StartForeground(const AService: JService; const ANotificationCaption, ANotificationText: string;
  const AType: Integer; const AChannelId: string = '');
var
  LBuilder: JNotification_Builder;
  LIntent: JIntent;
  LFlags: Integer;
  LChannelId: JString;
begin
  if TOSVersion.Check(8) then
  begin
    if AChannelId.IsEmpty then
      LChannelId := CreateNotificationChannel
    else
      LChannelId := StringToJString(AChannelId);
    LIntent := TJIntent.Create;
    LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString('com.embarcadero.firemonkey.FMXNativeActivity'));
    LFlags := TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK or TJPendingIntent.JavaClass.FLAG_IMMUTABLE;
    LBuilder := TJNotification_Builder.JavaClass.init(TAndroidHelper.Context, LChannelId);
    LBuilder.setAutoCancel(True);
    LBuilder.setContentTitle(StrToJCharSequence(ANotificationCaption));
    LBuilder.setContentText(StrToJCharSequence(ANotificationText));
    LBuilder.setSmallIcon(TAndroidHelperEx.GetDefaultIconID);
    LBuilder.setTicker(StrToJCharSequence(ANotificationCaption));
    LBuilder.setPriority(TJNotification.JavaClass.PRIORITY_MIN);
    LBuilder.setContentIntent(TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, 0, LIntent, LFlags));
    LBuilder.setOngoing(True);
    if TOSVersion.Check(10) then
    begin
      if TOSVersion.Check(12) then
        LBuilder.setForegroundServiceBehavior(TJNotification.JavaClass.FOREGROUND_SERVICE_IMMEDIATE);
      AService.startForeground(cServiceForegroundId, LBuilder.build, AType);
    end
    else
      AService.startForeground(cServiceForegroundId, LBuilder.build);
  end;
end;

class procedure TForegroundServiceHelper.StopForeground(const AService: JService);
begin
  if TOSVersion.Check(8) and TAndroidHelperEx.IsServiceForeground(JStringToString(AService.getClass.getName)) then
    AService.stopForeground(True);
end;

end.

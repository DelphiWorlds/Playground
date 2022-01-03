unit LD.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
  DW.Location.Types;

type
  TServiceModule = class(TAndroidIntentService)
    procedure AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
  private
    procedure HandleAlarm(const AIntent: JIntent);
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Androidapi.Helpers, Androidapi.JNI.Location, Androidapi.JNI.JavaTypes,
  DW.OSLog,
  DW.Androidapi.JNI.Location, DW.Androidapi.JNI.Os, DW.LocationHelpers.Android, DW.Consts.Android,
  LD.LocationUpdater;

procedure TServiceModule.AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
var
  LLocationResult: JLocationResult;
  LLocation: JLocation;
  LHelper: TLocationHelper;
begin
  if TJLocationResult.JavaClass.hasResult(AnIntent) then
  begin
    LLocationResult := TJLocationResult.JavaClass.extractResult(AnIntent);
    LLocation := LLocationResult.getLastLocation;
    if LLocation <> nil then
    begin
      LHelper.BroadcastLocationData(LLocation);
      TLocationUpdater.HandleLocationData(LHelper.Data);
    end;
  end
  else if AnIntent.getAction <> nil then
  begin
    if AnIntent.getAction.equals(StringToJString(cDWFusedLocationClientActionAlarm)) then
      HandleAlarm(AnIntent);
  end;
end;

procedure TServiceModule.HandleAlarm(const AIntent: JIntent);
var
  LTimestamp, LInterval, LCurrentTimestamp: Int64;
begin
  TOSLog.d('Alarm intent: %s', [JStringToString(AIntent.toUri(0))]);
  LInterval := AIntent.getLongExtra(StringToJString(cDWFusedLocationClientExtraAlarmInterval), 0);
  LTimestamp := AIntent.getLongExtra(StringToJString(cDWFusedLocationClientExtraAlarmTimestamp), 0);
  LCurrentTimestamp := TJSystem.JavaClass.currentTimeMillis;
  // Here, the difference between LCurrentTimestamp and the sent LTimestamp could be checked to determine whether the alarm was fired while
  //   the device was in doze mode. A choice then could be made to discard the intent.
  // Otherwise, HandleAlarm could end up handling two consecutive intents
end;

end.

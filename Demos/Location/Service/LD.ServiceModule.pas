unit LD.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os;

type
  TServiceModule = class(TAndroidIntentService)
    procedure AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Androidapi.Helpers, Androidapi.JNI.Location,
  DW.OSLog, DW.Location.Types, DW.Androidapi.JNI.Location, DW.LocationHelpers.Android;

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
      TOSLog.d('%.5f, %.5f', [LHelper.Data.Location.Latitude, LHelper.Data.Location.Longitude]);
    end;
  end;
end;

end.

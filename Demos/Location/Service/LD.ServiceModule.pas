unit LD.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
  DW.Location.Types;

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
  Androidapi.Helpers, Androidapi.JNI.Location, Androidapi.JNI.JavaTypes,
  DW.Androidapi.JNI.Location, DW.LocationHelpers.Android,
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
  end;
end;

end.

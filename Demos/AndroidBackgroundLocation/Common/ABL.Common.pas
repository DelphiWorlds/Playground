unit ABL.Common;

interface

uses
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText;

type
  TActionMode = (None, Start, Stop);

  TABLServiceInfo = record
  private
    function EXTRA_FOREGROUND: JString;
    function EXTRA_LOCATION_UPDATES: JString;
  public
    ForegroundMode: TActionMode;
    LocationMode: TActionMode;
    procedure FromIntent(const AIntent: JIntent);
    function ToIntent: JIntent;
  end;

implementation

uses
  Androidapi.Helpers;

{ TABLServiceInfo }

function TABLServiceInfo.EXTRA_FOREGROUND: JString;
begin
  Result := StringToJString('EXTRA_FOREGROUND')
end;

function TABLServiceInfo.EXTRA_LOCATION_UPDATES: JString;
begin
  Result := StringToJString('EXTRA_LOCATION_UPDATES');
end;

procedure TABLServiceInfo.FromIntent(const AIntent: JIntent);
begin
  ForegroundMode := TActionMode.None;
  LocationMode := TActionMode.None;
  if AIntent.hasExtra(EXTRA_FOREGROUND) then
  begin
    if AIntent.getBooleanExtra(EXTRA_FOREGROUND, False) then
      ForegroundMode := TActionMode.Start
    else
      ForegroundMode := TActionMode.Stop;
  end;
  if AIntent.hasExtra(EXTRA_LOCATION_UPDATES) then
  begin
    if AIntent.getBooleanExtra(EXTRA_LOCATION_UPDATES, False) then
      LocationMode := TActionMode.Start
    else
      LocationMode := TActionMode.Stop;
  end;
end;

function TABLServiceInfo.ToIntent: JIntent;
begin
  Result := TJIntent.JavaClass.init;
  if ForegroundMode <> TActionMode.None then
    Result.putExtra(EXTRA_FOREGROUND, ForegroundMode = TActionMode.Start);
  if LocationMode <> TActionMode.None then
    Result.putExtra(EXTRA_LOCATION_UPDATES, LocationMode = TActionMode.Start);
end;

end.

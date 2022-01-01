unit LD.LocationUpdater;

interface

uses
  DW.Location.Types;

type
  TLocationUpdater = record
  private
    const
      cMinimumDistance = 50; // metres
  private
    class function GetStoredLocationFileName: string; static;
  public
    class procedure HandleLocationData(const AData: TLocationData); static;
  end;

implementation

uses
  System.IOUtils, System.SysUtils,
  DW.OSLog,
  DW.OSDevice, DW.Geodetic,
  LD.LocationsDataModule;

{ TLocationUpdater }

class function TLocationUpdater.GetStoredLocationFileName: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'location.json');
end;

class procedure TLocationUpdater.HandleLocationData(const AData: TLocationData);
var
  LStoredData: TLocationData;
  LMessage, LFileName: string;
  LDistance: Double;
begin
  LStoredData.Reset;
  LFileName := GetStoredLocationFileName;
  if TFile.Exists(LFileName) then
    LStoredData.FromJSON(TFile.ReadAllText(LFileName));
  if LStoredData.Location.Latitude <> cInvalidLatitude then
    LDistance := TGeodetic.DistanceBetween(LStoredData.Location, AData.Location)
  else
    LDistance := 0;
  LMessage := Format('%s [%d] %.5f, %.5f, %.2f, %.2f',
    [TOSDevice.GetDeviceModel, Ord(AData.ApplicationState), AData.Location.Latitude, AData.Location.Longitude, LDistance, AData.Speed]);
  if (LStoredData.DateTime = 0) or (LDistance >= cMinimumDistance) then
  begin
    // Handle the first location, or a location greater than or equal to the minimum distance from the last location
    TFile.WriteAllText(LFileName, AData.ToJSON);
    TLocationsDataModule.AddLocation(AData);
  end;
  TOSLog.d(LMessage);
end;

end.

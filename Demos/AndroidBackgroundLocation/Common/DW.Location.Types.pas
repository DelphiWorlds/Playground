unit DW.Location.Types;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Sensors;

const
  cInvalidLatitude = 91;
  cInvalidLongitude = 181;

type
  TLocationDataFlag = (Accuracy, Altitude, Bearing, Speed);

  TLocationDataFlags = set of TLocationDataFlag;

  TLocationData = record
  public
    Accuracy: Double;
    Altitude: Double;
    Bearing: Double;
    DateTime: TDateTime;
    Flags: TLocationDataFlags;
    IsCached: Boolean;
    IsMocked: Boolean;
    Location: TLocationCoord2D;
    Speed: Double;
    procedure FromJSON(const AJSON: string);
    procedure Reset;
    function ToJSON: string;
  end;

implementation

uses
  System.JSON, System.DateUtils;

{ TLocationData }

procedure TLocationData.Reset;
begin
  Flags := [];
  Location := TLocationCoord2D.Create(cInvalidLatitude, cInvalidLongitude);
  Accuracy := 0;
  Altitude := 0;
  Bearing := 0;
  DateTime := 0;
  IsCached := False;
  IsMocked := False;
  Speed := 0;
end;

function TLocationData.ToJSON: string;
var
  LJSON, LLocation: TJSONObject;
begin
  // Flags!!!
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('Accuracy', Accuracy);
    LJSON.AddPair('Altitude', Altitude);
    LJSON.AddPair('Bearing', Altitude);
    LJSON.AddPair('DateTime', DateToISO8601(DateTime));
    LJSON.AddPair('IsCached', IsCached);
    LJSON.AddPair('IsMocked', IsMocked);
    LLocation := TJSONObject.Create;
    LLocation.AddPair('Latitude', Location.Latitude);
    LLocation.AddPair('Longitude', Location.Longitude);
    LJSON.AddPair('Location', LLocation);
    LJSON.AddPair('Speed', Speed);
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

procedure TLocationData.FromJSON(const AJSON: string);
var
  LValue, LLocation: TJSONValue;
  LDateTimeValue: string;
begin
  LValue := TJSONObject.ParseJSONValue(AJSON);
  if LValue <> nil then
  try
    LValue.TryGetValue('Accuracy', Accuracy);
    LValue.TryGetValue('Altitude', Altitude);
    LValue.TryGetValue('Bearing', Bearing);
    if LValue.TryGetValue('DateTime', LDateTimeValue) then
      DateTime := ISO8601ToDate(LDateTimeValue);
    LValue.TryGetValue('IsCached', IsCached);
    LValue.TryGetValue('IsMocked', IsMocked);
    if LValue.TryGetValue('Location', LLocation) then
    begin
      LLocation.TryGetValue('Latitude', Location.Latitude);
      LLocation.TryGetValue('Longitude', Location.Longitude);
    end;
    LValue.TryGetValue('Speed', Accuracy);
  finally
    LValue.Free;
  end;
end;

end.

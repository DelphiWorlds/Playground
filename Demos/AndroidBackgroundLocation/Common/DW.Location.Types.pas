unit DW.Location.Types;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

uses
  // RTL
  System.JSON, System.Sensors;

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
    procedure FromJSONValue(const AValue: TJSONValue);
    procedure Reset;
    function ToJSON: string;
  end;

implementation

uses
  DW.OSLog, System.SysUtils,
  System.DateUtils;

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
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('Accuracy', Accuracy);
    LJSON.AddPair('Altitude', Altitude);
    LJSON.AddPair('Bearing', Bearing);
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

procedure TLocationData.FromJSONValue(const AValue: TJSONValue);
var
  LLocation: TJSONValue;
  LDateTimeValue: string;
begin
  TOSLog.d('+TLocationData.FromJSONValue - AValue = nil: %s', [BoolToStr(AValue = nil, True)]);
  AValue.TryGetValue('Accuracy', Accuracy);
  AValue.TryGetValue('Altitude', Altitude);
  AValue.TryGetValue('Bearing', Bearing);
  if AValue.TryGetValue('DateTime', LDateTimeValue) then
  try
    TOSLog.d('> DateTime := ISO8601ToDate(LDateTimeValue) : %s', [LDateTimeValue]);
    DateTime := ISO8601ToDate(LDateTimeValue);
  except
    on E: Exception do
      TOSLog.d('%s: %s', [E.ClassName, E.Message]);
  end;
  TOSLog.d('> AValue.TryGetValue(''IsCached''');
  AValue.TryGetValue('IsCached', IsCached);
  TOSLog.d('> AValue.TryGetValue(''IsMocked'', IsMocked)');
  AValue.TryGetValue('IsMocked', IsMocked);
  if AValue.TryGetValue('Location', LLocation) then
  begin
    LLocation.TryGetValue('Latitude', Location.Latitude);
    LLocation.TryGetValue('Longitude', Location.Longitude);
  end;
  AValue.TryGetValue('Speed', Accuracy);
  TOSLog.d('-TLocationData.FromJSONValue');
end;

procedure TLocationData.FromJSON(const AJSON: string);
var
  LValue: TJSONValue;
begin
  LValue := TJSONObject.ParseJSONValue(AJSON);
  if LValue <> nil then
  try
    FromJSONValue(LValue);
  finally
    LValue.Free;
  end;
end;

end.

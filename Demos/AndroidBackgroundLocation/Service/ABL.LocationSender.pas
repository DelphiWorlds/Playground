unit ABL.LocationSender;

interface

uses
  System.Classes,
  DW.Location.Types;

type
  ILocationSender = interface(IInterface)
    ['{0CC6FF1B-0556-42AC-A10B-699C4B0F6998}']
    function GetURL: string;
    procedure SendLocation(const AData: TLocationData);
    procedure SetURL(const Value: string);
    property URL: string read GetURL write SetURL;
  end;

  TLocationSender = class(TInterfacedObject, ILocationSender)
  private
    FURL: string;
    procedure PostRequest(const ARequest: TStream);
  public
    { ILocationSender }
    function GetURL: string;
    procedure SendLocation(const AData: TLocationData);
    procedure SetURL(const Value: string);
  end;

implementation

uses
  System.SysUtils, System.Net.HTTPClient, System.Net.URLClient, System.NetConsts,
  DW.OSLog,
  DW.OSDevice;

const
  CONTENTTYPE_APPLICATION_JSON = 'application/json';
  // Change this to whatever format is required by your location update service.
  cLocationRequestJSONTemplate = '{"deviceid": "%s", "latitude": %.5f, "longitude": %.5f, "os": "%S"}';

{ TLocationSender }

function TLocationSender.GetURL: string;
begin
  Result := FURL;
end;

procedure TLocationSender.SetURL(const Value: string);
begin
  FURL := Value;
end;

procedure TLocationSender.PostRequest(const ARequest: TStream);
var
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
begin
  // Posts the JSON request to the server
  LHTTP := THTTPClient.Create;
  try
    LHTTP.ContentType := CONTENTTYPE_APPLICATION_JSON;
    LResponse := LHTTP.Post(FURL, ARequest);
    if LResponse.StatusCode <> 200 then
      TOSLog.d('Post unsuccessful - Status: %s, Response: %s', [LResponse.StatusText, LResponse.ContentAsString], True);
  finally
    LHTTP.Free;
  end;
end;

procedure TLocationSender.SendLocation(const AData: TLocationData);
var
  LStream: TStringStream;
  LJSON: string;
begin
  LJSON := Format(cLocationRequestJSONTemplate, [TOSDevice.GetUniqueDeviceID, AData.Location.Latitude, AData.Location.Longitude, 'Android']);
  LStream := TStringStream.Create(LJSON);
  try
    try
      PostRequest(LStream);
    except
      on E: Exception do
        TOSLog.e('Exception in SendLocation - %s: %s', [E.ClassName, E.Message], True);
    end;
  finally
    LStream.Free;
  end;
end;

end.

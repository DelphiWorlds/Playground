unit DW.APNSSender;

interface

implementation

uses
  System.Classes, System.Net.HttpClient, System.SysUtils,
  DW.APNS;

const
  cAPNSURLProduction = 'https://api.push.apple.com';
  cAPNSURLSandbox = 'https://api.sandbox.push.apple.com';

  cAPNSURLs: array[Boolean] of string = (cAPNSURLSandbox, cAPNSURLProduction);

type
  TAPNSSender = class(TInterfacedObject, IAPNSSender)
  private
    function DoPostMessage(const AParams: TAPNSMessageParams; const APayload: string): Boolean;
  public
    function PostMessage(const AParams: TAPNSMessageParams; const APayload: string): Boolean;
  end;

{ TAPNSSender }

function TAPNSSender.DoPostMessage(const AParams: TAPNSMessageParams; const APayload: string): Boolean;
var
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
  LContent: TStream;
begin
  LHTTP := THTTPClient.Create;
  try
    LHTTP.ProtocolVersion := THTTPProtocolVersion.HTTP_2_0;
    LHTTP.CustomHeaders['authorization'] := 'bearer ' + AParams.JWT;
    LHTTP.CustomHeaders['apns-topic'] := AParams.Topic;
    LHTTP.CustomHeaders['apns-push-type'] := 'alert';
    LHTTP.CustomHeaders['apns-priority'] := AParams.PriorityValue.ToString;
    LHTTP.CustomHeaders['apns-expiration'] := '0'; // Send immediately
    LContent := TStringStream.Create(APayload);
    try
      LResponse := LHTTP.Post(cAPNSURLs[AParams.IsProduction] + '/3/device/' + AParams.Token, LContent);
    finally
      LContent.Free;
    end;
    Result := LResponse.StatusCode = 200;
  finally
    LHTTP.Free;
  end;
end;

function TAPNSSender.PostMessage(const AParams: TAPNSMessageParams; const APayload: string): Boolean;
begin
  Result := False;
  try
    Result := DoPostMessage(AParams, APayload);
  except
    // TODO
  end;
end;

initialization
  APNSSender := TAPNSSender.Create;

end.

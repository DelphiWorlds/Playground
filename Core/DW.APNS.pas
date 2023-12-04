unit DW.APNS;

interface

uses
  System.JSON;

type
  TAPNSPriority = (Low, Medium, High);

  TAPNSMessageParams = record
    IsProduction: Boolean;
    JWT: string;
    Priority: TAPNSPriority;
    Token: string;
    Topic: string;
    constructor Create(const AJWT, ATopic, AToken: string; const APriority: TAPNSPriority = TAPNSPriority.Medium);
    function PriorityValue: Integer;
  end;

  IAPNSSender = interface(IInterface)
    ['{D17D3B04-AFAE-462D-B58A-9E6461218900}']
    function PostMessage(const AParams: TAPNSMessageParams; const APayload: string): Boolean;
  end;

  IAPNSMessage = interface(IInterface)
    ['{C9C92FF6-F2AA-45AA-8A52-4DAD87794FE1}']
    function GetBadgeCount: Integer;
    function GetBody: string;
    function GetCategory: string;
    function GetImageURL: string;
    function GetIsContentAvailable: Boolean;
    function GetIsMutableContent: Boolean;
    function GetSoundName: string;
    function GetSubtitle: string;
    function GetTitle: string;
    procedure SetBadgeCount(const Value: Integer);
    procedure SetBody(const Value: string);
    procedure SetCategory(const Value: string);
    procedure SetImageURL(const Value: string);
    procedure SetIsContentAvailable(const Value: Boolean);
    procedure SetIsMutableContent(const Value: Boolean);
    procedure SetSoundName(const Value: string);
    procedure SetSubtitle(const Value: string);
    procedure SetTitle(const Value: string);
    function ToJSON: string;
    function ToJSONValue: TJSONValue;
    property BadgeCount: Integer read GetBadgeCount write SetBadgeCount;
    property Body: string read GetBody write SetBody;
    property Category: string read GetCategory write SetCategory;
    property ImageURL: string read GetImageURL write SetImageURL;
    property IsContentAvailable: Boolean read GetIsContentAvailable write SetIsContentAvailable;
    property IsMutableContent: Boolean read GetIsMutableContent write SetIsMutableContent;
    property SoundName: string read GetSoundName write SetSoundName;
    property Subtitle: string read GetSubtitle write SetSubtitle;
    property Title: string read GetTitle write SetTitle;
  end;

  TAPNSMessage = class(TInterfacedObject, IAPNSMessage)
  private
    FBadgeCount: Integer;
    FBody: string;
    FCategory: string;
    FImageURL: string;
    FIsContentAvailable: Boolean;
    FIsMutableContent: Boolean;
    FSoundName: string;
    FSubtitle: string;
    FTitle: string;
  public
    { IAPNSMessage }
    function GetBadgeCount: Integer;
    function GetBody: string;
    function GetCategory: string;
    function GetImageURL: string;
    function GetIsContentAvailable: Boolean;
    function GetIsMutableContent: Boolean;
    function GetSoundName: string;
    function GetSubtitle: string;
    function GetTitle: string;
    procedure SetBadgeCount(const Value: Integer);
    procedure SetBody(const Value: string);
    procedure SetCategory(const Value: string);
    procedure SetImageURL(const Value: string);
    procedure SetIsContentAvailable(const Value: Boolean);
    procedure SetIsMutableContent(const Value: Boolean);
    procedure SetSoundName(const Value: string);
    procedure SetSubtitle(const Value: string);
    procedure SetTitle(const Value: string);
    function ToJSON: string;
    function ToJSONValue: TJSONValue;
  public
    constructor Create;
  end;

var
  APNSSender: IAPNSSender;

implementation

uses
  System.SysUtils;

{ TAPNSMessageParams }

constructor TAPNSMessageParams.Create(const AJWT, ATopic, AToken: string; const APriority: TAPNSPriority = TAPNSPriority.Medium);
begin
  JWT := AJWT;
  Topic := ATopic;
  Token := AToken;
  Priority := APriority;
end;

function TAPNSMessageParams.PriorityValue: Integer;
begin
  case Priority of
    TAPNSPriority.Low:
      Result := 0;
    TAPNSPriority.High:
      Result := 10;
  else
    Result := 5; // Medium, or forgot to implement level added
  end;
end;

{ TAPNSMessage }

constructor TAPNSMessage.Create;
begin
  inherited;
  FBadgeCount := -1; // i.e. Do not include
end;

function TAPNSMessage.GetBadgeCount: Integer;
begin
  Result := FBadgeCount;
end;

function TAPNSMessage.GetBody: string;
begin
  Result := FBody;
end;

function TAPNSMessage.GetCategory: string;
begin
  Result := FCategory;
end;

function TAPNSMessage.GetImageURL: string;
begin
  Result := FImageURL;
end;

function TAPNSMessage.GetIsContentAvailable: Boolean;
begin
  Result := FIsContentAvailable;
end;

function TAPNSMessage.GetIsMutableContent: Boolean;
begin
  Result := FIsMutableContent;
end;

function TAPNSMessage.GetSoundName: string;
begin
  Result := FSoundName;
end;

function TAPNSMessage.GetSubtitle: string;
begin
  Result := FSubtitle;
end;

function TAPNSMessage.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TAPNSMessage.SetBadgeCount(const Value: Integer);
begin
  FBadgeCount := Value;
end;

procedure TAPNSMessage.SetBody(const Value: string);
begin
  FBody := Value;
end;

procedure TAPNSMessage.SetCategory(const Value: string);
begin
  FCategory := Value;
end;

procedure TAPNSMessage.SetImageURL(const Value: string);
begin
  FImageURL := Value;
end;

procedure TAPNSMessage.SetIsContentAvailable(const Value: Boolean);
begin
  FIsContentAvailable := Value;
end;

procedure TAPNSMessage.SetIsMutableContent(const Value: Boolean);
begin
  FIsMutableContent := Value;
end;

procedure TAPNSMessage.SetSoundName(const Value: string);
begin
  FSoundName := Value;
end;

procedure TAPNSMessage.SetSubtitle(const Value: string);
begin
  FSubtitle := Value;
end;

procedure TAPNSMessage.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

function TAPNSMessage.ToJSON: string;
var
  LValue: TJSONValue;
begin
  LValue := ToJSONValue;
  try
    Result := LValue.ToJSON;
  finally
    LValue.Free;
  end;
end;

function TAPNSMessage.ToJSONValue: TJSONValue;
var
  LPayload, LAPS, LAlert: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  LAPS := TJSONObject.Create;
  LPayload.AddPair('aps', LAPS);
  if not FCategory.IsEmpty then
    LAPS.AddPair('category', FCategory);
  if not FSoundName.IsEmpty then
    LAPS.AddPair('sound', FSoundName);
  if FBadgeCount >= 0 then
    LAPS.AddPair('badge', TJSONNumber.Create(FBadgeCount));
  if FIsContentAvailable then
    LAPS.AddPair('content-available', TJSONNumber.Create(1));
  if not FTitle.IsEmpty then
  begin
    LAlert := TJSONObject.Create;
    LAlert.AddPair('title', TJSONString.Create(FTitle));
    if not FSubtitle.IsEmpty then
      LAlert.AddPair('subtitle', TJSONString.Create(FSubtitle));
    if not FBody.IsEmpty then
      LAlert.AddPair('body', TJSONString.Create(FBody));
    LAPS.AddPair('alert', LAlert);
  end;
  if FIsMutableContent then
    LAPS.AddPair('mutable-content', TJSONNumber.Create(1));
  if not FImageURL.IsEmpty then
    LPayload.AddPair('imageUrl', FImageURL);
  Result := LPayload;
end;

end.

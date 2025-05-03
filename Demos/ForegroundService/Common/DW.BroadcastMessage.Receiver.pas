unit DW.BroadcastMessage.Receiver;

interface

uses
  System.JSON,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  DW.MultiReceiver.Android;

type
  TMessageReceivedProc = reference to procedure(const Kind: Integer; const Msg: string);

  TCustomBroadcastMessageReceiver = class(TMultiReceiver)
  protected
    procedure ConfigureActions; override;
    procedure HandleMessage(const AKind: Integer; const AMsg: string); virtual; abstract;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create;
  end;

  TBroadcastMessageReceiver = class(TCustomBroadcastMessageReceiver)
  private
    FHandler: TMessageReceivedProc;
  protected
    procedure HandleMessage(const AKind: Integer; const AMsg: string); override;
  public
    constructor Create(const AHandler: TMessageReceivedProc);
  end;

  TJSONMessageReceivedProc = reference to procedure(const Kind: Integer; const JSON: TJSONValue);

  TJSONBroadcastMessageReceiver = class(TCustomBroadcastMessageReceiver)
  private
    FHandler: TJSONMessageReceivedProc;
  protected
    procedure HandleMessage(const AKind: Integer; const AMsg: string); override;
  public
    constructor Create(const AHandler: TJSONMessageReceivedProc);
  end;

implementation

uses
  Androidapi.Helpers,
  DW.BroadcastMessage.Consts;

{ TCustomBroadcastMessageReceiver }

constructor TCustomBroadcastMessageReceiver.Create;
begin
  inherited Create(True);
end;

procedure TCustomBroadcastMessageReceiver.ConfigureActions;
begin
  IntentFilter.addAction(BroadcastMessageConsts.ACTION_MESSAGE);
end;

procedure TCustomBroadcastMessageReceiver.Receive(context: JContext; intent: JIntent);
var
  LKind: Integer;
  LMessage: JString;
begin
  if intent.hasExtra(BroadcastMessageConsts.EXTRA_MESSAGE_KIND) and intent.hasExtra(BroadcastMessageConsts.EXTRA_MESSAGE) then
  begin
    LKind := intent.getIntExtra(BroadcastMessageConsts.EXTRA_MESSAGE_KIND, 0);
    LMessage := intent.getStringExtra(BroadcastMessageConsts.EXTRA_MESSAGE);
    HandleMessage(LKind, JStringToString(LMessage));
  end;
end;

{ TBroadcastMessageReceiver }

constructor TBroadcastMessageReceiver.Create(const AHandler: TMessageReceivedProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TBroadcastMessageReceiver.HandleMessage(const AKind: Integer; const AMsg: string);
begin
  if Assigned(FHandler) then
    FHandler(AKind, AMsg);
end;

{ TJSONBroadcastMessageReceiver }

constructor TJSONBroadcastMessageReceiver.Create(const AHandler: TJSONMessageReceivedProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TJSONBroadcastMessageReceiver.HandleMessage(const AKind: Integer; const AMsg: string);
var
  LJSON: TJSONValue;
begin
  if Assigned(FHandler) then
  begin
    LJSON := TJSONObject.ParseJSONValue(AMsg);
    try
      FHandler(AKind, LJSON);
    finally
      LJSON.Free;
    end;
  end;
end;

end.

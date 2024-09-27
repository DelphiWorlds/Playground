unit DW.MessageReceiver;

interface

uses
  System.JSON,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  DW.MultiReceiver.Android;

type
  TMessageReceivedProc = reference to procedure(const Kind: Integer; const Msg: string);

  TCustomMessageReceiver = class(TMultiReceiver)
  protected
    procedure ConfigureActions; override;
    procedure HandleMessage(const AKind: Integer; const AMsg: string); virtual; abstract;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create;
  end;

  TMessageReceiver = class(TCustomMessageReceiver)
  private
    FHandler: TMessageReceivedProc;
  protected
    procedure HandleMessage(const AKind: Integer; const AMsg: string); override;
  public
    constructor Create(const AHandler: TMessageReceivedProc);
  end;

  TJSONMessageReceivedProc = reference to procedure(const JSON: TJSONValue);

  TJSONMessageReceiver = class(TCustomMessageReceiver)
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
  DW.MessageReceiver.Consts;

{ TCustomMessageReceiver }

constructor TCustomMessageReceiver.Create;
begin
  inherited Create(True);
end;

procedure TCustomMessageReceiver.ConfigureActions;
begin
  IntentFilter.addAction(MessageReceiverConsts.ACTION_MESSAGE);
end;

procedure TCustomMessageReceiver.Receive(context: JContext; intent: JIntent);
var
  LKind: Integer;
  LMessage: JString;
begin
  if intent.hasExtra(MessageReceiverConsts.EXTRA_MESSAGE_KIND) and intent.hasExtra(MessageReceiverConsts.EXTRA_MESSAGE) then
  begin
    LKind := intent.getIntExtra(MessageReceiverConsts.EXTRA_MESSAGE_KIND, 0);
    LMessage := intent.getStringExtra(MessageReceiverConsts.EXTRA_MESSAGE);
    HandleMessage(LKind, JStringToString(LMessage));
  end;
end;

{ TMessageReceiver }

constructor TMessageReceiver.Create(const AHandler: TMessageReceivedProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TMessageReceiver.HandleMessage(const AKind: Integer; const AMsg: string);
begin
  if Assigned(FHandler) then
    FHandler(AKind, AMsg);
end;

{ TJSONMessageReceiver }

constructor TJSONMessageReceiver.Create(const AHandler: TJSONMessageReceivedProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TJSONMessageReceiver.HandleMessage(const AKind: Integer; const AMsg: string);
var
  LJSON: TJSONValue;
begin
  if Assigned(FHandler) then
  begin
    LJSON := TJSONObject.ParseJSONValue(AMsg);
    if LJSON <> nil then
    try
      FHandler(LJSON);
    finally
      LJSON.Free;
    end;
  end;
end;

end.

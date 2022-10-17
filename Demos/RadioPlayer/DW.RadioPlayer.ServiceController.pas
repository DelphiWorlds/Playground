unit DW.RadioPlayer.ServiceController;

interface

{$IF Defined(ANDROID) and not Defined(SERVICE)}
  {$DEFINE ANDROIDAPP}
{$ENDIF}

uses
{$IF Defined(ANDROIDAPP)}
  Androidapi.JNI.GraphicsContentViewText,
  DW.MultiReceiver.Android,
{$ENDIF}
  System.Classes,
  DW.RadioPlayer.Common;

type
  TRadioStatusChangedEvent = procedure(Sender: TObject; const Status: TRadioStatus) of object;

  TCustomRadioServiceController = class(TObject)
  private
    FServiceName: string;
    FOnRadioStatusChanged: TRadioStatusChangedEvent;
    FOnServiceStarted: TNotifyEvent;
    FOnStreamMetadata: TStreamMetadataEvent;
  protected
    procedure DoRadioStatusChanged(const AStatus: TRadioStatus);
    procedure DoServiceStarted;
    procedure DoStreamMetadata(const AMetadata: string);
  public
    procedure Pause; virtual;
    procedure Play; overload; virtual;
    procedure Play(const AURL: string); overload; virtual;
    // Returns True if the service is already running
    function StartService(const AServiceName: string): Boolean; virtual;
    procedure Stop; virtual;
    property ServiceName: string read FServiceName write FServiceName;
    property OnRadioStatusChanged: TRadioStatusChangedEvent read FOnRadioStatusChanged write FOnRadioStatusChanged;
    property OnServiceStarted: TNotifyEvent read FOnServiceStarted write FOnServiceStarted;
    property OnStreamMetadata: TStreamMetadataEvent read FOnStreamMetadata write FOnStreamMetadata;
  end;

{$IF Defined(ANDROIDAPP)}
  TMessageEvent = procedure(Sender: TObject; const Msg: string) of object;
  TStateEvent = procedure(Sender: TObject; const State: Integer) of object;

  TServiceReceiver = class(TMultiReceiver)
  private
    FOnMessage: TMessageEvent;
    FOnState: TStateEvent;
    procedure DoMessage(const AMsg: string);
    procedure DoState(const AState: Integer);
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnState: TStateEvent read FOnState write FOnState;
  end;

  TPlatformRadioServiceController = class(TCustomRadioServiceController)
  private
    FReceiver: TServiceReceiver;
    procedure ReceiverMessageHandler(Sender: TObject; const AMsg: string);
    procedure ReceiverStateHandler(Sender: TObject; const AState: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Pause; override;
    procedure Play; overload; override;
    procedure Play(const AURL: string); overload; override;
    function StartService(const AServiceName: string): Boolean; override;
    procedure Stop; override;
  end;
{$ENDIF}

implementation

uses
  System.JSON,
  {$IF Defined(ANDROIDAPP)}
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.ServiceCommander.Android, DW.Android.Helpers,
  {$ENDIF}
  DW.Consts.Android;

{ TCustomRadioServiceController }

procedure TCustomRadioServiceController.DoRadioStatusChanged(const AStatus: TRadioStatus);
begin
  if Assigned(FOnRadioStatusChanged) then
    FOnRadioStatusChanged(Self, AStatus);
end;

procedure TCustomRadioServiceController.DoServiceStarted;
begin
  if Assigned(FOnServiceStarted) then
    FOnServiceStarted(Self);
end;

procedure TCustomRadioServiceController.DoStreamMetadata(const AMetadata: string);
begin
  if Assigned(FOnStreamMetadata) then
    FOnStreamMetadata(Self, AMetadata);
end;

procedure TCustomRadioServiceController.Pause;
begin
  //
end;

procedure TCustomRadioServiceController.Play(const AURL: string);
begin

end;

procedure TCustomRadioServiceController.Play;
begin
  //
end;

function TCustomRadioServiceController.StartService(const AServiceName: string): Boolean;
begin
  Result := True;
end;

procedure TCustomRadioServiceController.Stop;
begin
  //
end;

{$IF Defined(ANDROIDAPP)}
{ TServiceReceiver }

procedure TServiceReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceStateAction));
  IntentFilter.addAction(StringToJString(cServiceMessageAction));
end;

procedure TServiceReceiver.DoState(const AState: Integer);
begin
  if Assigned(FOnState) then
    FOnState(Self, AState);
end;

procedure TServiceReceiver.Receive(context: JContext; intent: JIntent);
begin
  if intent.getAction.equals(StringToJString(cServiceStateAction)) then
    DoState(intent.getIntExtra(StringToJString(cServiceBroadcastParamState), cServiceStateUnknown))
  else if intent.getAction.equals(StringToJString(cServiceMessageAction)) then
    DoMessage(JStringToString(intent.getStringExtra(StringToJString(cServiceBroadcastParamMessage))));
end;

procedure TServiceReceiver.DoMessage(const AMsg: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, AMsg);
end;

{ TPlatformRadioServiceController }

constructor TPlatformRadioServiceController.Create;
begin
  inherited;
  FReceiver := TServiceReceiver.Create(True);
  FReceiver.OnMessage := ReceiverMessageHandler;
  FReceiver.OnState := ReceiverStateHandler;
end;

destructor TPlatformRadioServiceController.Destroy;
begin
  FReceiver.Free;
  inherited;
end;

procedure TPlatformRadioServiceController.ReceiverMessageHandler(Sender: TObject; const AMsg: string);
var
  LJSON: TJSONValue;
  LMessageType: Integer;
  LContent: string;
begin
  LJSON := TJSONObject.ParseJSONValue(AMsg);
  if LJSON <> nil then
  try
    if LJSON.TryGetValue('MessageType', LMessageType) then
    begin
      case LMessageType of
        cServiceMessageTypeRadioStreamMetadata:
        begin
          if LJSON.TryGetValue('Content', LContent) then
            DoStreamMetadata(LContent);
        end;
      end;
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TPlatformRadioServiceController.ReceiverStateHandler(Sender: TObject; const AState: Integer);
begin
  if AState < cServiceStateRadioBase then
  begin
    case AState of
      cServiceStateStarted:
        DoServiceStarted;
    end;
  end
  else
    DoRadioStatusChanged(TRadioStatus(AState - cServiceStateRadioBase));
end;

procedure TPlatformRadioServiceController.Pause;
begin
  TServiceCommander.SendCommandJSON(TRadioCommand.Create(cServiceCommandRadioPause).ToJSON);
end;

procedure TPlatformRadioServiceController.Play;
begin
  TServiceCommander.SendCommandJSON(TRadioCommand.Create(cServiceCommandRadioPlay).ToJSON);
end;

procedure TPlatformRadioServiceController.Play(const AURL: string);
begin
  TServiceCommander.SendCommandJSON(TRadioCommand.Create(cServiceCommandRadioPlay, AURL).ToJSON);
end;

function TPlatformRadioServiceController.StartService(const AServiceName: string): Boolean;
begin
  Result := TAndroidHelperEx.IsServiceRunning(AServiceName);
  if not Result then
    TServiceCommander.StartService(AServiceName);
end;

procedure TPlatformRadioServiceController.Stop;
begin
  TServiceCommander.SendCommandJSON(TRadioCommand.Create(cServiceCommandRadioStop).ToJSON);
end;
{$ENDIF}

end.

unit DW.RadioServiceController;

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
  TStateEvent = procedure(Sender: TObject; const State: Integer) of object;

  TCustomRadioServiceController = class(TObject)
  private
    FServiceName: string;
    FOnRadioStatusChanged: TRadioStatusChangedEvent;
    FOnServiceStarted: TNotifyEvent;
  protected
    procedure DoRadioStatusChanged(const AStatus: TRadioStatus);
    procedure DoServiceStarted;
  public
    procedure Pause; virtual;
    procedure Play; overload; virtual;
    procedure Play(const AURL: string); overload; virtual;
    procedure StartService(const AServiceName: string); virtual;
    procedure Stop; virtual;
    property ServiceName: string read FServiceName write FServiceName;
    property OnRadioStatusChanged: TRadioStatusChangedEvent read FOnRadioStatusChanged write FOnRadioStatusChanged;
    property OnServiceStarted: TNotifyEvent read FOnServiceStarted write FOnServiceStarted;
  end;

{$IF Defined(ANDROIDAPP)}
  TServiceStateReceiver = class(TMultiReceiver)
  private
    FOnState: TStateEvent;
    procedure DoState(const AState: Integer);
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    property OnState: TStateEvent read FOnState write FOnState;
  end;

  TPlatformRadioServiceController = class(TCustomRadioServiceController)
  private
    FReceiver: TServiceStateReceiver;
    procedure ReceiverStateHandler(Sender: TObject; const AState: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Pause; override;
    procedure Play; overload; override;
    procedure Play(const AURL: string); overload; override;
    procedure StartService(const AServiceName: string); override;
    procedure Stop; override;
  end;
{$ENDIF}

implementation

uses
  {$IF Defined(ANDROIDAPP)}
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.ServiceCommander.Android,
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

procedure TCustomRadioServiceController.StartService(const AServiceName: string);
begin
  //
end;

procedure TCustomRadioServiceController.Stop;
begin
  //
end;

{$IF Defined(ANDROIDAPP)}
{ TServiceStateReceiver }

procedure TServiceStateReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceStateAction));
end;

procedure TServiceStateReceiver.DoState(const AState: Integer);
begin
  if Assigned(FOnState) then
    FOnState(Self, AState);
end;

procedure TServiceStateReceiver.Receive(context: JContext; intent: JIntent);
begin
  if intent.getAction.equals(StringToJString(cServiceStateAction)) then
    DoState(intent.getIntExtra(StringToJString(cServiceBroadcastParamState), cServiceStateUnknown));
end;

{ TPlatformRadioServiceController }

constructor TPlatformRadioServiceController.Create;
begin
  inherited;
  FReceiver := TServiceStateReceiver.Create(True);
  FReceiver.OnState := ReceiverStateHandler;
end;

destructor TPlatformRadioServiceController.Destroy;
begin
  FReceiver.Free;
  inherited;
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

procedure TPlatformRadioServiceController.StartService(const AServiceName: string);
begin
  TServiceCommander.StartService(AServiceName);
end;

procedure TPlatformRadioServiceController.Stop;
begin
  TServiceCommander.SendCommandJSON(TRadioCommand.Create(cServiceCommandRadioStop).ToJSON);
end;
{$ENDIF}

end.

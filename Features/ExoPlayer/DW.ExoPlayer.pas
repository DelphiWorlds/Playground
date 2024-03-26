unit DW.ExoPlayer;

interface

uses
  System.Messaging,
  FMX.Controls.Presentation;

type
  TAppEventsHandler = class(TObject)
  private
    FIsBackground: Boolean;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
  protected
    procedure BecameActive; virtual;
    procedure EnteredBackground;
    procedure WillBecomeInactive; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCustomPlatformExoPlayer = class(TAppEventsHandler)
  private
    FView: TPresentedControl;
  protected
    FControlsTimeout: Integer;
    procedure Play(const AURL: string); virtual;
    procedure SetControlsTimeout(const Value: Integer); virtual;
    property ControlsTimeout: Integer read FControlsTimeout write SetControlsTimeout;
    property View: TPresentedControl read FView;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TExoPlayer = class(TObject)
  private
    FPlatformExoPlayer: TCustomPlatformExoPlayer;
    function GetView: TPresentedControl;
    function GetControlsTimeout: Integer;
    procedure SetControlsTimeout(const Value: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Play(const AURL: string);
    property ControlsTimeout: Integer read GetControlsTimeout write SetControlsTimeout;
    property View: TPresentedControl read GetView;
  end;

implementation

uses
  FMX.Platform, FMX.Types,
  DW.ExoPlayer.Android,
  DW.ExoPlayer.View;

{ TAppEventsHandler }

constructor TAppEventsHandler.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TAppEventsHandler.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TAppEventsHandler.BecameActive;
begin
  FIsBackground := False;
end;

procedure TAppEventsHandler.EnteredBackground;
begin
  //
end;

procedure TAppEventsHandler.WillBecomeInactive;
begin
  FIsBackground := True;
end;

procedure TAppEventsHandler.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
      BecameActive;
    TApplicationEvent.EnteredBackground:
      EnteredBackground;
    TApplicationEvent.WillBecomeInactive:
      WillBecomeInactive;
  end;
end;

{ TCustomPlatformExoPlayer }

constructor TCustomPlatformExoPlayer.Create;
begin
  inherited;
  FView := TExoPlayerView.Create(nil);
  FView.Align := TAlignLayout.Client;
end;

destructor TCustomPlatformExoPlayer.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TCustomPlatformExoPlayer.Play(const AURL: string);
begin
  //
end;

procedure TCustomPlatformExoPlayer.SetControlsTimeout(const Value: Integer);
begin
  FControlsTimeout := Value;
end;

{ TExoPlayer }

constructor TExoPlayer.Create;
begin
  inherited;
  FPlatformExoPlayer := TPlatformExoPlayer.Create;
end;

destructor TExoPlayer.Destroy;
begin
  FPlatformExoPlayer.Free;
  inherited;
end;

function TExoPlayer.GetControlsTimeout: Integer;
begin
  Result := FPlatformExoPlayer.ControlsTimeout;
end;

function TExoPlayer.GetView: TPresentedControl;
begin
  Result := FPlatformExoPlayer.View;
end;

procedure TExoPlayer.Play(const AURL: string);
begin
  FPlatformExoPlayer.Play(AURL);
end;

procedure TExoPlayer.SetControlsTimeout(const Value: Integer);
begin
  FPlatformExoPlayer.ControlsTimeout := Value;
end;

end.

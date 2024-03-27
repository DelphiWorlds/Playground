unit DW.ExoPlayer;

interface

uses
  System.Messaging, System.Classes,
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

  TPlayerState = (NoPlayer, Prepared, Playing, Paused, Stopped, Completed);

  TExoPlayer = class;

  TCustomPlatformExoPlayer = class(TAppEventsHandler)
  private
    FExoPlayer: TExoPlayer;
    FPlayerState: TPlayerState;
    FView: TPresentedControl;
  protected
    FControllerTimeout: Integer;
    FUseController: Boolean;
    procedure Pause; virtual;
    procedure Play(const AURL: string = ''); virtual;
    procedure Prepare(const AURL: string); virtual;
    procedure SetControllerTimeout(const Value: Integer); virtual;
    procedure SetPlayerState(const Value: TPlayerState);
    procedure SetUseController(const Value: Boolean); virtual;
    procedure ShowController(const Value: Boolean); virtual;
    procedure Stop; virtual;
    procedure ViewTouched;
    property ControllerTimeout: Integer read FControllerTimeout write SetControllerTimeout;
    property UseController: Boolean read FUseController write SetUseController;
    property View: TPresentedControl read FView;
  public
    constructor Create(const AExoPlayer: TExoPlayer); virtual;
    destructor Destroy; override;
  end;

  TPlayerStateChangedEvent = procedure(Sender: TObject; const PlayerState: TPlayerState) of object;

  TExoPlayer = class(TObject)
  private
    FPlatformExoPlayer: TCustomPlatformExoPlayer;
    FOnClick: TNotifyEvent;
    FOnPlayerStateChanged: TPlayerStateChangedEvent;
    function GetView: TPresentedControl;
    function GetControllerTimeout: Integer;
    function GetUseController: Boolean;
    procedure SetControllerTimeout(const Value: Integer); virtual;
    procedure SetUseController(const Value: Boolean);
  protected
//    function GetCurrentPosition: int64;
//    function GetDuration: int64;
    procedure PlayerStateChanged(const APlayerState: TPlayerState);
//    procedure SetLooping(const AValue: Boolean);
//    procedure SetVolume(const AValue: Single);
//    procedure SetPlaybackSpeed(const AValue: Single);
    procedure ViewTouched;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Pause;
    /// <summary>
    ///   Resumes playback if paused, or if a URL is supplied, prepares an item with the URL and plays it
    /// </summary>
    procedure Play(const AURL: string = '');
    procedure Prepare(const AURL: string);
//    procedure SeekTo(const AValue: Int64);
    procedure ShowController(const Value: Boolean);
    procedure Stop;
    /// <summary>
    ///   Determines when the player controls are hidden. To prevent them from showing at all, use a value <= 0
    /// </summary>
    property ControllerTimeout: Integer read GetControllerTimeout write SetControllerTimeout;
    property UseController: Boolean read GetUseController write SetUseController;
    property View: TPresentedControl read GetView;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    /// <summary>
    ///   Called when the player state changes
    /// </summary>
    property OnPlayerStateChanged: TPlayerStateChangedEvent read FOnPlayerStateChanged write FOnPlayerStateChanged;
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

constructor TCustomPlatformExoPlayer.Create(const AExoPlayer: TExoPlayer);
begin
  inherited Create;
  FExoPlayer := AExoPlayer;
  FView := TExoPlayerView.Create(nil);
  FView.Align := TAlignLayout.Client;
end;

destructor TCustomPlatformExoPlayer.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TCustomPlatformExoPlayer.Pause;
begin
  //
end;

procedure TCustomPlatformExoPlayer.Play(const AURL: string = '');
begin
  //
end;

procedure TCustomPlatformExoPlayer.Prepare(const AURL: string);
begin
  //
end;

procedure TCustomPlatformExoPlayer.SetControllerTimeout(const Value: Integer);
begin
  FControllerTimeout := Value;
end;

procedure TCustomPlatformExoPlayer.SetPlayerState(const Value: TPlayerState);
begin
  if Value <> FPlayerState then
  begin
    FPlayerState := Value;
    FExoPlayer.PlayerStateChanged(FPlayerState);
  end;
end;

procedure TCustomPlatformExoPlayer.SetUseController(const Value: Boolean);
begin
  FUseController := Value;
end;

procedure TCustomPlatformExoPlayer.ShowController(const Value: Boolean);
begin
  //
end;

procedure TCustomPlatformExoPlayer.Stop;
begin
  //
end;

procedure TCustomPlatformExoPlayer.ViewTouched;
begin
  FExoPlayer.ViewTouched;
end;

{ TExoPlayer }

constructor TExoPlayer.Create;
begin
  inherited;
  FPlatformExoPlayer := TPlatformExoPlayer.Create(Self);
end;

destructor TExoPlayer.Destroy;
begin
  FPlatformExoPlayer.Free;
  inherited;
end;

function TExoPlayer.GetControllerTimeout: Integer;
begin
  Result := FPlatformExoPlayer.ControllerTimeout;
end;

function TExoPlayer.GetUseController: Boolean;
begin
  Result := FPlatformExoPlayer.UseController;
end;

function TExoPlayer.GetView: TPresentedControl;
begin
  Result := FPlatformExoPlayer.View;
end;

procedure TExoPlayer.Pause;
begin
  FPlatformExoPlayer.Pause;
end;

procedure TExoPlayer.Play(const AURL: string = '');
begin
  FPlatformExoPlayer.Play(AURL);
end;

procedure TExoPlayer.PlayerStateChanged(const APlayerState: TPlayerState);
begin
  if Assigned(FOnPlayerStateChanged) then
    FOnPlayerStateChanged(Self, APlayerState);
end;

procedure TExoPlayer.Prepare(const AURL: string);
begin
  FPlatformExoPlayer.Prepare(AURL);
end;

procedure TExoPlayer.SetControllerTimeout(const Value: Integer);
begin
  FPlatformExoPlayer.ControllerTimeout := Value;
end;

procedure TExoPlayer.SetUseController(const Value: Boolean);
begin
  FPlatformExoPlayer.UseController := Value;
end;

procedure TExoPlayer.ShowController(const Value: Boolean);
begin
  FPlatformExoPlayer.ShowController(Value);
end;

procedure TExoPlayer.Stop;
begin
  FPlatformExoPlayer.Stop;
end;

procedure TExoPlayer.ViewTouched;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

end.

unit DW.VideoPlayer;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  System.Classes,
  DW.OSControl;

type
  TVideoPlayer = class;

  TPlayerView = class(TOSControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCustomPlatformVideoPlayer = class(TObject)
  private
    FFileName: string;
    FPlayerView: TPlayerView;
    FVideoPlayer: TVideoPlayer;
    function GetIsVisible: Boolean;
    procedure PlayerViewResizedHandler(Sender: TObject);
    procedure SetIsVisible(const Value: Boolean);
  protected
    procedure DoPlay; virtual;
    function IsPlaying: Boolean; virtual;
    procedure Pause; virtual;
    procedure Play(const AFileName: string);
    procedure PlayerViewResized; virtual;
    procedure Resume; virtual;
    property FileName: string read FFileName;
    property IsVisible: Boolean read GetIsVisible write SetIsVisible;
    property PlayerView: TPlayerView read FPlayerView;
    property VideoPlayer: TVideoPlayer read FVideoPlayer;
  public
    constructor Create(const AVideoPlayer: TVideoPlayer); virtual;
    destructor Destroy; override;
  end;

  TVideoPlayer = class(TObject)
  private
    FPlatformVideoPlayer: TCustomPlatformVideoPlayer;
    function GetPlayerView: TPlayerView;
    function GetIsVisible: Boolean;
    procedure SetIsVisible(const Value: Boolean);
  protected
    //
  public
    constructor Create;
    destructor Destroy; override;
    function IsPlaying: Boolean;
    procedure Pause;
    procedure Play(const AFileName: string);
    procedure Resume;
    procedure Stop;
    property IsVisible: Boolean read GetIsVisible write SetIsVisible;
    property PlayerView: TPlayerView read GetPlayerView;
  end;

implementation

uses
  FMX.Types,
  {$IF Defined(IOS)}
  DW.VideoPlayer.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.VideoPlayer.Android;
  {$ELSE}
  DW.VideoPlayer.Default;
  {$ENDIF}

{ TPlayerView }

constructor TPlayerView.Create(AOwner: TComponent);
begin
  inherited;
  Align := TAlignLayout.Client;
end;

{ TCustomPlatformVideoPlayer }

constructor TCustomPlatformVideoPlayer.Create(const AVideoPlayer: TVideoPlayer);
begin
  inherited Create;
  FVideoPlayer := AVideoPlayer;
  FPlayerView := TPlayerView.Create(nil);
  FPlayerView.OnResized := PlayerViewResizedHandler;
end;

destructor TCustomPlatformVideoPlayer.Destroy;
begin
  FPlayerView.Free;
  inherited;
end;

procedure TCustomPlatformVideoPlayer.DoPlay;
begin
  //
end;

function TCustomPlatformVideoPlayer.GetIsVisible: Boolean;
begin
  Result := FPlayerView.Visible;
end;

function TCustomPlatformVideoPlayer.IsPlaying: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformVideoPlayer.Pause;
begin
  //
end;

procedure TCustomPlatformVideoPlayer.Play(const AFileName: string);
begin
  FFileName := AFileName;
  DoPlay;
end;

procedure TCustomPlatformVideoPlayer.PlayerViewResized;
begin
  //
end;

procedure TCustomPlatformVideoPlayer.PlayerViewResizedHandler(Sender: TObject);
begin
  PlayerViewResized;
end;

procedure TCustomPlatformVideoPlayer.Resume;
begin
  //
end;

procedure TCustomPlatformVideoPlayer.SetIsVisible(const Value: Boolean);
begin
  FPlayerView.Visible := Value;
end;

{ TVideoPlayer }

constructor TVideoPlayer.Create;
begin
  inherited;
  FPlatformVideoPlayer := TPlatformVideoPlayer.Create(Self);
end;

destructor TVideoPlayer.Destroy;
begin
  FPlatformVideoPlayer.Free;
  inherited;
end;

function TVideoPlayer.GetIsVisible: Boolean;
begin
  Result := FPlatformVideoPlayer.IsVisible;
end;

function TVideoPlayer.GetPlayerView: TPlayerView;
begin
  Result := FPlatformVideoPlayer.PlayerView;
end;

function TVideoPlayer.IsPlaying: Boolean;
begin
  Result := FPlatformVideoPlayer.IsPlaying;
end;

procedure TVideoPlayer.Pause;
begin
  FPlatformVideoPlayer.Pause;
end;

procedure TVideoPlayer.Play(const AFileName: string);
begin
  FPlatformVideoPlayer.Play(AFileName);
end;

procedure TVideoPlayer.Resume;
begin
  FPlatformVideoPlayer.Resume;
end;

procedure TVideoPlayer.SetIsVisible(const Value: Boolean);
begin
  FPlatformVideoPlayer.IsVisible := Value;
end;

procedure TVideoPlayer.Stop;
begin
  FPlatformVideoPlayer.Pause;
end;

end.

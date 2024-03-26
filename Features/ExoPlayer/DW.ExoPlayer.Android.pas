unit DW.ExoPlayer.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  DW.ExoPlayer, DW.ExoPlayer.View.Android,
  DW.Androidapi.JNI.AndroidX.Media3.ExoPlayer, DW.Androidapi.JNI.AndroidX.Media3.UI, DW.Androidapi.JNI.AndroidX.Media3.Common;

(*
class PlayerActivity : AppCompatActivity() {

    private val viewBinding by lazy(LazyThreadSafetyMode.NONE) {
        ActivityPlayerBinding.inflate(layoutInflater)
    }

    private val playbackStateListener: Player.Listener = playbackStateListener()
    private var player: ExoPlayer? = null

    private var playWhenReady = true
    private var currentItem = 0
    private var playbackPosition = 0L

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(viewBinding.root)
    }

    public override fun onStart() {
        super.onStart()
        if (Util.SDK_INT > 23) {
            initializePlayer()
        }
    }

    public override fun onResume() {
        super.onResume()
        hideSystemUi()
        if (Util.SDK_INT <= 23 || player == null) {
            initializePlayer()
        }
    }

    public override fun onPause() {
        super.onPause()
        if (Util.SDK_INT <= 23) {
            releasePlayer()
        }
    }

    public override fun onStop() {
        super.onStop()
        if (Util.SDK_INT > 23) {
            releasePlayer()
        }
    }

    private fun initializePlayer() {
        val trackSelector = DefaultTrackSelector(this).apply {
            setParameters(buildUponParameters().setMaxVideoSizeSd())
        }
        player = ExoPlayer.Builder(this)
            .setTrackSelector(trackSelector)
            .build()
            .also { exoPlayer ->
                viewBinding.videoView.player = exoPlayer

                val mediaItem = MediaItem.Builder()
                    .setUri(getString(R.string.media_url_dash))
                    .setMimeType(MimeTypes.APPLICATION_MPD)
                    .build()
                exoPlayer.setMediaItem(mediaItem)
                exoPlayer.playWhenReady = playWhenReady
                exoPlayer.seekTo(currentItem, playbackPosition)
                exoPlayer.addListener(playbackStateListener)
                exoPlayer.prepare()
            }
    }

    private fun releasePlayer() {
        player?.let { exoPlayer ->
            playbackPosition = exoPlayer.currentPosition
            currentItem = exoPlayer.currentMediaItemIndex
            playWhenReady = exoPlayer.playWhenReady
            exoPlayer.removeListener(playbackStateListener)
            exoPlayer.release()
        }
        player = null
    }

    @SuppressLint("InlinedApi")
    private fun hideSystemUi() {
        WindowCompat.setDecorFitsSystemWindows(window, false)
        WindowInsetsControllerCompat(window, viewBinding.videoView).let { controller ->
            controller.hide(WindowInsetsCompat.Type.systemBars())
            controller.systemBarsBehavior = WindowInsetsControllerCompat.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE
        }
    }
}

*)

type
  TPlatformExoPlayer = class;

  TPlayerListener = class(TJavaLocal, JPlayer_Listener)
  private
    FPlatformExoPlayer: TPlatformExoPlayer;
  public
    { JPlayer_Listener }
    procedure onAudioAttributesChanged(audioattributes: JAudioAttributes); cdecl;
    procedure onAudioSessionIdChanged(int: Integer); cdecl;
    procedure onAvailableCommandsChanged(commands: JPlayer_Commands); cdecl;
    procedure onCues(list: JList); overload; cdecl;
    procedure onCues(cuegroup: JCueGroup); overload; cdecl;
    procedure onDeviceInfoChanged(deviceinfo: JDeviceInfo); cdecl;
    procedure onDeviceVolumeChanged(int: Integer; boolean: Boolean); cdecl;
    procedure onEvents(player: JPlayer; events: JPlayer_Events); cdecl;
    procedure onIsLoadingChanged(boolean: Boolean); cdecl;
    procedure onIsPlayingChanged(boolean: Boolean); cdecl;
    procedure onLoadingChanged(boolean: Boolean); cdecl;
    procedure onMaxSeekToPreviousPositionChanged(long: Int64); cdecl;
    procedure onMediaItemTransition(mediaitem: JMediaItem; int: Integer); cdecl;
    procedure onMediaMetadataChanged(mediametadata: JMediaMetadata); cdecl;
    procedure onMetadata(metadata: JMetadata); cdecl;
    procedure onPlayWhenReadyChanged(boolean: Boolean; int_1: Integer); cdecl;
    procedure onPlaybackParametersChanged(playbackparameters: JPlaybackParameters); cdecl;
    procedure onPlaybackStateChanged(int: Integer); cdecl;
    procedure onPlaybackSuppressionReasonChanged(int: Integer); cdecl;
    procedure onPlayerError(playbackexception: JPlaybackException); cdecl;
    procedure onPlayerErrorChanged(playbackexception: JPlaybackException); cdecl;
    procedure onPlayerStateChanged(boolean: Boolean; int_1: Integer); cdecl;
    procedure onPlaylistMetadataChanged(mediametadata: JMediaMetadata); cdecl;
    procedure onPositionDiscontinuity(positioninfo: JPlayer_PositionInfo; positioninfo_1: JPlayer_PositionInfo; int: Integer); overload; cdecl;
    procedure onPositionDiscontinuity(int: Integer); overload; cdecl;
    procedure onRenderedFirstFrame; cdecl;
    procedure onRepeatModeChanged(int: Integer); cdecl;
    procedure onSeekBackIncrementChanged(long: Int64); cdecl;
    procedure onSeekForwardIncrementChanged(long: Int64); cdecl;
    procedure onShuffleModeEnabledChanged(boolean: Boolean); cdecl;
    procedure onSkipSilenceEnabledChanged(boolean: Boolean); cdecl;
    procedure onSurfaceSizeChanged(int: Integer; int_1: Integer); cdecl;
    procedure onTimelineChanged(timeline: JTimeline; int: Integer); cdecl;
    procedure onTrackSelectionParametersChanged(trackselectionparameters: JTrackSelectionParameters); cdecl;
    procedure onTracksChanged(tracks: JTracks); cdecl;
    procedure onVideoSizeChanged(videosize: JVideoSize); cdecl;
    procedure onVolumeChanged(float: Single); cdecl;
  public
    constructor Create(const APlatformExoPlayer: TPlatformExoPlayer);
  end;

  TPlatformExoPlayer = class(TCustomPlatformExoPlayer)
  private
    FCurrentMediaItemIndex: Integer;
    FCurrentPosition: Int64;
    FPlayer: JExoPlayer;
    FPlayerListener: JPlayer_Listener;
    FPlayerView: JPlayerView;
    FPlayWhenReady: Boolean;
    FPresentation: TAndroidExoPlayerView;
    procedure PlayerViewTouchHandler(view: JView; event: JMotionEvent);
    procedure PreparePlayer;
    procedure ReleasePlayer;
  protected
    procedure BecameActive; override;
    procedure PlaybackStateChanged(const APlaybackState: Integer);
    procedure SetControlsTimeout(const Value: Integer); override;
    procedure WillBecomeInactive; override;
  public
    constructor Create;
    procedure Play(const AURL: string); override;
  end;

implementation

uses
  Androidapi.Helpers;

{ TPlayerListener }

constructor TPlayerListener.Create(const APlatformExoPlayer: TPlatformExoPlayer);
begin
  inherited Create;
  FPlatformExoPlayer := APlatformExoPlayer;
end;

procedure TPlayerListener.onAudioAttributesChanged(audioattributes: JAudioAttributes);
begin

end;

procedure TPlayerListener.onAudioSessionIdChanged(int: Integer);
begin

end;

procedure TPlayerListener.onAvailableCommandsChanged(commands: JPlayer_Commands);
begin

end;

procedure TPlayerListener.onCues(list: JList);
begin

end;

procedure TPlayerListener.onCues(cuegroup: JCueGroup);
begin

end;

procedure TPlayerListener.onDeviceInfoChanged(deviceinfo: JDeviceInfo);
begin

end;

procedure TPlayerListener.onDeviceVolumeChanged(int: Integer; boolean: Boolean);
begin

end;

procedure TPlayerListener.onEvents(player: JPlayer; events: JPlayer_Events);
begin

end;

procedure TPlayerListener.onIsLoadingChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onIsPlayingChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onLoadingChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onMaxSeekToPreviousPositionChanged(long: Int64);
begin

end;

procedure TPlayerListener.onMediaItemTransition(mediaitem: JMediaItem; int: Integer);
begin

end;

procedure TPlayerListener.onMediaMetadataChanged(mediametadata: JMediaMetadata);
begin

end;

procedure TPlayerListener.onMetadata(metadata: JMetadata);
begin

end;

procedure TPlayerListener.onPlaybackParametersChanged(playbackparameters: JPlaybackParameters);
begin

end;

procedure TPlayerListener.onPlaybackStateChanged(int: Integer);
begin
  FPlatformExoPlayer.PlaybackStateChanged(int);
end;

procedure TPlayerListener.onPlaybackSuppressionReasonChanged(int: Integer);
begin

end;

procedure TPlayerListener.onPlayerError(playbackexception: JPlaybackException);
begin

end;

procedure TPlayerListener.onPlayerErrorChanged(playbackexception: JPlaybackException);
begin

end;

procedure TPlayerListener.onPlayerStateChanged(boolean: Boolean; int_1: Integer);
begin

end;

procedure TPlayerListener.onPlaylistMetadataChanged(mediametadata: JMediaMetadata);
begin

end;

procedure TPlayerListener.onPlayWhenReadyChanged(boolean: Boolean; int_1: Integer);
begin

end;

procedure TPlayerListener.onPositionDiscontinuity(int: Integer);
begin

end;

procedure TPlayerListener.onPositionDiscontinuity(positioninfo, positioninfo_1: JPlayer_PositionInfo; int: Integer);
begin

end;

procedure TPlayerListener.onRenderedFirstFrame;
begin

end;

procedure TPlayerListener.onRepeatModeChanged(int: Integer);
begin

end;

procedure TPlayerListener.onSeekBackIncrementChanged(long: Int64);
begin

end;

procedure TPlayerListener.onSeekForwardIncrementChanged(long: Int64);
begin

end;

procedure TPlayerListener.onShuffleModeEnabledChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onSkipSilenceEnabledChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onSurfaceSizeChanged(int, int_1: Integer);
begin

end;

procedure TPlayerListener.onTimelineChanged(timeline: JTimeline; int: Integer);
begin

end;

procedure TPlayerListener.onTracksChanged(tracks: JTracks);
begin

end;

procedure TPlayerListener.onTrackSelectionParametersChanged(trackselectionparameters: JTrackSelectionParameters);
begin

end;

procedure TPlayerListener.onVideoSizeChanged(videosize: JVideoSize);
begin

end;

procedure TPlayerListener.onVolumeChanged(float: Single);
begin

end;

{ TPlatformExoPlayer }

constructor TPlatformExoPlayer.Create;
begin
  inherited;
  FPlayWhenReady := True;
  FPlayerListener := TPlayerListener.Create(Self);
  FPresentation := TAndroidExoPlayerView(View.Presentation);
  FPresentation.OnTouch := PlayerViewTouchHandler;
  FPlayerView := FPresentation.View;
  FControlsTimeout := FPlayerView.getControllerShowTimeoutMs;
  FPlayer := TJExoPlayer_Builder.JavaClass.init(TAndroidHelper.Context)
    // .setTrackSelector(FTrackSelector)
    .build;
  FPlayerView.setPlayer(FPlayer);
end;

procedure TPlatformExoPlayer.PlayerViewTouchHandler(view: JView; event: JMotionEvent);
begin
  if FPlayerView.isControllerFullyVisible then
    FPlayerView.hideController
  else
    FPlayerView.showController;
end;

procedure TPlatformExoPlayer.Play(const AURL: string);
var
  LMediaItem: JMediaItem;
begin
  if FPlayer = nil then
    PreparePlayer;
  // https://storage.googleapis.com/exoplayer-test-media-0/BigBuckBunny_320x180.mp4
  LMediaItem := TJMediaItem.JavaClass.fromUri(StringToJString(AURL));
  FPlayer.setMediaItem(LMediaItem);
  // FPlayer.setPlayWhenReady(FPlayWhenReady);
  // FPlayer.seekTo(FCurrentMediaItemIndex, FCurrentPosition);
  FPlayer.addListener(FPlayerListener);
  FPlayer.prepare;
  FPlayer.play;
end;

procedure TPlatformExoPlayer.PlaybackStateChanged(const APlaybackState: Integer);
begin
(*
private fun playbackStateListener() = object : Player.Listener {
    override fun onPlaybackStateChanged(playbackState: Int) {
        val stateString: String = when (playbackState) {
            ExoPlayer.STATE_IDLE -> "ExoPlayer.STATE_IDLE      -"
            ExoPlayer.STATE_BUFFERING -> "ExoPlayer.STATE_BUFFERING -"
            ExoPlayer.STATE_READY -> "ExoPlayer.STATE_READY     -"
            ExoPlayer.STATE_ENDED -> "ExoPlayer.STATE_ENDED     -"
            else -> "UNKNOWN_STATE             -"
        }
        Log.d(TAG, "changed state to $stateString")
    }
}
*)
end;

procedure TPlatformExoPlayer.PreparePlayer;
begin
  FPlayer := TJExoPlayer_Builder.JavaClass.init(TAndroidHelper.Context)
    // .setTrackSelector(FTrackSelector)
    .build;
  TAndroidExoPlayerView(View.Presentation).View.setPlayer(FPlayer);
end;

procedure TPlatformExoPlayer.ReleasePlayer;
begin
  FCurrentPosition := FPlayer.getCurrentPosition;
  FCurrentMediaItemIndex := FPlayer.getCurrentMediaItemIndex;
  FPlayWhenReady := FPlayer.getPlayWhenReady;
  // FPlayer.removeListener(FPlayerListener);
  FPlayer.release;
  FPlayer := nil;
end;

procedure TPlatformExoPlayer.SetControlsTimeout(const Value: Integer);
begin
  inherited;
  FPlayerView.setControllerShowTimeoutMs(Value);
end;

procedure TPlatformExoPlayer.BecameActive;
begin
  inherited;
  PreparePlayer;
end;

procedure TPlatformExoPlayer.WillBecomeInactive;
begin
  inherited;
  // If it is playing something
  ReleasePlayer;
end;

end.

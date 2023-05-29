unit DW.VideoPlayer.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  Androidapi.JNI.VideoView,
  DW.VideoPlayer;

type
  TPlatformVideoPlayer = class(TCustomPlatformVideoPlayer)
  private
    FMediaController: JMediaController;
    FView: JVideoView;
  protected
    procedure DoPlay; override;
    function IsPlaying: Boolean; override;
    procedure Pause; override;
    procedure PlayerViewResized; override;
    procedure Resume; override;
  public
    constructor Create(const AVideoPlayer: TVideoPlayer); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  System.TypInfo, System.SysUtils,
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Widget, Androidapi.JNI.Net,
  FMX.Controls, FMX.Presentation.Android, FMX.Presentation.Factory;

type
  TPlatformPlayerView = class(TAndroidNativeView)
  private
    FView: JVideoView;
  protected
    function CreateView: JView; override;
  public
    constructor Create; override;
    property View: JVideoView read FView;
  end;

{ TPlatformPlayerView }

constructor TPlatformPlayerView.Create;
begin
  inherited;
  //
end;

function TPlatformPlayerView.CreateView: JView;
begin
  FView := TJVideoView.JavaClass.init(TAndroidHelper.Context);
  Result := FView;
  TOSLog.d('TPlatformPlayerView.CreateView > View created');
end;

{ TPlatformVideoPlayer }

constructor TPlatformVideoPlayer.Create(const AVideoPlayer: TVideoPlayer);
//var
//  LParams: JRelativeLayout_LayoutParams;
begin
  inherited;
  FView := TPlatformPlayerView(PlayerView.Presentation).View;
//  LParams := TJRelativeLayout_LayoutParams.JavaClass.init(
//    TJRelativeLayout_LayoutParams.JavaClass.FILL_PARENT, // MATCH_PARENT,
//    TJRelativeLayout_LayoutParams.JavaClass.FILL_PARENT // MATCH_PARENT
//  );
//  LParams.addRule(TJRelativeLayout.JavaClass.CENTER_IN_PARENT);
//  FView.setLayoutParams(LParams);
  // FView.setContentMode(UIViewContentModeScaleAspectFit);
end;

destructor TPlatformVideoPlayer.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformVideoPlayer.DoPlay;
begin
  if FMediaController = nil then
  begin
    FMediaController := TJMediaController.JavaClass.init(TAndroidHelper.Context);
    FView.setMediaController(FMediaController);
    FMediaController.setAnchorView(FView);
  end;
  // FView.setBackgroundColor(TJColor.JavaClass.TRANSPARENT);
  FView.setVideoURI(TJnet_URI.JavaClass.parse(StringToJString(FileName)));
  TOSLog.d('TPlatformVideoPlayer.DoPlay > start - Width: %d, Height: %d', [FView.getWidth, FView.getHeight]);
  FView.start;
  FView.setZOrderOnTop(True);
end;

function TPlatformVideoPlayer.IsPlaying: Boolean;
begin
  Result := FView.isPlaying;
end;

procedure TPlatformVideoPlayer.Pause;
begin
  FView.pause;
end;

procedure TPlatformVideoPlayer.PlayerViewResized;
begin
  // TODO
end;

procedure TPlatformVideoPlayer.Resume;
begin
  FView.resume;
end;

initialization
  TPresentationProxyFactory.Current.Register(TPlayerView, TControlType.Platform, TAndroidPresentationProxy<TPlatformPlayerView>);

finalization
  TPresentationProxyFactory.Current.Unregister(TPlayerView, TControlType.Platform, TAndroidPresentationProxy<TPlatformPlayerView>);

end.

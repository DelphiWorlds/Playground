unit DW.VideoPlayer.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  iOSapi.AVFoundation, iOSapi.UIKit,
  DW.VideoPlayer;

type
  TPlatformVideoPlayer = class(TCustomPlatformVideoPlayer)
  private
    FPlayer: AVPlayer;
    FPlayerItem: AVPlayerItem;
    FPlayerLayer: AVPlayerLayer;
    FView: UIView;
    procedure CreateAVPlayer;
    procedure DestroyAVPlayer;
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
  System.TypInfo,
  Macapi.Helpers,
  iOSapi.Foundation,
  FMX.Controls, FMX.Presentation.iOS, FMX.Presentation.Factory;

type
  IPlayerViewUIView = interface(UIView)
    ['{CD779198-3F59-416F-8CE4-C3294B1139FE}']
  end;

  TPlatformPlayerView = class(TiOSNativeView)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  end;

function AVLayerVideoGravityResizeAspectFill: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVLayerVideoGravityResizeAspectFill');
end;

{ TPlatformPlayerView }

function TPlatformPlayerView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IPlayerViewUIView);
end;

{ TPlatformVideoPlayer }

constructor TPlatformVideoPlayer.Create(const AVideoPlayer: TVideoPlayer);
begin
  inherited;
  // FView := TPlatformPlayerView(PlayerView.Presentation).View;
  FView := TPlatformPlayerView(PlayerView.Presentation).View;
  FView.setContentMode(UIViewContentModeScaleAspectFit);
end;

destructor TPlatformVideoPlayer.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformVideoPlayer.CreateAVPlayer;
var
  LURL: NSURL;
begin
  DestroyAVPlayer;
  LURL := TNSURL.Wrap(TNSURL.Alloc.initFileURLWithPath(StrToNSStr(FileName)));
  FPlayerItem := TAVPlayerItem.Wrap(TAVPlayerItem.OCClass.playerItemWithURL(LURL)); // Also playerItemWithAsset
  FPlayer := TAVPlayer.Wrap(TAVPlayer.OCClass.playerWithPlayerItem(FPlayerItem));
  FPlayerLayer := TAVPlayerLayer.Wrap(TAVPlayerLayer.OCClass.playerLayerWithPlayer(FPlayer));
  FPlayerLayer.setFrame(FView.bounds);
  FPlayerLayer.setVideoGravity(AVLayerVideoGravityResizeAspectFill);
  FView.layer.addSublayer(FPlayerLayer);
end;

procedure TPlatformVideoPlayer.DestroyAVPlayer;
begin
  FPlayer := nil;
  FPlayerItem := nil;
  if FPlayerLayer <> nil then
    FPlayerLayer.removeFromSuperlayer;
  FPlayerLayer := nil;
end;

procedure TPlatformVideoPlayer.DoPlay;
begin
  CreateAVPlayer;
  FPlayer.play;
end;

function TPlatformVideoPlayer.IsPlaying: Boolean;
begin
  Result := (FPlayer <> nil) and (FPlayer.rate > 0);
end;

procedure TPlatformVideoPlayer.Pause;
begin
  if FPlayer <> nil then
    FPlayer.pause;
end;

procedure TPlatformVideoPlayer.PlayerViewResized;
begin
  if FPlayerLayer <> nil then
    FPlayerLayer.setFrame(FView.bounds);
end;

procedure TPlatformVideoPlayer.Resume;
begin
  if FPlayer <> nil then
    FPlayer.play;
end;

initialization
  TPresentationProxyFactory.Current.Register(TPlayerView, TControlType.Platform, TiOSPresentationProxy<TPlatformPlayerView>);

finalization
  TPresentationProxyFactory.Current.Unregister(TPlayerView, TControlType.Platform, TiOSPresentationProxy<TPlatformPlayerView>);

end.

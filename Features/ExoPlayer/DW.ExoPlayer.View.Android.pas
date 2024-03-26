unit DW.ExoPlayer.View.Android;

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
  // Android
  Androidapi.JNI.GraphicsContentViewText,
  // FMX
  FMX.Presentation.Android, FMX.Controls.Presentation, FMX.Presentation.Messages,
  // DW
  DW.Androidapi.JNI.AndroidX.Media3.ExoPlayer, DW.Androidapi.JNI.AndroidX.Media3.UI;

type
  TPlayerViewTouchEvent = procedure(view: JView; event: JMotionEvent) of object;

  TAndroidExoPlayerView = class(TAndroidNativeView)
  private
    FView: JPlayerView;
    FOnTouch: TPlayerViewTouchEvent;
  protected
    function CreateView: JView; override;
    function ProcessTouch(view: JView; event: JMotionEvent): Boolean; override;
  public
    constructor Create; override;
    property OnTouch: TPlayerViewTouchEvent read FOnTouch write FOnTouch;
    property View: JPlayerView read FView;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App,
  // FMX
  FMX.Presentation.Factory, FMX.Controls,
  // DW
  DW.ExoPlayer.View;

{ TAndroidExoPlayerView }

constructor TAndroidExoPlayerView.Create;
begin
  inherited;
  //
end;

function TAndroidExoPlayerView.CreateView: JView;
begin
  FView := TJPlayerView.JavaClass.init(TAndroidHelper.Context);
  Result := FView;
end;

function TAndroidExoPlayerView.ProcessTouch(view: JView; event: JMotionEvent): Boolean;
begin
  if Assigned(FOnTouch) then
    FOnTouch(view, event);
  Result := inherited;
end;

initialization
  TPresentationProxyFactory.Current.Register(TExoPlayerView, TControlType.Platform, TAndroidPresentationProxy<TAndroidExoPlayerView>);

finalization
  TPresentationProxyFactory.Current.Unregister(TExoPlayerView, TControlType.Platform, TAndroidPresentationProxy<TAndroidExoPlayerView>);

end.

unit DW.NativeCameraPreviewView.Android;

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
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  FMX.Presentation.Android, FMX.Presentation.Messages, FMX.Controls.Presentation, FMX.Controls.Model, FMX.Presentation.Factory, FMX.Controls,
  DW.NativeCameraPreviewView, DW.Androidapi.JNI.AndroidX.Camera;

type
  TAndroidNativeCameraPreviewView = class(TAndroidNativeView)
  private
    FView: JPreviewView;
    function GetModel: TCustomNativeCameraPreviewViewModel;
    procedure PMInit(var AMessage: TDispatchMessage); message PM_INIT;
  protected
    function CreateView: JView; override;
    function DefineModelClass: TDataModelClass; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeCameraPreviewViewModel read GetModel;
    property View: JPreviewView read FView;
  end;

implementation

{ TAndroidNativeCameraPreviewView }

constructor TAndroidNativeCameraPreviewView.Create;
begin
  inherited;
  //
end;

destructor TAndroidNativeCameraPreviewView.Destroy;
begin
  //
  inherited;
end;

function TAndroidNativeCameraPreviewView.CreateView: JView;
begin
  FView := TJPreviewView.JavaClass.init(TAndroidHelper.Context);
  FView.setImplementationMode(TJPreviewView_ImplementationMode.JavaClass.COMPATIBLE);
  Result := FView;
end;

function TAndroidNativeCameraPreviewView.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeCameraPreviewViewModel;
end;

function TAndroidNativeCameraPreviewView.GetModel: TCustomNativeCameraPreviewViewModel;
begin
  Result := inherited GetModel<TCustomNativeCameraPreviewViewModel>;
end;

procedure TAndroidNativeCameraPreviewView.PMInit(var AMessage: TDispatchMessage);
begin
  //
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeCameraPreviewView, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeCameraPreviewView>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeCameraPreviewView, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeCameraPreviewView>);

end.

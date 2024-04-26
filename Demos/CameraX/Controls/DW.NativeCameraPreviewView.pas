unit DW.NativeCameraPreviewView;

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
  // RTL
  System.Classes, System.Types, System.UITypes,
  // FMX
  FMX.Controls.Presentation, FMX.Controls.Model, FMX.Controls, FMX.Graphics, FMX.Types;

type
  TCustomNativeCameraPreviewViewModel = class(TDataModel)
  private
    //
  protected
    //
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCustomNativeCameraPreviewView = class(TPresentedControl)
  private
    function GetModel: TCustomNativeCameraPreviewViewModel; overload;
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure Paint; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  [ComponentPlatformsAttribute(pidAndroidArm32 or pidAndroidArm64)]
  TNativeCameraPreviewView = class(TCustomNativeCameraPreviewView)
  published
    property Align;
    property Anchors;
    property Height;
    property Margins;
    property Position;
    property Size;
    property Visible default True;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnResized;
  end;

procedure Register;

implementation

uses
  {$IF Defined(ANDROID)}
  DW.NativeCameraPreviewView.Android,
  {$ENDIF}
  // RTL
  System.SysUtils;

procedure Register;
begin
  RegisterComponents('Kastri FMX', [TNativeCameraPreviewView]);
end;

{ TCustomNativeCameraPreviewViewModel }

constructor TCustomNativeCameraPreviewViewModel.Create(const AOwner: TComponent);
begin
  inherited;
  //
end;

destructor TCustomNativeCameraPreviewViewModel.Destroy;
begin
  //
  inherited;
end;

{ TCustomNativeCameraPreviewView }

constructor TCustomNativeCameraPreviewView.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
  //
end;

function TCustomNativeCameraPreviewView.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeCameraPreviewViewModel;
end;

function TCustomNativeCameraPreviewView.GetModel: TCustomNativeCameraPreviewViewModel;
begin
  Result := inherited GetModel<TCustomNativeCameraPreviewViewModel>;
end;

procedure TCustomNativeCameraPreviewView.Paint;
begin
{
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
  end;
}
end;

function TCustomNativeCameraPreviewView.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

end.

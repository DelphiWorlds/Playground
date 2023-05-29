unit DW.OSControl;

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

//{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes, System.Types, System.Messaging,
  // FMX
  FMX.Controls.Presentation;

type
  TOSControl = class(TPresentedControl)
  private
    FOnOrientationChange: TNotifyEvent;
    FOnSizeChange: TNotifyEvent;
    procedure OrientationChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure SizeChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
  protected
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnOrientationChange: TNotifyEvent read FOnOrientationChange write FOnOrientationChange;
    property OnSizeChange: TNotifyEvent read FOnSizeChange write FOnSizeChange;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.UITypes,
  // FMX
  FMX.Types, FMX.Forms;

{ TOSControl }

constructor TOSControl.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChangedMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TSizeChangedMessage, SizeChangedMessageHandler);
  ControlType := TControlType.Platform;
end;

destructor TOSControl.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, OrientationChangedMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, SizeChangedMessageHandler);
  inherited;
end;

procedure TOSControl.OrientationChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  if Assigned(FOnOrientationChange) then
    FOnOrientationChange(Self);
end;

procedure TOSControl.SizeChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  if (Root <> nil) and (Root.GetObject = Sender) and Assigned(FOnSizeChange) then
    FOnSizeChange(Self);
end;

function TOSControl.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

end.

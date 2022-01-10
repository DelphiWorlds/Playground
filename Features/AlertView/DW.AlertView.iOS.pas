unit DW.AlertView.iOS;

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
  // RTL
  System.Classes, System.Generics.Collections,
  // iOS
  iOSapi.CocoaTypes, iOSapi.UIKit, // Needs to be before DW.iOSapi.UIKit
  // DW
  DW.AlertView, DW.iOSapi.UIKit;

type
  TAlertAction = record
    Index: Integer;
    Title: string;
    Handler: TNotifyEvent;
    Style: TAlertActionStyle;
  end;

  TAlertActions = TArray<TAlertAction>;

  TAlertDictionary = TDictionary<Pointer, TAlertAction>;

  TPlatformAlertView = class(TCustomPlatformAlertView)
  private
    FAlertActions: TAlertActions;
    FAlerts: TAlertDictionary;
    FController: UIAlertController;
    procedure ControllerActionHandler(action: UIAlertAction);
    procedure ControllerCompletionHandler;
    procedure InternalAddAction(const AAction: TAlertAction);
    procedure ReleaseController;
  protected
    procedure AddAction(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle); override;
    procedure ClearActions; override;
    procedure DoAlert(const ATitle, AMessageText: string); override;
    procedure DoShow(const ATitle, AMessageText: string); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog, DW.Classes.Helpers,
  // RTL
  System.SysUtils, System.Types,
  // Mac
  Macapi.Helpers, Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.Helpers;

function StrToNSStrOrNil(const ASource: string): NSString;
begin
  Result := nil;
  if not ASource.IsEmpty then
    Result := StrToNSStr(ASource);
end;

{ TPlatformAlertView }

constructor TPlatformAlertView.Create;
begin
  inherited;
  FAlerts := TAlertDictionary.Create;
end;

destructor TPlatformAlertView.Destroy;
begin
  FAlerts.Free;
  inherited;
end;

procedure TPlatformAlertView.AddAction(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle);
var
  LAlertAction: TAlertAction;
begin
  LAlertAction.Title := ATitle;
  LAlertAction.Handler := AHandler;
  LAlertAction.Style := AStyle;
  LAlertAction.Index := Length(FAlertActions);
  FAlertActions := FAlertActions + [LAlertAction];
end;

procedure TPlatformAlertView.DoAlert(const ATitle, AMessageText: string);
var
  LAlertAction: TAlertAction;
begin
  FController := TUIAlertController.Wrap(TUIAlertController.OCClass.alertControllerWithTitle(StrToNSStrOrNil(ATitle), StrToNSStrOrNil(AMessageText),
    UIAlertControllerStyleAlert));
  FController.retain;
  FAlerts.Clear;
  for LAlertAction in FAlertActions do
    InternalAddAction(LAlertAction);
  TiOSHelper.SharedApplication.keyWindow.rootViewController.presentViewController(FController, True, nil);
end;

procedure TPlatformAlertView.DoShow(const ATitle: string; const AMessageText: string);
//const
//  cViewStyles: array[TAlertViewStyle] of NSInteger = (UIAlertControllerStyleActionSheet, UIAlertControllerStyleAlert);
var
  LAlertAction: TAlertAction;
  LPopover: UIPopoverPresentationController;
  LView: UIView;
begin
  FController := TUIAlertController.Wrap(TUIAlertController.OCClass.alertControllerWithTitle(StrToNSStrOrNil(ATitle), StrToNSStrOrNil(AMessageText),
    UIAlertControllerStyleActionSheet));
  LPopover := FController.popoverPresentationController;
  if LPopover <> nil then
  begin
    LView := TiOSHelper.SharedApplication.keyWindow.rootViewController.view;
    LPopover.setSourceView(LView);
    // Middle of the view horizontally, bottom of the view vertically minus a few pixels, 1 pixel x 1 pixel size
    LPopover.setSourceRect(CGRectMake(LView.bounds.size.width / 2.0, LView.bounds.size.height - 8, 1, 1));
  end;
  FController.retain;
  FAlerts.Clear;
  for LAlertAction in FAlertActions do
    InternalAddAction(LAlertAction);
  TiOSHelper.SharedApplication.keyWindow.rootViewController.presentViewController(FController, True, ControllerCompletionHandler);
end;

procedure TPlatformAlertView.InternalAddAction(const AAction: TAlertAction);
var
  LNativeAction: UIAlertAction;
begin
  LNativeAction := TUIAlertAction.Wrap(TUIAlertAction.OCClass.actionWithTitle(StrToNSStr(AAction.Title), UIAlertActionStyleDefault, ControllerActionHandler));
  if AAction.Style = TAlertActionStyle.Cancel then
    LNativeAction.setStyle(UIAlertActionStyleCancel)
  else if AAction.Style = TAlertActionStyle.Destructive then
    LNativeAction.setStyle(UIAlertActionStyleDestructive);
  FAlerts.Add(NSObjectToID(LNativeAction), AAction);
  FController.addAction(LNativeAction);
end;

procedure TPlatformAlertView.ReleaseController;
begin
  if FController <> nil then
  begin
    FController.release;
    FCancelAdded := False;
  end;
  FController := nil;
end;

procedure TPlatformAlertView.ClearActions;
begin
  FAlertActions := [];
end;

procedure TPlatformAlertView.ControllerActionHandler(action: UIAlertAction);
var
  LAction: TAlertAction;
  LSuccess: Boolean;
begin
  LSuccess := FAlerts.TryGetValue(NSObjectToID(action), LAction);
  TiOSHelper.SharedApplication.keyWindow.rootViewController.dismissViewControllerAnimated(True, ControllerCompletionHandler);
  if LSuccess then
  begin
    if Assigned(LAction.Handler) then
      LAction.Handler(Self)
    else
      DoAlertAction(LAction.Index);
  end;
end;

procedure TPlatformAlertView.ControllerCompletionHandler;
begin
  ReleaseController;
end;

end.

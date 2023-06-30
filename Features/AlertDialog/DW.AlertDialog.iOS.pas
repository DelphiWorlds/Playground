unit DW.AlertDialog.iOS;

interface

implementation

uses
  System.SysUtils,
  Macapi.ObjectiveC, Macapi.Helpers,
  iOSapi.Foundation, iOSapi.UIKit, iOSapi.Helpers, iOSapi.CoreGraphics,
  DW.iOSapi.UIKit,
  DW.AlertDialog;

type
  TAlertActionsHelper = record helper for TAlertActions
    function AddAction(const ATitle: string; const AStyle: TAlertActionStyle): Integer;
    function IndexOf(const ANativeAction: Pointer): Integer;
  end;

  TAlertDialog = class(TCustomAlertDialog)
  private
    FController: UIAlertController;
    FInternalActions: TAlertActions;
    procedure ControllerActionHandler(action: UIAlertAction);
    procedure ControllerCompletionHandler;
    function CreateNativeAction(const AAction: TAlertAction): UIAlertAction;
  protected
    procedure DoShow(const ATitle, AMessage: string); override;
  end;

  TAlertDialogProvider = class(TInterfacedObject, IAlertDialogProvider)
  public
    { IAlertDialogProvider }
    function GetDialog: IAlertDialog;
  end;

function StrToNSStrOrNil(const ASource: string): NSString;
begin
  Result := nil;
  if not ASource.IsEmpty then
    Result := StrToNSStr(ASource);
end;

{ TAlertActionsHelper }

function TAlertActionsHelper.AddAction(const ATitle: string; const AStyle: TAlertActionStyle): Integer;
begin
  Self := Self + [TAlertAction.Create(ATitle, AStyle)];
  Result := High(Self);
end;

function TAlertActionsHelper.IndexOf(const ANativeAction: Pointer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Self) do
  begin
    if Self[I].NativeAction = ANativeAction then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{ TAlertDialog }

procedure TAlertDialog.DoShow(const ATitle: string; const AMessage: string);
var
  LPopover: UIPopoverPresentationController;
  LView: UIView;
  I: Integer;
  LNativeAction: UIAlertAction;
  LHasCancel: Boolean;
  LIndex: Integer;
begin
  FController := TUIAlertController.Wrap(TUIAlertController.OCClass.alertControllerWithTitle(StrToNSStrOrNil(ATitle), StrToNSStrOrNil(AMessage),
    UIAlertControllerStyleActionSheet));
  LPopover := FController.popoverPresentationController;
  if LPopover <> nil then
  begin
    LView := TiOSHelper.SharedApplication.keyWindow.rootViewController.view;
    LPopover.setSourceView(LView);
    // Middle of the view horizontally, bottom of the view vertically minus a few pixels, 1 pixel x 1 pixel size
    LPopover.setSourceRect(CGRectMake(LView.bounds.size.width / 2.0, LView.bounds.size.height - 8, 1, 1));
  end;
  LHasCancel := False;
  FInternalActions := Copy(Actions);
  for I := 0 to High(FInternalActions) do
  begin
    LNativeAction := CreateNativeAction(FInternalActions[I]);
    if LNativeAction.style = UIAlertActionStyleCancel then
      LHasCancel := True;
    FInternalActions[I].NativeAction := NSObjectToID(LNativeAction);
    FController.addAction(LNativeAction);
  end;
  if Length(Actions) = 0 then
  begin
    LIndex := FInternalActions.AddAction(PositiveText, TAlertActionStyle.Default);
    LNativeAction := CreateNativeAction(FInternalActions[LIndex]);
    FInternalActions[LIndex].NativeAction := NSObjectToID(LNativeAction);
    FController.addAction(LNativeAction);
  end;
  if not LHasCancel then
  begin
    LIndex := FInternalActions.AddAction(NegativeText, TAlertActionStyle.Cancel);
    LNativeAction := CreateNativeAction(FInternalActions[LIndex]);
    FInternalActions[LIndex].NativeAction := NSObjectToID(LNativeAction);
    FController.addAction(LNativeAction);
  end;
  TiOSHelper.SharedApplication.keyWindow.rootViewController.presentViewController(FController, True, ControllerCompletionHandler);
end;

function TAlertDialog.CreateNativeAction(const AAction: TAlertAction): UIAlertAction;
begin
  Result := TUIAlertAction.Wrap(TUIAlertAction.OCClass.actionWithTitle(StrToNSStr(AAction.Title), UIAlertActionStyleDefault, ControllerActionHandler));
  if AAction.Style = TAlertActionStyle.Cancel then
    Result.setStyle(UIAlertActionStyleCancel)
  else if AAction.Style = TAlertActionStyle.Destructive then
    Result.setStyle(UIAlertActionStyleDestructive);
end;

procedure TAlertDialog.ControllerActionHandler(action: UIAlertAction);
var
  LAction: TAlertAction;
  LIndex: Integer;
begin
  TiOSHelper.SharedApplication.keyWindow.rootViewController.dismissViewControllerAnimated(True, ControllerCompletionHandler);
  LIndex := FInternalActions.IndexOf(NSObjectToID(action));
  if LIndex > -1 then
  begin
    if FInternalActions[LIndex].Style = TAlertActionStyle.Cancel then
      DoAction(-2)
    else if Length(Actions) = 0 then
      DoAction(-1)
    else
      DoAction(LIndex);
  end;
end;

procedure TAlertDialog.ControllerCompletionHandler;
begin
  FController := nil;
end;

{ TAlertDialogProvider }

function TAlertDialogProvider.GetDialog: IAlertDialog;
begin
  Result := TAlertDialog.Create;
end;

initialization
  AlertDialogProvider := TAlertDialogProvider.Create;

end.

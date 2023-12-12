unit DW.AlertDialog.Mac;

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

implementation

uses
  // RTL
  System.SysUtils,
  // Mac
  Macapi.AppKit, Macapi.ObjectiveC, Macapi.Foundation, Macapi.CocoaTypes, Macapi.Helpers,
  // FMX
  FMX.Platform.Mac, FMX.Forms,
  // DW
  DW.AlertDialog;

type
  NSAlertStyle = NSUInteger;
  NSHelpAnchorName = NSString;
  NSModalResponse = NSInteger;

  TNSAlertBlockMethod1 = procedure(returnCode: NSModalResponse) of object;

  NSAlertClass = interface(NSObjectClass)
    ['{178DBBEB-E019-461A-BCAC-A6535E8713EB}']
    {class} function alertWithError(error: NSError): NSAlert; cdecl;
    {class} function alertWithMessageText(message: NSString; defaultButton: NSString; alternateButton: NSString; otherButton: NSString;
      informativeTextWithFormat: NSString): NSAlert; cdecl; // API_DEPRECATED("Use -init instead", macos(10.3,10.10))
  end;

  NSAlert = interface(NSObject)
    ['{7D7DCF0A-A582-42E5-B238-89CFB0E773D1}']
    function accessoryView: NSView; cdecl;
    function addButtonWithTitle(title: NSString): NSButton; cdecl;
    function alertStyle: NSAlertStyle; cdecl;
    procedure beginSheetModalForWindow(window: NSWindow; modalDelegate: Pointer; didEndSelector: SEL; contextInfo: Pointer); overload; cdecl; // API_DEPRECATED("Use -beginSheetModalForWindow:completionHandler: instead", macos(10.3,10.10))
    procedure beginSheetModalForWindow(sheetWindow: NSWindow; completionHandler: TNSAlertBlockMethod1); overload; cdecl;
    function buttons: NSArray; cdecl;
    function delegate: Pointer; cdecl;
    function helpAnchor: NSHelpAnchorName; cdecl;
    function icon: NSImage; cdecl;
    function informativeText: NSString; cdecl;
    procedure layout; cdecl;
    function messageText: NSString; cdecl;
    function runModal: NSModalResponse; cdecl;
    procedure setAccessoryView(accessoryView: NSView); cdecl;
    procedure setAlertStyle(alertStyle: NSAlertStyle); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setHelpAnchor(helpAnchor: NSHelpAnchorName); cdecl;
    procedure setIcon(icon: NSImage); cdecl;
    procedure setInformativeText(informativeText: NSString); cdecl;
    procedure setMessageText(messageText: NSString); cdecl;
    procedure setShowsHelp(showsHelp: Boolean); cdecl;
    procedure setShowsSuppressionButton(showsSuppressionButton: Boolean); cdecl;
    function showsHelp: Boolean; cdecl;
    function showsSuppressionButton: Boolean; cdecl;
    function suppressionButton: NSButton; cdecl;
    function window: NSWindow; cdecl;
  end;
  TNSAlert = class(TOCGenericImport<NSAlertClass, NSAlert>) end;

  TAlertDialog = class(TCustomAlertDialog)
  private
    FAlert: NSAlert;
    FInternalActions: TAlertActions;
    procedure AlertCompletionHandler(returnCode: NSModalResponse);
  protected
    procedure DoShow(const ATitle, AMessage: string); override;
  end;

  TAlertDialogProvider = class(TInterfacedObject, IAlertDialogProvider)
  public
    { IAlertDialogProvider }
    function GetDialog: IAlertDialog;
  end;

{ TAlertDialogProvider }

function TAlertDialogProvider.GetDialog: IAlertDialog;
begin
  Result := TAlertDialog.Create;
end;

{ TAlertDialog }

procedure TAlertDialog.AlertCompletionHandler(returnCode: NSModalResponse);
var
  LIndex: Integer;
begin
  FAlert := nil;
  LIndex := returnCode - 1000;
  if FInternalActions[LIndex].Style = TAlertActionStyle.Cancel then
    DoAction(-2)
  else if Length(Actions) = 0 then
    DoAction(-1)
  else
    DoAction(LIndex);
end;

procedure TAlertDialog.DoShow(const ATitle, AMessage: string);
var
  LWindow: NSWindow;
  I: Integer;
  LButton: NSButton;
  LHasCancel: Boolean;
begin
  if Screen.ActiveForm <> nil then
  begin
    LWindow := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;
    if LWindow <> nil then
    begin
      FInternalActions := Copy(Actions);
      FAlert := TNSAlert.Create;
      FAlert.setMessageText(StrToNSStr(ATitle));
      FAlert.setInformativeText(StrToNSStr(AMessage));
      LHasCancel := False;
      for I := 0 to High(FInternalActions) do
      begin
        LButton := FAlert.addButtonWithTitle(StrToNSStr(FInternalActions[I].Title));
        if FInternalActions[I].Style = TAlertActionStyle.Cancel then
          LHasCancel := True;
      end;
      if Length(Actions) = 0 then
      begin
        FInternalActions := FInternalActions + [TAlertAction.Create(PositiveText, TAlertActionStyle.Default)];
        LButton := FAlert.addButtonWithTitle(StrToNSStr(PositiveText));
        FAlert.window.setDefaultButtonCell(TNSButtonCell.Wrap(LButton.cell));
      end;
      if not LHasCancel then
      begin
        FInternalActions := FInternalActions + [TAlertAction.Create(NegativeText, TAlertActionStyle.Cancel)];
        FAlert.addButtonWithTitle(StrToNSStr(NegativeText));
      end;
      FAlert.beginSheetModalForWindow(LWindow, AlertCompletionHandler);
    end;
  end;
end;

initialization
  AlertDialogProvider := TAlertDialogProvider.Create;

end.

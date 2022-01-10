unit DW.AlertView;

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
  System.Classes,
  // FMX
  FMX.Dialogs;

resourcestring
  SCancelActionCaption = 'Cancel';
  SDeleteActionCaption = 'Delete';

type
  TAlertActionStyle = (Default, Cancel, Destructive);
  TAlertViewStyle = (ActionSheet, Alert);
  // TAlertViewResultProc = reference to procedure(const AResult: Integer);

  TAddAlertActionMethod = procedure (const Title: string; const Handler: TNotifyEvent) of object;

  TCustomPlatformAlertView = class(TObject)
  private
    FAlertHandler: TInputCloseDialogProc;
  protected
    FCancelAdded: Boolean;
    procedure AddAction(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle); virtual; abstract;
    procedure Alert(const ATitle, AMessageText: string; const ACaptions: TArray<string>; const AHandler: TInputCloseDialogProc);
    procedure DoAlertAction(const AIndex: Integer);
    procedure DoAlert(const ATitle, AMessageText: string); virtual;
    procedure ClearActions; virtual; abstract;
    procedure DoShow(const ATitle, AMessageText: string); virtual; abstract;
    procedure Show(const ATitle, AMessageText: string; const ACancelHandler: TNotifyEvent);
  end;

  TCustomPlatformAlertViews = TArray<TCustomPlatformAlertView>;

  TAlertView = class(TObject)
  private
    class var FViews: TCustomPlatformAlertViews;
  private
    FPlatformAlertView: TCustomPlatformAlertView;
    FTagObject: TObject;
    FOnCancel: TNotifyEvent;
  protected
    class procedure DeleteView(const AView: TCustomPlatformAlertView);
  public
    class procedure Alert(const ATitle, AMessage: string; const ACaptions: TArray<string>; const AHandler: TInputCloseDialogProc = nil);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAction(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle = TAlertActionStyle.Default);
    procedure ClearActions;
    procedure Show(const ATitle: string = ''; const AMessage: string = '');
    property TagObject: TObject read FTagObject write FTagObject;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

implementation

uses
{$IF Defined(IOS)}
  DW.AlertView.iOS;
{$ELSE}
  DW.AlertView.Default;
{$ENDIF}

{ TCustomPlatformAlertView }

procedure TCustomPlatformAlertView.Alert(const ATitle, AMessageText: string; const ACaptions: TArray<string>; const AHandler: TInputCloseDialogProc);
var
  LCaption: string;
begin
  FAlertHandler := AHandler;
  for LCaption in ACaptions do
    AddAction(LCaption, nil, TAlertActionStyle.Default);
  DoAlert(ATitle, AMessageText);
end;

procedure TCustomPlatformAlertView.DoAlert(const ATitle, AMessageText: string);
begin
  //
end;

procedure TCustomPlatformAlertView.DoAlertAction(const AIndex: Integer);
begin
  if Assigned(FAlertHandler) then
    FAlertHandler(AIndex);
  TAlertView.DeleteView(Self);
end;

procedure TCustomPlatformAlertView.Show(const ATitle, AMessageText: string; const ACancelHandler: TNotifyEvent);
begin
  if not FCancelAdded then
    AddAction(SCancelActionCaption, ACancelHandler, TAlertActionStyle.Cancel);
  FCancelAdded := True;
  DoShow(ATitle, AMessageText);
end;

{ TAlertView }

constructor TAlertView.Create;
begin
  inherited Create;
  FPlatformAlertView := TPlatformAlertView.Create;
end;

destructor TAlertView.Destroy;
begin
  FPlatformAlertView.Free;
  inherited;
end;

class procedure TAlertView.Alert(const ATitle, AMessage: string; const ACaptions: TArray<string>; const AHandler: TInputCloseDialogProc = nil);
var
  LView: TCustomPlatformAlertView;
begin
  LView := TPlatformAlertView.Create;
  FViews := FViews + [LView];
  LView.Alert(ATitle, AMessage, ACaptions, AHandler);
end;

class procedure TAlertView.DeleteView(const AView: TCustomPlatformAlertView);
var
  I: Integer;
begin
  for I := 0 to Length(FViews) - 1 do
  begin
    if FViews[I] = AView then
    begin
      FViews[I].Free;
      Delete(FViews, I, 1);
      Break;
    end;
  end;
end;

procedure TAlertView.Show(const ATitle: string = ''; const AMessage: string = '');
begin
  FPlatformAlertView.Show(ATitle, AMessage, FOnCancel)
end;

procedure TAlertView.AddAction(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle = TAlertActionStyle.Default);
begin
  FPlatformAlertView.AddAction(ATitle, AHandler, AStyle);
end;

procedure TAlertView.ClearActions;
begin
  FPlatformAlertView.ClearActions;
end;

end.

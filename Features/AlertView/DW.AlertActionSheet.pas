unit DW.AlertActionSheet;

//TODO: Doc

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{          Copyright(c) 2020 David Nottage              }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes;

resourcestring
  SCancelActionCaption = 'Cancel';

type
  TAddAlertActionMethod = procedure (const Title: string; const Handler: TNotifyEvent) of object;

  TCustomPlatformAlertActionSheet = class(TObject)
  private
    FCancelAdded: Boolean;
  protected
    procedure AddAction(const ATitle: string; const AHandler: TNotifyEvent); virtual; abstract;
    procedure DoShow(const ATitle: string; const AMessageText: string); virtual; abstract;
    procedure Show(const ATitle: string; const AMessageText: string; const ACancelHandler: TNotifyEvent);
  end;

  TAlertActionSheet = class(TObject)
  private
    FMessageText: string;
    FPlatformInstance: TCustomPlatformAlertActionSheet;
    FTitle: string;
    FOnCancel: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAction(const ATitle: string; const AHandler: TNotifyEvent);
    procedure Show;
    property Title: string read FTitle write FTitle;
    property MessageText: string read FMessageText write FMessageText;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

implementation

uses
{$IF Defined(IOS)}
  DW.AlertActionSheet.iOS;
{$ELSE}
  DW.AlertActionSheet.Default;
{$ENDIF}

{ TCustomPlatformAlertActionSheet }

procedure TCustomPlatformAlertActionSheet.Show(const ATitle: string; const AMessageText: string; const ACancelHandler: TNotifyEvent);
begin
  if not FCancelAdded then
    AddAction(SCancelActionCaption, ACancelHandler);
  FCancelAdded := True;
  DoShow(ATitle, AMessageText);
end;

{ TAlertActionSheet }

constructor TAlertActionSheet.Create;
begin
  inherited Create;
  FPlatformInstance := TPlatformAlertActionSheet.Create;
end;

destructor TAlertActionSheet.Destroy;
begin
  FPlatformInstance.Free;
  inherited;
end;

procedure TAlertActionSheet.Show;
begin
  FPlatformInstance.Show(FTitle, FMessageText, FOnCancel);
end;

procedure TAlertActionSheet.AddAction(const ATitle: string; const AHandler: TNotifyEvent);
begin
  FPlatformInstance.AddAction(ATitle, AHandler);
end;

end.

unit DW.AlertView.Default;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  // FMX
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.Objects,
  // DW
  DW.AlertView;

type
  TButtonClickHandlerDictionary = TDictionary<TButton, TNotifyEvent>;

  TAlertViewFrame = class(TFrame)
    BackgroundRectangle: TRectangle;
    ContentLayout: TLayout;
    ButtonsLayout: TLayout;
    CancelButtonLayout: TLayout;
    CancelButton: TButton;
    TitleLayout: TLayout;
    TitleLabel: TLabel;
    MessageLabel: TLabel;
    ContentRectangle: TRectangle;
    procedure CancelButtonClick(Sender: TObject);
    procedure FrameClick(Sender: TObject);
  private
    FDictionary: TButtonClickHandlerDictionary;
    FUseFader: Boolean;
    FOnCancel: TNotifyEvent;
    procedure AdjustTitleLayout;
    procedure ButtonClickHandler(Sender: TObject);
    procedure DoCancel;
    function GetContentHeight: Single;
    function GetContentWidth: Single;
    procedure HideView;
    procedure Resize; override;
  protected
    procedure ClearButtons;
    procedure ShowView(const ATitle: string; const AMessageText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddButton(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle);
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

  TPlatformAlertView = class(TCustomPlatformAlertView)
  private
    FView: TAlertViewFrame;
  protected
    procedure AddAction(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle); override;
    procedure ClearActions; override;
    procedure DoShow(const ATitle: string; const AMessageText: string); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  DW.OSLog,
  // FMX
  FMX.Ani;

{ TAlertViewFrame }

constructor TAlertViewFrame.Create(AOwner: TComponent);
begin
  inherited;
  TitleLabel.TagFloat := TitleLabel.Height;
  MessageLabel.TagFloat := MessageLabel.Height;
  FDictionary := TButtonClickHandlerDictionary.Create;
  FUseFader := True;
  ContentLayout.Align := TAlignLayout.None;
end;

destructor TAlertViewFrame.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

procedure TAlertViewFrame.DoCancel;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TAlertViewFrame.FrameClick(Sender: TObject);
begin
  HideView;
end;

function TAlertViewFrame.GetContentHeight: Single;
var
  LControl: TStyledControl;
  I: Integer;
begin
  Result := 0;
  AdjustTitleLayout;
  for I := 0 to ButtonsLayout.ControlsCount - 1 do
  begin
    if not (ButtonsLayout.Controls[I] is TStyledControl) then
      Continue;
    LControl := TStyledControl(ButtonsLayout.Controls[I]);
    if LControl.Visible then
    begin
      LControl.NeedStyleLookup;
      LControl.ApplyStyleLookup;
      Result := Result + LControl.Height + LControl.Margins.Top + LControl.Margins.Bottom;
    end;
  end;
  Result := Result + TitleLayout.Height + CancelButtonLayout.Height;
end;

function TAlertViewFrame.GetContentWidth: Single;
begin
  Result := Width;
end;

procedure TAlertViewFrame.ShowView(const ATitle: string; const AMessageText: string);
begin
  TitleLabel.Text := ATitle;
  MessageLabel.Text := AMessageText;
  ContentLayout.Position.Y := Height;
  Parent := Screen.ActiveForm;
  Visible := True;
  Resize;
  if FUseFader then
    TAnimator.AnimateFloat(BackgroundRectangle, 'Opacity', 0.5);
  TAnimator.AnimateFloat(ContentLayout, 'Position.Y', Height - ContentLayout.Height);
end;

procedure TAlertViewFrame.HideView;
begin
  if FUseFader then
    TAnimator.AnimateFloat(BackgroundRectangle, 'Opacity', 0);
  TAnimator.AnimateFloatWait(ContentLayout, 'Position.Y', Height);
  Visible := False;
end;

procedure TAlertViewFrame.Resize;
begin
  inherited;
  if ContentLayout <> nil then
  begin
    ContentLayout.Width := GetContentWidth;
    ContentLayout.Height := GetContentHeight;
  end;
end;

procedure TAlertViewFrame.AddButton(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle);
var
  LButton: TButton;
begin
  LButton := TButton.Create(Self);
  LButton.Text := ATitle;
  LButton.Height := CancelButton.Height;
  LButton.Margins.Rect := RectF(4, 4, 4, 0);
  LButton.OnClick := ButtonClickHandler;
  LButton.Align := TAlignLayout.Bottom;
  LButton.Position.Y := ButtonsLayout.Height - (LButton.Height + LButton.Margins.Rect.Height);
  LButton.Parent := ButtonsLayout;
  FDictionary.Add(LButton, AHandler);
end;

procedure TAlertViewFrame.ClearButtons;
var
  LButton: TButton;
begin
  for LButton in FDictionary.Keys do
    LButton.Free;
  FDictionary.Clear;
end;

procedure TAlertViewFrame.AdjustTitleLayout;
begin
  TitleLabel.Visible := not TitleLabel.Text.IsEmpty;
  MessageLabel.Visible := not MessageLabel.Text.IsEmpty;
  TitleLayout.Height := (TitleLabel.TagFloat * Ord(TitleLabel.Visible)) + (MessageLabel.TagFloat * Ord(MessageLabel.Visible));
end;

procedure TAlertViewFrame.ButtonClickHandler(Sender: TObject);
var
  LButton: TButton;
  LHandler: TNotifyEvent;
begin
  HideView;
  LButton := TButton(Sender);
  if FDictionary.TryGetValue(LButton, LHandler) then
    LHandler(LButton);
end;

procedure TAlertViewFrame.CancelButtonClick(Sender: TObject);
begin
  HideView;
  DoCancel;
end;

{ TPlatformAlertView }

constructor TPlatformAlertView.Create;
begin
  inherited;
  FView := TAlertViewFrame.Create(nil);
end;

destructor TPlatformAlertView.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TPlatformAlertView.AddAction(const ATitle: string; const AHandler: TNotifyEvent; const AStyle: TAlertActionStyle);
begin
  if ATitle.Equals(SCancelActionCaption) then
    FView.OnCancel := AHandler
  else
    FView.AddButton(ATitle, AHandler, AStyle);
end;

procedure TPlatformAlertView.ClearActions;
begin
  FView.ClearButtons;
end;

procedure TPlatformAlertView.DoShow(const ATitle: string; const AMessageText: string);
begin
  inherited;
  FView.ShowView(ATitle, AMessageText);
end;

end.

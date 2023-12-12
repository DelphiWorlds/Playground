unit DW.AlertDialog.Default;

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

uses
  // RTL
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  // FMX
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.Objects,
  // DW
  DW.AlertDialog;

type
  TAlertActionEvent = procedure(Sender: TObject; const Index: Integer) of object;

  TAlertView = class(TFrame)
    BackgroundRectangle: TRectangle;
    ContentLayout: TLayout;
    ButtonsLayout: TLayout;
    CancelButtonLayout: TLayout;
    CancelButton: TButton;
    TitleLayout: TLayout;
    TitleLabel: TLabel;
    MessageLabel: TLabel;
    ContentRectangle: TRectangle;
    procedure FrameClick(Sender: TObject);
  private
    FUseFader: Boolean;
    FNegativeText: string;
    FPositiveText: string;
    FOnAction: TAlertActionEvent;
    procedure AddButton(const ATitle: string; const AIndex: Integer; const AStyle: TAlertActionStyle);
    procedure AdjustTitleLayout;
    procedure ButtonClickHandler(Sender: TObject);
    procedure ClearButtons;
    procedure CreateButtons(const AActions: TAlertActions);
    procedure DoAction(const AIndex: Integer);
    function GetContentHeight: Single;
    function GetContentWidth: Single;
    procedure HideView;
  protected
    procedure Resize; override;
    procedure ShowActions(const ATitle: string; const AMessageText: string; const AActions: TAlertActions);
    property NegativeText: string read FNegativeText write FNegativeText;
    property PositiveText: string read FPositiveText write FPositiveText;
    property OnAction: TAlertActionEvent read FOnAction write FOnAction;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TAlertDialog = class(TCustomAlertDialog)
  private
    FView: TAlertView;
    procedure ViewActionHandler(Sender: TObject; const AIndex: Integer);
  protected
    procedure DoShow(const ATitle, AMessage: string); override;
  public
    destructor Destroy; override;
  end;

  TAlertDialogProvider = class(TInterfacedObject, IAlertDialogProvider)
  public
    { IAlertDialogProvider }
    function GetDialog: IAlertDialog;
  end;

implementation

{$R *.fmx}

uses
  // FMX
  FMX.Ani;

{ TAlertView }

constructor TAlertView.Create(AOwner: TComponent);
begin
  inherited;
  CancelButton.OnClick := ButtonClickHandler;
  TitleLabel.TagFloat := TitleLabel.Height;
  MessageLabel.TagFloat := MessageLabel.Height;
  FUseFader := True;
  ContentLayout.Align := TAlignLayout.None;
end;

destructor TAlertView.Destroy;
begin
  //
  inherited;
end;

procedure TAlertView.DoAction(const AIndex: Integer);
begin
  if Assigned(FOnAction) then
    FOnAction(Self, AIndex);
end;

procedure TAlertView.FrameClick(Sender: TObject);
begin
  HideView;
end;

function TAlertView.GetContentHeight: Single;
var
  LControl: TStyledControl;
  I: Integer;
begin
  Result := 0;
  AdjustTitleLayout;
  for I := 0 to ButtonsLayout.ControlsCount - 1 do
  begin
    if ButtonsLayout.Controls[I] is TStyledControl then
    begin
      LControl := TStyledControl(ButtonsLayout.Controls[I]);
      if LControl.Visible then
      begin
        LControl.NeedStyleLookup;
        LControl.ApplyStyleLookup;
        Result := Result + LControl.Height + LControl.Margins.Top + LControl.Margins.Bottom;
      end;
    end;
  end;
  Result := Result + TitleLayout.Height + CancelButtonLayout.Height;
end;

function TAlertView.GetContentWidth: Single;
begin
  Result := Width;
end;

procedure TAlertView.ShowActions(const ATitle: string; const AMessageText: string; const AActions: TAlertActions);
begin
  CreateButtons(AActions);
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

procedure TAlertView.CreateButtons(const AActions: TAlertActions);
var
  I: Integer;
begin
  ClearButtons;
  CancelButton.Text := FNegativeText;
  for I := 0 to Length(AActions) - 1 do
  begin
    if AActions[I].Style <> TAlertActionStyle.Cancel then
      AddButton(AActions[I].Title, I, AActions[I].Style)
    else
      CancelButton.Text := AActions[I].Title;
  end;
  if Length(AActions) = 0 then
    AddButton(FPositiveText, -1, TAlertActionStyle.Default)
end;

procedure TAlertView.HideView;
begin
  if FUseFader then
    TAnimator.AnimateFloat(BackgroundRectangle, 'Opacity', 0);
  TAnimator.AnimateFloatWait(ContentLayout, 'Position.Y', Height);
  Visible := False;
  Parent := nil;
end;

procedure TAlertView.Resize;
begin
  inherited;
  if ContentLayout <> nil then
  begin
    ContentLayout.Width := GetContentWidth;
    ContentLayout.Height := GetContentHeight;
  end;
end;

procedure TAlertView.AddButton(const ATitle: string; const AIndex: Integer; const AStyle: TAlertActionStyle);
var
  LButton: TButton;
begin
  LButton := TButton.Create(Self);
  LButton.Tag := AIndex;
  LButton.Text := ATitle;
  LButton.Height := CancelButton.Height;
  LButton.Margins.Rect := RectF(4, 4, 4, 0);
  LButton.OnClick := ButtonClickHandler;
  LButton.Align := TAlignLayout.Bottom;
  LButton.Position.Y := ButtonsLayout.Height - (LButton.Height + LButton.Margins.Rect.Height);
  LButton.Parent := ButtonsLayout;
end;

procedure TAlertView.ClearButtons;
var
  I: Integer;
begin
  for I := ButtonsLayout.ChildrenCount - 1 downto 0 do
  begin
    if ButtonsLayout.Children[I] is TButton then
      ButtonsLayout.Children[I].Free;
  end;
end;

procedure TAlertView.AdjustTitleLayout;
begin
  TitleLabel.Visible := not TitleLabel.Text.IsEmpty;
  MessageLabel.Visible := not MessageLabel.Text.IsEmpty;
  TitleLayout.Height := (TitleLabel.TagFloat * Ord(TitleLabel.Visible)) + (MessageLabel.TagFloat * Ord(MessageLabel.Visible));
end;

procedure TAlertView.ButtonClickHandler(Sender: TObject);
begin
  HideView;
  DoAction(TComponent(Sender).Tag);
end;

{ TAlertDialog }

destructor TAlertDialog.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TAlertDialog.DoShow(const ATitle, AMessage: string);
begin
  if FView = nil then
  begin
    FView := TAlertView.Create(nil);
    FView.OnAction := ViewActionHandler;
  end;
  FView.PositiveText := PositiveText;
  FView.NegativeText := NegativeText;
  FView.ShowActions(ATitle, AMessage, Actions);
end;

procedure TAlertDialog.ViewActionHandler(Sender: TObject; const AIndex: Integer);
begin
  DoAction(AIndex);
end;

{ TAlertDialogProvider }

function TAlertDialogProvider.GetDialog: IAlertDialog;
begin
  Result := TAlertDialog.Create;
end;

initialization
  AlertDialogProvider := TAlertDialogProvider.Create;

end.

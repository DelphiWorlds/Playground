unit DW.AlertDialog.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.Helpers,
  // DW
  DW.AlertDialog;

type
  TAlertActionsHelper = record helper for TAlertActions
    function GetCaptions: TJavaObjectArray<JCharSequence>;
  end;

  TAlertDialog = class;

  TAlertDialogClickListener = class(TJavaLocal, JDialogInterface_OnClickListener)
  private
    FAlertDialog: TAlertDialog;
  public
    { JDialogInterface_OnClickListener }
    procedure onClick(dialog: JDialogInterface; which: Integer); cdecl;
  public
    constructor Create(const AAlertDialog: TAlertDialog);
  end;

  TAlertDialog = class(TCustomAlertDialog)
  private
    FDialog: JAlertDialog;
    FClickListener: JDialogInterface_OnClickListener;
  protected
    procedure DoShow(const ATitle, AMessage: string); override;
    procedure OnClick(const ADialog: JDialogInterface; const AWhich: Integer);
  public
    constructor Create;
  end;

  TAlertDialogProvider = class(TInterfacedObject, IAlertDialogProvider)
  public
    { IAlertDialogProvider }
    function GetDialog: IAlertDialog;
  end;

{ TAlertActionsHelper }

function TAlertActionsHelper.GetCaptions: TJavaObjectArray<JCharSequence>;
var
  I: Integer;
begin
  Result := TJavaObjectArray<JCharSequence>.Create(Length(Self));
  for I := 0 to High(Self) do
    Result[I] := StrToJCharSequence(Self[I].Title);
end;

{ TAlertDialogClickListener }

constructor TAlertDialogClickListener.Create(const AAlertDialog: TAlertDialog);
begin
  inherited Create;
  FAlertDialog := AAlertDialog;
end;

procedure TAlertDialogClickListener.onClick(dialog: JDialogInterface; which: Integer);
begin
  FAlertDialog.OnClick(dialog, which);
end;

{ TAlertDialog }

constructor TAlertDialog.Create;
begin
  inherited;
  FClickListener := TAlertDialogClickListener.Create(Self);
end;

procedure TAlertDialog.OnClick(const ADialog: JDialogInterface; const AWhich: Integer);
begin
  DoAction(AWhich);
end;

procedure TAlertDialog.DoShow(const ATitle, AMessage: string);
var
  LBuilder: JAlertDialog_Builder;
  LCaptions: TJavaObjectArray<JCharSequence>;
begin
  FDialog := nil;
  LBuilder := TJAlertDialog_Builder.JavaClass.init(TAndroidHelper.Context)
    .setTitle(StrToJCharSequence(ATitle))
    .setCancelable(False)
    .setNegativeButton(StrToJCharSequence(NegativeText), FClickListener);
  if Length(Actions) > 0 then
  begin
    LCaptions := Actions.GetCaptions;
    try
      LBuilder.setItems(LCaptions, FClickListener);
    finally
      LCaptions.Free;
    end;
  end
  else
  begin
    LBuilder.setMessage(StrToJCharSequence(AMessage))
      .setPositiveButton(StrToJCharSequence(PositiveText), FClickListener);
  end;
  FDialog := LBuilder.create;
  FDialog.show;
end;

{ TAlertDialogProvider }

function TAlertDialogProvider.GetDialog: IAlertDialog;
begin
  Result := TAlertDialog.Create;
end;

initialization
  AlertDialogProvider := TAlertDialogProvider.Create;

end.

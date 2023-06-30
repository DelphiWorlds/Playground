unit DW.AlertDialog;

interface

type
  TAlertActionStyle = (Default, Cancel, Destructive);

  TAlertDialogResultProc = reference to procedure(const ActionIndex: Integer);

  IAlertDialog = interface(IInterface)
    ['{ED3E6C95-1EC7-4D7E-8362-FD31C700AA43}']
    procedure AddAction(const ATitle: string; const AStyle: TAlertActionStyle = TAlertActionStyle.Default);
    procedure ClearActions;
    function GetNegativeText: string;
    function GetPositiveText: string;
    procedure SetNegativeText(const Value: string);
    procedure SetPositiveText(const Value: string);
    procedure Show(const ATitle, AMessage: string; const AResultHandler: TAlertDialogResultProc);
    property NegativeText: string read GetNegativeText write SetNegativeText;
    property PositiveText: string read GetPositiveText write SetPositiveText;
  end;

  IAlertDialogProvider = interface(IInterface)
    ['{71D36429-4185-47C7-88E0-BC9E0CB06C05}']
    function GetDialog: IAlertDialog;
  end;

  TAlertAction = record
    Title: string;
    Style: TAlertActionStyle;
    NativeAction: Pointer;
    constructor Create(const ATitle: string; const AStyle: TAlertActionStyle);
  end;

  TAlertActions = TArray<TAlertAction>;

  TCustomAlertDialog = class(TInterfacedObject, IAlertDialog)
  private
    FActions: TAlertActions;
    FNegativeText: string;
    FPositiveText: string;
    FResultHandler: TAlertDialogResultProc;
  protected
    procedure DoAction(const AIndex: Integer);
    procedure DoShow(const ATitle, AMessage: string); virtual;
    property Actions: TAlertActions read FActions;
  public
    { IAlertDialog }
    procedure AddAction(const ATitle: string; const AStyle: TAlertActionStyle = TAlertActionStyle.Default);
    procedure ClearActions;
    function GetNegativeText: string;
    function GetPositiveText: string;
    procedure SetNegativeText(const Value: string);
    procedure SetPositiveText(const Value: string);
    procedure Show(const ATitle, AMessage: string; const AResultHandler: TAlertDialogResultProc);
    property NegativeText: string read GetNegativeText;
    property PositiveText: string read GetPositiveText;
  public
    constructor Create;
  end;

var
  AlertDialogProvider: IAlertDialogProvider;

implementation

uses
  {$IF Defined(ANDROID)}
  DW.AlertDialog.Android;
  {$ENDIF}
  {$IF Defined(IOS)}
  DW.AlertDialog.iOS;
  {$ENDIF}

{ TAlertAction }

constructor TAlertAction.Create(const ATitle: string; const AStyle: TAlertActionStyle);
begin
  Title := ATitle;
  Style := AStyle;
end;

{ TCustomAlertDialog }

constructor TCustomAlertDialog.Create;
begin
  inherited;
  FPositiveText := 'OK';
  FNegativeText := 'Cancel';
end;

procedure TCustomAlertDialog.DoAction(const AIndex: Integer);
begin
  if Assigned(FResultHandler) then
    FResultHandler(AIndex);
end;

procedure TCustomAlertDialog.DoShow(const ATitle, AMessage: string);
begin
  //
end;

procedure TCustomAlertDialog.AddAction(const ATitle: string; const AStyle: TAlertActionStyle);
begin
  FActions := FActions + [TAlertAction.Create(ATitle, AStyle)];
end;

procedure TCustomAlertDialog.ClearActions;
begin
  FActions := [];
end;

function TCustomAlertDialog.GetNegativeText: string;
begin
  Result := FNegativeText;
end;

function TCustomAlertDialog.GetPositiveText: string;
begin
  Result := FPositiveText;
end;

procedure TCustomAlertDialog.SetNegativeText(const Value: string);
begin
  FNegativeText := Value;
end;

procedure TCustomAlertDialog.SetPositiveText(const Value: string);
begin
  FPositiveText := Value;
end;

procedure TCustomAlertDialog.Show(const ATitle, AMessage: string; const AResultHandler: TAlertDialogResultProc);
begin
  FResultHandler := AResultHandler;
  DoShow(ATitle, AMessage);
end;

end.

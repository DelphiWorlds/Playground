unit DW.VKObserver;

interface

uses
  System.Types, System.Messaging,
  FMX.Types, FMX.Controls;

type
  TVKObserver = class;

  TControlContainer = record
    ContentBounds: TRectF;
    Control: TControl;
    HasSaved: Boolean;
    Height: Single;
    Margins: TRectF;
    constructor Create(const AControl: TControl);
    procedure Restore;
    procedure Save;
  end;

  TControlContainers = TArray<TControlContainer>;

  TCustomPlatformVKObserver = class(TObject)
  private
    class function IsParentOf(const AParent, AControl: TControl): Boolean;
  private
    FActiveContainerIndex: Integer;
    FActiveFrame: TRect;
    FContainers: TControlContainers;
    FVKObserver: TVKObserver;
    procedure AdjustContainer(const AVKTop: Integer);
    procedure DoFrameChanged;
    function FindContainer(const AControl: TControl; out AIndex: Integer): Boolean;
    procedure FormChangingFocusControlMessageHandler(const ASender: TObject; const AMsg: TMessage);
    procedure RestoreContainer;
  protected
    procedure FrameChanged(const AFrame: TRect); virtual;
    procedure FocusChanged(const ANewFocusedControl: IControl); virtual;
    property ActiveFrame: TRect read FActiveFrame;
  public
    constructor Create(const AVKObserver: TVKObserver); virtual;
    destructor Destroy; override;
    procedure AddContainer(const AControl: TControl);
  end;

  TVKObserver = class(TObject)
  private
    FPlatformVKObserver: TCustomPlatformVKObserver;
    function GetActiveFrame: TRect;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContainer(const AControl: TControl);
    property ActiveFrame: TRect read GetActiveFrame;
  end;

implementation

uses
  FMX.Forms,
  {$IF Defined(IOS)}
  DW.VKObserver.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.VKObserver.Android;
  {$ELSE}
  // DW.VKObserver.Default;
  {$ENDIF}

{ TControlContainer }

constructor TControlContainer.Create(const AControl: TControl);
begin
  Control := AControl;
  HasSaved := False;
end;

procedure TControlContainer.Restore;
begin
  if HasSaved then
  begin
    Control.Margins.Rect := Margins;
    Control.Height := Height;
  end;
end;

procedure TControlContainer.Save;
begin
  Margins := Control.Margins.Rect;
  Height := Control.Height;
  HasSaved := True;
end;

{ TCustomPlatformVKObserver }

constructor TCustomPlatformVKObserver.Create(const AVKObserver: TVKObserver);
begin
  inherited Create;
  FActiveContainerIndex := -1;
  FVKObserver := AVKObserver;
  TMessageManager.DefaultManager.SubscribeToMessage(TFormChangingFocusControl, FormChangingFocusControlMessageHandler);
end;

destructor TCustomPlatformVKObserver.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormChangingFocusControl, FormChangingFocusControlMessageHandler);
  inherited;
end;

class function TCustomPlatformVKObserver.IsParentOf(const AParent, AControl: TControl): Boolean;
var
  LParent: TControl;
begin
  LParent := AControl.ParentControl;
  while (LParent <> nil) and (LParent <> AParent) do
    LParent := LParent.ParentControl;
  Result := LParent = AParent;
end;

procedure TCustomPlatformVKObserver.AddContainer(const AControl: TControl);
begin
  FContainers := FContainers + [TControlContainer.Create(AControl)];
end;

procedure TCustomPlatformVKObserver.AdjustContainer(const AVKTop: Integer);
var
  LControl: TControl;
  LOffset: Single;
begin
  if Screen.FocusControl <> nil then
  begin
    RestoreContainer;
    LControl := TControl(Screen.FocusControl.GetObject);
    if FindContainer(LControl, FActiveContainerIndex) then
    begin
      LOffset := (LControl.AbsoluteRect.Bottom + 2) - AVKTop;
      if LOffset > 0 then
      begin
        FContainers[FActiveContainerIndex].Save;
        FContainers[FActiveContainerIndex].Control.Margins.Top := -LOffset;
        FContainers[FActiveContainerIndex].Control.Height := FContainers[FActiveContainerIndex].Control.Height + LOffset;
      end;
    end;
  end;
end;

procedure TCustomPlatformVKObserver.RestoreContainer;
begin
  if FActiveContainerIndex > -1 then
    FContainers[FActiveContainerIndex].Restore;
  FActiveContainerIndex := -1;
end;

function TCustomPlatformVKObserver.FindContainer(const AControl: TControl; out AIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  AIndex := -1;
  for I := 0 to Length(FContainers) - 1 do
  begin
    if IsParentOf(FContainers[I].Control, AControl) then
    begin
      AIndex := I;
      Result := True;
      Break;
    end;
  end;
end;

procedure TCustomPlatformVKObserver.DoFrameChanged;
begin
  if FActiveFrame.Height > 0 then
    AdjustContainer(FActiveFrame.Top)
  else
    RestoreContainer;
end;

procedure TCustomPlatformVKObserver.FrameChanged(const AFrame: TRect);
begin
  FActiveFrame := AFrame;
  DoFrameChanged;
end;

procedure TCustomPlatformVKObserver.FocusChanged(const ANewFocusedControl: IControl);
begin
  DoFrameChanged;
end;

procedure TCustomPlatformVKObserver.FormChangingFocusControlMessageHandler(const ASender: TObject; const AMsg: TMessage);
begin
  FocusChanged(TFormChangingFocusControl(AMsg).NewFocusedControl);
end;

{ TVKObserver }

constructor TVKObserver.Create;
begin
  inherited;
  FPlatformVKObserver := TPlatformVKObserver.Create(Self);
end;

destructor TVKObserver.Destroy;
begin
  FPlatformVKObserver.Free;
  inherited;
end;

function TVKObserver.GetActiveFrame: TRect;
begin
  Result := FPlatformVKObserver.ActiveFrame;
end;

procedure TVKObserver.AddContainer(const AControl: TControl);
begin
  FPlatformVKObserver.AddContainer(AControl);
end;

end.

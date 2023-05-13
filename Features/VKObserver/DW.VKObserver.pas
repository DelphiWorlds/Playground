unit DW.VKObserver;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  System.Types, System.Messaging,
  FMX.Types, FMX.Controls;

type
  TVKObserver = class;

  TControlContainers = TArray<TControl>;

  TCustomPlatformVKObserver = class(TObject)
  private
    class function IsParentOf(const AParent, AControl: TControl): Boolean;
  private
    FActiveContainer: TControl;
    FActiveFrame: TRect;
    FContainers: TControlContainers;
    FVKObserver: TVKObserver;
    procedure AdjustContainer(const AVKTop: Integer);
    procedure DoFrameChanged;
    function FindContainer(const AControl: TControl; out AContainer: TControl): Boolean;
    procedure FormChangingFocusControlMessageHandler(const ASender: TObject; const AMsg: TMessage);
    procedure RestoreContainer;
  protected
    procedure FrameChanged(const AFrame: TRect); virtual;
    procedure FocusChanged(const ANewFocusedControl: IControl); virtual;
  public
    constructor Create(const AVKObserver: TVKObserver); virtual;
    destructor Destroy; override;
    procedure AddContainer(const AContainer: TControl);
  end;

  TVKObserver = class(TObject)
  private
    FPlatformVKObserver: TCustomPlatformVKObserver;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContainer(const AContainer: TControl);
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

{ TCustomPlatformVKObserver }

constructor TCustomPlatformVKObserver.Create(const AVKObserver: TVKObserver);
begin
  inherited Create;
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

procedure TCustomPlatformVKObserver.AddContainer(const AContainer: TControl);
begin
  FContainers := FContainers + [AContainer];
end;

procedure TCustomPlatformVKObserver.AdjustContainer(const AVKTop: Integer);
var
  LControl: TControl;
  LOffset: Single;
begin
  if Screen.FocusControl <> nil then
  begin
    LControl := TControl(Screen.FocusControl.GetObject);
    if FindContainer(LControl, FActiveContainer) then
    begin
      FActiveContainer.Margins.Top := 0;
      LOffset := (LControl.AbsoluteRect.Bottom + 2) - AVKTop;
      if LOffset > 0 then
        FActiveContainer.Margins.Top := -LOffset
      else
        RestoreContainer;
    end;
  end;
end;

function TCustomPlatformVKObserver.FindContainer(const AControl: TControl; out AContainer: TControl): Boolean;
var
  LContainer: TControl;
begin
  Result := False;
  for LContainer in FContainers do
  begin
    if IsParentOf(LContainer, AControl) then
    begin
      AContainer := LContainer;
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

procedure TCustomPlatformVKObserver.RestoreContainer;
begin
  if FActiveContainer <> nil then
    FActiveContainer.Margins.Top := 0;
  FActiveContainer := nil;
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

procedure TVKObserver.AddContainer(const AContainer: TControl);
begin
  FPlatformVKObserver.AddContainer(AContainer);
end;

end.

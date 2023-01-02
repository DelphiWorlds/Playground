unit DW.SwipeView;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Ani;

type
  TMouseInfo = record
  public
    DownPt: TPointF;
    DownTime: TDateTime;
    HasMoved: Boolean;
    IsDown: Boolean;
    MovePt: TPointF;
    MoveThreshold: TPoint;
    UpTime: TDateTime;
    function GetDistance: TPointF;
    function GetSpeed: Single;
    procedure MouseDown(const X, Y: Single; const AShift: TShiftState);
    procedure MouseMove(const X, Y: Single); overload;
    function MouseMove(const X, Y: Single; out ADistance: TPointF): Boolean; overload;
    function MouseUp: Boolean;
  end;

  TSwipeDirection = (Left, Right);

  TSwipedEvent = procedure(Sender: TObject; const SwipeDirection: TSwipeDirection) of object;

  TLayoutPosition = (Left, Center, Right);

  TLayouts = array[TLayoutPosition] of TLayout;

  TSwipeView = class(TFrame)
    MainLayout: TLayout;
    MainLayoutAnimation: TFloatAnimation;
    procedure FrameResized(Sender: TObject);
    procedure MainLayoutAnimationFinish(Sender: TObject);
  private
    FCanSwipeLeft: Boolean;
    FCanSwipeRight: Boolean;
    FLayouts: TLayouts;
    FMouseInfo: TMouseInfo;
    FSwipeDirection: TSwipeDirection;
    FSwipeSpeed: Single;
    FOnSwiped: TSwipedEvent;
    procedure AnimateMainLayout(const APositionX: Single; const AIsSwipe: Boolean = True);
    procedure CreateLayouts;
    function GetLayout(const APosition: TLayoutPosition): TLayout;
    procedure MouseDone;
  protected
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    property CanSwipeLeft: Boolean read FCanSwipeLeft write FCanSwipeLeft;
    property CanSwipeRight: Boolean read FCanSwipeRight write FCanSwipeRight;
    property Layout[const APosition: TLayoutPosition]: TLayout read GetLayout;
    property SwipeSpeed: Single read FSwipeSpeed write FSwipeSpeed;
    property OnSwiped: TSwipedEvent read FOnSwiped write FOnSwiped;
  end;

implementation

{$R *.fmx}

uses
  System.DateUtils,
  DW.OSLog;

const
  cSwipeVelocityDefault = 150;

{ TMouseInfo }

procedure TMouseInfo.MouseDown(const X, Y: Single; const AShift: TShiftState);
begin
  IsDown := True;
  HasMoved := False;
  DownPt := PointF(X, Y);
  MovePt := PointF(X, Y);
  DownTime := Now;
end;

procedure TMouseInfo.MouseMove(const X, Y: Single);
var
  LHasMovedX, LHasMovedY: Boolean;
begin
  if IsDown and not HasMoved then
  begin
    LHasMovedX := (MoveThreshold.X > -1) and (Abs(X - MovePt.X) > MoveThreshold.X);
    LHasMovedY := (MoveThreshold.Y > -1) and (Abs(Y - MovePt.Y) > MoveThreshold.Y);
    HasMoved := LHasMovedX or LHasMovedY;
  end;
end;

function TMouseInfo.MouseMove(const X, Y: Single; out ADistance: TPointF): Boolean;
var
  LMoved: Boolean;
begin
  Result := False;
  if IsDown then
  begin
    LMoved := HasMoved;
    MouseMove(X, Y);
    Result := LMoved or HasMoved;
    if Result then
    begin
      ADistance := PointF(X - MovePt.X, Y - MovePt.Y);
      MovePt := PointF(X, Y);
    end;
  end;
end;

function TMouseInfo.MouseUp: Boolean;
begin
  Result := IsDown;
  IsDown := False;
  HasMoved := False;
  UpTime := Now;
end;

function TMouseInfo.GetDistance: TPointF;
begin
  Result := PointF(MovePt.X - DownPt.X, MovePt.Y - DownPt.Y);
end;

function TMouseInfo.GetSpeed: Single;
begin
  Result := (Abs(MovePt.X - DownPt.X) / SecondSpan(UpTime, DownTime)) / 2;
end;

{ TSwipeView }

constructor TSwipeView.Create(AOwner: TComponent);
begin
  inherited;
  FCanSwipeLeft := True;
  FCanSwipeRight := True;
  FSwipeSpeed := cSwipeVelocityDefault;
  FMouseInfo.MoveThreshold := Point(7, -1);
  CreateLayouts;
end;

procedure TSwipeView.CreateLayouts;
var
  LPosition: TLayoutPosition;
begin
  for LPosition := Low(TLayoutPosition) to High(TLayoutPosition) do
  begin
    FLayouts[LPosition] := TLayout.Create(Self);
    FLayouts[LPosition].Position.X := Ord(LPosition) * Width;
    FLayouts[LPosition].Align := TAlignLayout.Left;
    FLayouts[LPosition].Parent := MainLayout;
  end;
end;

function TSwipeView.GetLayout(const APosition: TLayoutPosition): TLayout;
begin
  Result := FLayouts[APosition];
end;

procedure TSwipeView.FrameResized(Sender: TObject);
var
  LPosition: TLayoutPosition;
begin
  MainLayout.Width := Width * 3;
  MainLayout.Position.X := -Width;
  for LPosition := Low(TLayoutPosition) to High(TLayoutPosition) do
    FLayouts[LPosition].Width := Width;
end;

procedure TSwipeView.AnimateMainLayout(const APositionX: Single; const AIsSwipe: Boolean = True);
begin
  if AIsSwipe then
    MainLayoutAnimation.OnFinish := MainLayoutAnimationFinish
  else
    MainLayoutAnimation.OnFinish := nil;
  if APositionX < 0 then
    FSwipeDirection := TSwipeDirection.Left
  else
    FSwipeDirection := TSwipeDirection.Right;
  MainLayoutAnimation.StopValue := APositionX;
  MainLayoutAnimation.Start;
end;

procedure TSwipeView.MainLayoutAnimationFinish(Sender: TObject);
var
  LLayout: TLayout;
  LPosition: TLayoutPosition;
begin
  for LPosition := Low(TLayoutPosition) to High(TLayoutPosition) do
    FLayouts[LPosition].Align := TAlignLayout.None;
  case FSwipeDirection of
    TSwipeDirection.Left:
    begin
      LLayout := FLayouts[TLayoutPosition.Left];
      FLayouts[TLayoutPosition.Left] := FLayouts[TLayoutPosition.Center];
      FLayouts[TLayoutPosition.Center] := FLayouts[TLayoutPosition.Right];
      FLayouts[TLayoutPosition.Right] := LLayout;
    end;
    TSwipeDirection.Right:
    begin
      LLayout := FLayouts[TLayoutPosition.Right];
      FLayouts[TLayoutPosition.Right] := FLayouts[TLayoutPosition.Center];
      FLayouts[TLayoutPosition.Center] := FLayouts[TLayoutPosition.Left];
      FLayouts[TLayoutPosition.Left] := LLayout;
    end;
  end;
  for LPosition := Low(TLayoutPosition) to High(TLayoutPosition) do
  begin
    FLayouts[LPosition].Position.X := Ord(LPosition) * Width;
    FLayouts[LPosition].Align := TAlignLayout.Left;
  end;
  MainLayout.Position.X := -Width;
  if Assigned(FOnSwiped) then
    FOnSwiped(Self, FSwipeDirection);
end;

procedure TSwipeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouseInfo.MouseDown(X, Y, Shift);
end;

procedure TSwipeView.MouseMove(Shift: TShiftState; X, Y: Single);
var
  LDistance: TPointF;
begin
  inherited;
  if FMouseInfo.MouseMove(X, Y, LDistance) then
  begin
    if ((LDistance.X < 0) and FCanSwipeLeft) or ((LDistance.X > 0) and FCanSwipeRight) then
      MainLayout.Position.X := MainLayout.Position.X + LDistance.X;
    if (LDistance.X < 0) and (MainLayout.Position.X < -(Width * 1.5)) then
      AnimateMainLayout(-(Width * 2))
    else if (LDistance.X > 0) and (MainLayout.Position.X > -(Width * 0.5)) then
      AnimateMainLayout(0);
   end;
end;

procedure TSwipeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  MouseDone;
end;

procedure TSwipeView.DoMouseLeave;
begin
  inherited;
  MouseDone;
end;

procedure TSwipeView.MouseDone;
begin
  if FMouseInfo.MouseUp then
  begin
    if not MainLayoutAnimation.Running then
    begin
      if FMouseInfo.GetSpeed > FSwipeSpeed then
      begin
        if FMouseInfo.GetDistance.X < 0 then
          AnimateMainLayout(-(Width * 2))
        else
          AnimateMainLayout(0);
      end
      else
        AnimateMainLayout(-Width, False);
    end;
  end;
end;

end.

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.SwipeView;

type
  TMouseInfo = record
    Down: Boolean;
    DownPt: TPointF;
    Moved: Boolean;
    procedure MouseDown(const X, Y: Single);
    procedure MouseMove(const X, Y: Single);
    procedure MouseUp;
  end;

  // Interposer class that adds functionality to TImage
  TImage = class(FMX.Objects.TImage)
  private
    FMouse: TMouseInfo;
    FMouseParent: TControl;
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    property MouseParent: TControl read FMouseParent write FMouseParent;
  end;

  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    SwipeView1: TSwipeView;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SlideTimer: TTimer;
    procedure SlideTimerTimer(Sender: TObject);
  private
    FImageCount: Integer;
    FImageFileNames: TArray<string>;
    FImageIndex: Integer;
    function GetLayoutImage(const APosition: TLayoutPosition): TImage;
    procedure ImageClickHandler(Sender: TObject);
    procedure LoadImageFileNames;
    procedure SetupImages;
    procedure SwipeViewSwipeHandler(Sender: TObject; const ASwipeDirection: TMoveDirection);
    procedure UpdateLeftImage;
    procedure UpdateRightImage;
    property Image[const APosition: TLayoutPosition]: TImage read GetLayoutImage;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

type
  TOpenControl = class(TControl);

{ TMouseInfo }

procedure TMouseInfo.MouseDown(const X, Y: Single);
begin
  Down := True;
  Moved := False;
  DownPt := PointF(X, Y);
end;

procedure TMouseInfo.MouseMove(const X, Y: Single);
begin
  if Down and not Moved then
    Moved := (Abs(X - DownPt.X) > 10) or (Abs(Y - DownPt.Y) > 10);
end;

procedure TMouseInfo.MouseUp;
begin
  Down := False;
end;

{ TImage }

procedure TImage.Click;
begin
  // Call the click event only if the mouse has not "moved"
  if not FMouse.Moved then
    inherited;
end;

procedure TImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouse.MouseDown(X, Y);
  // Pass the mouse event on to the control interested in the event
  TOpenControl(FMouseParent).MouseDown(Button, Shift, X, Y);
end;

procedure TImage.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouse.MouseMove(X, Y);
  // Pass the mouse event on to the control interested in the event
  TOpenControl(FMouseParent).MouseMove(Shift, X, Y);
end;

procedure TImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouse.MouseUp;
  // Pass the mouse event on to the control interested in the event
  TOpenControl(FMouseParent).MouseUp(Button, Shift, X, Y);
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  Image1.Parent := SwipeView1.Layout[TLayoutPosition.Left];
  Image1.MouseParent := SwipeView1;
  Image1.Align := TAlignLayout.Client;
  Image1.OnClick := ImageClickHandler;
  Image2.Parent := SwipeView1.Layout[TLayoutPosition.Center];
  Image2.MouseParent := SwipeView1;
  Image2.Align := TAlignLayout.Client;
  Image2.OnClick := ImageClickHandler;
  Image3.Parent := SwipeView1.Layout[TLayoutPosition.Right];
  Image3.MouseParent := SwipeView1;
  Image3.Align := TAlignLayout.Client;
  Image3.OnClick := ImageClickHandler;
  // Works around an apparent bug in Delphi
  SwipeView1.Width := Width;
  SwipeView1.OnSwiped := SwipeViewSwipeHandler;
  LoadImageFileNames;
end;

function TForm1.GetLayoutImage(const APosition: TLayoutPosition): TImage;
var
  I: Integer;
  LLayout: TFmxObject;
begin
  Result := nil;
  LLayout := SwipeView1.Layout[APosition];
  for I := 0 to LLayout.ChildrenCount - 1 do
  begin
    if LLayout.Children.Items[I] is TImage then
    begin
      Result := TImage(LLayout.Children.Items[I]);
      Break;
    end;
  end;
end;

procedure TForm1.ImageClickHandler(Sender: TObject);
begin
  ShowMessage('Clicked');
end;

procedure TForm1.SetupImages;
begin
  FImageIndex := 0;
  Image[TLayoutPosition.Center].Bitmap.LoadFromFile(FImageFileNames[FImageIndex]);
  if FImageCount > 1 then
  begin
    UpdateRightImage;
    UpdateLeftImage;
  end
  else
  begin
    SwipeView1.CanSwipeLeft := False;
    SwipeView1.CanSwipeRight := False;
  end;
end;

procedure TForm1.LoadImageFileNames;
var
  LImagesPath: string;
begin
  {$IF Defined(MSWINDOWS)}
  // Assumes the output path is the default
  LImagesPath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\Images');
  {$ELSE}
  LImagesPath := TPath.Combine(TPath.GetDocumentsPath, 'Images');
  {$ENDIF}
  FImageFileNames := TDirectory.GetFiles(LImagesPath, '*.*', TSearchOption.soTopDirectoryOnly);
  FImageIndex := -1;
  FImageCount := Length(FImageFileNames);
  if FImageCount > 0 then
    SetupImages;
end;

procedure TForm1.SlideTimerTimer(Sender: TObject);
begin
  // SwipeView1.SwipeLeft(True);
end;

procedure TForm1.SwipeViewSwipeHandler(Sender: TObject; const ASwipeDirection: TMoveDirection);
begin
  if FImageCount > 1 then
  begin
    case ASwipeDirection of
      TMoveDirection.Left:
      begin
        // If the end of the list has not been reached, increment the index, otherwise set it to the start
        if FImageIndex < FImageCount - 1 then
          Inc(FImageIndex)
        else
          FImageIndex := 0;
        UpdateRightImage;
      end;
      TMoveDirection.Right:
      begin
        // If the start of the list has not been reached, decrement the index, otherwise set it to the end
        if FImageIndex > 0 then
          Dec(FImageIndex)
        else
          FImageIndex := FImageCount - 1;
        UpdateLeftImage;
      end;
    end;
  end;
end;

procedure TForm1.UpdateLeftImage;
begin
  // Update the left hand image to the one before the current index, or to the one at the end of the list
  if FImageIndex > 0 then
    Image[TLayoutPosition.Left].Bitmap.LoadFromFile(FImageFileNames[FImageIndex - 1])
  else
    Image[TLayoutPosition.Left].Bitmap.LoadFromFile(FImageFileNames[FImageCount - 1]);
end;

procedure TForm1.UpdateRightImage;
begin
  // Update the right hand image to the one after the current index, or to the one at the start of the list
  if FImageIndex < FImageCount - 1 then
    Image[TLayoutPosition.Right].Bitmap.LoadFromFile(FImageFileNames[FImageIndex + 1])
  else
    Image[TLayoutPosition.Right].Bitmap.LoadFromFile(FImageFileNames[0]);
end;

end.

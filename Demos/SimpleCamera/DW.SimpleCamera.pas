unit DW.SimpleCamera;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes,
  System.SysUtils,
  DW.ImageProcessor,
  DW.OSControl;

type
  TRecordingState = (Unknown, Recording, Stopped, Unauthorized, Error);

  TCameraPreview = class(TOSControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimpleCamera = class;

  TCustomPlatformSimpleCamera = class(TObject)
  private
    FImageProcessor: IImageProcessor;
    FPreview: TCameraPreview;
    FRecordingState: TRecordingState;
    FSimpleCamera: TSimpleCamera;
    FIncludeAudio: Boolean;
    procedure DoRecordingStateChanged;
    function GetIsVisible: Boolean;
    procedure PreviewResizedHandler(Sender: TObject);
    procedure SetIsVisible(const Value: Boolean);
    procedure SetRecordingState(const Value: TRecordingState);
  protected
    procedure DoImageAvailable(const AImage: TStream);
    function IsRecording: Boolean; virtual;
    procedure PreviewResized; virtual;
    procedure RecordingStateChanged; virtual;
    procedure StartRecording(const AFileName: string); virtual;
    procedure StopRecording; virtual;
    property ImageProcessor: IImageProcessor read FImageProcessor write FImageProcessor;
    property IncludeAudio: Boolean read FIncludeAudio write FIncludeAudio;
    property IsVisible: Boolean read GetIsVisible write SetIsVisible;
    property Preview: TCameraPreview read FPreview;
    property RecordingState: TRecordingState read FRecordingState write SetRecordingState;
  public
    constructor Create(const ASimpleCamera: TSimpleCamera); virtual;
    destructor Destroy; override;
  end;

  TImageAvailableEvent = procedure(Sender: TObject; const Image: TStream) of object;
  TRecordingStateChangedEvent = procedure(Sender: TObject; const State: TRecordingState) of object;

  TSimpleCamera = class(TObject)
  private
    FPlatformSimpleCamera: TCustomPlatformSimpleCamera;
    FOnImageAvailable: TImageAvailableEvent;
    FOnRecordingStateChanged: TRecordingStateChangedEvent;
    function GetIncludeAudio: Boolean;
    function GetPreview: TCameraPreview;
    function GetImageProcessor: IImageProcessor;
    function GetIsVisible: Boolean;
    procedure SetImageProcessor(const Value: IImageProcessor);
    procedure SetIncludeAudio(const Value: Boolean);
    procedure SetIsVisible(const Value: Boolean);
  protected
    procedure DoImageAvailable(const AImage: TStream);
    procedure DoRecordingStateChanged(const AState: TRecordingState);
  public
    constructor Create;
    destructor Destroy; override;
    function IsRecording: Boolean;
    procedure StartRecording(const AFileName: string = '');
    procedure StopRecording;
    property ImageProcessor: IImageProcessor read GetImageProcessor write SetImageProcessor;
    property IsVisible: Boolean read GetIsVisible write SetIsVisible;
    property Preview: TCameraPreview read GetPreview;
    property IncludeAudio: Boolean read GetIncludeAudio write SetIncludeAudio;
    property OnRecordingStateChanged: TRecordingStateChangedEvent read FOnRecordingStateChanged write FOnRecordingStateChanged;
    property OnImageAvailable: TImageAvailableEvent read FOnImageAvailable write FOnImageAvailable;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.SimpleCamera.iOS,
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.SimpleCamera.Android,
  {$ENDIF}
  FMX.Types;

{ TCameraPreview }

constructor TCameraPreview.Create(AOwner: TComponent);
begin
  inherited;
  Align := TAlignLayout.Client;
end;

{ TCustomPlatformSimpleCamera }

constructor TCustomPlatformSimpleCamera.Create(const ASimpleCamera: TSimpleCamera);
begin
  inherited Create;
  FIncludeAudio := True;
  FSimpleCamera := ASimpleCamera;
  FPreview := TCameraPreview.Create(nil);
  FPreview.OnResized := PreviewResizedHandler;
end;

destructor TCustomPlatformSimpleCamera.Destroy;
begin
  FPreview.Free;
  inherited;
end;

procedure TCustomPlatformSimpleCamera.RecordingStateChanged;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Queue(nil, DoRecordingStateChanged)
  else
    DoRecordingStateChanged;
end;

procedure TCustomPlatformSimpleCamera.DoImageAvailable(const AImage: TStream);
begin
  FSimpleCamera.DoImageAvailable(AImage);
end;

procedure TCustomPlatformSimpleCamera.DoRecordingStateChanged;
begin
  FSimpleCamera.DoRecordingStateChanged(FRecordingState);
end;

function TCustomPlatformSimpleCamera.GetIsVisible: Boolean;
begin
  Result := FPreview.Visible;
end;

function TCustomPlatformSimpleCamera.IsRecording: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformSimpleCamera.PreviewResized;
begin
  //
end;

procedure TCustomPlatformSimpleCamera.PreviewResizedHandler(Sender: TObject);
begin
  PreviewResized;
end;

procedure TCustomPlatformSimpleCamera.SetIsVisible(const Value: Boolean);
begin
  FPreview.Visible := Value;
end;

procedure TCustomPlatformSimpleCamera.SetRecordingState(const Value: TRecordingState);
begin
  if Value <> FRecordingState then
  begin
    FRecordingState := Value;
    RecordingStateChanged;
  end;
end;

procedure TCustomPlatformSimpleCamera.StartRecording(const AFileName: string);
begin
  //
end;

procedure TCustomPlatformSimpleCamera.StopRecording;
begin
  //
end;

{ TSimpleCamera }

constructor TSimpleCamera.Create;
begin
  inherited Create;
  FPlatformSimpleCamera := TPlatformSimpleCamera.Create(Self);
end;

destructor TSimpleCamera.Destroy;
begin
  FPlatformSimpleCamera.Free;
  inherited;
end;

function TSimpleCamera.GetImageProcessor: IImageProcessor;
begin
  Result := FPlatformSimpleCamera.ImageProcessor;
end;

function TSimpleCamera.GetIsVisible: Boolean;
begin
  Result := FPlatformSimpleCamera.IsVisible;
end;

function TSimpleCamera.GetPreview: TCameraPreview;
begin
  Result := FPlatformSimpleCamera.Preview;
end;

function TSimpleCamera.GetIncludeAudio: Boolean;
begin
  Result := FPlatformSimpleCamera.IncludeAudio;
end;

function TSimpleCamera.IsRecording: Boolean;
begin
  Result := FPlatformSimpleCamera.IsRecording;
end;

procedure TSimpleCamera.DoImageAvailable(const AImage: TStream);
begin
  if Assigned(FOnImageAvailable) then
    FOnImageAvailable(Self, AImage);
end;

procedure TSimpleCamera.DoRecordingStateChanged(const AState: TRecordingState);
begin
  if Assigned(FOnRecordingStateChanged) then
    FOnRecordingStateChanged(Self, AState);
end;

procedure TSimpleCamera.SetImageProcessor(const Value: IImageProcessor);
begin
  FPlatformSimpleCamera.ImageProcessor := Value;
end;

procedure TSimpleCamera.SetIsVisible(const Value: Boolean);
begin
  FPlatformSimpleCamera.IsVisible := Value;
end;

procedure TSimpleCamera.SetIncludeAudio(const Value: Boolean);
begin
  FPlatformSimpleCamera.IncludeAudio := Value;
end;

procedure TSimpleCamera.StartRecording(const AFileName: string = '');
begin
  FPlatformSimpleCamera.StartRecording(AFileName);
end;

procedure TSimpleCamera.StopRecording;
begin
  FPlatformSimpleCamera.StopRecording;
end;


end.

unit DW.SimpleCamera.Android;

interface

uses
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Util, Androidapi.JNIBridge, Androidapi.JNI.Os,
  Androidapi.JNI.Media,
  FMX.Controls,
  DW.Androidapi.JNI.Hardware.Camera2, DW.Androidapi.JNI.DWCameraHelpers, DW.Androidapi.JNI.View,
  DW.SimpleCamera;

type
  TPlatformSimpleCamera = class;

  TTextureAvailableEvent = procedure(Sender: TObject; const Texture: JSurfaceTexture; const Width, Height: Integer) of object;
  TTextureDestroyedEvent = procedure(Sender: TObject; const Texture: JSurfaceTexture) of object;

  TSurfaceTextureListener = class(TJavaLocal, JTextureView_SurfaceTextureListener)
  private
    FOnTextureAvailable: TTextureAvailableEvent;
    FOnTextureDestroyed: TTextureDestroyedEvent;
  public
    { JTextureView_SurfaceTextureListener }
    procedure onSurfaceTextureAvailable(texture: JSurfaceTexture; width: Integer; height: Integer); cdecl;
    function onSurfaceTextureDestroyed(texture: JSurfaceTexture): Boolean; cdecl;
    procedure onSurfaceTextureUpdated(texture: JSurfaceTexture); cdecl;
    procedure onSurfaceTextureSizeChanged(texture: JSurfaceTexture; width: Integer; height: Integer); cdecl;
  public
    property OnTextureAvailable: TTextureAvailableEvent read FOnTextureAvailable write FOnTextureAvailable;
    property OnTextureDestroyed: TTextureDestroyedEvent read FOnTextureDestroyed write FOnTextureDestroyed;
  end;

  TCaptureSessionState = (Stopped, Starting, Creating, Started);

  TCaptureSession = class(TObject)
  private
    {$IF Defined(USEGL)}
    FCameraView: JDWGLCameraView;
    FCameraViewDelegate: JDWGLCameraView_ViewDelegate;
    {$ELSE}
    FCameraView: JDWCameraView;
    {$ENDIF}
    FCaptureRequestHelper: JDWCaptureRequestBuilderHelper;
    FCaptureRequestBuilder: JCaptureRequest_Builder;
    FCaptureSessionStateCallback: JDWCameraCaptureSessionStateCallback;
    FCaptureSessionStateCallbackDelegate: JDWCameraCaptureSessionStateCallbackDelegate;
    FIsRecording: Boolean;
    FPlatformSimpleCamera: TPlatformSimpleCamera;
    FMediaRecorder: JMediaRecorder;
    FPreviewSurface: JSurface;
    FSession: JCameraCaptureSession;
    FSessionState: TCaptureSessionState;
    FSurfaceTexture: JSurfaceTexture;
    FSurfaceTextureListener: TSurfaceTextureListener;
    FWantRecord: Boolean;
    function GetCameraDevice: JCameraDevice;
    function GetHandler: JHandler;
    function GetPreview: TCameraPreview;
    function GetViewSize: Jutil_Size;
    procedure SessionStarted;
    procedure SetupMediaRecorder;
    procedure TextureAvailableHandler(Sender: TObject; const ATexture: JSurfaceTexture; const AWidth, AHeight: Integer);
    procedure TextureDestroyedHandler(Sender: TObject; const ATexture: JSurfaceTexture);
    procedure UpdatePreview;
    procedure UpdateSurfaceTexture;
  protected
    procedure SessionConfigured(const ASession: JCameraCaptureSession; const AFailed: Boolean);
    procedure DoSessionStarted;
    procedure DoStartSession;
    procedure DoStopSession;
    procedure StartSession(const AWantRecord: Boolean);
    procedure StopSession;
    property CameraDevice: JCameraDevice read GetCameraDevice;
    property CaptureSessionStateCallback: JDWCameraCaptureSessionStateCallback read FCaptureSessionStateCallback;
    property Handler: JHandler read GetHandler;
    property IsRecording: Boolean read FIsRecording;
    property Preview: TCameraPreview read GetPreview;
    property Session: JCameraCaptureSession read FSession;
    property SessionState: TCaptureSessionState read FSessionState;
    property SimpleCamera: TPlatformSimpleCamera read FPlatformSimpleCamera;
    property ViewSize: Jutil_Size read GetViewSize;
  public
    constructor Create(const APlatformSimpleCamera: TPlatformSimpleCamera); virtual;
    destructor Destroy; override;
  end;

  TPlatformSimpleCamera = class(TCustomPlatformSimpleCamera)
  private
    FCameraDevice: JCameraDevice;
    FCameraID: JString;
    FCameraManager: JCameraManager;
    FCameraOrientation: Integer;
    FCanIncludeAudio: Boolean;
    FCaptureSession: TCaptureSession;
    FDeviceStateCallbackDelegate: JDWCameraDeviceStateCallbackDelegate;
    FDeviceStateCallback: JDWCameraDeviceStateCallback;
    FFileName: string;
    FHandler: JHandler;
    FPreviewView: JView;
    FThread: JHandlerThread;
    FViewSize: Jutil_Size;
    function CameraPositionChanged: Boolean;
    procedure OpenCamera;
    procedure StartThread;
    procedure StopThread;
    procedure UpdateViewSize(const ASizes: TJavaObjectArray<Jutil_Size>);
  protected
    procedure CameraDisconnected(camera: JCameraDevice);
    procedure CameraError(camera: JCameraDevice; error: Integer);
    procedure CameraOpened(camera: JCameraDevice);
    function IsRecording: Boolean; override;
    procedure PreviewResized; override;
    procedure StartRecording(const AFileName: string); override;
    procedure StopRecording; override;
    property CameraDevice: JCameraDevice read FCameraDevice;
    property CanIncludeAudio: Boolean read FCanIncludeAudio;
    property FileName: string read FFileName;
    property Handler: JHandler read FHandler;
    property PreviewView: JView read FPreviewView;
    property ViewSize: Jutil_Size read FViewSize;
  public
    constructor Create(const ASimpleCamera: TSimpleCamera); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  System.SysUtils, System.Classes, System.Permissions, System.Types,
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI,
  FMX.Types, FMX.Forms, FMX.Presentation.Android, FMX.Presentation.Factory,
  DW.Permissions.Helpers, DW.Consts.Android;

type
  TPlatformCameraPreview = class(TAndroidNativeView)
  private
    {$IF Defined(USEGL)}
    FView: JDWGLCameraView;
    {$ELSE}
    FView: JDWCameraView;
    {$ENDIF}
  protected
    function CreateView: JView; override;
  public
    constructor Create; override;
    {$IF Defined(USEGL)}
    property View: JDWGLCameraView read FView;
    {$ELSE}
    property View: JDWCameraView read FView;
    {$ENDIF}
  end;

  TDWCameraDeviceStateCallbackDelegate = class(TJavaLocal, JDWCameraDeviceStateCallbackDelegate)
  private
    FPlatformSimpleCamera: TPlatformSimpleCamera;
  public
    { JDWCameraDeviceStateCallbackDelegate }
    procedure onDisconnected(camera: JCameraDevice); cdecl;
    procedure onError(camera: JCameraDevice; error: Integer); cdecl;
    procedure onOpened(camera: JCameraDevice); cdecl;
  public
    constructor Create(const APlatformSimpleCamera: TPlatformSimpleCamera);
  end;

  TDWCameraCaptureSessionStateCallbackDelegate = class(TJavaLocal, JDWCameraCaptureSessionStateCallbackDelegate)
  private
    FCaptureSession: TCaptureSession;
  public
    { JDWCameraCaptureSessionStateCallbackDelegate }
    procedure onConfigureFailed(session: JCameraCaptureSession); cdecl;
    procedure onConfigured(session: JCameraCaptureSession); cdecl;
  public
    constructor Create(const ACaptureSession: TCaptureSession);
  end;

function GetSizeArea(const ASize: Jutil_Size): Integer;
begin
  Result := ASize.getHeight * ASize.getWidth;
end;

{ TPlatformCameraPreview }

constructor TPlatformCameraPreview.Create;
begin
  inherited;
  //
end;

function TPlatformCameraPreview.CreateView: JView;
begin
  {$IF Defined(USEGL)}
  FView := TJDWGLCameraView.JavaClass.init(TAndroidHelper.Activity);
  {$ELSE}
  FView := TJDWCameraView.JavaClass.init(TAndroidHelper.Activity);
  {$ENDIF}
  Result := FView;
end;

{ TDWCameraCaptureSessionStateCallbackDelegate }

constructor TDWCameraCaptureSessionStateCallbackDelegate.Create(const ACaptureSession: TCaptureSession);
begin
  inherited Create;
  FCaptureSession := ACaptureSession;
end;

procedure TDWCameraCaptureSessionStateCallbackDelegate.onConfigured(session: JCameraCaptureSession);
begin
  FCaptureSession.SessionConfigured(session, False);
end;

procedure TDWCameraCaptureSessionStateCallbackDelegate.onConfigureFailed(session: JCameraCaptureSession);
begin
  FCaptureSession.SessionConfigured(session, True);
end;

{ TSurfaceTextureListener }

procedure TSurfaceTextureListener.onSurfaceTextureAvailable(texture: JSurfaceTexture; width, height: Integer);
begin
  if Assigned(FOnTextureAvailable) then
    FOnTextureAvailable(Self, texture, width, height);
end;

function TSurfaceTextureListener.onSurfaceTextureDestroyed(texture: JSurfaceTexture): Boolean;
begin
  // FCaptureSession.SurfaceTextureDestroyed(texture);
  Result := True;
end;

procedure TSurfaceTextureListener.onSurfaceTextureSizeChanged(texture: JSurfaceTexture; width: Integer; height: Integer);
begin
  //
end;

procedure TSurfaceTextureListener.onSurfaceTextureUpdated(texture: JSurfaceTexture);
begin
  //
end;

{ TCaptureSession }

constructor TCaptureSession.Create(const APlatformSimpleCamera: TPlatformSimpleCamera);
begin
  inherited Create;
  FPlatformSimpleCamera := APlatformSimpleCamera;
  FSessionState := TCaptureSessionState.Stopped;
  FSurfaceTextureListener := TSurfaceTextureListener.Create;
  FSurfaceTextureListener.OnTextureAvailable := TextureAvailableHandler;
  FSurfaceTextureListener.OnTextureDestroyed := TextureDestroyedHandler;
  FCaptureRequestHelper := TJDWCaptureRequestBuilderHelper.JavaClass.init;
  FCaptureSessionStateCallbackDelegate := TDWCameraCaptureSessionStateCallbackDelegate.Create(Self);
  FCaptureSessionStateCallback := TJDWCameraCaptureSessionStateCallback.JavaClass.init(FCaptureSessionStateCallbackDelegate);
  FCameraView := TPlatformCameraPreview(Preview.Presentation).View;
  {$IF Defined(USEGL)}
  FCameraViewDelegate := TDWGLCameraViewDelegate.Create(Self);
  FCameraView.setCaptureFPS(1); // Needs to be controlled from FPlatformCamera
  FCameraView.setViewDelegate(FCameraViewDelegate);
  {$ELSE}
  FCameraView.setSurfaceTextureListener(FSurfaceTextureListener);
  {$ENDIF}
end;

destructor TCaptureSession.Destroy;
begin
  FSurfaceTextureListener.Free;
  inherited;
end;

function TCaptureSession.GetCameraDevice: JCameraDevice;
begin
  Result := FPlatformSimpleCamera.CameraDevice;
end;

function TCaptureSession.GetHandler: JHandler;
begin
  Result := FPlatformSimpleCamera.Handler;
end;

procedure TCaptureSession.SessionStarted;
begin
  FSessionState := TCaptureSessionState.Started;
  DoSessionStarted;
end;

procedure TCaptureSession.SessionConfigured(const ASession: JCameraCaptureSession; const AFailed: Boolean);
begin
  TOSLog.d('TCustomCaptureSession.SessionConfigured - AFailed: %s', [BoolToStr(AFailed, True)]);
  if not AFailed then
  begin
    FSession := ASession;
    SessionStarted;
  end
  else
    FSessionState := TCaptureSessionState.Stopped;
end;

procedure TCaptureSession.StartSession(const AWantRecord: Boolean);
begin
  FSessionState := TCaptureSessionState.Starting;
  FWantRecord := AWantRecord;
  DoStartSession;
end;

procedure TCaptureSession.StopSession;
begin
  DoStopSession;
  if FSession <> nil then
  begin
    FSession.close;
    FSession := nil;
  end;
  FSessionState := TCaptureSessionState.Stopped;
  FIsRecording := False;
end;

function TCaptureSession.GetPreview: TCameraPreview;
begin
  Result := SimpleCamera.Preview;
end;

procedure TCaptureSession.DoSessionStarted;
var
  LTemplateType: Integer;
begin
  TOSLog.d('TRecordingSession.DoSessionStarted');
  if FWantRecord then
    LTemplateType := TJCameraDevice.JavaClass.TEMPLATE_RECORD
  else
    LTemplateType := TJCameraDevice.JavaClass.TEMPLATE_PREVIEW;
  FCaptureRequestBuilder := CameraDevice.createCaptureRequest(LTemplateType);
  FCaptureRequestBuilder.addTarget(FPreviewSurface);
  if FWantRecord then
   FCaptureRequestBuilder.addTarget(FMediaRecorder.getSurface);
  TOSLog.d('> Session.setRepeatingRequest');
  Session.setRepeatingRequest(FCaptureRequestBuilder.build, nil, Handler); // FCaptureSessionCaptureCallback, Handler);
  TOSLog.d('> FMediaRecorder.start');
  if FWantRecord then
  begin
    FMediaRecorder.start;
    FIsRecording := True;
  end;
end;

procedure TCaptureSession.SetupMediaRecorder;
begin
  if FMediaRecorder = nil then
    FMediaRecorder := TJMediaRecorder.JavaClass.init;
  if SimpleCamera.CanIncludeAudio then
    FMediaRecorder.setAudioSource(TJMediaRecorder_AudioSource.JavaClass.MIC)
  else
    FMediaRecorder.setAudioSource(TJMediaRecorder_AudioSource.JavaClass.DEFAULT);
  FMediaRecorder.setVideoSource(TJMediaRecorder_VideoSource.JavaClass.SURFACE);
  // FMediaRecorder.setOutputFormat(TJMediaRecorder_OutputFormat.JavaClass.MPEG_4);
  // FMediaRecorder.setAudioEncoder(TJMediaRecorder_AudioEncoder.JavaClass.AAC);
  // FMediaRecorder.setVideoEncoder(TJMediaRecorder_VideoEncoder.JavaClass.H264);
  // setProfile replaces the lines above
  FMediaRecorder.setProfile(TJCamcorderProfile.JavaClass.get(TJCamcorderProfile.JavaClass.QUALITY_HIGH));
  FMediaRecorder.setVideoSize(FPlatformSimpleCamera.ViewSize.getWidth, FPlatformSimpleCamera.ViewSize.getHeight());
  FMediaRecorder.setVideoFrameRate(30);
  FMediaRecorder.setOutputFile(StringToJString(FPlatformSimpleCamera.FileName));
  FMediaRecorder.prepare;
end;

procedure TCaptureSession.DoStartSession;
var
  LOutputs: JArrayList;
begin
  if FSurfaceTexture <> nil then
  begin
    FSessionState := TCaptureSessionState.Creating; // protected
    UpdateSurfaceTexture;
    if FWantRecord then
      SetupMediaRecorder;
    LOutputs := TJArrayList.JavaClass.init(Ord(FWantRecord) + 1);
    LOutputs.add(FPreviewSurface);
    if FWantRecord then
      LOutputs.add(FMediaRecorder.getSurface);
    CameraDevice.createCaptureSession(TJList.Wrap(LOutputs), CaptureSessionStateCallback, Handler);
  end;
end;

procedure TCaptureSession.DoStopSession;
begin
  if FIsRecording and (FMediaRecorder <> nil) then
    FMediaRecorder.stop;
end;

function TCaptureSession.GetViewSize: Jutil_Size;
begin
  Result := SimpleCamera.ViewSize;
end;

procedure TCaptureSession.TextureAvailableHandler(Sender: TObject; const ATexture: JSurfaceTexture; const AWidth, AHeight: Integer);
begin
  FSurfaceTexture := ATexture;
  if SessionState <> TCaptureSessionState.Stopped then
    UpdateSurfaceTexture;
end;

procedure TCaptureSession.TextureDestroyedHandler(Sender: TObject; const ATexture: JSurfaceTexture);
begin
  FSurfaceTexture := nil;
end;

procedure TCaptureSession.UpdateSurfaceTexture;
begin
  FPreviewSurface := nil;
  FSurfaceTexture.setDefaultBufferSize(ViewSize.getWidth, ViewSize.getHeight);
  FPreviewSurface := TJSurface.JavaClass.init(FSurfaceTexture);
  TThread.Synchronize(nil, UpdatePreview); // SurfaceTextureAvailable is coming from the view's render thread
  if FSessionState = TCaptureSessionState.Starting then
    DoStartSession;
end;

procedure TCaptureSession.UpdatePreview;
var
  {$IF not Defined(USEGL)}
  LScreenScale: Single;
  LPreviewSize: TSize;
  {$ENDIF}
  LSize: TSizeF;
  LViewSize: TSize;
  LIsPortrait: Boolean;
  LPreview: TCameraPreview;
begin
  LIsPortrait := Screen.Height > Screen.Width;
  LPreview := SimpleCamera.Preview;
  if LIsPortrait then
    LViewSize := TSize.Create(ViewSize.getWidth, ViewSize.getHeight)
  else
    LViewSize := TSize.Create(ViewSize.getHeight, ViewSize.getWidth);
  if LIsPortrait then
    LSize := TSizeF.Create(LPreview.ParentControl.Height * (LViewSize.cy / LViewSize.cx), LPreview.ParentControl.Height)
  else
    LSize := TSizeF.Create(LPreview.ParentControl.Width, LPreview.ParentControl.Width * (LViewSize.cx / LViewSize.cy));
  LPreview.Size.Size := LSize;
  // FCameraView.setCameraRotation(GetSurfaceRotation(TAndroidHelper.Activity.getWindowManager.getDefaultDisplay.getRotation));
  {$IF not Defined(USEGL)}
  LScreenScale := TPlatformCameraPreview(LPreview.Presentation).ScreenScale;
  LPreviewSize := TSize.Create(Round(LPreview.Size.Size.cx * LScreenScale), Round(LPreview.Size.Size.cy * LScreenScale));
  FCameraView.setPreviewSize(TJutil_Size.JavaClass.init(LPreviewSize.cx, LPreviewSize.cy));
  {$ENDIF}
end;

{ TDWCameraDeviceStateCallbackDelegate }

constructor TDWCameraDeviceStateCallbackDelegate.Create(const APlatformSimpleCamera: TPlatformSimpleCamera);
begin
  inherited Create;
  FPlatformSimpleCamera := APlatformSimpleCamera;
end;

procedure TDWCameraDeviceStateCallbackDelegate.onDisconnected(camera: JCameraDevice);
begin
  FPlatformSimpleCamera.CameraDisconnected(camera);
end;

procedure TDWCameraDeviceStateCallbackDelegate.onError(camera: JCameraDevice; error: Integer);
begin
  FPlatformSimpleCamera.CameraError(camera, error);
end;

procedure TDWCameraDeviceStateCallbackDelegate.onOpened(camera: JCameraDevice);
begin
  FPlatformSimpleCamera.CameraOpened(camera);
end;

{ TPlatformSimpleCamera }

constructor TPlatformSimpleCamera.Create(const ASimpleCamera: TSimpleCamera);
begin
  inherited;
  FCameraManager := TJCameraManager.Wrap(TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.CAMERA_SERVICE));
  FDeviceStateCallbackDelegate := TDWCameraDeviceStateCallbackDelegate.Create(Self);
  FDeviceStateCallback := TJDWCameraDeviceStateCallback.JavaClass.init(FDeviceStateCallbackDelegate);
  FHandler := TJHandler.JavaClass.init(TJLooper.JavaClass.getMainLooper);
  FPreviewView := TPlatformCameraPreview(Preview.Presentation).View;
  FCaptureSession := TCaptureSession.Create(Self);
  StartThread;
end;

destructor TPlatformSimpleCamera.Destroy;
begin
  StopThread;
  FCaptureSession.Free;
  inherited;
end;

procedure TPlatformSimpleCamera.StartThread;
begin
  FThread := TJHandlerThread.JavaClass.init(StringToJString('SimpleCamera'));
  FThread.start;
  FHandler := TJHandler.JavaClass.init(FThread.getLooper);
end;

procedure TPlatformSimpleCamera.StopThread;
begin
  FThread.quitSafely;
  FThread.join;
  FThread := nil;
  FHandler := nil;
end;

function TPlatformSimpleCamera.IsRecording: Boolean;
begin
  Result := FCaptureSession.IsRecording;
end;

procedure TPlatformSimpleCamera.PreviewResized;
begin
  //
end;

procedure TPlatformSimpleCamera.CameraDisconnected(camera: JCameraDevice);
begin
  FCameraDevice := nil;
  // FIsCapturing := False;
  // InternalSetActive(False);
end;

procedure TPlatformSimpleCamera.CameraError(camera: JCameraDevice; error: Integer);
begin
  TOSLog.d('TPlatformCamera.CameraError - Error: %d', [error]);
end;

procedure TPlatformSimpleCamera.CameraOpened(camera: JCameraDevice);
begin
  TOSLog.d('TPlatformCamera.CameraOpened');
  FCameraDevice := camera;
  // InternalSetActive(True);
  // StartCapture;
  FCaptureSession.StartSession(True); // needs to start the recording session, too
end;

procedure TPlatformSimpleCamera.UpdateViewSize(const ASizes: TJavaObjectArray<Jutil_Size>);
var
  I: Integer;
  LSize: Jutil_Size;
begin
  // Find the maximum view size
  FViewSize := nil;
  for I := 0 to ASizes.Length - 1 do
  begin
    LSize := ASizes.Items[I];
    if ((FViewSize = nil) or (GetSizeArea(LSize) > GetSizeArea(FViewSize))) then
      FViewSize := LSize;
  end;
  if (FViewSize <> nil) and ((FCameraOrientation mod 180) <> 0) then
    FViewSize := TJutil_Size.JavaClass.init(FViewSize.getWidth, FViewSize.getHeight);
end;

function TPlatformSimpleCamera.CameraPositionChanged: Boolean;
var
  LCharacteristics: JCameraCharacteristics;
  LCameraIDList: TJavaObjectArray<JString>;
  LItem: JString;
  LHelper: JDWCameraCharacteristicsHelper;
  I, LLensFacing: Integer;
  LMap: JStreamConfigurationMap;
  LAvailableViewSizes: TJavaObjectArray<Jutil_Size>;
begin
  Result := False;
  LCameraIDList := FCameraManager.getCameraIdList;
  FCameraID := nil;
  LHelper := TJDWCameraCharacteristicsHelper.JavaClass.init;
  for I := 0 to LCameraIDList.Length - 1 do
  begin
    TOSLog.d('> Found %d cameras', [LCameraIDList.Length ]);
    LItem := LCameraIDList.Items[I];
	  LCharacteristics := FCameraManager.getCameraCharacteristics(LItem);
    LHelper.setCameraCharacteristics(LCharacteristics);
    LLensFacing := LHelper.getLensFacing;
    if LLensFacing = TJCameraMetadata.JavaClass.LENS_FACING_BACK then
      FCameraID := LItem;
    if FCameraID <> nil then
    begin
      FCameraOrientation := LHelper.getSensorOrientation;
      TOSLog.d('> Camera orientation: %d', [FCameraOrientation]);
      LMap := LHelper.getMap;
      LAvailableViewSizes := LMap.getOutputSizes(TJImageFormat.JavaClass.RAW_SENSOR);
      if LAvailableViewSizes = nil then
        LAvailableViewSizes := LMap.getOutputSizes(TJImageFormat.JavaClass.JPEG);
      if LAvailableViewSizes <> nil then
      try
        UpdateViewSize(LAvailableViewSizes);
        Result := True;
      finally
        LAvailableViewSizes.Free;
      end;
      Break;
    end;
  end;
end;

procedure TPlatformSimpleCamera.OpenCamera;
begin
  if CameraPositionChanged then
    FCameraManager.openCamera(FCameraID, FDeviceStateCallback, Handler)
  else
    TOSLog.w('> No camera or view sizes available');
end;

procedure TPlatformSimpleCamera.StartRecording(const AFileName: string);
begin
  PermissionsService.RequestPermissions([cPermissionCamera, cPermissionRecordAudio],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults[0] = TPermissionStatus.Granted then
      begin
        FFileName := AFileName;
        FCanIncludeAudio := AGrantResults[1] = TPermissionStatus.Granted;
        OpenCamera;
      end;
    end
  );
end;

procedure TPlatformSimpleCamera.StopRecording;
begin
  FCaptureSession.StopSession;
  if FCameraDevice <> nil then
    FCameraDevice.close;
  FCameraDevice := nil;
end;

initialization
  TPresentationProxyFactory.Current.Register(TCameraPreview, TControlType.Platform, TAndroidPresentationProxy<TPlatformCameraPreview>);

finalization
  TPresentationProxyFactory.Current.Unregister(TCameraPreview, TControlType.Platform, TAndroidPresentationProxy<TPlatformCameraPreview>);

end.

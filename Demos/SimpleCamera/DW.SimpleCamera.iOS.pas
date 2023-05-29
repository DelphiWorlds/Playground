unit DW.SimpleCamera.iOS;

interface

uses
  iOSapi.AVFoundation, iOSapi.CoreMedia, iOSapi.UIKit, iOSapi.CoreImage, iOSapi.Foundation,
  Macapi.ObjectiveC,
  DW.SimpleCamera;

type
  AVCaptureFileOutputRecordingDelegate = interface(IObjectiveC)
    ['{87E3723C-0615-46F4-B03F-0C83B6BBE4C5}']
    [MethodName('captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error:')]
    procedure captureOutputDidFinishRecordingToOutputFileAtURL(output: AVCaptureFileOutput; didFinishRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray; error: NSError); cdecl;
    [MethodName('captureOutput:didPauseRecordingToOutputFileAtURL:fromConnections:')]
    procedure captureOutputDidPauseRecordingToOutputFileAtURL(output: AVCaptureFileOutput; didPauseRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray); cdecl;
    [MethodName('captureOutput:didResumeRecordingToOutputFileAtURL:fromConnections:')]
    procedure captureOutputDidResumeRecordingToOutputFileAtURL(output: AVCaptureFileOutput; didResumeRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray); cdecl;
    [MethodName('captureOutput:didStartRecordingToOutputFileAtURL:fromConnections:')]
    procedure captureOutputDidStartRecordingToOutputFileAtURL(output: AVCaptureFileOutput; didStartRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray); cdecl;
    [MethodName('captureOutput:willFinishRecordingToOutputFileAtURL:fromConnections:error:')]
    procedure captureOutputWillFinishRecordingToOutputFileAtURL(output: AVCaptureFileOutput; willFinishRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray; error: NSError); cdecl;
  end;

  TAVCaptureFileOutputRecordingDelegate = class(TOCLocal, AVCaptureFileOutputRecordingDelegate)
  public
    { AVCaptureFileOutputRecordingDelegate }
    [MethodName('captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error:')]
    procedure captureOutputDidFinishRecordingToOutputFileAtURL(output: AVCaptureFileOutput; didFinishRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray; error: NSError); cdecl;
    [MethodName('captureOutput:didPauseRecordingToOutputFileAtURL:fromConnections:')]
    procedure captureOutputDidPauseRecordingToOutputFileAtURL(output: AVCaptureFileOutput; didPauseRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray); cdecl;
    [MethodName('captureOutput:didResumeRecordingToOutputFileAtURL:fromConnections:')]
    procedure captureOutputDidResumeRecordingToOutputFileAtURL(output: AVCaptureFileOutput; didResumeRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray); cdecl;
    [MethodName('captureOutput:didStartRecordingToOutputFileAtURL:fromConnections:')]
    procedure captureOutputDidStartRecordingToOutputFileAtURL(output: AVCaptureFileOutput; didStartRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray); cdecl;
    [MethodName('captureOutput:willFinishRecordingToOutputFileAtURL:fromConnections:error:')]
    procedure captureOutputWillFinishRecordingToOutputFileAtURL(output: AVCaptureFileOutput; willFinishRecordingToOutputFileAtURL: NSURL;
      fromConnections: NSArray; error: NSError); cdecl;
  end;

  AVCaptureVideoDataOutputSampleBufferDelegate = interface(IObjectiveC)
    ['{DE7BD31F-C91D-4EC7-8D0F-9BA0483E2398}']
    [MethodName('captureOutput:didDropSampleBuffer:fromConnection:')]
    procedure captureOutputDidDropSampleBuffer(output: AVCaptureOutput; didDropSampleBuffer: CMSampleBufferRef;
      fromConnection: AVCaptureConnection); cdecl;
    [MethodName('captureOutput:didOutputSampleBuffer:fromConnection:')]
    procedure captureOutputDidOutputSampleBuffer(output: AVCaptureOutput; didOutputSampleBuffer: CMSampleBufferRef;
      fromConnection: AVCaptureConnection); cdecl;
  end;

  TPlatformSimpleCamera = class;

  TAVCaptureVideoDataOutputSampleBufferDelegate = class(TOCLocal, AVCaptureVideoDataOutputSampleBufferDelegate)
  private
    FPlatformSimpleCamera: TPlatformSimpleCamera;
  public
    { AVCaptureVideoDataOutputSampleBufferDelegate }
    [MethodName('captureOutput:didDropSampleBuffer:fromConnection:')]
    procedure captureOutputDidDropSampleBuffer(output: AVCaptureOutput; didDropSampleBuffer: CMSampleBufferRef;
      fromConnection: AVCaptureConnection); cdecl;
    [MethodName('captureOutput:didOutputSampleBuffer:fromConnection:')]
    procedure captureOutputDidOutputSampleBuffer(output: AVCaptureOutput; didOutputSampleBuffer: CMSampleBufferRef;
      fromConnection: AVCaptureConnection); cdecl;
  public
    constructor Create(const APlatformSimpleCamera: TPlatformSimpleCamera);
  end;

  TPlatformSimpleCamera = class(TCustomPlatformSimpleCamera)
  private
    FCanIncludeAudio: Boolean;
    FFileName: string;
    FFileOutput: AVCaptureMovieFileOutput;
    FFileOutputDelegate: TAVCaptureFileOutputRecordingDelegate;
    FPreviewOutput: AVCaptureVideoDataOutput;
    FPreviewOutputDelegate: TAVCaptureVideoDataOutputSampleBufferDelegate;
    FPreviewView: UIView;
    FSession: AVCaptureSession;
    procedure DoStartRecording;
    procedure RequestAccessForAVMediaTypeAudioHandler(granted: Boolean);
    procedure RequestAccessForAVMediaTypeVideoHandler(granted: Boolean);
  protected
    function IsRecording: Boolean; override;
    procedure PreviewResized; override;
    procedure ProcessImage(const AImage: Pointer);
    procedure StartRecording(const AFileName: string); override;
    procedure StopRecording; override;
    property PreviewView: UIView read FPreviewView;
  public
    constructor Create(const ASimpleCamera: TSimpleCamera); override;
    destructor Destroy; override;
  end;

implementation

uses
  // System.IOUtils,
  DW.OSLog,
  System.Classes, System.SysUtils, System.TypInfo,
  Macapi.Helpers, Macapi.Dispatch,
  iOSapi.CoreVideo, iOSapi.CoreGraphics,
  FMX.Controls, FMX.Presentation.iOS, FMX.Presentation.Factory;

const
  cPI = 3.141592654;

type
  ICameraPreviewUIView = interface(UIView)
    ['{4307E990-3ABC-41AC-94A8-9E574799218F}']
  end;

  TPlatformCameraPreview = class(TiOSNativeView)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  end;

function AVLayerVideoGravityResizeAspectFill: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVLayerVideoGravityResizeAspectFill');
end;

{ TPlatformCameraPreview }

function TPlatformCameraPreview.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(ICameraPreviewUIView);
end;

{ TAVCaptureFileOutputRecordingDelegate }

procedure TAVCaptureFileOutputRecordingDelegate.captureOutputDidFinishRecordingToOutputFileAtURL(output: AVCaptureFileOutput;
  didFinishRecordingToOutputFileAtURL: NSURL; fromConnections: NSArray; error: NSError);
begin

end;

procedure TAVCaptureFileOutputRecordingDelegate.captureOutputDidPauseRecordingToOutputFileAtURL(output: AVCaptureFileOutput;
  didPauseRecordingToOutputFileAtURL: NSURL; fromConnections: NSArray);
begin

end;

procedure TAVCaptureFileOutputRecordingDelegate.captureOutputDidResumeRecordingToOutputFileAtURL(output: AVCaptureFileOutput;
  didResumeRecordingToOutputFileAtURL: NSURL; fromConnections: NSArray);
begin

end;

procedure TAVCaptureFileOutputRecordingDelegate.captureOutputDidStartRecordingToOutputFileAtURL(output: AVCaptureFileOutput;
  didStartRecordingToOutputFileAtURL: NSURL; fromConnections: NSArray);
begin

end;

procedure TAVCaptureFileOutputRecordingDelegate.captureOutputWillFinishRecordingToOutputFileAtURL(output: AVCaptureFileOutput;
  willFinishRecordingToOutputFileAtURL: NSURL; fromConnections: NSArray; error: NSError);
begin

end;

{ TAVCaptureVideoDataOutputSampleBufferDelegate }

constructor TAVCaptureVideoDataOutputSampleBufferDelegate.Create(const APlatformSimpleCamera: TPlatformSimpleCamera);
begin
  inherited Create;
  FPlatformSimpleCamera := APlatformSimpleCamera;
end;

procedure TAVCaptureVideoDataOutputSampleBufferDelegate.captureOutputDidDropSampleBuffer(output: AVCaptureOutput;
  didDropSampleBuffer: CMSampleBufferRef; fromConnection: AVCaptureConnection);
begin
  //
end;

procedure TAVCaptureVideoDataOutputSampleBufferDelegate.captureOutputDidOutputSampleBuffer(output: AVCaptureOutput;
  didOutputSampleBuffer: CMSampleBufferRef; fromConnection: AVCaptureConnection);
var
  LPixelBuffer: CVPixelBufferRef;
  LCIImage: CIImage;
  LImageRect: CGRect;
  LCGImage: CGImageRef;
  LContext: CIContext;
  LTransform: CGAffineTransform;
  // LUIImage: UIImage;
begin
  LPixelBuffer := CMSampleBufferGetImageBuffer(didOutputSampleBuffer);
  LImageRect := CGRectMake(0, 0, CVPixelBufferGetWidth(LPixelBuffer), CVPixelBufferGetHeight(LPixelBuffer));
  LCIImage := TCIImage.Wrap(TCIImage.OCClass.imageWithCVPixelBuffer(LPixelBuffer));
  LTransform := CGAffineTransformIdentity;
  case fromConnection.videoOrientation of
    AVCaptureVideoOrientationLandscapeRight:
      LTransform := CGAffineTransformMakeRotation(cPI * -0.5);
    AVCaptureVideoOrientationLandscapeLeft:
      LTransform := CGAffineTransformMakeRotation(cPI * 0.5);
    AVCaptureVideoOrientationPortraitUpsideDown:
      LTransform := CGAffineTransformMakeRotation(cPI);
  end;
  LCIImage := LCIImage.imageByApplyingTransform(LTransform);
  LImageRect := CGRectApplyAffineTransform(LImageRect, LTransform);
  LContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
  LCGImage := LContext.createCGImage(LCIImage, LImageRect);
  try
    FPlatformSimpleCamera.ProcessImage(LCGImage);
  finally
    CGImageRelease(LCGImage);
  end;
end;

{ TPlatformSimpleCamera }

constructor TPlatformSimpleCamera.Create(const ASimpleCamera: TSimpleCamera);
begin
  inherited;
  FPreviewView := TPlatformCameraPreview(Preview.Presentation).View;
  FPreviewView.setContentMode(UIViewContentModeScaleAspectFit);
end;

destructor TPlatformSimpleCamera.Destroy;
begin
  FFileOutputDelegate.Free;
  FPreviewOutputDelegate.Free;
  inherited;
end;

function TPlatformSimpleCamera.IsRecording: Boolean;
begin
  Result := (FSession <> nil) and FSession.isRunning and (FFileOutput <> nil) and FFileOutput.isRecording;
end;


procedure TPlatformSimpleCamera.PreviewResized;
begin
  //
end;

procedure TPlatformSimpleCamera.ProcessImage(const AImage: Pointer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      // Allow a processor to do something with the image
      if ImageProcessor <> nil then
        ImageProcessor.ProcessImage(AImage);
      // Synch the image with the view
      PreviewView.layer.setContents(AImage);
    end
  );
end;

procedure TPlatformSimpleCamera.DoStartRecording;
var
  LAudioInput, LVideoInput: AVCaptureDeviceInput;
  LOutputURL: NSURL;
  LError: Pointer;
  LQueue: dispatch_queue_t;
  LAudioDevice, LVideoDevice: AVCaptureDevice;
begin
  FSession := TAVCaptureSession.Create;
  FSession.setSessionPreset(AVCaptureSessionPresetHigh);
  LVideoDevice := TAVCaptureDevice.Wrap(TAVCaptureDevice.OCClass.defaultDeviceWithMediaType(AVMediaTypeVideo));
  LVideoInput := TAVCaptureDeviceInput.Wrap(TAVCaptureDeviceInput.OCClass.deviceInputWithDevice(LVideoDevice, @LError));
  if LVideoInput <> nil then
  begin
    FSession.addInput(LVideoInput);
    if IncludeAudio and FCanIncludeAudio then
    begin
      LAudioDevice := TAVCaptureDevice.Wrap(TAVCaptureDevice.OCClass.defaultDeviceWithMediaType(AVMediaTypeAudio));
      LAudioInput := TAVCaptureDeviceInput.Wrap(TAVCaptureDeviceInput.OCClass.deviceInputWithDevice(LAudioDevice, @LError));
      if LAudioInput <> nil then
        FSession.addInput(LAudioInput);
    end;
    // If recording..
    FFileOutput := TAVCaptureMovieFileOutput.Create;
    FSession.addOutput(FFileOutput);
    // ..but always include preview
    FPreviewOutput := TAVCaptureVideoDataOutput.Create;
    FSession.addOutput(FPreviewOutput);
    if FPreviewOutputDelegate = nil then
      FPreviewOutputDelegate := TAVCaptureVideoDataOutputSampleBufferDelegate.Create(Self);
    LQueue := dispatch_queue_create('Capture Queue', 0);
    try
      FPreviewOutput.setSampleBufferDelegate(FPreviewOutputDelegate.GetObjectID, LQueue);
    finally
      dispatch_release(LQueue);
    end;
    FSession.startRunning;
    LOutputURL := TNSURL.Wrap(TNSURL.Alloc.initFileURLWithPath(StrToNSStr(FFileName)));
    if FFileOutputDelegate = nil then
      FFileOutputDelegate := TAVCaptureFileOutputRecordingDelegate.Create;
    FFileOutput.startRecordingToOutputFileURL(LOutputURL, FFileOutputDelegate.GetObjectID);
    RecordingState := TRecordingState.Recording;
  end
  else
    RecordingState := TRecordingState.Error;
end;

procedure TPlatformSimpleCamera.RequestAccessForAVMediaTypeAudioHandler(granted: Boolean);
begin
  FCanIncludeAudio := granted;
//  if not FCanIncludeAudio then
    // Warn that there will be no audio?
  DoStartRecording; // Regardless
end;

procedure TPlatformSimpleCamera.RequestAccessForAVMediaTypeVideoHandler(granted: Boolean);
begin
  if granted then
  begin
    if IncludeAudio then
      TAVCaptureDevice.OCClass.requestAccessForMediaType(AVMediaTypeAudio, RequestAccessForAVMediaTypeAudioHandler)
    else
      DoStartRecording;
  end
  else
    RecordingState := TRecordingState.Unauthorized;
end;

procedure TPlatformSimpleCamera.StartRecording(const AFileName: string);
begin
  FFileName := AFileName;
  TAVCaptureDevice.OCClass.requestAccessForMediaType(AVMediaTypeVideo, RequestAccessForAVMediaTypeVideoHandler);
end;

procedure TPlatformSimpleCamera.StopRecording;
begin
  if FFileOutput <> nil then
    FFileOutput.stopRecording;
  FFileOutput := nil;
  FSession.stopRunning;
  FSession := nil;
  RecordingState := TRecordingState.Stopped;
end;

initialization
  TPresentationProxyFactory.Current.Register(TCameraPreview, TControlType.Platform, TiOSPresentationProxy<TPlatformCameraPreview>);

finalization
  TPresentationProxyFactory.Current.Unregister(TCameraPreview, TControlType.Platform, TiOSPresentationProxy<TPlatformCameraPreview>);

end.

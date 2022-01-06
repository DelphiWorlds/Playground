unit Unit1;

// Based on this example for Android Studio:
//   https://developer.android.com/codelabs/camerax-getting-started#1
// But "Delphi-ized" :-)

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  DW.NativeCameraPreviewView,
  DW.Androidapi.JNI.AndroidX.Camera, DW.Androidapi.JNI.Concurrent, DW.Androidapi.JNI.AndroidX.Lifecycle,
  DW.Androidapi.JNI.AndroidX.Camera.Lifecycle;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FCameraExecutor: JExecutorService;
    FCameraProvider: JProcessCameraProvider;
    FCameraProviderFuture: JListenableFuture;
    FCameraProviderFutureListener: JRunnable;
    FCameraPreview: TNativeCameraPreviewView;
    FPreview: JPreview;
    FPreviewUseCaseGroup: JUseCaseGroup;
    procedure CameraProviderFutureListenerHandler;
    procedure RequestPermissions;
    procedure StartCamera;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  DW.OSLog,
  System.Permissions,
  Androidapi.Helpers, Androidapi.JNI.Support,
  DW.Consts.Android, DW.Permissions.Helpers, DW.Android.Helpers,
  DW.NativeCameraPreviewView.Android;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FCameraProviderFutureListener := TRunnable.Create(CameraProviderFutureListenerHandler, True);
  FCameraExecutor := TJExecutors.JavaClass.newSingleThreadExecutor;
  FCameraPreview := TNativeCameraPreviewView.Create(Self);
  FCameraPreview.Align := TAlignLayout.Client;
  FCameraPreview.Parent := Self;
end;

destructor TForm1.Destroy;
begin
  FCameraExecutor.shutdown;
  inherited;
end;

procedure TForm1.RequestPermissions;
begin
  PermissionsService.RequestPermissions([cPermissionReadExternalStorage, cPermissionWriteExternalStorage, cPermissionCamera],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        StartCamera;
    end
  );
end;

procedure TForm1.StartCamera;
begin
  TJProcessCameraProvider.JavaClass.configureInstance(TJCamera2Config.JavaClass.defaultConfig);
  FCameraProviderFuture := TJProcessCameraProvider.JavaClass.getInstance(TAndroidHelper.Context);
  FCameraProviderFuture.addListener(FCameraProviderFutureListener, TJcontent_ContextCompat.JavaClass.getMainExecutor(TAndroidHelper.Context));
end;

procedure TForm1.CameraProviderFutureListenerHandler;
var
  LNativePreviewView: JPreviewView;
  LLifecycleOwner: JLifecycleOwner;
begin
  FCameraProvider := TJProcessCameraProvider.Wrap(FCameraProviderFuture.&get);
  FPreview := TJPreview_Builder.JavaClass.init.build;
  FPreviewUseCaseGroup := TJUseCaseGroup_Builder.JavaClass.init
    .addUseCase(FPreview)
    .build;
  FCameraProvider.unbindAll;
  // FPreviewView provides the surface for LPreview
  LNativePreviewView := TAndroidNativeCameraPreviewView(FCameraPreview.Presentation).View;
  FPreview.setSurfaceProvider(LNativePreviewView.getSurfaceProvider);
  LLifecycleOwner := TJProcessLifecycleOwner.JavaClass.get;
  if LLifecycleOwner <> nil then
  begin
    if LLifecycleOwner.getLifecycle <> nil then
    begin
      Log.d('LifecycleOwner.getLifecycle: %s', [JStringToString(LLifecycleOwner.getLifecycle.toString)]);
    end
    else
      Log.d('LifecycleOwner.getLifecycle is nil');
  end
  else
    Log.d('LifecycleOwner is nil');
  FCameraProvider.bindToLifecycle(TJProcessLifecycleOwner.JavaClass.get, TJCameraSelector.JavaClass.DEFAULT_BACK_CAMERA, FPreviewUseCaseGroup);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RequestPermissions;
end;

end.

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.Firebase.Analytics;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FAnalytics: TFirebaseAnalytics;
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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers;

type
  JCrash = interface;
  JFirebaseCrashlytics = interface;

  JCrashClass = interface(JObjectClass)
    ['{4798A4C8-B033-49E7-97B9-349874ACABC9}']
    function init: JCrash; cdecl;
  end;

  [JavaSignature('com/crash/Crash')]
  JCrash = interface(JObject)
    ['{07ABF6E5-B235-4B43-A723-717C54CB837F}']
  end;
  TJCrash = class(TJavaGenericImport<JCrashClass, JCrash>) end;

  JFirebaseCrashlyticsClass = interface(JObjectClass)
    ['{0CBE882E-FE74-4FB4-B9EB-F6B45A96A0F4}']
    {class} function getInstance: JFirebaseCrashlytics; cdecl;
  end;

  [JavaSignature('com/google/firebase/crashlytics/FirebaseCrashlytics')]
  JFirebaseCrashlytics = interface(JObject)
    ['{53A94CAE-3EB5-4AAE-A031-BA6B602575C8}']
    // function checkForUnsentReports: JTask; cdecl;
    procedure deleteUnsentReports; cdecl;
    function didCrashOnPreviousExecution: Boolean; cdecl;
    procedure log(msg: JString); cdecl;
    procedure recordException(throwable: JThrowable); cdecl;
    procedure sendUnsentReports; cdecl;
    procedure setCrashlyticsCollectionEnabled(enabled: Boolean); cdecl; overload;
    procedure setCrashlyticsCollectionEnabled(enabled: JBoolean); cdecl; overload;
    procedure setCustomKey(key: JString; value: JString); cdecl; overload;
    procedure setCustomKey(key: JString; value: Single); cdecl; overload;
    procedure setCustomKey(key: JString; value: Double); cdecl; overload;
    procedure setCustomKey(key: JString; value: Boolean); cdecl; overload;
    procedure setCustomKey(key: JString; value: Int64); cdecl; overload;
    procedure setCustomKey(key: JString; value: Integer); cdecl; overload;
    // procedure setCustomKeys(keysAndValues: JCustomKeysAndValues); cdecl;
    procedure setUserId(identifier: JString); cdecl;
  end;
  TJFirebaseCrashlytics = class(TJavaGenericImport<JFirebaseCrashlyticsClass, JFirebaseCrashlytics>) end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FAnalytics := TFirebaseAnalytics.Create;
end;

destructor TForm1.Destroy;
begin
  FAnalytics.Free;
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  raise Exception.Create('Delphi native exception');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LCrash: JCrash;
begin
  LCrash := TJCrash.JavaClass.init;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  LIntent: JIntent;
begin
  // LIntent contains either nil, or garbage - either way it will cause an Android exception
  LIntent := TJIntent.JavaClass.init(LIntent);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  LCrashlytics: JFirebaseCrashlytics;
begin
  TOSLog.d('TForm1.Button4Click');
  LCrashlytics := TJFirebaseCrashlytics.JavaClass.getInstance;
  TOSLog.d('> log');
  LCrashlytics.log(StringToJString('Crashlytics test')); //  crash'));
  TOSLog.d('> recordException');
  LCrashlytics.recordException(TJException.JavaClass.init(StringToJString('Crashlytics Test Exception')));
end;

end.

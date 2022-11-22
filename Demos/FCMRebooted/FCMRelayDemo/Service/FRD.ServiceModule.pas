unit FRD.ServiceModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os;

type
  TServiceModule = class(TAndroidIntentService)
    procedure AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.OSLog;

procedure TServiceModule.AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
begin
  TOSLog.d('+TServiceModule.AndroidIntentServiceHandleIntent');
  TOSLog.d('> Intent:');
  TOSLog.d(JStringToString(AnIntent.toUri(0)));
  TOSLog.d('-TServiceModule.AndroidIntentServiceHandleIntent');
end;

end.

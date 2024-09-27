unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging, System.JSON,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo,
  DW.MessageReceiver;

type
  TForm1 = class(TForm)
    ButtonsLayout: TLayout;
    StartStopLocationButton: TButton;
    MessagesMemo: TMemo;
    procedure StartStopLocationButtonClick(Sender: TObject);
  private
    FHasLaunched: Boolean;
    FMessageReceiver: TJSONMessageReceiver;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure StartService(const AServiceName: string; const AIntent: JIntent);
    procedure ServiceMessageHandler(const AMsg: TJSONValue);
    procedure SetLocationUpdates(const AIsActive: Boolean);
    procedure SetServiceForeground(const ANeedsForeground: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.App,
  FMX.Platform,
  DW.OSLog,
  DW.JSON,
  ABL.LocationPermissions, ABL.Common;

const
  EMBTJavaServicePrefix = 'com.embarcadero.services.';

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  FMessageReceiver := TJSONMessageReceiver.Create(ServiceMessageHandler);
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      // This check is only if the service should not start as soon as the app does
      if FHasLaunched then
      begin
        TOSLog.d('TApplicationEvent.BecameActive > SetServiceForeground(False)');
        SetServiceForeground(False);
      end;
      FHasLaunched := False;
    end;
    TApplicationEvent.WillBecomeInactive:
    begin
      if LocationPermissions.CanStartForeground then
      begin
        TOSLog.d('TApplicationEvent.WillBecomeInactive > SetServiceForeground(True)');
        SetServiceForeground(True);
      end;
    end;
  end;
end;

procedure TForm1.StartService(const AServiceName: string; const AIntent: JIntent);
var
  LService: string;
begin
  LService := AServiceName;
  if not LService.StartsWith(EMBTJavaServicePrefix) then
    LService := EMBTJavaServicePrefix + LService;
  AIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(LService));
  TAndroidHelper.Activity.startService(AIntent);
end;

procedure TForm1.StartStopLocationButtonClick(Sender: TObject);
begin
  LocationPermissions.RequestBackground(
    procedure(const ACanStart: Boolean)
    begin
      if ACanStart then
      begin
        TOSLog.d('StartStopLocationButtonClick > SetLocationUpdates(True)');
        SetLocationUpdates(True);
      end;
    end
  );
end;

procedure TForm1.SetLocationUpdates(const AIsActive: Boolean);
var
  LInfo: TABLServiceInfo;
begin
  if AIsActive then
    LInfo.LocationMode := TActionMode.Start
  else
    LInfo.LocationMode := TActionMode.Stop;
  StartService('ABLDemoService', LInfo.ToIntent);
end;

procedure TForm1.SetServiceForeground(const ANeedsForeground: Boolean);
var
  LInfo: TABLServiceInfo;
begin
  if ANeedsForeground then
    LInfo.ForegroundMode := TActionMode.Start
  else
    LInfo.ForegroundMode := TActionMode.Stop;
  StartService('ABLDemoService', LInfo.ToIntent);
end;

procedure TForm1.ServiceMessageHandler(const AMsg: TJSONValue);
begin
  MessagesMemo.Lines.Add(TJSONHelper.Tidy(AMsg));
end;

end.

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON, System.Messaging, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.Maps, FMX.ActnList,
  DW.BroadcastMessage.Receiver;

type
  TForm1 = class(TForm)
    ButtonsLayout: TLayout;
    StartStopButton: TButton;
    MessagesMemo: TMemo;
    TabControl: TTabControl;
    MessagesTab: TTabItem;
    ActionList: TActionList;
    StartStopAction: TAction;
    procedure StartStopActionExecute(Sender: TObject);
    procedure StartStopActionUpdate(Sender: TObject);
  private
    FIsServiceRunning: Boolean;
    FMessageReceiver: TJSONBroadcastMessageReceiver;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure ServiceMessageHandler(const AKind: Integer; const AMsg: TJSONValue);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Sensors,
  FMX.Platform,
  DW.OSLog,
  DW.JSON, DW.ServiceManager.Android;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  TabControl.ActiveTab := MessagesTab;
  FMessageReceiver := TJSONBroadcastMessageReceiver.Create(ServiceMessageHandler);
  FIsServiceRunning := ServiceManager.IsServiceRunning('FSDemoService');
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.WillBecomeInactive:
      ServiceManager.StartService('FSDemoService');
  end;
end;

procedure TForm1.StartStopActionExecute(Sender: TObject);
begin
  if FIsServiceRunning then
    ServiceManager.StopService('FSDemoService')
  else
    ServiceManager.StartService('FSDemoService');
end;

procedure TForm1.StartStopActionUpdate(Sender: TObject);
const
  cStartStopCaptions: array[Boolean] of string = ('Start', 'Stop');
begin
  StartStopAction.Text := cStartStopCaptions[FIsServiceRunning];
end;

procedure TForm1.ServiceMessageHandler(const AKind: Integer; const AMsg: TJSONValue);
const
  cStartedStoppedCaptions: array[Boolean] of string = ('stopped', 'started');
begin
  case AKind of
    0:
    begin
      AMsg.TryGetValue('IsRunning', FIsServiceRunning);
      MessagesMemo.Lines.Add('Service ' + cStartedStoppedCaptions[FIsServiceRunning]);
    end;
    1:
    begin
      //
    end;
  end;
end;

end.

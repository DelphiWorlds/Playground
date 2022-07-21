unit DW.FCMManager;

interface

uses
  System.PushNotification, System.Json, System.Classes;

type
  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TPushServiceNotification) of object;
  TMessageReceivedEvent = procedure(Sender: TObject; const JSON: TJSONObject) of object;
  TCheckPushEnabledMethod = reference to procedure(const Enabled: Boolean);

  IFCMManager = interface(IInterface)
    ['{961FE28D-76AA-466D-AA0D-3086FBE4D678}']
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
    function GetOnMessageReceived: TMessageReceivedEvent;
    function GetOnStatusChange: TNotifyEvent;
    function GetShowBannerIfForeground: Boolean;
    function GetToken: string;
    function IsStarted: Boolean;
    procedure SetOnMessageReceived(const AValue: TMessageReceivedEvent);
    procedure SetOnStatusChange(const AValue: TNotifyEvent);
    procedure SetShowBannerIfForeground(const AValue: Boolean);
    procedure Start;
    procedure SubscribeToTopic(const ATopic: string);
    procedure UnsubscribeFromTopic(const ATopic: string);
    property ShowBannerIfForeground: Boolean read GetShowBannerIfForeground write SetShowBannerIfForeground;
    property OnMessageReceived: TMessageReceivedEvent read GetOnMessageReceived write SetOnMessageReceived;
    property OnStatusChange: TNotifyEvent read GetOnStatusChange write SetOnStatusChange;
  end;

var
  FCM: IFCMManager;

implementation

uses
  System.SysUtils, System.Notification, System.Messaging, System.IOUtils,
  {$IF Defined(ANDROID)}
  Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.Firebase,
  Androidapi.JNI.App, Androidapi.JNIBridge,
  FMX.PushNotification.Android,
  {$ENDIF}
  {$IF Defined(IOS)}
  Macapi.Helpers, iOSapi.FirebaseMessaging, iOSapi.UserNotifications,
  FMX.PushNotification.FCM.iOS,
  {$ENDIF}
  FMX.Platform,
  DW.OSLog,
  {$IF Defined(IOS)}
  DW.UserDefaults.iOS,
  {$ENDIF}
  DW.OSMetadata;

type
  {$IF Defined(ANDROID)}
  JDWNotificationPresenterClass = interface(JObjectClass)
    ['{F7D03A16-5C6D-47C4-A2C9-1B9A826A610C}']
    {class} procedure presentNotification(context: JContext; intent: JIntent; channelId: JString; defaultSmallIcon: Integer); cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWNotificationPresenter')]
  JDWNotificationPresenter = interface(JObject)
    ['{6977BA6C-67AA-4AD5-A0DB-1E7EE441771E}']
  end;
  TJDWNotificationPresenter = class(TJavaGenericImport<JDWNotificationPresenterClass, JDWNotificationPresenter>) end;
  {$ENDIF}

  TFCMManager = class(TInterfacedObject, IFCMManager)
  private
    FChannelId: string;
    {$IF Defined(IOS)}
    FCheckPushEnabledHandler: TCheckPushEnabledMethod;
    {$ENDIF}
    FDeviceID: string;
    FDeviceToken: string;
    FIsForeground: Boolean;
    FServiceConnection: TPushServiceConnection;
    FShowBannerIfForeground: Boolean;
    FOnMessageReceived: TMessageReceivedEvent;
    FOnNotificationReceived: TNotificationReceivedEvent;
    FOnStatusChange: TNotifyEvent;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    {$IF Defined(IOS)}
    procedure CheckPushEnabledCompletionHandler(settings: UNNotificationSettings);
    {$ENDIF}
    procedure CheckStartupNotifications;
    procedure CreateChannel;
    procedure CreateConnection;
    function GetPushService: TPushService;
    {$IF Defined(ANDROID)}
    function IsAndroidPushEnabled: Boolean;
    {$ENDIF}
    procedure MessageReceivedNotificationHandler(const Sender: TObject; const AMsg: TMessage);
    procedure PushDeviceTokenMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
    procedure ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
  public
    { IFCMManager }
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
    function GetOnMessageReceived: TMessageReceivedEvent;
    function GetOnStatusChange: TNotifyEvent;
    function GetShowBannerIfForeground: Boolean;
    function GetToken: string;
    function IsStarted: Boolean;
    procedure SetOnMessageReceived(const AValue: TMessageReceivedEvent);
    procedure SetOnStatusChange(const AValue: TNotifyEvent);
    procedure SetShowBannerIfForeground(const AValue: Boolean);
    procedure Start;
    procedure SubscribeToTopic(const ATopic: string);
    procedure UnsubscribeFromTopic(const ATopic: string);
  public
    constructor Create;
    destructor Destroy; override;
    property DeviceID: string read FDeviceID;
    property DeviceToken: string read FDeviceToken;
  end;

const
  cMetadataFCMDefaultChannelId = 'com.google.firebase.messaging.default_notification_channel_id';
  cMetadataFCMDefaultNotificationIcon = 'com.google.firebase.messaging.default_notification_icon';

  UNAuthorizationStatusEphemeral = 4;

{ TFCMManager }

constructor TFCMManager.Create;
begin
  inherited Create;
  TOSMetadata.GetValue(cMetadataFCMDefaultChannelId, FChannelId);
  FShowBannerIfForeground := True;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationHandler);
end;

destructor TFCMManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  FServiceConnection.Free;
  inherited;
end;

procedure TFCMManager.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
      FIsForeground := True;
    TApplicationEvent.EnteredBackground:
      FIsForeground := False;
  end;
end;

procedure TFCMManager.CreateConnection;
var
  LPushService: TPushService;
begin
  LPushService := GetPushService;
  if LPushService <> nil  then
  begin
    FServiceConnection := TPushServiceConnection.Create(LPushService);
    FServiceConnection.OnChange := ServiceConnectionChangeHandler;
    FServiceConnection.OnReceiveNotification := ReceiveNotificationHandler;
    FDeviceId := LPushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceId];
    FServiceConnection.Active := True;
  end;
end;

procedure TFCMManager.CreateChannel;
var
  LNotificationCenter: TNotificationCenter;
  LChannel: TChannel;
begin
  if not FChannelId.IsEmpty then
  begin
    LNotificationCenter := TNotificationCenter.Create(nil);
    try
      LChannel := TChannel.Create;
      try
        LChannel.Id := FChannelId;
        LChannel.Title := FChannelId + ' FCM';
        LChannel.Description := '';
        // Required for appearing as a banner when the app is not running, or when in the foreground
        LChannel.Importance := TImportance.High;
        LNotificationCenter.CreateOrUpdateChannel(LChannel);
      finally
        LChannel.Free;
      end;
    finally
      LNotificationCenter.Free;
    end;
  end;
end;

{$IF Defined(IOS)}
procedure TFCMManager.CheckPushEnabledCompletionHandler(settings: UNNotificationSettings);
var
  LIsPushEnabled: Boolean;
begin
  LIsPushEnabled := settings.authorizationStatus in
    [UNAuthorizationStatusAuthorized, UNAuthorizationStatusProvisional, UNAuthorizationStatusEphemeral];
  TThread.Queue(nil, procedure begin FCheckPushEnabledHandler(LIsPushEnabled) end);
end;
{$ENDIF}

procedure TFCMManager.CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
begin
  {$IF Defined(ANDROID)}
  AHandler(IsAndroidPushEnabled)
  {$ELSEIF Defined(IOS)}
  FCheckPushEnabledHandler := AHandler;
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.getNotificationSettingsWithCompletionHandler(CheckPushEnabledCompletionHandler);
  {$ELSE}
  AHandler(True);
  {$ENDIF}
end;

{$IF Defined(ANDROID)}
function TFCMManager.IsAndroidPushEnabled: Boolean;
var
  LService: JObject;
  LNotificationManager: JNotificationManager;
  LChannels: JList;
  LChannel: JNotificationChannel;
  I: Integer;
begin
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  LNotificationManager := TJNotificationManager.Wrap(TAndroidHelper.JObjectToID(LService));
  Result := LNotificationManager.areNotificationsEnabled;
  if Result and (TJBuild_Version.JavaClass.SDK_INT >= 26) then
  begin
    LChannels := LNotificationManager.getNotificationChannels;
    for I := 0 to LChannels.size - 1 do
    begin
      LChannel := TJNotificationChannel.Wrap(LChannels.get(I));
      if LChannel.getId.equals(StringToJString(FChannelId)) and (LChannel.getImportance = TJNotificationManager.JavaClass.IMPORTANCE_NONE) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;
{$ENDIF}

function TFCMManager.GetOnMessageReceived: TMessageReceivedEvent;
begin
  Result := FOnMessageReceived;
end;

function TFCMManager.GetOnStatusChange: TNotifyEvent;
begin
  Result := FOnStatusChange;
end;

procedure TFCMManager.SetOnMessageReceived(const AValue: TMessageReceivedEvent);
begin
  FOnMessageReceived := AValue;
end;

procedure TFCMManager.SetOnStatusChange(const AValue: TNotifyEvent);
begin
  FOnStatusChange := AValue;
end;

procedure TFCMManager.SetShowBannerIfForeground(const AValue: Boolean);
begin
  FShowBannerIfForeground := AValue;
end;

procedure TFCMManager.CheckStartupNotifications;
var
  LNotification: TPushServiceNotification;
begin
  // Handle startup notifications
  if FServiceConnection <> nil then
  begin
    for LNotification in FServiceConnection.Service.StartupNotifications do
      ReceiveNotificationHandler(FServiceConnection, LNotification);
  end;
end;

function TFCMManager.GetPushService: TPushService;
begin
  Result := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.FCM);
end;

function TFCMManager.GetShowBannerIfForeground: Boolean;
begin
  Result := FShowBannerIfForeground;
end;

function TFCMManager.IsStarted: Boolean;
begin
  if FServiceConnection <> nil then
    Result := FServiceConnection.Service.Status = TPushService.TStatus.Started
  else
    Result := False;
end;

function TFCMManager.GetToken: string;
begin
  Result := FDeviceToken;
end;

procedure TFCMManager.MessageReceivedNotificationHandler(const Sender: TObject; const AMsg: TMessage);
{$IF Defined(ANDROID)}
var
  LIcon: string;
  LIntent: JIntent;
begin
  if FShowBannerIfForeground and FIsForeground then
  begin
    TOSMetadata.GetValue(cMetadataFCMDefaultNotificationIcon, LIcon);
    LIntent := TMessageReceivedNotification(AMsg).Value;
    TJDWNotificationPresenter.JavaClass.presentNotification(TAndroidHelper.Context, LIntent, StringToJString(FChannelId), StrToIntDef(LIcon, 0));
  end;
end;
{$ELSE}
begin
  // Not applicable to other platforms
end;
{$ENDIF}

procedure TFCMManager.PushDeviceTokenMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  // APNs token is saved purely for reference and diagnostic purposes
  {$IF Defined(IOS)}
  TUserDefaults.SetValue('APNS', TPushDeviceTokenMessage(AMsg).Value.Token);
  TOSLog.d('> APNS Token: %s', [TUserDefaults.GetValue('APNS')]);
  {$ENDIF}
end;

procedure TFCMManager.ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
begin
  if Assigned(FOnNotificationReceived) then
    FOnNotificationReceived(Self, AServiceNotification);
  if AServiceNotification.Json <> nil then
  begin
    TOSLog.d('TFCMManager.ReceiveNotificationHandler - JSON: %s', [AServiceNotification.Json.ToString]);
    if Assigned(FOnMessageReceived) then
      FOnMessageReceived(Self, AServiceNotification.Json);
  end;
end;

procedure TFCMManager.ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
begin
  if TPushService.TChange.DeviceToken in APushChanges then
  begin
    TOSLog.d('TFCMManager.ServiceConnectionChangeHandler');
    FDeviceToken := GetPushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];
    TOSLog.d('> Token: %s', [FDeviceToken]);
  end;
  if (TPushService.TChange.Status in APushChanges) and Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

procedure TFCMManager.Start;
begin
  CreateChannel;
  CreateConnection;
  CheckStartupNotifications;
end;

procedure TFCMManager.SubscribeToTopic(const ATopic: string);
begin
  {$IF Defined(ANDROID)}
  TJFirebaseMessaging.JavaClass.getInstance.subscribeToTopic(StringToJString(ATopic));
  {$ENDIF}
  {$IF Defined(IOS)}
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).subscribeToTopic(StrToNSStr(ATopic));
  {$ENDIF}
end;

procedure TFCMManager.UnsubscribeFromTopic(const ATopic: string);
begin
  {$IF Defined(ANDROID)}
  TJFirebaseMessaging.JavaClass.getInstance.unsubscribeFromTopic(StringToJString(ATopic));
  {$ENDIF}
  {$IF Defined(IOS)}
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).unsubscribeFromTopic(StrToNSStr(ATopic));
  {$ENDIF}
end;

initialization
  FCM := TFCMManager.Create;

end.

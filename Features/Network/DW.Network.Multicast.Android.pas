unit DW.Network.Multicast.Android;

// Sources:
//   https://stackoverflow.com/questions/20059106/ipv6-multicast-example
//   https://www.developer.com/java/data/how-to-multicast-using-java-sockets.html

interface

uses
  System.SysUtils,
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Java.Net, Androidapi.JNI.Os, Androidapi.JNI.Net,
  DW.Network.Multicast;

type
  TPlatformMulticastReceiver = class;

  TMulticastReceiverRunnable = class(TJavaLocal, JRunnable)
  private
    FReceiver: TPlatformMulticastReceiver;
    FThread: JThread;
  public
    { JRunnable }
    procedure run; cdecl;
  public
    constructor Create(const AReceiver: TPlatformMulticastReceiver); virtual;
    destructor Destroy; override;
  end;

  TPlatformMulticastReceiver = class(TMulticastReceiver)
  private
    FRunnable: JRunnable;
    FLock: JWifiManager_MulticastLock;
    procedure CreateMulticastLock;
  protected
    procedure Run;
  public
    function Start: Boolean; override;
    procedure Stop; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPlatformMulticastSender = class(TMulticastSender)
  private
    procedure InternalSend(const AData: TBytes; const APort: Integer; const AGroup: string);
  public
    procedure Broadcast(const AData: TBytes; const APort: Integer); override;
  end;

implementation

uses
  System.Classes,
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI,
  DW.OSLog,
  DW.Androidapi.JNI.Net, DW.Classes.Helpers;

{ TMulticastReceiverRunnable }

constructor TMulticastReceiverRunnable.Create(const AReceiver: TPlatformMulticastReceiver);
begin
  inherited Create;
  FReceiver := AReceiver;
  FThread := TJThread.JavaClass.init(Self);
  FThread.start;
end;

destructor TMulticastReceiverRunnable.Destroy;
begin
  FThread := nil;
  inherited;
end;

procedure TMulticastReceiverRunnable.run;
begin
  FReceiver.Run;
end;

{ TPlatformMulticastReceiver }

constructor TPlatformMulticastReceiver.Create;
begin
  inherited;
  //
end;

destructor TPlatformMulticastReceiver.Destroy;
begin
  if IsActive then
    Stop;
  inherited;
end;

procedure TPlatformMulticastReceiver.CreateMulticastLock;
var
  LObject: JObject;
begin
  LObject := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.WIFI_SERVICE);
  FLock := TJWifiManager.Wrap(LObject).createMulticastLock(StringToJString(ClassName));
  FLock.setReferenceCounted(True);
end;

function TPlatformMulticastReceiver.Start: Boolean;
begin
  Stop;
  if FLock = nil then
    CreateMulticastLock;
  FLock.acquire;
  FRunnable := TMulticastReceiverRunnable.Create(Self);
  Result := True;
end;

procedure TPlatformMulticastReceiver.Stop;
begin
  if FLock <> nil then
    FLock.release;
  FRunnable := nil;
end;

//!!!! Needs exception handling!!!!
procedure TPlatformMulticastReceiver.Run;
var
  LBuffer: TJavaArray<Byte>;
  LSocket: JMulticastSocket;
  LGroup: JInetAddress;
  LPacket: JDatagramPacket;
begin
  LBuffer := TJavaArray<Byte>.Create(1024); // MaxPacketSize
  LGroup := TJInetAddress.JavaClass.getByName(StringToJString(GroupIPv4));
  LSocket := TJMulticastSocket.JavaClass.init(Port);
  // LSocket.setLoopbackMode(True); // True = loopback is disabled :-/
  LSocket.joinGroup(LGroup);
  while IsActive do
  begin
    LPacket := TJDatagramPacket.JavaClass.init(LBuffer, LBuffer.Length);
    LSocket.receive(LPacket);
    DoDataReceived(JStringToString(LPacket.getAddress.getHostAddress), TJavaArrayToTBytes(LPacket.getData));
  end;
  LSocket.leaveGroup(LGroup);
  LSocket.close;
end;

{ TPlatformMulticastSender }

procedure TPlatformMulticastSender.Broadcast(const AData: TBytes; const APort: Integer);
begin
  TThread.CreateAnonymousThread(procedure begin InternalSend(AData, APort, GroupIPv4); end).Start;
  TThread.CreateAnonymousThread(procedure begin InternalSend(AData, APort, GroupIPv6); end).Start;
end;

//!!!! Needs exception handling???
procedure TPlatformMulticastSender.InternalSend(const AData: TBytes; const APort: Integer; const AGroup: string);
var
  LSocket: JMulticastSocket;
  LPacket: JDatagramPacket;
  LData: TJavaArray<Byte>;
  LGroup: JInetAddress;
begin
// Needs conversion from TBytes to string
//  TOSLog.d('Sending: %s on Port: %d for Group: %s', [AData, APort, AGroup]);
  LGroup := TJInetAddress.JavaClass.getByName(StringToJString(AGroup));
  LSocket := TJMulticastSocket.JavaClass.init;
  LSocket.setLoopbackMode(True); // True = loopback is disabled :-/
  LData := TBytesToTJavaArray(AData);
  LPacket := TJDatagramPacket.JavaClass.init(LData, LData.Length, LGroup, APort);
  LSocket.send(LPacket, 1);
  LSocket.close;
  TOSLog.d('Sent packet on Port: %d for Group: %s', [APort, AGroup]);
end;

end.

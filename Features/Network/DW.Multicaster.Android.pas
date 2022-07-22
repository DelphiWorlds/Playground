unit DW.Multicaster.Android;

// Sources:
//   https://stackoverflow.com/questions/20059106/ipv6-multicast-example
//   https://www.developer.com/java/data/how-to-multicast-using-java-sockets.html

interface

uses
  System.SysUtils,
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Java.Net, Androidapi.JNI.Os, Androidapi.JNI.Net,
  DW.Multicaster;

type
  TPlatformMulticaster = class;

  TMulticastRunnable = class(TJavaLocal, JRunnable)
  private
    FPlatformMulticaster: TPlatformMulticaster;
    FThread: JThread;
  public
    { JRunnable }
    procedure run; cdecl;
  public
    constructor Create(const APlatformMulticaster: TPlatformMulticaster); virtual;
    destructor Destroy; override;
  end;

  TPlatformMulticaster = class(TCustomPlatformMulticaster)
  private
    FRunnable: JRunnable;
    FLock: JWifiManager_MulticastLock;
    procedure CreateMulticastLock;
    procedure InternalSend(const AData: TBytes; const APort: Integer; const AGroup: string);
  protected
    procedure Broadcast(const AData: TBytes; const APort: Integer); override;
    procedure DoRun;
    procedure SetGroupIPv4(const Value: string); override;
    procedure SetGroupIPv6(const Value: string); override;
    procedure SetPort(const Value: Integer); override;
    function Start: Boolean; override;
    procedure Stop; override;
  public
    constructor Create(const AMulticaster: TMulticaster); override;
    destructor Destroy; override;
  end;

implementation

uses
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI,
  DW.OSLog,
  DW.Androidapi.JNI.Net, DW.Classes.Helpers;

{ TMulticastRunnable }

constructor TMulticastRunnable.Create(const APlatformMulticaster: TPlatformMulticaster);
begin
  inherited Create;
  FPlatformMulticaster := APlatformMulticaster;
  FThread := TJThread.JavaClass.init(Self);
  FThread.start;
end;

destructor TMulticastRunnable.Destroy;
begin
  FThread := nil;
  inherited;
end;

procedure TMulticastRunnable.run;
begin
  FPlatformMulticaster.DoRun;
end;

{ TPlatformMulticaster }

constructor TPlatformMulticaster.Create(const AMulticaster: TMulticaster);
begin
  inherited;
  //
end;

destructor TPlatformMulticaster.Destroy;
begin
  if Active then
    Stop;
  inherited;
end;

procedure TPlatformMulticaster.CreateMulticastLock;
var
  LObject: JObject;
begin
  LObject := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.WIFI_SERVICE);
  FLock := TJWifiManager.Wrap(LObject).createMulticastLock(StringToJString(ClassName));
  FLock.setReferenceCounted(True);
end;

function TPlatformMulticaster.Start: Boolean;
begin
  Stop;
  if FLock = nil then
    CreateMulticastLock;
  FLock.acquire;
  FRunnable := TMulticastRunnable.Create(Self);
  Result := True;
end;

procedure TPlatformMulticaster.Stop;
begin
  if FLock <> nil then
    FLock.release;
  FRunnable := nil;
end;

//!!!! Needs exception handling!!!!
procedure TPlatformMulticaster.DoRun;
var
  LBuffer: TJavaArray<Byte>;
  LSocket: JMulticastSocket;
  LGroup: JInetAddress;
  LPacket: JDatagramPacket;
begin
  LBuffer := TJavaArray<Byte>.Create(MaxPacketSize);
  LGroup := TJInetAddress.JavaClass.getByName(StringToJString(GroupIPv4));
  LSocket := TJMulticastSocket.JavaClass.init(Port);
  // LSocket.setLoopbackMode(True); // True = loopback is disabled :-/
  LSocket.joinGroup(LGroup);
  while Active do
  begin
    LPacket := TJDatagramPacket.JavaClass.init(LBuffer, LBuffer.Length);
    LSocket.receive(LPacket);
    DoDataReceived(JStringToString(LPacket.getAddress.getHostAddress), TJavaArrayToTBytes(LPacket.getData));
  end;
  LSocket.leaveGroup(LGroup);
  LSocket.close;
end;

//!!!! Needs exception handling!!!!
procedure TPlatformMulticaster.InternalSend(const AData: TBytes; const APort: Integer; const AGroup: string);
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

procedure TPlatformMulticaster.Broadcast(const AData: TBytes; const APort: Integer);
begin
  TDo.Run(
    procedure
    begin
      InternalSend(AData, APort, GroupIPv4);
    end
  );
  TDo.Run(
    procedure
    begin
      InternalSend(AData, APort, GroupIPv6);
    end
  );
end;

procedure TPlatformMulticaster.SetGroupIPv4(const Value: string);
begin
  inherited;
  // If active, then stop, change, start
end;

procedure TPlatformMulticaster.SetGroupIPv6(const Value: string);
begin
  inherited;
  // If active, then stop, change, start
end;

procedure TPlatformMulticaster.SetPort(const Value: Integer);
begin
  inherited;
  // If active, then stop, change, start
end;

end.

unit DW.RadioPlayer;

interface

{$IF Defined(ANDROID) and not Defined(SERVICE)}
  {$DEFINE ANDROIDAPP}
{$ENDIF}

uses
  System.Classes,
  Bass,
  DW.RadioPlayer.ServiceController, DW.RadioPlayer.Common, DW.OSTimer;

const
  cIcyName = 'icy-name';
  cIcyBitrate = 'icy-br';

type
  TRadioStatus = DW.RadioPlayer.Common.TRadioStatus;

  TRadioPlayer = class(TObject)
  private
    {$IF not Defined(MACOS)}
    FPluginHandle: HPLUGIN;
    {$ENDIF}
    {$IF Defined(MSWINDOWS)}
    FHandle: UIntPtr;
    {$ELSE}
    FHandle: Pointer;
    {$ENDIF}
    FIsBASSLoaded: Boolean;
    FMetadataTime: TDateTime;
    FMetaTimer: TOSTimer;
    FRadioStatus: TRadioStatus;
    FServiceController: TCustomRadioServiceController;
    FStream: HSTREAM;
    FURL: string;
    FUseService: Boolean;
    FOnServiceStarted: TNotifyEvent;
    FOnStatusChanged: TNotifyEvent;
    FOnStreamMetadata: TStreamMetadataEvent;
    procedure DoError;
    function DoBASSPause: Boolean;
    function DoBASSPlay: Boolean;
    function DoBASSResume: Boolean;
    function DoPlay: Boolean;
    procedure DoServiceStarted;
    procedure DoStreamMetadata(const AMetadata: TArray<string>);
    function GetTags(const ATagIndex: Cardinal; out ATags: TArray<string>): Boolean;
    function LoadPlugin: Boolean;
    procedure MetaTimerIntervalHandler(Sender: TObject);
    procedure MetaTimerStart;
    procedure MetaTimerStop;
    function Resume: Boolean;
    {$IF Defined(ANDROIDAPP)}
    procedure ServiceControllerRadioStatusChangedHandler(Sender: TObject; const AStatus: TRadioStatus);
    procedure ServiceControllerServiceStartedHandler(Sender: TObject);
    procedure ServiceControllerStreamMetadataHandler(Sender: TObject; const AMetadata: TArray<string>);
    {$ENDIF}
    function ShouldUseService: Boolean;
    procedure StatusChange(const AStatus: TRadioStatus);
    procedure Stopped;
  protected
    procedure DoEndSync;
    procedure DoMetaSync;
    procedure DoPosSync(const AData: DWORD);
  public
    constructor Create;
    destructor Destroy; override;
    function Play: Boolean;
    function Pause: Boolean;
    procedure StartService(const AServiceName: string);
    procedure Stop;
    property URL: string read FURL write FURL;
    property Status: TRadioStatus read FRadioStatus;
    property UseService: Boolean read FUseService write FUseService;
    property OnServiceStarted: TNotifyEvent read FOnServiceStarted write FOnServiceStarted;
    property OnStatusChanged: TNotifyEvent read FOnStatusChanged write FOnStatusChanged;
    property OnStreamMetadata: TStreamMetadataEvent read FOnStreamMetadata write FOnStreamMetadata;
  end;

  TRadioMetadata = record
  public
    class function GetStreamTitle(const AMetadata: TArray<string>; out AValue: string): Boolean; static;
    class function GetValue(const AMetadata: TArray<string>; const ATag: string; out AValue: string): Boolean; static;
  end;

implementation

uses
  DW.OSLog,
  {$IF Defined(SERVICE)}
  DW.RadioPlayer.ServiceHelper,
  {$ENDIF}
  System.SysUtils, System.DateUtils;

const
  cMetadataTimeMinimum = 30000;
  {$IF Defined(MSWINDOWS)}
  cBASSPluginLib = 'bass_aac.dll';
  {$ELSE}
  cBASSPluginLib = 'libbass_aac.so';
  {$ENDIF}

{ TRadioMetadata }

class function TRadioMetadata.GetStreamTitle(const AMetadata: TArray<string>; out AValue: string): Boolean;
var
  LItem: string;
begin
  Result := False;
  for LItem in AMetadata do
  begin
    if LItem.StartsWith(cMetadataStreamTitlePrefix) then
    begin
      AValue := AnsiDequotedStr(LItem.Substring(Length(cMetadataStreamTitlePrefix)).Trim([';']), '''');
      Result := True;
      Break;
    end
  end;
end;

class function TRadioMetadata.GetValue(const AMetadata: TArray<string>; const ATag: string; out AValue: string): Boolean;
var
  LItem: string;
begin
  Result := False;
  for LItem in AMetadata do
  begin
    if LItem.StartsWith(ATag + ':') then
    begin
      AValue := LItem.Substring(Length(ATag) + 1).Trim;
      Result := True;
      Break;
    end
  end;
end;

procedure BASSSyncMeta(handle: HSYNC; channel, data: DWORD; user: Pointer); {$IF Defined(MSWINDOWS)} stdcall {$ELSE} cdecl {$ENDIF};
begin
  TRadioPlayer(user).DoMetaSync;
end;

//procedure BASSSyncPos(handle: HSYNC; channel, data: DWORD; user: Pointer); {$IF Defined(MSWINDOWS)} stdcall {$ELSE} cdecl {$ENDIF};
//begin
//  TRadioPlayer(user).DoPosSync(data);
//end;

procedure BASSSyncEnd(handle: HSYNC; channel, data: DWORD; user: Pointer); {$IF Defined(MSWINDOWS)} stdcall {$ELSE} cdecl {$ENDIF};
begin
  TRadioPlayer(user).DoEndSync;
end;

{ TRadioPlayer }

constructor TRadioPlayer.Create;
begin
  inherited;
  {$IF Defined(ANDROIDAPP)}
  if System.DelphiActivity <> nil then
  begin
    FServiceController := TPlatformRadioServiceController.Create;
    FServiceController.OnRadioStatusChanged := ServiceControllerRadioStatusChangedHandler;
    FServiceController.OnServiceStarted := ServiceControllerServiceStartedHandler;
    FServiceController.OnStreamMetadata := ServiceControllerStreamMetadataHandler;
  end;
  FUseService := True;
  {$ENDIF}
  FRadioStatus := TRadioStatus.Stopped;
  FMetaTimer := TOSTimer.Create;
  FMetaTimer.Interval := cMetadataTimeMinimum;
  FMetaTimer.OnInterval := MetaTimerIntervalHandler;
end;

destructor TRadioPlayer.Destroy;
begin
  {$IF Defined(ANDROIDAPP)}
  FServiceController.Free;
  {$ENDIF}
  inherited;
end;

{$IF Defined(ANDROIDAPP)}
procedure TRadioPlayer.ServiceControllerRadioStatusChangedHandler(Sender: TObject; const AStatus: TRadioStatus);
begin
  StatusChange(AStatus);
end;

procedure TRadioPlayer.ServiceControllerServiceStartedHandler(Sender: TObject);
begin
  DoServiceStarted;
end;

procedure TRadioPlayer.ServiceControllerStreamMetadataHandler(Sender: TObject; const AMetadata: TArray<string>);
begin
  DoStreamMetadata(AMetadata);
end;
{$ENDIF}

procedure TRadioPlayer.MetaTimerIntervalHandler(Sender: TObject);
begin
  if SecondsBetween(Now, FMetadataTime) > cMetadataTimeMinimum div 2 then
    DoMetaSync;
end;

procedure TRadioPlayer.MetaTimerStart;
begin
  FMetadataTime := Now;
  FMetaTimer.Enabled := True;
end;

procedure TRadioPlayer.MetaTimerStop;
begin
  FMetaTimer.Enabled := False;
end;

procedure TRadioPlayer.DoServiceStarted;
begin
  if Assigned(FOnServiceStarted) then
    FOnServiceStarted(Self);
end;

procedure TRadioPlayer.DoStreamMetadata(const AMetadata: TArray<string>);
begin
  if Assigned(FOnStreamMetadata) then
    FOnStreamMetadata(Self, AMetadata);
end;

function TRadioPlayer.ShouldUseService: Boolean;
begin
  Result := (FServiceController <> nil) and FUseService;
end;

procedure TRadioPlayer.StartService(const AServiceName: string);
begin
  if ShouldUseService then
  begin
    // Returns True if the service has already started
    if FServiceController.StartService(AServiceName) then
      DoServiceStarted;
  end
  else
    DoServiceStarted;
end;

procedure TRadioPlayer.DoEndSync;
begin
  Stopped;
end;

procedure TRadioPlayer.DoError;
var
  LCode: Integer;
begin
  LCode := Bass_ErrorGetCode;
  TOSLog.d('BASS Error Code: %d', [LCode]);
end;

function TRadioPlayer.GetTags(const ATagIndex: Cardinal; out ATags: TArray<string>): Boolean;
const
  cNullChar: AnsiChar = #0;
var
  LTagAString: MarshaledAString;
begin
  Result := False;
  LTagAString := BASS_ChannelGetTags(FStream, ATagIndex);
  if LTagAString <> nil then
  begin
    while LTagAString^ <> cNullChar do
    begin
      ATags := ATags + [UTF8ToString(Copy(LTagAString, 1, MaxInt))];
      LTagAString := LTagAString + Length(LTagAString) + 1;
    end;
    Result := Length(ATags) > 0;
  end
  else
    TOSLog.d('Could not get tags for index: %d. Error code: %d', [ATagIndex, BASS_ErrorGetCode]);
end;

procedure TRadioPlayer.DoMetaSync;
var
  LTags: TArray<string>;
begin
  // e.g. StreamTitle='Stephen Howie - The Grooveline Series';
  if GetTags(BASS_TAG_META, LTags) then
  begin
    FMetadataTime := Now;
    {$IF Defined(SERVICE)}
    TRadioServiceHelper.ReceivedStreamMetadata(LTags);
    {$ENDIF}
    DoStreamMetadata(LTags);
  end;
end;

procedure TRadioPlayer.DoPosSync(const AData: DWORD);
begin
  //
end;

function TRadioPlayer.LoadPlugin: Boolean;
begin
  Result := False;
  if not FUseService then
  begin
    if not FIsBASSLoaded then
      FIsBASSLoaded := BASS_Init(-1, 44100, 0, FHandle, nil);
    if FIsBASSLoaded then
    begin
      BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);
      BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);
      // iOS and macOS do not need the plugin
      {$IF Defined(MSWINDOWS) or Defined(ANDROID)}
      if FPluginHandle = 0 then
        FPluginHandle := BASS_PluginLoad(cBASSPluginLib, 0 or BASS_UNICODE);
      Result := FPluginHandle <> 0;
      {$ELSE}
      Result := True;
      {$ENDIF}
    end;
  end
  else
    Result := True;
end;

procedure TRadioPlayer.StatusChange(const AStatus: TRadioStatus);
begin
  FRadioStatus := AStatus;
  if FRadioStatus = TRadioStatus.Playing then
    MetaTimerStart
  else
    MetaTimerStop;
  {$IF Defined(SERVICE)}
  TRadioServiceHelper.RadioStatusChanged(AStatus);
  {$ENDIF}
  if Assigned(FOnStatusChanged) then
    FOnStatusChanged(Self);
end;

procedure TRadioPlayer.Stop;
begin
  if not ShouldUseService then
  begin
    if FStream > 0 then
      BASS_StreamFree(FStream);
    FStream := 0;
    Stopped;
  end
  else
    FServiceController.Stop;
end;

procedure TRadioPlayer.Stopped;
begin
  StatusChange(TRadioStatus.Stopped);
end;

function TRadioPlayer.DoBASSPause: Boolean;
begin
  Result := False;
  if (FRadioStatus = TRadioStatus.Playing) and (FStream > 0) then
    Result := BASS_ChannelPlay(FStream, True);
  if Result then
    StatusChange(TRadioStatus.Paused)
  else
    StatusChange(TRadioStatus.Unknown);
end;

function TRadioPlayer.Pause: Boolean;
begin
  if ShouldUseService then
  begin
    FServiceController.Pause;
    Result := True;
  end
  else
    Result := DoBASSPause;
end;

function TRadioPlayer.Play: Boolean;
begin
  Result := False;
  if LoadPlugin then
  begin
    if FRadioStatus in [TRadioStatus.Unknown, TRadioStatus.Stopped] then
    begin
      if not FURL.IsEmpty then
        Result := DoPlay;
      // else no URL
    end
    else
      Result := Resume;
  end
  else
    TOSLog.d('Failed to load plugin');
end;

function TRadioPlayer.DoBASSResume: Boolean;
begin
  if FStream <> 0 then
    Result := BASS_ChannelPlay(FStream, True)
  else
    Result := False;
  if Result then
    StatusChange(TRadioStatus.Playing)
  else
    StatusChange(TRadioStatus.Unknown);
end;

function TRadioPlayer.Resume: Boolean;
begin
  if ShouldUseService then
  begin
    FServiceController.Play;
    Result := True;
  end
  else
    Result := DoBASSResume;
end;

function TRadioPlayer.DoBASSPlay: Boolean;
var
  LTags: TArray<string>;
begin
  Result := False;
  FStream := BASS_StreamCreateURL(PChar(FURL), 0, BASS_STREAM_BLOCK or BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE or BASS_UNICODE, nil, nil);
  if FStream > 0 then
  begin
    if GetTags(BASS_TAG_ICY, LTags) or GetTags(BASS_TAG_HTTP, LTags) then
    begin
      {
      if LTag.StartsWith(cIcyNamePrefix, True) then
        LBroadcastName := LTag.Substring(Length(cIcyNamePrefix))
      else if LTag.StartsWith(cIcyBitratePrefix, True) then
        LBitRate := LTag.Substring(Length(cIcyBitratePrefix));
      if LBroadcastName.IsEmpty and not LBitRate.IsEmpty then
        LBroadcastName := 'Unknown';
      }
      {$IF Defined(SERVICE)}
      TRadioServiceHelper.ReceivedStreamMetadata(LTags);
      {$ENDIF}
      DoStreamMetadata(LTags);
    end;
    // Get metadata where there may not be any callback
    DoMetaSync;
    BASS_ChannelSetSync(FStream, BASS_SYNC_META, 0, @BASSSyncMeta, Self);
    // BASS_ChannelSetSync(FStream, BASS_SYNC_POS, 0, @BASSSyncPos, Self);
    BASS_ChannelSetSync(FStream, BASS_SYNC_END, 0, @BASSSyncEnd, Self);
    Result := BASS_ChannelPlay(FStream, False);

    // FStatusProc(strCompleted,100);
    if Result then
      StatusChange(TRadioStatus.Playing)
    else
      StatusChange(TRadioStatus.Unknown);
  end
  else
    DoError;
end;

function TRadioPlayer.DoPlay: Boolean;
begin
  if ShouldUseService then
  begin
    FServiceController.Play(FURL);
    Result := True;
  end
  else
    Result := DoBASSPlay;
end;

{$IF Defined(IOS)}
// libbass.a was obatined from:
//   https://github.com/TDDung/Delphi-BASS/blob/main/BassVersions/BASS/Core/Libs/iOS/libbass.a
const
  libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
  libAVFAudio = '/System/Library/Frameworks/AVFAudio.framework/AVFAudio';
  libCFNetwork = '/System/Library/Frameworks/CFNetwork.framework/CFNetwork';

procedure AudioToolboxLoader; cdecl; external libAudioToolbox;
procedure AVFAudioLoader; cdecl; external libAVFAudio;
procedure CFNetworkLoader; cdecl; external libCFNetwork;
{$ENDIF}

end.

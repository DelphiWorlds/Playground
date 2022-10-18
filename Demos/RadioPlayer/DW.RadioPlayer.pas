unit DW.RadioPlayer;

interface

{$IF Defined(ANDROID) and not Defined(SERVICE)}
  {$DEFINE ANDROIDAPP}
{$ENDIF}

uses
  System.Classes,
  Bass,
  DW.RadioPlayer.ServiceController, DW.RadioPlayer.Common;

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
    function GetTags(const ATagIndex: Cardinal; out ATags: string): Boolean;
    function LoadPlugin: Boolean;
    function Resume: Boolean;
    {$IF Defined(ANDROIDAPP)}
    procedure ServiceControllerRadioStatusChangedHandler(Sender: TObject; const AStatus: TRadioStatus);
    procedure ServiceControllerServiceStartedHandler(Sender: TObject);
    procedure ServiceControllerStreamMetadataHandler(Sender: TObject; const AMetadata: string);
    {$ENDIF}
    function ShouldUseService: Boolean;
    procedure StatusChange(const AStatus: TRadioStatus);
    procedure Stopped;
  protected
    procedure DoEndSync;
    procedure DoMetaSync;
    procedure DoPosSync(const AData: DWORD);
    procedure DoStreamMetadata(const AMetadata: string);
  public
    constructor Create;
    destructor Destroy; override;
    function ParseStreamTitle(const AMetadata: string; const ADefault: string = ''): string;
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

implementation

uses
  DW.OSLog,
  {$IF Defined(SERVICE)}
  DW.RadioPlayer.ServiceHelper,
  {$ENDIF}
  System.SysUtils;

const
  {$IF Defined(MSWINDOWS)}
  cBASSPluginLib = 'bass_aac.dll';
  {$ELSE}
  cBASSPluginLib = 'libbass_aac.so';
  {$ENDIF}
  cIcyNamePrefix = 'icy-name:';
  cIcyBitratePrefix = 'icy-br:';

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

(*
procedure TFMXPlatformRadio.DoMeta();
var
  meta: MarshaledAString;
  line: string;
  p: Integer;
begin
  meta := BASS_ChannelGetTags(FActiveChannel, BASS_TAG_META);
  if (meta <> nil) then
  begin
    line:=UTF8Decode(meta);
    p := Pos('StreamTitle=', line);
    if (p = 0) then
      Exit;
    p := p + 13;

    if Assigned(FBroadcastMetaProc)
      then begin
               FBroadcastMetaProc(Copy(meta, p, Pos(';', line) - p - 1));
           end;
  end;
end;
*)

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

procedure TRadioPlayer.ServiceControllerStreamMetadataHandler(Sender: TObject; const AMetadata: string);
begin
  DoStreamMetadata(AMetadata);
end;
{$ENDIF}

procedure TRadioPlayer.DoServiceStarted;
begin
  if Assigned(FOnServiceStarted) then
    FOnServiceStarted(Self);
end;

procedure TRadioPlayer.DoStreamMetadata(const AMetadata: string);
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

function TRadioPlayer.GetTags(const ATagIndex: Cardinal; out ATags: string): Boolean;
var
  LTagAString: MarshaledAString;
begin
  Result := False;
  LTagAString := BASS_ChannelGetTags(FStream, ATagIndex);
  if LTagAString <> nil then
  begin
    ATags := UTF8ToString(LTagAString);
    Result := True;
  end;
end;

procedure TRadioPlayer.DoMetaSync;
var
  LMeta: string;
begin
  // e.g. StreamTitle='Stephen Howie - The Grooveline Series';
  if GetTags(BASS_TAG_META, LMeta) then
  begin
    {$IF Defined(SERVICE)}
    TRadioServiceHelper.ReceivedStreamMetadata(LMeta);
    {$ENDIF}
    DoStreamMetadata(LMeta);
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

function TRadioPlayer.ParseStreamTitle(const AMetadata: string; const ADefault: string = ''): string;
begin
  if AMetadata.StartsWith(cMetadataStreamTitlePrefix) then
    Result := AnsiDequotedStr(AMetadata.Substring(Length(cMetadataStreamTitlePrefix)).Trim([';']), '''')
  else
    Result := ADefault;
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
  LTag, LBroadcastName, LBitRate: string;
begin
  TOSLog.d('TRadioPlayer.DoBASSPlay');
  Result := False;
  FStream := BASS_StreamCreateURL(PChar(FURL), 0, BASS_STREAM_BLOCK or BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE or BASS_UNICODE, nil, nil);
  if FStream > 0 then
  begin
    if GetTags(BASS_TAG_ICY, LTag) then
    begin
      if LTag.StartsWith(cIcyNamePrefix, True) then
        LBroadcastName := LTag.Substring(Length(cIcyNamePrefix))
      else if LTag.StartsWith(cIcyBitratePrefix, True) then
        LBitRate := LTag.Substring(Length(cIcyBitratePrefix));
      // Might be more than one tag?
      // DoBroadcastInfo(LBroadcastName, LBitRate);
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
  TOSLog.d('TRadioPlayer.DoPlay');
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

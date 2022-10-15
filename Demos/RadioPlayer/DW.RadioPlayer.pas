unit DW.RadioPlayer;

interface

{$IF Defined(ANDROID) and not Defined(SERVICE)}
  {$DEFINE ANDROIDAPP}
{$ENDIF}

uses
  System.Classes,
  Bass,
  DW.RadioServiceController, DW.RadioPlayer.Common;

type
  TRadioStatus = DW.RadioPlayer.Common.TRadioStatus;

  TRadioPlayer = class(TObject)
  private
    FPluginHandle: HPLUGIN;
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
    FOnServiceStarted: TNotifyEvent;
    FOnStatusChanged: TNotifyEvent;
    procedure StatusChange(const AStatus: TRadioStatus);
    procedure DoError;
    function DoBASSPause: Boolean;
    function DoBASSPlay: Boolean;
    function DoBASSResume: Boolean;
    function DoPlay: Boolean;
    procedure DoServiceStarted;
    function LoadPlugin: Boolean;
    function Resume: Boolean;
    procedure ServiceControllerRadioStatusChangedHandler(Sender: TObject; const AStatus: TRadioStatus);
    procedure ServiceControllerServiceStartedHandler(Sender: TObject);
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
    property OnServiceStarted: TNotifyEvent read FOnServiceStarted write FOnServiceStarted;
    property OnStatusChanged: TNotifyEvent read FOnStatusChanged write FOnStatusChanged;
  end;

implementation

uses
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
  end;
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

procedure TRadioPlayer.ServiceControllerRadioStatusChangedHandler(Sender: TObject; const AStatus: TRadioStatus);
begin
  StatusChange(AStatus);
end;

procedure TRadioPlayer.ServiceControllerServiceStartedHandler(Sender: TObject);
begin
  DoServiceStarted;
end;

procedure TRadioPlayer.DoServiceStarted;
begin
  if Assigned(FOnServiceStarted) then
    FOnServiceStarted(Self);
end;

procedure TRadioPlayer.StartService(const AServiceName: string);
begin
  if FServiceController <> nil then
    FServiceController.StartService(AServiceName)
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
  if LCode <> 0 then
    Sleep(0);
end;

procedure TRadioPlayer.DoMetaSync;
var
  LMeta: string;
begin
  LMeta := UTF8ToString(BASS_ChannelGetTags(FStream, BASS_TAG_META));
  Sleep(0);
  // DoMeta(LMeta);
end;

procedure TRadioPlayer.DoPosSync(const AData: DWORD);
begin
  //
end;

function TRadioPlayer.LoadPlugin: Boolean;
begin
  Result := False;
  if FServiceController = nil then
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
  if Assigned(FOnStatusChanged) then
    FOnStatusChanged(Self);
end;

procedure TRadioPlayer.Stop;
begin
  if FStream > 0 then
    BASS_StreamFree(FStream);
  FStream := 0;
  Stopped;
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
  if FServiceController <> nil then
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
  end;
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
  if FServiceController <> nil then
  begin
    FServiceController.Play;
    Result := True;
  end
  else
    Result := DoBASSResume;
end;

function TRadioPlayer.DoBASSPlay: Boolean;
var
  LTagAString: MarshaledAString;
  LTag, LBroadcastName, LBitRate: string;
begin
  Result := False;
  FStream := BASS_StreamCreateURL(PChar(FURL), 0, BASS_STREAM_BLOCK or BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE or BASS_UNICODE, nil, nil);
  if FStream > 0 then
  begin
    // +Tag
    LTagAString := BASS_ChannelGetTags(FStream, BASS_TAG_ICY);
    if LTagAString = nil then
      LTagAString := BASS_ChannelGetTags(FStream, BASS_TAG_HTTP);
    if LTagAString <> nil then
    begin
      LTag := UTF8ToString(LTagAString);
      if LTag.StartsWith(cIcyNamePrefix, True) then
        LBroadcastName := LTag.Substring(Length(cIcyNamePrefix))
      else if LTag.StartsWith(cIcyBitratePrefix, True) then
        LBitRate := LTag.Substring(Length(cIcyBitratePrefix));
      // Might be more than one tag?
      // DoBroadcastInfo(LBroadcastName, LBitRate);
    end;
    // DoMeta
    // -Tag
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
  if FServiceController <> nil then
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

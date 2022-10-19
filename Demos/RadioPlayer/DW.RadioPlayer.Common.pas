unit DW.RadioPlayer.Common;

interface

const
  cServiceStateRadioBase = 100;

  cServiceCommandRadioSetURL = 100;
  cServiceCommandRadioPlay = 101;
  cServiceCommandRadioStop = 102;
  cServiceCommandRadioPause = 103;
  cServiceCommandRadioVolumeUp = 104;
  cServiceCommandRadioVolumeDown = 105;

  cServiceMessageTypeRadioStreamMetadata = 1;

  cMetadataStreamTitlePrefix = 'StreamTitle=';

type
  TRadioStatus = (Unknown, Stopped, Playing, Paused);

  TRadioCommand = record
    Command: Integer;
    URL: string;
    constructor Create(const ACommand: Integer; const AURL: string = '');
    procedure FromJSON(const AJSON: string);
    function ToJSON: string;
  end;

  TStreamMetadataEvent = procedure(Sender: TObject; const Metadata: TArray<string>) of object;

implementation

uses
  System.JSON, System.SysUtils;

{ TRadioCommand }

constructor TRadioCommand.Create(const ACommand: Integer; const AURL: string);
begin
  Command := ACommand;
  URL := AURL;
end;

procedure TRadioCommand.FromJSON(const AJSON: string);
var
  LJSON: TJSONValue;
begin
  LJSON := TJSONObject.ParseJSONValue(AJSON);
  if LJSON <> nil then
  try
    LJSON.TryGetValue('Command', Command);
    LJSON.TryGetValue('URL', URL);
  finally
    LJSON.Free;
  end;
end;

function TRadioCommand.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('Command', TJSONNumber.Create(Command));
    if not URL.IsEmpty then
      LJSON.AddPair('URL', TJSONString.Create(URL));
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

end.

unit DW.RadioPlayer.ServiceHelper;

interface

uses
  DW.RadioPlayer.Common;

type
  TRadioServiceHelper = record
  private
    class procedure SendMessage(const AMessage: string); static;
  public
    class procedure RadioStatusChanged(const AStatus: TRadioStatus); static;
    class procedure ReceivedStreamMetadata(const AMetadata: TArray<string>); static;
    class procedure SendState(const AState: Integer); static;
  end;

implementation

uses
  System.JSON,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  {$IF CompilerVersion < 35}
  DW.Androidapi.JNI.SupportV4,
  {$ELSE}
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager, DW.Androidapi.JNI.AndroidX.App,
  {$ENDIF}
  DW.Consts.Android;

{ TRadioServiceHelper }

class procedure TRadioServiceHelper.RadioStatusChanged(const AStatus: TRadioStatus);
begin
  SendState(cServiceStateRadioBase + Ord(AStatus));
end;

class procedure TRadioServiceHelper.ReceivedStreamMetadata(const AMetadata: TArray<string>);
var
  LJSON: TJSONObject;
  LValues: TJSONArray;
  LValue: string;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('MessageType', cServiceMessageTypeRadioStreamMetadata);
    LValues := TJSONArray.Create;
    for LValue in AMetadata do
      LValues.Add(LValue);
    LJSON.AddPair('Content', LValues);
    SendMessage(LJSON.ToJSON);
  finally
    LJSON.Free;
  end;
end;

class procedure TRadioServiceHelper.SendMessage(const AMessage: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceMessageAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamMessage), StringToJString(AMessage));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

class procedure TRadioServiceHelper.SendState(const AState: Integer);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceStateAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamState), AState);
  // TOSLog.d('Sending state broadcast: %s', [JStringToString(LIntent.toURI)]);
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

end.

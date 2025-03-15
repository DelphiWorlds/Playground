unit DW.BroadcastMessage.Sender;

interface

type
  TBroadcastMessageSender = record
  public
    class procedure Send(const AKind: Integer; const AMsg: string); static;
  end;

implementation

uses
  AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNI.JavaTypes, Androidapi.Helpers,
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager, DW.BroadcastMessage.Consts;

class procedure TBroadcastMessageSender.Send(const AKind: Integer; const AMsg: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(BroadcastMessageConsts.ACTION_MESSAGE);
  LIntent.putExtra(BroadcastMessageConsts.EXTRA_MESSAGE_KIND, AKind);
  LIntent.putExtra(BroadcastMessageConsts.EXTRA_MESSAGE, StringToJString(AMsg));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

end.

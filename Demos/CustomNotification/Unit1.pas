unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, System.Notification, FMX.Objects;

type
  TForm1 = class(TForm)
    NotificationCenter: TNotificationCenter;
    Button1: TButton;
    Image: TImage;
    procedure Button1Click(Sender: TObject);
  private
    procedure CreateChannel;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Widget, Androidapi.JNI.GraphicsContentViewText,
  // DW.Androidapi.JNI.SupportV4,  <--- 10.4
  DW.Androidapi.JNI.AndroidX.App, Androidapi.JNI.App,
  DW.Android.Helpers, DW.Graphics.Helpers.Android;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  CreateChannel;
end;

procedure TForm1.CreateChannel;
var
  LChannel: TChannel;
begin
  LChannel := NotificationCenter.CreateChannel;
  try
    LChannel.Id := 'CNTest';
    LChannel.Title := LChannel.Id;
    LChannel.Importance := TImportance.High;
    NotificationCenter.CreateOrUpdateChannel(LChannel);
  finally
    LChannel.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LBuilder: JNotificationCompat_Builder;
  LContentSmall, LContentBig: JRemoteViews;
  LTitle, LBody: string;
begin
  LTitle := 'Testing';
  LBody := 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.' +
    'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.' +
    'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.' +
    'Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum';

  LContentBig := TJRemoteViews.JavaClass.init(TAndroidHelper.Context.getPackageName, TAndroidHelper.GetResourceID('layout/notification_big'));
  LContentBig.setTextViewText(TAndroidHelper.GetResourceID('id/notification_big_title'), StrToJCharSequence(LTitle));
  LContentBig.setTextViewText(TAndroidHelper.GetResourceID('id/notification_big_body'), StrToJCharSequence(LBody));
  LContentBig.setImageViewBitmap(TAndroidHelper.GetResourceID('id/notification_big_image'), Image.Bitmap.ToJBitmap);

  LContentSmall := TJRemoteViews.JavaClass.init(TAndroidHelper.Context.getPackageName, TAndroidHelper.GetResourceID('layout/notification_small'));
  LContentSmall.setTextViewText(TAndroidHelper.GetResourceID('id/notification_small_title'), StrToJCharSequence(LTitle));
  LContentSmall.setTextViewText(TAndroidHelper.GetResourceID('id/notification_small_body'), StrToJCharSequence(LBody));
  LContentSmall.setImageViewBitmap(TAndroidHelper.GetResourceID('id/notification_small_image'), Image.Bitmap.ToJBitmap);

  LBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context)
    .setChannelId(StringToJString('CNTest'))
    .setSmallIcon(TAndroidHelper.GetResourceID('drawable/ic_notification'))
    .setCustomContentView(LContentSmall)
    .setCustomBigContentView(LContentBig)
    .setStyle(TJNotificationCompat_DecoratedCustomViewStyle.JavaClass.init)
    .setContentTitle(StrToJCharSequence(LTitle))
    .setContentText(StrToJCharSequence(LBody));
  Tag := Tag + 1;
  TAndroidHelperEx.NotificationManager.notify(Tag, LBuilder.build);
end;

end.

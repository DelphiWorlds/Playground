unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText; // , Androidapi.JNI.Net;

type
  JCrash = interface;

  JCrashClass = interface(JObjectClass)
    ['{4798A4C8-B033-49E7-97B9-349874ACABC9}']
    function init: JCrash; cdecl;
  end;

  [JavaSignature('com/crash/Crash')]
  JCrash = interface(JObject)
    ['{07ABF6E5-B235-4B43-A723-717C54CB837F}']
  end;
  TJCrash = class(TJavaGenericImport<JCrashClass, JCrash>) end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  raise Exception.Create('Delphi native exception');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LCrash: JCrash;
begin
  LCrash := TJCrash.JavaClass.init;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  LIntent: JIntent;
begin
  // LIntent contains either nil, or garbage - either way it will cause an Android exception
  LIntent := TJIntent.JavaClass.init(LIntent);
end;

end.

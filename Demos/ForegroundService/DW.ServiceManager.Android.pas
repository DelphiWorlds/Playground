unit DW.ServiceManager.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNI.GraphicsContentViewText;

type
  IServiceManager = interface(IInterface)
    ['{6CB84FA4-A95B-4F6A-99A6-21A18EAEB591}']
    function IsServiceRunning(const AServiceName: string): Boolean;
    procedure StartService(const AServiceName: string; const AIntent: JIntent = nil);
    procedure StopService(const AServiceName: string);
  end;

var
  ServiceManager: IServiceManager;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.Helpers, Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.Os;

const
  EMBTJavaServicePrefix = 'com.embarcadero.services.';

type
  TServiceManager = class(TInterfacedObject, IServiceManager)
  private
    function GetFullServiceName(const AServiceName: string): string;
    function GetRunningServiceInfo(const AServiceName: string): JActivityManager_RunningServiceInfo;
  public
    { IServiceManager }
    function IsServiceRunning(const AServiceName: string): Boolean;
    procedure StartService(const AServiceName: string; const AIntent: JIntent = nil);
    procedure StopService(const AServiceName: string);
  end;

{ TServiceManager }

function TServiceManager.GetRunningServiceInfo(const AServiceName: string): JActivityManager_RunningServiceInfo;
var
  LActivityManager: JActivityManager;
  LRunningServices: JList;
  LServiceInfo: JActivityManager_RunningServiceInfo;
  LServiceName: string;
  I: Integer;
begin
  Result := nil;
  LServiceName := GetFullServiceName(AServiceName);
  LActivityManager := TJActivityManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.ACTIVITY_SERVICE));
  LRunningServices := LActivityManager.getRunningServices(MaxInt);
  for I := 0 to LRunningServices.size - 1 do
  begin
    LServiceInfo := TJActivityManager_RunningServiceInfo.Wrap(TAndroidHelper.JObjectToID(LRunningServices.get(I)));
    if LServiceName.Equals(JStringToString(LServiceInfo.service.getClassName)) then
    begin
      Result := LServiceInfo;
      Break;
    end;
  end;
end;

function TServiceManager.GetFullServiceName(const AServiceName: string): string;
begin
  Result := AServiceName;
  if not Result.Contains('.') and not Result.StartsWith(EMBTJavaServicePrefix) then
    Result := EMBTJavaServicePrefix + Result;
end;

procedure TServiceManager.StartService(const AServiceName: string; const AIntent: JIntent);
var
  LIntent: JIntent;
begin
  LIntent := AIntent;
  if LIntent = nil then
    LIntent := TJIntent.JavaClass.init;
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(GetFullServiceName(AServiceName)));
  TAndroidHelper.Activity.startService(LIntent);
end;

procedure TServiceManager.StopService(const AServiceName: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init;
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(GetFullServiceName(AServiceName)));
  TAndroidHelper.Activity.stopService(LIntent);
end;

function TServiceManager.IsServiceRunning(const AServiceName: string): Boolean;
begin
  Result := GetRunningServiceInfo(AServiceName) <> nil;
end;

initialization
  ServiceManager := TServiceManager.Create;

end.

unit DW.LocationService.Manager;

interface

uses
  Androidapi.JNI.GraphicsContentViewText,
  DW.LocationService.Common;

type
  ILocationServiceManager = interface(IInterface)
    ['{7FB512CF-308D-4239-AF63-413F914B0D95}']
    procedure StartLocationUpdates; overload;
    procedure StartLocationUpdates(const AInfo: TLocationServiceInfo); overload;
    procedure StopLocationUpdates;
  end;

  TLocationServiceManager = class(TInterfacedObject, ILocationServiceManager)
  private
    FServiceName: string;
    procedure StartService(const AIntent: JIntent);
  public
    { ILocationServiceManager }
    procedure StartLocationUpdates; overload;
    procedure StartLocationUpdates(const AInfo: TLocationServiceInfo); overload;
    procedure StopLocationUpdates;
  public
    constructor Create(const AServiceName: string);
  end;

implementation

uses
  System.SysUtils,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.App,
  DW.OSLog, DW.FusedLocation;

const
  EMBTJavaServicePrefix = 'com.embarcadero.services.';

{ TLocationServiceManager }

constructor TLocationServiceManager.Create(const AServiceName: string);
begin
  inherited Create;
  FServiceName := AServiceName;
  if not FServiceName.StartsWith(EMBTJavaServicePrefix) then
    FServiceName := EMBTJavaServicePrefix + FServiceName;
end;

procedure TLocationServiceManager.StartService(const AIntent: JIntent);
begin
  AIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(FServiceName));
  TAndroidHelper.Activity.startService(AIntent);
  TOSLog.d('Called startService for: %s', [FServiceName]);
end;

procedure TLocationServiceManager.StopLocationUpdates;
var
  LInfo: TLocationServiceInfo;
begin
  LInfo.IsActive := False;
  StartService(LInfo.ToIntent);
end;

procedure TLocationServiceManager.StartLocationUpdates;
var
  LInfo: TLocationServiceInfo;
begin
  LInfo.IsActive := True;
  LInfo.Options := TFusedLocationOptions.Defaults;
  StartService(LInfo.ToIntent);
end;

procedure TLocationServiceManager.StartLocationUpdates(const AInfo: TLocationServiceInfo);
begin
  StartService(AInfo.ToIntent);
end;

end.

program BeaconsDemoService;

uses
  System.Android.ServiceApplication,
  BD.ServiceModule in 'BD.ServiceModule.pas' {ServiceModule: TAndroidIntentService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.

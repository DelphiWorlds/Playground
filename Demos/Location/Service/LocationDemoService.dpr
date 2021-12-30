program LocationDemoService;

uses
  System.Android.ServiceApplication,
  LD.ServiceModule in 'LD.ServiceModule.pas' {ServiceModule: TAndroidIntentService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.

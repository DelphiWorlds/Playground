program ABLDemoService;

uses
  System.Android.ServiceApplication,
  ABL.ServiceModule in 'ABL.ServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.

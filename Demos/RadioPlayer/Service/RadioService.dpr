program RadioService;

uses
  System.Android.ServiceApplication,
  RT.ServiceModule in 'RT.ServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.

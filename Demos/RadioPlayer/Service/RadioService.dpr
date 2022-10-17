program RadioService;

uses
  System.Android.ServiceApplication,
  RT.ServiceModule in 'RT.ServiceModule.pas' {ServiceModule: TAndroidService},
  DW.RadioPlayer.ServiceHelper in '..\DW.RadioPlayer.ServiceHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.

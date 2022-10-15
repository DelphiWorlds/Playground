object ServiceModule: TServiceModule
  OnCreate = AndroidServiceCreate
  OnDestroy = AndroidServiceDestroy
  OnStartCommand = AndroidServiceStartCommand
  Height = 238
  Width = 324
end

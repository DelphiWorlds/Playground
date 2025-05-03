object ServiceModule: TServiceModule
  OnCreate = AndroidServiceCreate
  OnDestroy = AndroidServiceDestroy
  OnStartCommand = AndroidServiceStartCommand
  Height = 298
  Width = 405
  PixelsPerInch = 120
end

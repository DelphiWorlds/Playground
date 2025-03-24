# Android Background Location test project

## Purpose

Potential replacement for the Cross Platform Location demo, **except for Android ONLY**.

Intended to overcome difficulties in providing continuous location updates in newer versions of Android, paticularly Android 10, Android 12 etc

**NOTE: The demo project has been updated for Delphi 12.3**. If you wish to use it with an earlier version of Delphi, you will need to use the `Revert System files to Default` function, as per [this information](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AndroidLibraries).

## Description

This app has been largely refactored from the original (mentioned above) to separate some of the functionality so that parts can be easily left out when required, or include implementations of your own for those parts.

Aside from relying on some code from Kastri, this demo should otherwise be self-contained, including its own copy of the dependent jars.

Incorporates suggestions made by Alex Sawers from [this issue on the Kastri repo](https://github.com/DelphiWorlds/Kastri/issues/248).

Changes made in early March 2025 have greatly improved the reliability.

## Project Configuration

These instructions are for when you are creating your own project. They have already been included in the demo.

In Project Options of the application:

In the Uses Permissions section for both Android 32-bit and Android 64-bit targets:

Standard
* Foreground service
* Foreground service location

"Dangerous"
* Access background location

If you use a MapView (such as the demo does), in the Version Info section, enter your [`apiKey` for Google Maps](https://developers.google.com/maps/documentation/android-sdk/get-api-key).

In `AndroidManifest.template.xml` for the application:

**Replace** the `<%services%>` tag with:

```xml
  <service android:name="com.embarcadero.services.ABLDemoService" android:exported="false" android:foregroundServiceType="location" />
```

This is because Delphi is yet (as of Delphi 12.3) to support having the `foregroundServiceType` attribute

**Add** the following _after_ the `<%application-meta-data%>` tag:

```xml
  <meta-data android:name="com.google.android.gms.version" android:value="12451000" />
  <meta-data android:name="com.delphiworlds.kastri.DWFusedLocationClient.KEY_SERVICE_CLASS_NAME" android:value="com.embarcadero.services.ABLDemoService" />
```

The meta-data second entry is used by the DWFusedLocationClient class to determine which service is passed to the Android `FusedLocationProviderClient` class when location updates occur. If the service in your application has a different name, you will need to change the `android:value` attribute value to the fully qualified name of your service. 

## Usage

As per the description, much of the code has been refactored into parts that can be easily swapped out or replaced with your own code. 

### Permissions

The `DW.LocationPermissions` unit contains an implementation of runtime permission requests that ensure that sufficient permissions are granted in order for location updates to start.

Call `RequestPermissions` on the `LocationPermissions` reference, which kicks off the permission requests workflow, and handle the result in an anonymous method passed to the call, as per the example in Unit1.

### Start/Stop Location updates

Location updates are started/stopped in the demo by calling `StartLocationUpdates`/`StopLocationUpdates` on an instance of `TLocationServiceManager`. Ultimately this code calls `startService` using an `Intent` that contains information about whether the updates are started/stopped, and if started, what properties the location updates request should have.

### Receiving location updates in the application

When location updates are received by the service, or when the location updates become active/inactive, the service sends a broadcast message, which the application can receive via a message receiver. In the demo, these messages are handled by the `ServiceMessageHandler` method, which will plot points and draw lines between them in the MapView.

## Technical information

Location updates are provided using a Java class in `dw-fused-location-3.0.0.jar` called `DWFusedLocationClient`, which the Delphi class `TFusedLocation` uses to control the location updates. This class is used by the Service (which is in the `ABLDemoService` project in the demo).

The application code controls starting and stopping of location updates by starting the service with an `Intent` that contains information about whether location updates to be started or stopped, and if they are to be started, the properties of the location updates, such as the interval etc.

```delphi
procedure TForm1.StartStopLocationButtonClick(Sender: TObject);
begin
  if not FPreferences.GetIsActive then
  begin
    LocationPermissions.RequestBackground(
      procedure(const ACanStart: Boolean)
      begin
        if ACanStart then
          FServiceManager.StartLocationUpdates
      end
    );
  end
  else
    FServiceManager.StopLocationUpdates;
end;
```

In this demo code, `StartLocationUpdates` without parameters means that the default options will be used. `FServiceManager` is a reference to `ILocationServiceManager`

When location updates occur, the Android location code starts the service in the application with an `Intent` that contains the location information.

When the service starts, the `AndroidServiceStartCommand` is called, and the `Intent` is examined as to whether it contains start/stop information, then the `CheckIntent` method of the `IFusedLocation` reference is called to determine whether or not the `Intent` contains location update information:

```delphi
function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
var
  LInfo: TABLServiceInfo;
begin
  LInfo := TABLServiceInfo.Create(Intent);
  // If the service was *not* started from the app, just start it in the foreground.
  // There is no RELIABLE way to know whether the app is in the foreground - the system could kill it and not be able to persist its state anywhere
  if CanStartForeground and not LInfo.IsServiceInfo then
    TForegroundServiceHelper.StartForeground(JavaService, 'ABL Demo', 'Service in foreground');
  CheckServiceInfo(LInfo);
  FFusedLocation.CheckIntent(Intent);
  // START_NOT_STICKY is returned, as the service does not need to continue running if the application is.
  Result := TJService.JavaClass.START_NOT_STICKY;
end;
```

If the `Intent` does contain location information `CheckIntent` method will call back into the `LocationReceived` method of the service.

The `LocationReceived` method then sends a broadcast message that the application can receive, if the application needs the location information for any reason. The service can also perform other tasks using the location information, such as sending it to a server, as per the example code in the demo:

```delphi
procedure TServiceModule.LocationReceived(const ALocation: JLocation);
var
  LData: TLocationData;
begin
  LData.FromLocation(ALocation);
  TOSLog.d('Received: %.5f, %.5f', [LData.Location.Latitude, LData.Location.Longitude]);
  // Uncomment this line when using a REST server that can update location data
  // FLocationSender.SendLocation(LData);
  // Message type 0 is a notification of a location update
  TBroadcastMessageSender.Send(0, LData.ToJSON);
end;
```

## Status

* March 18th, 2025

  - Added MapView, plotting of locations received by the app
  - Ensured location updates default to high accuracy

* March 15th, 2025

  - No longer fails when the app is quit
  - Is yet to support "start on boot"

* Sep 27th, 2024
  
  - Works OK when the app is active, or the app is switched to the background
  - Fails when the app is quit. Please refer to [this Stack Overflow post](https://stackoverflow.com/questions/70044393/fatal-android-12-exception-startforegroundservice-not-allowed-due-to-mallows) for information about why.


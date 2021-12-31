# Location Demo

## Purpose

To replace the existing Cross-Platform Location Demo in Kastri, subsituting a standard Service with a JobIntentService (even though this is deprecated)

## Description

### Android

For the Android platform, this demo (as per a later version of the original demo) uses the Fused Location API. The Fused Location API support in Kastri has been modified to cater for the use of a BroadcastReceiver (part of dw-fusedlocation-2.0.0.jar) which receives location update intents sent by location services. These intents are forwarded on to the service part of the application.

This means that the Delphi code no longer needs to start a service - the service will be invoked when the location updates are sent. This also means that the service code is greatly simplified.

### iOS

The iOS implementation is a complete rewrite of the original, which was based on TLocationSensor. It now behaves as it should, e.g.:

* When setting IsActive to True, it requests permissions **first**, and waits for the authorization result before attempting to start location monitoring
* Location data is now retrieved from the underlying CLLocation itself, rather than relying on the Sensor property of TLocationSensor.
* The application state is included in the location data passed to the event. `Normal` means that the application was launched normally, by the user. `Background` means that the app was launched by the user, but is now in the background. `Hidden` means that the app was launched by the system but not shown. This is how background location updates happen.

## Creating your own project from scratch

If starting your own project (rather than basing it on the demo), these are suggested steps:

1. Start a new blank multi-device application
2. Save the application to a folder called `Application`, under what would be the root folder for your application and service source
3. Right click `ProjectGroup1` in the Project Manager
4. Click `Add` > `New Project`
5. Select Android Service and click OK
6. Select **Intent Local Service**
7. Save the service to a folder called `Service`, under the root folder, like in step 2, and make the service project name the same as the application project, but with a suffix of `Service`
8. Save group as the name of the application, suffixed with `Group`

## Project Configuration

### AndroidManifest.template.xml

Add the following, after `<%application-meta-data%>`:

```
        <meta-data android:name="com.delphiworlds.kastri.DWFusedLocationClient.KEY_SERVICE_CLASS_NAME" android:value="com.embarcadero.services.LocationDemoService" />
        <meta-data android:name="com.google.android.gms.version" android:value="12451000" />
```

The first meta-data entry tells the broadcast receiver (DWLocationReceiver, mentioned below) what the name of the service is that the location update intents should be forwarded to.

**Note: If you have a different name for your service, you will need to replace the `LocationDemoService` value with your own.**

Add the following, after `<%receivers%>`:

```
  <receiver android:name="com.delphiworlds.kastri.DWLocationReceiver" />
```

### Permissions

In the project options, ensure that the application has the `Access Background Location` permission checked.

### Version Info

For iOS, ensure that the `location` option is selected for `UIBackgroundModes`

## Status of this demo (Dec 30th, 2021)

### General status

* In the process of determining whether it fits requirements of those already using projects based on the original demo. 
* Testing, testing, testing...

### Android status

According to my research, location updates should still be sent when the device is in "doze" mode - usually if location updates occur it means the device has moved sufficiently to bring it out of "doze" mode anyway.

If your app needs to do "something" (like a "keep-alive") at regular intervals regardless of whether the device has moved, a technique similar to [this one in the How To repo](https://github.com/DelphiWorlds/HowTo/tree/main/Demos/GenericJob) could be incorporated into the code in this demo. 


# Foreground Service Demo

## Description

Demonstrates how to implement foreground services of certain types, as described by the [Android documentation](https://developer.android.com/develop/background-work/services/fgs/service-types). 

As per the documentation, foreground service types are now categorized, the aim being to allow certain types have certain restrictions placed on them depending what they do. 

If your service needs to provide data synchronization (for example), you might choose the `dataSync` `foregroundServiceType`. Having said that, if the app targets Android 15 or higher, the service cannot be started at boot time.

In contrast, the `connectedDevice` foreground service type **can** be started at boot, however it also requires at least ONE of these permissions:

"Standard"
* `CHANGE_NETWORK_STATE`
* `CHANGE_WIFI_STATE`

"Dangerous" (needs to be requested)
* `BLUETOOTH_CONNECT`
* `BLUETOOTH_ADVERTISE`
* `BLUETOOTH_SCAN`
* `UWB_RANGING`

This demo will cover both these types, and you can choose which one, or another service type entirely, depending on your needs - just remember that if you need your service to start at boot time, you will need to choose one that Android allows. This is the full list of those that are **NOT** permitted to start at boot time:

* `dataSync`
* `camera`
* `mediaPlayback`
* `phoneCall`
* `mediaProjection`
* `microphone`

Which all make sense, except for `dataSync`

## Supported Delphi versions

Delphi 12, Delphi 11.x.

## Project Configuration

If you are creating a new project (i.e. other than the demo) you will need to follow the instructions in this section.

### Permissions

Foreground services require at least the Foreground service permission, and one of the Foreground service type permissions (the demo uses Foreground service connected device)

Depending on the foreground service type, it may require other permissions depending on the type, as described in the Description section

### Dependent libraries

Add these jars to the `Libraries` node under the Android 32-bit platform in Project Manager:

* `dw-kastri-base-3.0.0.jar` (or `dw-kastri-base-2.0.0.jar` for Delphi 11.x)
* `dw-startservice.jar` - **at present, this jar is required only if your service needs to start at boot time**

..which are located in the `Lib` folder of the [Kastri repo](https://github.com/DelphiWorlds/Kastri). The demo uses copies (at the time this project was last updated) of these libraries

### Manifest changes

#### Service entry

As of Delphi 12.3, there is yet to be support for configuring a service to have a `foregroundServiceType` value, so the `<%services%>` tag in `AndroidManifest.template.xml` will need to be replaced manually (since normally, the IDE would do this). You will need to deploy the project at least once so that `AndroidManifest.template.xml` is generated, then it can be modified.

In the case of this demo, the entry is:

```xml
    <service android:name="com.embarcadero.services.FSDemoService" android:exported="false" android:foregroundServiceType="connectedDevice">
        <meta-data android:name="startAtBoot" android:value="true" />
    </service>
```

As per the Description section, change the value for `foregroundServiceType` to a relevant value for your app. NOTE: the `meta-data` entry is used by the receiver (see below) in order to determine whether or not the service should be started at boot time. The `meta-data` entry (and the `receiver` entry - see below) can be omitted if the service does not need to start at boot

#### Receiver entry

If you require the service to start at boot, a receiver that can respond to the `BOOT_COMPLETED` system broadcast needs to be registered in the manifest. The jar mentioned earlier (`dw-startservice.jar`) has such a receiver which contains code that at boot time, will examine the app manifest and start any services that have the relevant `meta-data` entry (see above).

If this is the case for you, add this to just after `<%receivers%>` in `AndroidManifest.template.xml` (i.e. do not completely replace the `<%receivers%>` tag):

```xml
    <receiver android:name="com.delphiworlds.kastri.DWStartServiceReceiver" android:exported="true">
        <intent-filter>
            <action android:name="android.intent.action.BOOT_COMPLETED"/>
            <action android:name="android.intent.action.QUICKBOOT_POWERON" />
        </intent-filter>
    </receiver>
```

## Usage

As per the description, the demo service is classified as a `connectedDevice` foreground service type, even though the demo code does not actually connect to anything. It does however create a timer, which fires off every 10 seconds, just to demonstrate that this is possible.

You could use this demo as a template, change the `foregroundServiceType` if necessary, and implement your own code in either the `AndroidServiceStartCommand` method or `TimerIntervalHandler` if there is something you want to run at regular intervals.

## Status

* May 3rd, 2025
  
  The code has had very limited testing - it may be that Android could decide to shutdown the service if the device goes into "doze" mode for a while, or for whatever other reason that they have dreamt up to restrict background execution.

  **I strongly advise that you create a test project first and run it through its paces before proceeding with implementing this code in any serious way.**





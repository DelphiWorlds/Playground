# FCM Rebooted Demo

## Purpose

To test a revamp of Firebase Cloud Messaging (FCM) support.

## Description

This incarnation of FCM support is a substantial reworking of the original FCM support in Kastri, however some elements remain.

In this implementation, support has been added for a customised notification on Android, using RemoteViews, similar to the CustomNotification demo. This allows multiple lines of text in the notification banner, and an optional image, placed to the right of the text.

There is now a single unit: `DW.FCMManager`, that handles management of FCM, and is exposed as a reference to an interface: `IFCMManager`. Now you do not need to create any classes; just assign event handlers, and call the `Start` method on the `FCM` reference.

**In order for messages to be received properly on Android, FCM message payloads will need to omit `notification` elements, and include `title`, `body` and `imageUrl` (if needed) properties in the `data` element.** Please refer to the [Sending test messages](#sending-test-messages) section.

## Supported Versions

At present, the only supported version is Delphi 11.x. Support for 10.4.x may come later, however it is unlikely.

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Project Configuration

Note that this project relies on the Kastri repo, and for this demo expects that you have a user override environment variable named `Kastri` that points to the root of the Kastri source.

For setup in Firebase Console and the Apple Developer site, please refer to the [instructions in the original FCM demo](https://github.com/DelphiWorlds/Kastri/blob/master/Demos/FirebaseCloudMessaging/Readme.md).

### iOS

FCM Rebooted can be used with the latest of the 8.x versions of the Firebase iOS SDK, which is v8.15.0. Due to technical issues as reported here, there is presently no support for v9.x, however it may come later.

Please download the [Firebase iOS SDK from here](https://github.com/firebase/firebase-ios-sdk/releases/download/v8.15.0/Firebase.zip), and for the demo, extract it into the `ThirdParty` folder, so that the path matches the framework search paths in the project, i.e. folders starting with `ThirdParty\Firebase`.

Download the `GoogleServices-info.plist` file from your project configured in [Firebase Console](https://console.firebase.google.com/), and save it to the Resources folder in the demo.

If you are creating your own project:

Note that the iOSapi.FirebaseMessaging unit needed to be "patched" in order to work with this version of the Firebase iOS SDK, so that patched unit will need to be included with your project.

Ensure that a value of `-ObjC` for the `Options passed to the LD linker` option is in the `Linking` section of the Project Options.

Add `GoogleServices-info.plist` to the deployment, as per the demo, as described above.

### Android

If you are creating your own project:

FCM Rebooted relies on `dw-kastri-base-2.1.0.jar` and `dw-fcm-2.0.0.jar` from the `Lib` folder of this repo, so add them to the `Libraries` node under the `Android 32 bit` platform in Project Manager. (There is no need to add them to `Android 64 bit`).

**Note that these jars are not (yet) the same as those in the Kastri repo, and you do not need `dw-firebase-messaging.jar`**

Please ensure that the `Receive push notifications` checkbox is checked in the Entitlements Section of the Project Options.

In the `Build Events` section of Project Options, please ensure that you configure the `Post Build` event to execute the `manifestmerge` tool (as per the demo), which ensures that the correct overridden `FirebaseMessageingService` is configured in the manifest when the project is built.

For your own project, **or** the demo:

In the `Services` section of the Project Options, import the google-services.json file that you download from your project configured in [Firebase Console](https://console.firebase.google.com/).

## Sending test messages

Test messages can be sent using the [PushIt](https://github.com/DelphiWorlds/PushIt) tool. Please take note of the section on how to obtain the service account file from the [Cloud Console](https://console.cloud.google.com/iam-admin/serviceaccounts).

**For this implementation of FCM to work, you will need to check the "Data Only Notification" checkbox.** If you do not check this checkbox, the title and body of the notification will be blank.

This is due to a limitation in FCM where messages that contain a "notification" property (regardless of whether it contains a "data" property) do not cause the customized FCM service to be invoked when the app is not running or is in the background. 

Added bonus: images can be included in the customised notification (they will appear on the left). Use the Image URL edit box to include a URL to an image, e.g: https://sd.keepcalms.com/i/keep-calm-and-love-delphi-26.png

With PushIt, you can see what the resulting payload looks like by selecting the `JSON` tab, which can help guide you in constructing message payloads in your server.

## Status

Jul 21st, 2022: Waiting for feedback/suggestions













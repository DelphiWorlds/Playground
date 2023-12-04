# Apple Push Demo

## Purpose

Implementation of sending push notifications direct to APNs

Demonstration of simple creation of JWTs (JSON Web Tokens) 

## Description

This code was created specifically to debug issues with custom notifications on iOS via a Notification Content app extension created with Xcode.

It serves to eliminate FCM (Firebase Cloud Messaging) as a potential cause of the failure of the custom notification implementation. It also ensures that APNs messages are actually being delivered, and in the correct format.

Notwithstanding the above, the demo can be used for everyday testing of push notifications sent via APNs.

## Using The Demo

Use the P8 file edit to specify the `.p8` file that will be used for the secret when creating the JWT. This is the same file that would have been downloaded when creating a key for APNS on the [Apple Developer site](https://developer.apple.com/account/resources/authkeys/list), which can also be used when configuring Firebase Cloud Messaging (FCM) in the [Firebase Console](https://console.firebase.google.com/)

If the P8 filename is the original filename when it was downloaded (i.e. it starts with `AuthKey_` and is followed by the key id), the demo will automatically populate the Key edit, otherwise enter the key.

Team ID refers to the Team ID attached to the [App ID](https://developer.apple.com/account/resources/identifiers/list) for the application. 

The value to put in the Bundle Identifier edit is the same as the identifier used for the App Id for your application.

The token is the **APNS** token (i.e. not an FCM token) for the target device. If you're using [`DW.FCMManager` from Kastri](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted#readme), you can use the `GetAPNSToken` method to get this value.

Image URL is not in use as yet, as I am having trouble making a [Notification Content app extension](https://developer.apple.com/documentation/usernotificationsui/customizing_the_appearance_of_notifications?language=objc) work with Delphi.

The P8 file, Key, Team ID and Bundle Identifier values are persisted to a file, and the values are re-populated when the demo is restarted.

The JWT creation uses [Grijjy's OAuth2 support](https://github.com/grijjy/DelphiOpenSsl/blob/master/Grijjy.OAuth2.pas), so on Windows the demo will require the relevant [OpenSSL binaries](https://github.com/grijjy/DelphiOpenSsl/tree/master/Bin) to be in the same folder as the application, or in the Windows library path.

## Project Configuration

The project search paths in the demo have references to 2 macros:

* `$(Grijjy)` - this refers to the [Grijjy Foundation repo](https://github.com/grijjy/GrijjyFoundation).
* `$(GrijjySSL)` - this refers to the [Grijjy DelphiOpenSSL repo](https://github.com/grijjy/DelphiOpenSsl).

Either create macthing User System Overrides in the IDE options, or replace the macros with paths to copies of the respective repos.

## Technical information

### JWT Creation

As described above, the code uses [Grijjy's OAuth2 support](https://github.com/grijjy/DelphiOpenSsl/blob/master/Grijjy.OAuth2.pas), which was chosen for its simplicity, compactness and support for signing using [`ES256`](https://datatracker.ietf.org/doc/html/rfc7518#section-3.4), which is required for JWT's used with APNs and [Apple Store API](https://developer.apple.com/documentation/appstoreconnectapi).

### HTTP communications

The code uses `THTTPClient` (part of the Delphi RTL) because it has native support for `HTTP/2`. The `PostMessage` method of `TAPNSSender` is synchronous, so will block the UI while it runs.  

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status 

As at Dec 4th, 2023:

Waiting for feedback from interested developers.



# Location Requester Demo

## Purpose

Provide convenient "one shot" location requests, i.e. **not** continuous location updates. If you are after those, please refer to the [Location demo](https://github.com/DelphiWorlds/Playground/tree/main/Demos/Location).

This demo is based on work specifically related to obtaining a more accurate location in relatively remote areas.

It is also specifically built for Android and iOS. Compiling for other platforms would be useful only to see what the UI looks like at runtime, although the code could be modified if those platforms were required.

As per other demos in Playground, this is a Work In Progress.

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Description

`LocationRequester` uses the traditional TLocationSensor component that ships with Delphi, however it is used in a way that requests permissions and locations only at times when they are absolutely needed, thus being less intrusive to the user, and saving on battery life.

Instead of having to create an instance of a class, `LocationRequester` is a reference to an interface that is created on initialization. Interactions with permissions and the actual location sensor are deferred until the `Request` method is called.

`Request` will behave differently depending on what values are passed in `AParams`, which is a `TLocationRequestParams` record. There are two class methods on `TLocationRequestParams` which allow the `Request` method to be called simply, as per the demo. 

The first method is `CreateSingle` which can be used to initialize a record for when a request requires one location only.

The `CreateMultiple` method can be used to initialize a record for when a request requires a number of locations within a specified time period. This can be useful when the device is in a relatively remote location i.e. a substantial distance from wifi or other network devices that might be able to provide a more accurate result. Usually, it is the last result of the samples taken that is the most accurate, so this is the one `LocationRequester` will use when the `Request` method returns. Regardless, the gathered samples are available at the end of the request in the `Locations` property.

## Location references file

The file that ships with the demo: `locationrefs.json` contains a number of locations that are near my home, and are evidently a fair distance away from network devices such as wifi etc. You should be able to easily modify this file to suit locations that you wish to test against.

## Technical Notes

For those who are curious about why `LocationRequester` was written the way it was, this is an explanation of a couple of aspects of the code:

### Permissions request on iOS

This is particularly important when the app is first run, however it could also be important if the user removes location permissions for the app, and it needs to request permissions at a later time:

Due to the way `TLocationSensor` is implemented on iOS, permissions are requested when the `Active` property is set to `True`, however the code then continues to attempt to start location updates regardless of the result. In order to work around this, it was necessary to check the authorization status, and use platform-specific code to request the permissions first. This causes the app to go into the background, so when the app becomes active again, the authorization status is checked again before setting the `Active` property to `True`

### Android

Given the nature of `LocationRequester`, it is not envisaged that it should be used in an Android service. Given that it uses standard `TTimer` components for handling timeouts for sampling, and for the sensor itself, presently the code will not work in an Android service, due to [this issue](https://quality.embarcadero.com/browse/RSP-17857).

## Status of this demo (Jul 2nd, 2022)

This is the first cut of `LocationRequester`, and any feedback about how it works (or should work) would be appreciated.


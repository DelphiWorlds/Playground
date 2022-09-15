# Android App Update Demo

## Purpose

To implement [in-app updating of Play Store apps on Android](https://developer.android.com/guide/playcore/in-app-updates).

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Description

Please refer to the demo as a guide on how to use `TAppUpdate`.

* `CheckForUpdate` is an **asynchronous** call to check if an update is available, and returns the info in the `OnAppUpdateInfo` event
* `StartUpdate` is an **asynchronous** call to start the update flow for the specified type. The `OnAppUpdateStartedFlow` event is called with the `Started` parameter indicating whether or not the flow did actually start. The `OnAppUpdateResult` event is called when the flow finishes, and the `UpdateResult` parameter indicates whether or not the update succeeded, or the user canceled.

**NOTES:**

As per the demo status (below), it is currently **UNTESTED**, because I don't have an app in the Play Store to test it against.

## Project Configuration

### Android libraries

This demo uses the Play Core library, which does not ship with Delphi. When creating your own project, add the [`play-core-1.10.0.jar`](https://github.com/DelphiWorlds/Playground/blob/master/ThirdParty/Android/play-core-1.10.0.jar) library to the Libraries node, under the Android target in Project Manager.

## Status of this demo (Jan 3rd, 2022)

Waiting for anyone interested to test it

**The code/demo was built for Delphi 11, however it might be able to be modified to work in Delphi 10.4.2**


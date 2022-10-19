# Radio Player Test

## Purpose

Implement radio streaming in Delphi apps

## Description

**NOTE** This code is dependent on code from the [Kastri](https://github.com/DelphiWorlds/Kastri) repo. It is also dependent on the BASS library, which I have created a [new repo](https://github.com/DelphiWorlds/BASS) for.

**Please see the Configuration section of the main readme for how to configure project compiler search paths.**. 

Unashamed rewrite of [FMX.Radio](https://github.com/ersanyakit/FMX.Radio)

**See Status section below**

On Android, radio playing is implemented as a service, though an option may be added later to allow playing from the app itself. The service implementation is designed to allow play to continue when the app goes into the background, and/or the device is in the lock screen.

If this project goes forward, documentation will be added.

**Please note that the code/files for this project will be re-organised later**

## Supported Versions

The code is currently for Delphi 11.x. It may or may not work on earlier versions for Windows, iOS and macOS

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status

Oct 19th, 2022:

* Fixed the GetTags code, and refactored metadata so that it is passed as an array of strings
* Updated the demo to load stations from a JSON file, and present more metadata

Oct 18th, 2022:

* Performed some refactoring so that less code is required in the Android service.
* Implemented an event for the stream metadata
* Changed project search paths to rely on new [BASS repo](https://github.com/DelphiWorlds/BASS)

Oct 16th, 2022:

Early test version - some things may not be finished, implemented, or even working, e.g:

* Pause does not work, and probably won't, ever
* Some parts of the original code are commented out - they may or may not get implemented




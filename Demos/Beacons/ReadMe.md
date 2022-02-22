# Beacons Demo

## Purpose

Implementation of Beacons on Android (and potentially iOS) as an alternative to Embarcadero's built-in support

**NOTE: This demo is subject to a LOT of changes. See the Status section, below.**

## Description

This is a proof of concept related to potential work for a client of mine.

The current implementation of Beacons that ships with Delphi is problematic for my client, so rather than attempt to solve those issues, this serves as potential replacement code.

The demo consists of an application and a service. The application can be used to do ad-hoc scans, or to create an intent (API 21 - i.e. Android 6, or greater) that can be handled by a receiver class (in Java) which then passes the results on to the service.

The code is based in part on information from these links:

http://www.davidgyoungtech.com/2017/08/07/beacon-detection-with-android-8
https://stackoverflow.com/questions/50248178/oreo-ble-scanner-startscan-with-pendingintent-not-working-on-samsung-device

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status 

As at Feb 22nd, 2022

Support is for Android **only**. iOS may or may not come later, or just use Embarcadero's implementation on iOS, however bear in mind it [still has its own issues](https://quality.embarcadero.com/browse/RSP-21460).

As indicated above, currently this demo iis subject to a LOT of changes. I'm not satisfied with some of the naming, and properties/methods may be added/removed, or the whole idea may be scrapped altogether.


# Android Background Location test project

## Purpose

Potential replacement for the Cross Platform Location demo, **except for Android ONLY**.

Intended to overcome difficulties in providing continuous location updates in newer versions of Android, paticularly Android 10, Android 12 etc

## Description

This app has been largely refactored from the original (mentioned above) to separate some of the functionality so that parts can be easily left out when required, or include implementations of your own for those parts.

Aside from relying on some code from Kastri, this demo should otherwise be self-contained, including its own copy of the dependent jars.

Incorporates suggestions made by Alex Sawers from [this issue on the Kastri repo](https://github.com/DelphiWorlds/Kastri/issues/248).

If (and this is fairly unlikely given the issues, see below) it can be improved, this description will be expanded.

## Status

* Sep 27th, 2024
  
  - Works OK when the app is active, or the app is switched to the background
  - Fails when the app is quit. Please refer to [this Stack Overflow post](https://stackoverflow.com/questions/70044393/fatal-android-12-exception-startforegroundservice-not-allowed-due-to-mallows) for information about why.


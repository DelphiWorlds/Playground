# Grijjy Error Reporting demo

## Purpose

To automatically log when exceptions occur, and make it (slightly) easier to handle the Grijjy exception report

**NOTE: This code is meant for Android, iOS and macOS only, i.e. Windows is NOT supported**

This code can be especially useful for catching Delphi exceptions in *startup* code, which on Android can result in the **app being stuck on the splash screen, or crash before the user actually sees anything at all**. In those particular scenarios you will need a [log viewer](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/LogViewers#viewing-the-logs) to see what happened.

## Project Configuration

If using the `DW.GrijjyErrorReporting` unit in your own code, please refer to the comments at the top of that unit.

## Usage

If you just want logging for when an exception occurs, just add `DW.GrijjyErrorReporting` to your project. See the comments in that unit regarding configuration regarding Android.

As per the demo, if you want to handle when a Grijjy exception report is created, use the `AddHandler` method of `ExceptionInterceptor`

## Status

In the process of determining what to do next (Feb 28th, 2026)
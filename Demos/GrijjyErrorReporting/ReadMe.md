# Grijjy Error Reporting demo

## Purpose

To automatically log when exceptions occur, and make it (slightly) easier to handle the Grijjy exception report

**NOTE: This code is meant for Android, iOS and macOS only, i.e. Windows is NOT supported**

## Project Configuration

If using the `DW.GrijjyErrorReporting` unit in your own code, please refer to the comments at the top of that unit.

## Usage

If you just want logging for when an exception occurs, just add `DW.GrijjyErrorReporting` to your project. See the comments in that unit regarding configuration regarding Android.

As per the demo, if you want to handle when a Grijjy exception report is created, use the `AddHandler` method of `ExceptionInterceptor`

## Status

In the process of determining what to do next (Feb 28th, 2026)
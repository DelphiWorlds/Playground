# Virtual Keyboard Observer Demo

## Purpose

Handle moving of controls so that they move above the virtual keyboard

## Description

This is a revamp of existing code in Kastri, namely [VKVertScrollBox](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/VKVertScrollbox)

The difference here is that it does not matter what the control is that contain the edits that result in the virtual keyboard showing, as long as the containing control is aligned to `Top`

At present, all the code does is move the container up far enough for the focused control to be visible. The original code would treat TMemo controls differently, by calculating the offset based on the caret position.

## Known issues

There is [a bug](https://quality.embarcadero.com/browse/RSP-41684) that causes edit controls to lose focus when the screen is rotated, on iOS. This affects the operation of this code.

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status 

May 13th, 2023:

Initial check-in, looking for feedback




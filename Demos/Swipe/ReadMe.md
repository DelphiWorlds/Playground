# Swipe Demo

## Purpose

Implement "swiping" of controls

## Description

**NOTE** This code is dependent on code from the [Kastri](https://github.com/DelphiWorlds/Kastri) repo. 

This implementation is a complete rewrite of some code I had written years ago, which was far more complex than it needed to be

It allows a developer to implement left and right swiping of controls contained in the `TSwipeView`. This is achieved by parenting controls to the Left, Center and Right layouts (see the demo code as to how). When a user swipes left or right, the controls simply swap around. Controls can be re-populated in the OnSwiped event e.g. if the user has swiped left, the control parented to the right hand layout could have its content changed, and the same for the left hand layout for when the user swipes right.

If there's enough interest, the demo will be expanded, and more documentation will follow.

## Supported Versions

The code is currently for Delphi 11.x, however it should work in earlier versions

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status

Jan 2nd, 2023:

Initial version

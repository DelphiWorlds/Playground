# Camera X Demo

## Purpose

To implement CameraX in Delphi, potentially as a replacement for the existing camera support in Kastri.

** NOTE: This demo is currently non-functional. See the Status section, below. **

## Description

This project is based on the

  [CameraX overview](https://developer.android.com/training/camerax)

 and

  [Getting Started instructions](https://developer.android.com/codelabs/camerax-getting-started)

I’ve pushed past the problems of working out the dependencies, how to “Delphi-ize” the Kotlin code, and this is the current result

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status 

As at Jan 6th, 2022

As indicated above, currently this demo is non-functional

Going by the logcat entries, it appears to be succeeding at everything, however the output is completely white (the preview area is totally black before starting the camera). 

I’m not sure whether this is my fault, or whether there’s a limitation in being able to “host” a [PreviewView](https://developer.android.com/reference/androidx/camera/view/PreviewView) in FMX. Regardless, I’ve made this available in case anyone has some clue as to what the problem is and/or just sharing the code.


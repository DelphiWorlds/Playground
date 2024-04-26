# Camera X Demo

## Purpose

To implement CameraX in Delphi, potentially as a replacement for the existing camera support in Kastri.

**NOTE: This demo is currently non-functional. See the Status section, below.**

## Description

This project is based on the

  [CameraX overview](https://developer.android.com/training/camerax)

 and

  [Getting Started instructions](https://developer.android.com/codelabs/camerax-getting-started)

I’ve pushed past the problems of working out the dependencies, how to “Delphi-ize” the Kotlin code, and this is the current result

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status 

* Apr 26th, 2024:

Updated Android libraries
Included resources from libraries and merged with project resources

Sadly, still non-functional

* May 24th, 2023:

Updated Android libraries
Included resources from libraries and merged with project resources

Sadly, still non-functional, but now the screen is black, rather than white. That may be because of other changes.
Logcat entries seem to be more promising, as rotating the screen emits messages about the orientation changing.

More later..

* Jan 6th, 2022:

As indicated above, currently this demo is non-functional

Going by the logcat entries, it appears to be succeeding at everything, however the output is completely white (the preview area is totally black before starting the camera). 

I’m not sure whether this is my fault, or whether there’s a limitation in being able to “host” a [PreviewView](https://developer.android.com/reference/androidx/camera/view/PreviewView) in FMX. Regardless, I’ve made this available in case anyone has some clue as to what the problem is and/or just sharing the code.


# Simple Camera Demo

## Purpose

To implement camera preview/recording/playback as simply as possible on at least Android and iOS.

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Description

This project may also serve as a starting point to replace current Camera support in [Kastri](https://github.com/DelphiWorlds/Kastri).

As per the purpose section, this demo contains as simple as practicable, an implementation of camera functionality, including showing a preview, recording video (and audio where permission is granted), and playback of the recorded video.

## Barcode detection

This code also has support for integrating detection of barcodes on Android and iOS, by way of implementing an image processor that uses the native image classes, rather than converting them to Delphi bitmaps, which can be quite costly in terms of processing time.

There are examples of image processors that use MLVision Kit (in `DW.BarcodeImageProcessor.Android` and `DW.BarcodeImageProcessor.iOS`), and using Zxing library by Edward Spelt (in `DW.ZXingImageProcessor.Android` and `DW.ZXingImageProcessor.iOS`). For the latter, converting the image to a bitmap is unavoidable, since the Zxing code works with bitmaps.

In the demo, uncomment the appropriate code in order to use the barcode image processors.

## Project Configuration

**These are details regarding configuration if using this code in your own project.**

This code is dependent on [Kastri](https://github.com/DelphiWorlds/Kastri), so the project search paths will need to refer to it.

If using the MLVision Kit based image processor:

* iOS: You will need paths to the MLVision Kit framework, as per the `Framework search path` in the project options.
* Android: You will need to add `play-services-vision-17.0.2.jar` and `play-services-vision-common-17.0.2.jar` from the `Lib` folder to the Libraries node of the relevant Android target(s) in Project Manager. **Please [see this](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) regarding an issue in Delphi 11.3 related to adding libraries**

If using the Zxing based image processor, you will need to configure your project search paths to point to the Zxing folders.

## Issues

* Zxing performance:

    As described above, converting native images (eg UIImage or CGImage) to Delphi bitmaps can be quite costly in terms of performance, so help is sought for improving that code.

* Barcode image processors:

    As per the description, the current image processors are for Android and iOS (see [Status](#status)). Contributions of image processors for Android and/or alternative implementations are welcome. Please ensure that contributions adhere to [Delphi Worlds coding standards](https://github.com/DelphiWorlds/Kastri/blob/master/CodingStandards.md).

**The code/demo was built for Delphi 11.3, however it might be able to be modified to work in earlier versions**

## Status

Jun 5th, 2023

* Added Vision image processor for Android (i.e. Barcode scanning)

    Now the Vision barcode scanning has both iOS and Android support. This is recommended over Zxing, which has extremely poor performance.

Jun 4th, 2023

* Added Zxing image processor for Android (i.e. Barcode scanning)

    Note: Performance for this library is absolutely horrendous on Android. iOS not so bad.

* Added placeholder event for `OnImageAvailable` - it may or may not be removed

May 30th, 2023

* Recording issue:

    On my Android device (Pixel 3a), when pressing "Stop Recording", it causes this error (in logcat, and a message is displayed on the screen):

    ```
    2023/05/29 21:37:00.863	E	com.embarcadero.SimpleCameraDemo	30035	30035	MediaRecorder	stop failed: -1007
    ```

    The Pixel 3a is also unable to playback the recorded video (similar problem to the Pixel 6 Pro)

May 29th, 2023

* Playback issue:

    On my Android device (Pixel 6 Pro with Android 13), the video does not play back, however the audio does play. When it plays, these lines appear in the logcat messages:

    ```
    2023/05/29 09:58:32.201	E		1258	32216	NuPlayerDecoder	Failed to create video/avc decoder
    2023/05/29 09:58:32.201	E		1258	32213	NuPlayer	received error(0x80000000) from video decoder, flushing(0), now shutting down
    ```

    So presumably my device does not support the recorded format, which is MP4 (H264). I'm hoping someone might have a solution for this issue.



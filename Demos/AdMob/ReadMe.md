# AdMob test project

## Purpose

To display ads :-)

## Description

Advertising implementation, specifically for AdMob (at present)

## Project Configuration

### Library

If creating your own project, you will need to add the [`dw-admob.jar`](https://github.com/DelphiWorlds/Playground/blob/master/Lib/dw-admob.jar) file to the Libraries node under the Android platform in Project Manager

### Entitlements

Ensure your project has the `AdMob Service` enabled. This adds Google Play services metadata and the Ads activity to the manifest.

### Permission

Ensure your project has the `Access Network State` permission in Project Options

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status

Jan 14th, 2022

Implemented so far (Android only):

* TAdMobBannerAd:  [Banner Ad](https://developers.google.com/admob/android/banner)  - very basic testing has been performed
* TAppOpenAd: [App Open Ad](https://developers.google.com/admob/android/app-open) - basic testing
* TInterstitialAd: [Interstitial Ad](https://developers.google.com/admob/android/interstitial) - basic testing
* TRewardedAd: [Rewarded Ad](https://developers.google.com/admob/android/rewarded) - untested

**The code/demo was built for Delphi 11, however it might be able to be modified to work in Delphi 10.4.2**


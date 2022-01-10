# Media Manager demo

## Purpose

Shows how to use some of the functionality of `TMediaManager`, namely:

* Enumerate the albums (collections) and media (photos and videos) on the device
* Load photos
* Delete media
* Test changes to media properties
* Save photos to a collection
* Add new collections
  
## Description

`TMediaManager` allows access to media (e.g. photos and videos) in the collections of media (albums) on the users device

On iOS, this is implemented via the [Photos framework](https://developer.apple.com/documentation/photokit/phphotolibrary?language=objc), which is a misnomer since it also handles video.

On Android, this is implemented via the [Media Store](https://developer.android.com/reference/android/provider/MediaStore), specifically for `MEDIA_TYPE_IMAGE` and `MEDIA_TYPE_VIDEO`

A number of operations are performed asynchronously (because the operation is handled asynchronously on iOS, for example)

## Requesting permissions

After creating an instance of `TMediaManager`, typically you will call `RequestPermissions` and handle the result in the `OnAuthorizationStatus` event

## Working with collections and media items

Call the `Load` method of the `Collections` property of `TMediaManager` to load the "albums" on the device. This will just load the collections which includes their title. (iOS only certain album types?)

To load the media contained within a collection call the `Load` method on the collection. This will load the ID, and properties for each item, however it does not load the item itself (e.g. a photo image)

Call the `Load` method of an item to load the actual image. This method is asynchronous, as this is the process for iOS (and perhaps in future may be implemented this way for Android).

Call `DeleteItems` to delete one or more items. Be aware that on iOS, the OS will prompt the user for permission to delete an item.

## Status

Jan 11th, 2022

Waiting for people to test


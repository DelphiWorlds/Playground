# Java2OP list of issues

This is a temporary file that documents some shortcomings with Java2OP. It is not really essential to the Playground repo

## Issues in QP (some may need verification that they are still valid, and this list may not be complete)

* https://quality.embarcadero.com/browse/RSP-14212
  
  Exports classes from android.net.wifi incorrectly?

* https://quality.embarcadero.com/browse/RSP-15308
  
  "Class or interface expected" - Checking with Allen Drennan

* https://quality.embarcadero.com/browse/RSP-15473
  
  Original issue appears to have been fixed. Needs a new one for my first comment

* https://quality.embarcadero.com/browse/RSP-19295

  AV - Checking with Markus Humm

* https://quality.embarcadero.com/browse/RSP-21462

  Incorrect code (class methods which are not, etc) - Checking with reporter (Lloyd)

* https://quality.embarcadero.com/browse/RSP-23492

  "Skipping methods" - seems to be resolved, however same .jar now has problems like in: https://quality.embarcadero.com/browse/RSP-44033

* https://quality.embarcadero.com/browse/RSP-24029

  Instance vs Class methods - Checking with the reporter (Kevin)

* https://quality.embarcadero.com/browse/RSP-26550

  Adds "deprecated" when it shouldn't - still happens in latest version

* https://quality.embarcadero.com/browse/RSP-27991

  "Duplicates not allowed" - still happens in latest version

* https://quality.embarcadero.com/browse/RSP-40372

  Generic Type "T" not defined - still happens in latest version

* https://quality.embarcadero.com/browse/RSP-40380

  Java 11 vs Java 8 - Seems to work using Adoptium JDK, however it then suffers from: https://quality.embarcadero.com/browse/RSP-44033

* https://quality.embarcadero.com/browse/RSP-40400

  Might be more a case of user error - monitoring

* https://quality.embarcadero.com/browse/RSP-44033

  Emits anonymous inner classes when it does not need to

  e.g. For exifinterface-1.3.6.jar from AndroidX, Java2OP emits this:

  ```java
  JExifInterface_1Class = interface(JMediaDataSourceClass)
    ['{94C2A439-8B4D-46DF-8E9F-429E9BB0384C}']
  end;

  [JavaSignature('androidx/exifinterface/media/ExifInterface$1')]
  JExifInterface_1 = interface(JMediaDataSource)
    ['{EB61AD2B-8739-4453-83B2-517E09E1F5BB}']
    procedure close; cdecl;
    function getSize: Int64; cdecl;
    function readAt(l: Int64; b: TJavaArray<Byte>; i: Integer; i1: Integer): Integer; cdecl;
  end;
  TJExifInterface_1 = class(TJavaGenericImport<JExifInterface_1Class, JExifInterface_1>) end;
  ```

  This corresponds to this Java code fragment:
  ```
  ExifInterfaceUtils.Api23Impl.setDataSource(retriever, new MediaDataSource()
  ```

  i.e. an anonymous descendant of `MediaDataSource` is being created, and Java exports this when emitting headers using the `jar` tool like this:
  ```
  jar tf exifinterface-1.3.6.jar
  ```

  Resulting in:
  ```
  androidx/exifinterface/media/ExifInterface$ExifAttribute.class
  androidx/exifinterface/media/ExifInterface$IfdType.class
  androidx/exifinterface/media/ExifInterface$SeekableByteOrderedDataInputStream.class
  androidx/exifinterface/media/ExifInterface$ExifTag.class
  androidx/exifinterface/media/ExifInterface$ExifStreamType.class
  androidx/exifinterface/media/ExifInterface$ByteOrderedDataInputStream.class
  androidx/exifinterface/media/ExifInterfaceUtils$Api23Impl.class
  androidx/exifinterface/media/ExifInterface$ByteOrderedDataOutputStream.class
  androidx/exifinterface/media/ExifInterfaceUtils.class
  androidx/exifinterface/media/ExifInterface$Rational.class
  META-INF/androidx.exifinterface_exifinterface.version
  androidx/exifinterface/media/ExifInterfaceUtils$Api21Impl.class
  androidx/exifinterface/media/ExifInterface$1.class
  androidx/exifinterface/media/ExifInterface.class
  ```

  Java2OP can **safely** ignore any classes where the name is just a number, as classes cannot be named this way in source.

  There is a flow-on issue from this one, in that some init methods are declared with a parameter of the anonymous inner class type, rather than the outer type e.g. when importing ExoPlayer2 mentioned in this report:

  https://quality.embarcadero.com/browse/RSP-40380

  emits:

  ```delphi
  JNetworkTypeObserver_ReceiverClass = interface(JBroadcastReceiverClass)
    ['{A183356D-FB9A-436B-A96A-C647937DCC65}']
    {class} function init(networkTypeObserver: JNetworkTypeObserver; 1: JNetworkTypeObserver_1): JNetworkTypeObserver_Receiver; cdecl;
    {class} procedure onReceive(context: JContext; intent: JIntent); cdecl;
  end;
 
  [JavaSignature('com/google/android/exoplayer2/util/NetworkTypeObserver$Receiver')]
  JNetworkTypeObserver_Receiver = interface(JBroadcastReceiver)
    ['{B7B8C4AE-7096-4881-9486-13ADED1CB660}']
  end;
  TJNetworkTypeObserver_Receiver = class(TJavaGenericImport<JNetworkTypeObserver_ReceiverClass, JNetworkTypeObserver_Receiver>) end;
  ```

  JNetworkTypeObserver_1 should actually be JNetworkTypeObserver, which is the outer class.
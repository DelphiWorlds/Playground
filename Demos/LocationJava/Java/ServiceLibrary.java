package com.delphiworlds.LocationDemo;

public class ServiceLibrary {
  static { System.loadLibrary("Service");  }
  static public native int Test(int value);
  static public native void ReceivedLocation(String json);
}
package com.delphiworlds.kastri.bluetooth.le;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2021 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.bluetooth.le.ScanCallback;
import android.bluetooth.le.ScanResult;
import java.util.List;

public class DWScanCallback extends ScanCallback {
  private DWScanCallbackDelegate mDelegate;

  public DWScanCallback(DWScanCallbackDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onBatchScanResults(List<ScanResult> results) {
    mDelegate.onBatchScanResults(results);
  }

  @Override
  public void onScanFailed(int errorCode) {
    mDelegate.onScanFailed(errorCode);
  }

  @Override
  public void onScanResult(int callbackType, ScanResult result) {
    mDelegate.onScanResult(callbackType, result);
  }
}

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

import android.bluetooth.le.ScanResult;
import java.util.List;

public interface DWScanCallbackDelegate {

    void onBatchScanResults(List<ScanResult> results);
    void onScanFailed(int errorCode);
    void onScanResult(int callbackType, ScanResult result);

}
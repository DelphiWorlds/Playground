package com.delphiworlds.kastri;

import android.telephony.TelephonyManager;

public class DWUssdResponseCallback extends TelephonyManager.UssdResponseCallback {

  private DWUssdResponseCallbackDelegate mDelegate;

  public DWUssdResponseCallback(DWUssdResponseCallbackDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onReceiveUssdResponse(TelephonyManager telephonyManager, String request, CharSequence response) {
    mDelegate.onReceiveUssdResponse(telephonyManager, request, response);
  }

  @Override
  public void onReceiveUssdResponseFailed(TelephonyManager telephonyManager, String request, int failureCode) {
    mDelegate.onReceiveUssdResponseFailed(telephonyManager, request, failureCode);
  }

}
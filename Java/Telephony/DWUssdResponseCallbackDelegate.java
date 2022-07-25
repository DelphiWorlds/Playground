package com.delphiworlds.kastri;

import android.telephony.TelephonyManager;

public interface DWUssdResponseCallbackDelegate {

  public void onReceiveUssdResponse(TelephonyManager telephonyManager, String request, CharSequence response);

  public void onReceiveUssdResponseFailed(TelephonyManager telephonyManager, String request, int failureCode);

}
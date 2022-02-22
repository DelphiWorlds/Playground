package com.delphiworlds.kastri;

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

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.util.Log;
import androidx.core.app.JobIntentService;

public class DWBeaconsReceiver extends BroadcastReceiver {

  private static final String TAG = "DWBeaconsReceiver";
  private static final String KEY_JOB_ID = "com.delphiworlds.kastri.DWBeaconsReceiver.KEY_JOB_ID";
  private static final String KEY_SERVICE_CLASS_NAME = "com.delphiworlds.kastri.DWBeaconsReceiver.KEY_SERVICE_CLASS_NAME";

  private boolean forwardIntent(Context context, Intent intent) {
    String serviceClassName = null;
    int jobId = 345678; // Just a random number. If it clashes with any other job ids in your app, add the KEY_JOB_ID metadata
    try {
      Bundle metaData = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA).metaData;
      if (metaData.containsKey(DWBeaconsReceiver.KEY_SERVICE_CLASS_NAME))
        serviceClassName = metaData.getString(DWBeaconsReceiver.KEY_SERVICE_CLASS_NAME);
      if (metaData.containsKey(DWBeaconsReceiver.KEY_JOB_ID))
        jobId = metaData.getInt(DWBeaconsReceiver.KEY_JOB_ID);
    } catch (PackageManager.NameNotFoundException exception) {
      Log.e(TAG, "Could not obtain metadata");
    }
    if (serviceClassName != null) {
      try {
        JobIntentService.enqueueWork(context, Class.forName(serviceClassName), jobId, intent);
        return true;
      } catch (ClassNotFoundException e) {
        Log.e(TAG, "Could not find service: " + serviceClassName);
      }
    }
    else
      Log.e(TAG, "No service specified");
    return false;
  }

  @Override
  public void onReceive(Context context, Intent intent) {
    forwardIntent(context, intent);
  }
}

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

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.util.Log;
import androidx.core.app.JobIntentService;

public class DWBeaconsReceiver extends BroadcastReceiver {

  private static final String TAG = "DWBeaconsReceiver";
  private static final String ACTION_ALARM = "com.delphiworlds.kastri.DWBeaconsReceiver.ACTION_ALARM";
  private static final String KEY_JOB_ID = "com.delphiworlds.kastri.DWBeaconsReceiver.KEY_JOB_ID";
  private static final String KEY_SERVICE_CLASS_NAME = "com.delphiworlds.kastri.DWBeaconsReceiver.KEY_SERVICE_CLASS_NAME";
  private static final String EXTRA_ALARM_INTERVAL = "com.delphiworlds.kastri.DWBeaconsReceiver.EXTRA_ALARM_INTERVAL";

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

  public static void startAlarm(Context context, long interval) {
    if (interval > 0) {
      Intent intent = new Intent(context, DWBeaconsReceiver.class);
      intent.setAction(DWBeaconsReceiver.ACTION_ALARM);
      intent.putExtra(DWBeaconsReceiver.EXTRA_ALARM_INTERVAL, interval);
      PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT);
      AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
      alarmManager.setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP, System.currentTimeMillis() + interval, pendingIntent);
    }
  }

  public static void cancelAlarm(Context context) {
    Intent intent = new Intent(context, DWBeaconsReceiver.class);
    intent.setAction(DWBeaconsReceiver.ACTION_ALARM);
    PendingIntent.getBroadcast(context, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT).cancel();
  }

  @Override
  public void onReceive(Context context, Intent intent) {
    Log.d(TAG, "onReceive");
    if ((intent.getAction() != null) && (intent.getAction().equals(DWBeaconsReceiver.ACTION_ALARM)))
      startAlarm(context, intent.getLongExtra(DWBeaconsReceiver.EXTRA_ALARM_INTERVAL, 0));
    forwardIntent(context, intent);
  }
}

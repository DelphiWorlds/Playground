package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2022 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.app.KeyguardManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.os.PowerManager;
import com.google.android.gms.location.LocationResult;

// Delphi 10.4.2
// import android.support.v4.app.JobIntentService;

// Delphi 11
import androidx.core.app.JobIntentService;

import android.util.Log;

public class DWLocationReceiver extends BroadcastReceiver {

  private static final String TAG = "DWLocationReceiver";

  private boolean forwardIntent(Context context, Intent intent) {
		Log.d(TAG, "forwardIntent");
    String serviceClassName = null;
    int jobId = 234567; // Just a random number. If it clashes with any other job ids in your app, add the KEY_JOB_ID metadata
    try {
      Bundle metaData = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA).metaData;
      if (metaData.containsKey(DWFusedLocationClient.KEY_SERVICE_CLASS_NAME))
        serviceClassName = metaData.getString(DWFusedLocationClient.KEY_SERVICE_CLASS_NAME);
      if (metaData.containsKey(DWFusedLocationClient.KEY_JOB_ID))
        jobId = metaData.getInt(DWFusedLocationClient.KEY_JOB_ID);
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

  private boolean getStoredDozeMode(Context context) {
    SharedPreferences preferences = context.getSharedPreferences(TAG, Context.MODE_PRIVATE);
    return preferences.getBoolean("isDozed", false);
  }

  private void setStoredDozeMode(Context context, boolean isDozed) {
    SharedPreferences preferences = context.getSharedPreferences(TAG, Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = preferences.edit();
    editor.putBoolean("isDozed", isDozed);
    editor.commit();
  }

  private boolean isDozeMode(Context context) {
    PowerManager powerManager = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
    return powerManager.isDeviceIdleMode();   
  }

  private void handleAlarmIntent(Context context, Intent intent) {
    // Restart the alarm
    // DWFusedLocationClient.startAlarm(context, intent.getLongExtra(DWFusedLocationClient.EXTRA_ALARM_INTERVAL, 0));
    // Pass the intent on to the service
    boolean wasDozed = getStoredDozeMode(context);
    boolean isDozed = isDozeMode(context);
    // Forward only if it is going INTO doze mode, or is NOT dozed
    if ((!wasDozed && isDozed) || !isDozed) {
      intent.putExtra(DWFusedLocationClient.EXTRA_ALARM_TIMESTAMP, System.currentTimeMillis());
      if (!forwardIntent(context, intent))
        Log.d(TAG, "Intent forward failed?");
    }
    setStoredDozeMode(context, isDozed);
  }

  private void checkPowerMode(Context context) {
    if (isDozeMode(context))
      Log.d(TAG, "Entered doze mode");
    else
      Log.d(TAG, "Exited doze mode");
  }

  private void checkScreenLock(Context context) {
    KeyguardManager keyguardManager = (KeyguardManager) context.getSystemService(Context.KEYGUARD_SERVICE);
    if (keyguardManager.inKeyguardRestrictedInputMode())
      Log.d(TAG, "Screen locked");
    else
      Log.d(TAG, "Screen unlocked");
  }

  @Override
  public void onReceive(Context context, Intent intent) {
    if (LocationResult.hasResult(intent))
      forwardIntent(context, intent);
    else if (intent.getAction() != null) {
      Log.d(TAG, "Action: " + intent.getAction());
      if (intent.getAction().equals(Intent.ACTION_BOOT_COMPLETED))
        DWFusedLocationClient.startFromBoot(context);
      else if (intent.getAction().equals(DWFusedLocationClient.ACTION_ALARM))
        handleAlarmIntent(context, intent);
    }
		else
		  Log.d(TAG, "Received an intent with no location result and no action");
  }
}

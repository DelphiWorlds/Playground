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

// Inspired by:
// https://github.com/android/location-samples/blob/master/LocationUpdates/app/src/main/java/com/google/android/gms/location/sample/locationupdates/MainActivity.java

import android.app.Activity;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.pm.PackageManager;
import android.content.SharedPreferences;
import android.location.Location;
import android.location.LocationManager;
import android.os.Build;
import android.os.Bundle;
import android.os.Looper;
import android.os.PowerManager;
import android.os.SystemClock;
import android.util.Log;
import android.widget.Toast;
import androidx.annotation.NonNull;
import com.google.android.gms.common.api.ApiException;
import com.google.android.gms.common.api.ResolvableApiException;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.LocationSettingsRequest;
import com.google.android.gms.location.LocationSettingsResponse;
import com.google.android.gms.location.LocationSettingsStatusCodes;
import com.google.android.gms.location.SettingsClient;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
import java.util.Date;

public class DWFusedLocationClient {

  private static final int REQUEST_CHECK_SETTINGS = 1;
  private static final int REQUEST_LOCATION = 2;
  private static final String TAG = "DWFusedLocationClient";
  private static PowerManager.WakeLock gWakeLock;

  public static final String KEY_SERVICE_CLASS_NAME = "com.delphiworlds.kastri.DWFusedLocationClient.KEY_SERVICE_CLASS_NAME";
  public static final String KEY_JOB_ID = "com.delphiworlds.kastri.DWFusedLocationClient.KEY_JOB_ID";
  public static final String ACTION_ALARM = "com.delphiworlds.kastri.DWFusedLocationClient.ACTION_ALARM";
  public static final String EXTRA_ALARM_INTERVAL = "com.delphiworlds.kastri.DWFusedLocationClient.EXTRA_ALARM_INTERVAL";
  public static final String EXTRA_ALARM_TIMESTAMP = "com.delphiworlds.kastri.DWFusedLocationClient.EXTRA_ALARM_TIMESTAMP";

  private long mAlarmInterval = 0; // Milliseconds
  private Context mContext;
  private DWFusedLocationClientDelegate mDelegate;
  private long mFastestInterval = 10000; // Milliseconds
  private long mInterval = 10000; // Milliseconds
  private boolean mIsActive = false;
  private boolean mIsMockMode = false;
  private PendingIntent mLocationServiceIntent;
  private DWLocationCallback mLocationCallback;
  private FusedLocationProviderClient mLocationClient;
  private LocationManager mLocationManager;
  private LocationRequest mLocationRequest;
  private SharedPreferences preferences;
  private int mPriority = LocationRequest.PRIORITY_HIGH_ACCURACY;
  private SettingsClient mSettingsClient;
  private LocationSettingsRequest mSettingsRequest;
  private float mSmallestDisplacement = 10; // Metres

  public DWFusedLocationClient(Context context, DWFusedLocationClientDelegate delegate) {
    mContext = context;
    mDelegate = delegate;
    preferences = mContext.getSharedPreferences(TAG, Context.MODE_PRIVATE);
    readPreferences();
    if (mAlarmInterval >= 0)
      DWFusedLocationClient.createWakeLock(mContext);
    String serviceClassName = null;
    try {
      Bundle metaData = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA).metaData;
      if (metaData != null) {
        if (metaData.containsKey(KEY_SERVICE_CLASS_NAME))
          serviceClassName = metaData.getString(KEY_SERVICE_CLASS_NAME);
      }
    } catch (PackageManager.NameNotFoundException exception) {
      // Do nothing
    }
    // If a service class name is present in the metadata, use broadcast intent, else use callbacks
    if (serviceClassName != null)
      createIntent();
    else if (mDelegate != null)
      mLocationCallback = new DWLocationCallback(mDelegate);
    mLocationClient = LocationServices.getFusedLocationProviderClient(mContext);
    mSettingsClient = LocationServices.getSettingsClient(mContext);
    mLocationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
    if (mIsActive)
      startLocationUpdates();
  }

  private static void createWakeLock(Context context) {
    /*
    if (gWakeLock == null) {
      PowerManager powerManager = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
      gWakeLock = powerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, TAG + ".wakelock");
    }
    if (!gWakeLock.isHeld())
      gWakeLock.acquire();
    */
  }

  private static void destroyWakeLock() {
    /*
    if ((gWakeLock != null) && gWakeLock.isHeld())
      gWakeLock.release();
    gWakeLock = null;
    */
  }

  private void createIntent() {
    Intent intent = new Intent(mContext, DWLocationReceiver.class);
    mLocationServiceIntent = PendingIntent.getBroadcast(mContext, REQUEST_LOCATION, intent, PendingIntent.FLAG_UPDATE_CURRENT);
  }

  protected void readPreferences() {
    mFastestInterval = preferences.getLong("FastestInterval", mFastestInterval);
    mInterval = preferences.getLong("FastestInterval", mInterval);
    mPriority = preferences.getInt("Priority", mPriority);
    mSmallestDisplacement = preferences.getFloat("SmallestDisplacement", mSmallestDisplacement);
    mIsActive = preferences.getBoolean("IsActive", false);
  }

  protected void writePreferences() {
    SharedPreferences.Editor editor = preferences.edit();
    editor.putLong("FastestInterval", mFastestInterval);
    editor.putLong("Interval", mInterval);
    editor.putInt("Priority", mPriority);
    editor.putFloat("SmallestDisplacement", mSmallestDisplacement);
    editor.commit();
  }

  public static void startAlarm(Context context, Long interval) {
    Intent intent = new Intent(context, DWLocationReceiver.class);
    intent.setAction(DWFusedLocationClient.ACTION_ALARM);
    intent.putExtra(DWFusedLocationClient.EXTRA_ALARM_INTERVAL, interval);
    PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT);
    AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
    alarmManager.setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP, System.currentTimeMillis() + interval, pendingIntent);
  }

  public static void stopAlarm(Context context) {
    Intent intent = new Intent(context, DWLocationReceiver.class);
    intent.setAction(DWFusedLocationClient.ACTION_ALARM);
    PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT);
    pendingIntent.cancel();
  }

  private void createSettingsRequest() {
    mLocationRequest = new LocationRequest();
    mLocationRequest.setFastestInterval(mFastestInterval);
    mLocationRequest.setInterval(mInterval);
    mLocationRequest.setPriority(mPriority);
    mLocationRequest.setSmallestDisplacement(mSmallestDisplacement);
    mSettingsRequest = new LocationSettingsRequest.Builder()
     .addLocationRequest(mLocationRequest)
     .build();
    writePreferences(); 
  }

  public long getFastestInterval() {
    return mFastestInterval;
  }

  public void setFastestInterval(long interval) {
    // Sets the fastest rate for active location updates. This interval is exact, and your
    // application will never receive updates faster than this value.
    mFastestInterval = interval;
  }

  public long getAlarmInterval() {
    return mAlarmInterval;
  }

  public void setAlarmInterval(long interval) {
    boolean changed = (mAlarmInterval != interval);
    mAlarmInterval = interval;
    if (mAlarmInterval >= 0)
      DWFusedLocationClient.createWakeLock(mContext);
    else
      DWFusedLocationClient.destroyWakeLock();
    SharedPreferences.Editor editor = preferences.edit();
    editor.putLong("AlarmInterval", mAlarmInterval);
    if (mIsActive && changed) {
      DWFusedLocationClient.stopAlarm(mContext);
      DWFusedLocationClient.startAlarm(mContext, mAlarmInterval);
    }
  }

  public void setSmallestDisplacement(float value) {
    mSmallestDisplacement = value;
  }

  public float getSmallestDisplacement() {
    return mSmallestDisplacement;
  }

  public long getInterval() {
    return mInterval;
  }

  public void setInterval(long interval) {
    // Sets the desired interval for active location updates. This interval is
    // inexact. You may not receive updates at all if no location sources are available, or
    // you may receive them slower than requested. You may also receive updates faster than
    // requested if other applications are requesting location at a faster interval.
    mInterval = interval;
  }

  public boolean getIsMockMode() {
    return mIsMockMode;
  }

  public boolean getIsActive() {
    return mIsActive;
  }

  public int getPriority() {
    return mPriority;
  }

  public void setPriority(int priority) {
    mPriority = priority;
  }

  public void requestLastKnownLocation() {
    mLocationClient.getLastLocation().addOnSuccessListener(new OnSuccessListener<Location>() {
      @Override
      public void onSuccess(Location location) {
        // Got last known location. In some rare situations this can be null.
        if ((location != null) && (mDelegate != null)) {
            mDelegate.onLocation(location);
        }
      }
    });
  }

  private void handleTaskException(Exception e, String taskDescription) {
    String errorMessage = null;
    if (e instanceof ApiException) 
      errorMessage = "ApiException code " + Integer.toString(((ApiException) e).getStatusCode());
    else
      errorMessage = e.getMessage();
    Log.e(TAG, "Could not set mock mode - " + errorMessage);
  }

  private void internalSetMockLocation(Location location) {
    mLocationClient.setMockLocation(location).addOnCompleteListener(new OnCompleteListener<Void>() {
      @Override
      public void onComplete(@NonNull Task<Void> task) {
        if (task.isSuccessful()) {
          // Log.d(TAG, "Set mock location successful");
          if (mDelegate != null)
            mDelegate.onSetMockLocationResult(location);
        }
        else {
          handleTaskException(task.getException(), "Could not set mock location");
          if (mDelegate != null)
            mDelegate.onSetMockLocationResult(null);
        }
      }
    });    
  }

  protected Location getLocation(double latitude, double longitude, String provider) {
    Location location = new Location(provider);
    location.setLatitude(latitude);
    location.setLongitude(longitude);
    location.setTime(new Date().getTime());
    location.setAccuracy(3.0f);
    location.setElapsedRealtimeNanos(SystemClock.elapsedRealtimeNanos());
    return location;      
  }

  private boolean mockLocation(double latitude, double longitude, String provider) {
    if (mLocationManager.isProviderEnabled(provider)) {
      Location location = getLocation(latitude, longitude, provider);   
      // if (!mIsMockMode) {
         internalSetMockMode(true, location);
      // } else
      //   internalSetMockLocation(location);
      return true;
    }
    return false;    
  }

  public void setMockLocation(double latitude, double longitude) {
    boolean mocked = false;
    if (Build.VERSION.SDK_INT >= 31)
      mocked = mockLocation(latitude, longitude, LocationManager.FUSED_PROVIDER);       
    mocked = mocked || mockLocation(latitude, longitude, LocationManager.GPS_PROVIDER);   
    mocked = mocked || mockLocation(latitude, longitude, LocationManager.NETWORK_PROVIDER);   
    mocked = mocked || mockLocation(latitude, longitude, LocationManager.PASSIVE_PROVIDER);
    if (mocked)
      Log.i(TAG, "Mocked: " + String.valueOf(latitude) + ", " + String.valueOf(longitude));   
  }

  protected void internalSetMockMode(boolean isMockMode, Location location) {
    mLocationClient.setMockMode(isMockMode).addOnCompleteListener(new OnCompleteListener<Void>() {
      @Override
      public void onComplete(@NonNull Task<Void> task) {
        mIsMockMode = task.isSuccessful();
        if (mDelegate != null)
          mDelegate.onSetMockModeResult(mIsMockMode);
        if (!mIsMockMode) {
          handleTaskException(task.getException(), "Could not set mock mode");
        } else {
          // Log.d(TAG, "Set mock mode successful");
          if (location != null)
            internalSetMockLocation(location);
        }
      }
    });
  }

  public void setMockMode(boolean isMockMode) {
    if (!mIsMockMode)
      internalSetMockMode(isMockMode, null);
    else if (mDelegate != null)
      mDelegate.onSetMockModeResult(mIsMockMode);
  }

  private void stateChange(boolean isActive) {
    mIsActive = isActive;
    SharedPreferences.Editor editor = preferences.edit();
    editor.putBoolean("IsActive", mIsActive);
    editor.commit();
    if (mIsActive) {
      String message = "Interval: " + String.valueOf(mInterval) + ", Fastest: " + String.valueOf(mFastestInterval) + ", Min Disp: " 
        + String.valueOf(mSmallestDisplacement) + ", Priority: " + String.valueOf(mPriority);
      Log.i(TAG, "Now active with the following settings - " + message);
    }
    if (mDelegate != null)
      mDelegate.onLocationUpdatesChange(mIsActive);
  }

  public static void startFromBoot(Context context) {
    Log.d(TAG, "startFromBoot");
    DWFusedLocationClient client = new DWFusedLocationClient(context, null);
    client.readPreferences();
    if (client.getIsActive())
      client.startLocationUpdates();
  }

  public void startLocationUpdates() {
    createSettingsRequest();
    // Begin by checking if the device has the necessary location settings.
    // Log.d(TAG, "checkLocationSettings");
    mSettingsClient.checkLocationSettings(mSettingsRequest)
      .addOnSuccessListener(new OnSuccessListener<LocationSettingsResponse>() {
        @Override
        public void onSuccess(LocationSettingsResponse locationSettingsResponse) {
          Log.i(TAG, "All location settings are satisfied");
          if ((Build.VERSION.SDK_INT >= 23) && (mAlarmInterval > 0))
            DWFusedLocationClient.startAlarm(mContext, mAlarmInterval);
          boolean started = false;
          if (mLocationCallback != null) {
            mLocationClient.requestLocationUpdates(mLocationRequest, mLocationCallback, Looper.myLooper());
            started = true;
          }
          else if (mLocationServiceIntent != null) {
            mLocationClient.requestLocationUpdates(mLocationRequest, mLocationServiceIntent);
            started = true;
          }
          else
            Log.e(TAG, "No callback or location service specified");  
          stateChange(started);
        }
      })
      .addOnFailureListener(new OnFailureListener() {
        @Override
        public void onFailure(@NonNull Exception e) {
          Log.e(TAG, "OnFailureListener");
          int statusCode = ((ApiException) e).getStatusCode();
          switch (statusCode) {
            case LocationSettingsStatusCodes.RESOLUTION_REQUIRED:
              // Changed this part to show a toast until a solution about startResolutionForResult from a service is worked out
              /*
              Log.i(TAG, "Location settings are not satisfied. Attempting to upgrade location settings ");
              try {
                // Show the dialog by calling startResolutionForResult(), and check the
                // result in onActivityResult().
                ResolvableApiException rae = (ResolvableApiException) e;
                rae.startResolutionForResult((Activity) mContext, REQUEST_CHECK_SETTINGS);
              } catch (IntentSender.SendIntentException sie) {
                Log.i(TAG, "PendingIntent unable to execute request.");
              }
              break;
              */
            case LocationSettingsStatusCodes.SETTINGS_CHANGE_UNAVAILABLE:
              String errorMessage = "Location settings are inadequate, and cannot be fixed here. Please fix in Settings.";
              Log.e(TAG, errorMessage);
              Toast.makeText(mContext, errorMessage, Toast.LENGTH_LONG).show();
              // mRequestingLocationUpdates = false;
              break;
            }
            stateChange(false);
          }
        });
  }

  public void stopLocationUpdates() {
    Task<Void> removeTask = null;
    if (Build.VERSION.SDK_INT >= 23)
      DWFusedLocationClient.stopAlarm(mContext);
    if (mLocationCallback != null)    
      removeTask = mLocationClient.removeLocationUpdates(mLocationCallback);
    else if (mLocationServiceIntent != null)
      removeTask = mLocationClient.removeLocationUpdates(mLocationServiceIntent);
    if (removeTask != null) {
      removeTask.addOnCompleteListener(new OnCompleteListener<Void>() {
        @Override
        public void onComplete(@NonNull Task<Void> task) {
          // mRequestingLocationUpdates = false;
          Log.d(TAG, "removeLocationUpdates onComplete");
          mLocationRequest = null;
          mSettingsRequest = null;
          stateChange(false);
        }
      });
    }
  }

  private class DWLocationCallback extends LocationCallback {

    private DWFusedLocationClientDelegate mDelegate;

    public DWLocationCallback(DWFusedLocationClientDelegate delegate) {
      mDelegate = delegate;
    }

    @Override
    public void onLocationResult(LocationResult locationResult) {
      super.onLocationResult(locationResult);
      // Log.d(TAG, "DWLocationCallback onLocationResult");
      mDelegate.onLocation(locationResult.getLastLocation());
    }

  }
}
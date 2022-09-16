package com.delphiworlds.LocationDemo;

import android.content.Intent;
import android.location.Location;
import android.util.Log;
import androidx.core.app.JobIntentService;
import com.google.android.gms.location.LocationResult;
import com.google.gson.Gson;

public class LocationDemoService extends JobIntentService {

  private static final String TAG = "LocationDemoService";

  @Override
  protected void onHandleWork(Intent intent) {
	Log.d(TAG, "+onHandleWork");
	// ServiceLibrary.Test(1);
	if (LocationResult.hasResult(intent)) {
	  LocationResult result = LocationResult.extractResult(intent);
	  Location location = result.getLastLocation();
	  if (location != null) {
	    // TODO: LHelper.BroadcastLocationData(LLocation);
		ServiceLibrary.ReceivedLocation(new Gson().toJson(location));
	  }
	}
	Log.d(TAG, "-onHandleWork");
  }
}

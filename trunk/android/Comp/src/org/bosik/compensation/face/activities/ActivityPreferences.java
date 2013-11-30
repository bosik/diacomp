package org.bosik.compensation.face.activities;

import java.util.Map;
import org.bosik.compensation.face.R;
import org.bosik.compensation.persistence.Storage;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import android.preference.EditTextPreference;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.util.Log;

public class ActivityPreferences extends PreferenceActivity implements OnSharedPreferenceChangeListener
{
	/* =========================== КОНСТАНТЫ ================================ */

	private static final String		TAG	= ActivityPreferences.class.getSimpleName();

	public static SharedPreferences	preferences;

	public static String			PREF_SYNC_PASSWORD_KEY;
	public static String			PREF_SYNC_PASSWORD_DEFAULT;
	public static String			PREF_SYNC_SERVER_KEY;
	public static String			PREF_SYNC_SERVER_DEFAULT;
	public static String			PREF_SYNC_USERNAME_KEY;
	public static String			PREF_SYNC_USERNAME_DEFAULT;

	/**
	 * Initializes the preference trunk
	 * 
	 * @param context
	 * @param resolver
	 */
	public static void init(Context context)
	{
		Log.v(TAG, "Preferences inititalization...");

		// Initialize preferences
		PreferenceManager.setDefaultValues(context, R.xml.preferences, false);

		// Setup constants
		PREF_SYNC_SERVER_KEY = context.getString(R.string.prefServer);
		PREF_SYNC_SERVER_DEFAULT = context.getString(R.string.prefDefaultServer);
		PREF_SYNC_USERNAME_KEY = context.getString(R.string.prefUsername);
		PREF_SYNC_USERNAME_DEFAULT = context.getString(R.string.prefDefaultUsername);
		PREF_SYNC_PASSWORD_KEY = context.getString(R.string.prefPassword);
		PREF_SYNC_PASSWORD_DEFAULT = context.getString(R.string.prefDefaultPassword);

		// Make singleton
		preferences = PreferenceManager.getDefaultSharedPreferences(context);
	}

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		Log.v(TAG, "Preferences activity created");

		addPreferencesFromResource(R.xml.preferences);

		Map<String, ?> map = preferences.getAll();
		for (String key : map.keySet())
		{
			onSharedPreferenceChanged(preferences, key);
		}

		/*
		 * Preference customPref = (Preference) findPreference("customPref");
		 * customPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
		 * 
		 * public boolean onPreferenceClick(Preference preference) {
		 * Toast.makeText(getBaseContext(), "The custom preference has been clicked",
		 * Toast.LENGTH_LONG).show(); SharedPreferences customSharedPreference =
		 * getSharedPreferences( "myCustomSharedPrefs", Activity.MODE_PRIVATE);
		 * SharedPreferences.Editor editor = customSharedPreference.edit();
		 * editor.putString("myCustomPref", "The preference has been clicked"); editor.commit();
		 * return true; }
		 * 
		 * });
		 */

	}

	@Override
	protected void onResume()
	{
		super.onResume();
		PreferenceManager.getDefaultSharedPreferences(this).registerOnSharedPreferenceChangeListener(this);
	}

	@Override
	protected void onPause()
	{
		super.onPause();
		Log.v(TAG, "Preferences listener unregistered");
		PreferenceManager.getDefaultSharedPreferences(this).unregisterOnSharedPreferenceChangeListener(this);
	}

	@Override
	public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key)
	{
		Preference p = findPreference(key);

		Log.d(TAG, "Preferences changed, key=" + key + ", editor=" + p.getClass().getSimpleName());
		Storage.applyPreference(sharedPreferences, key);

		if (p instanceof EditTextPreference)
		{
			if (!PREF_SYNC_PASSWORD_KEY.equals(key))
			{
				findPreference(key).setSummary(sharedPreferences.getString(key, ""));
			}
		}
	}
}
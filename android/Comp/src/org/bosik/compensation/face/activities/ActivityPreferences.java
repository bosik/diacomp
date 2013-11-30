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

	public static String			PREF_ACCOUNT_PASSWORD_KEY;
	public static String			PREF_ACCOUNT_PASSWORD_DEFAULT;
	public static String			PREF_ACCOUNT_SERVER_KEY;
	public static String			PREF_ACCOUNT_SERVER_DEFAULT;
	public static String			PREF_ACCOUNT_USERNAME_KEY;
	public static String			PREF_ACCOUNT_USERNAME_DEFAULT;

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
		PREF_ACCOUNT_SERVER_KEY = context.getString(R.string.pref_Account_Server_Key);
		PREF_ACCOUNT_SERVER_DEFAULT = context.getString(R.string.pref_Account_Server_Default);
		PREF_ACCOUNT_USERNAME_KEY = context.getString(R.string.pref_Account_Username_Key);
		PREF_ACCOUNT_USERNAME_DEFAULT = context.getString(R.string.pref_Account_Username_Default);
		PREF_ACCOUNT_PASSWORD_KEY = context.getString(R.string.pref_Account_Password_Key);
		PREF_ACCOUNT_PASSWORD_DEFAULT = context.getString(R.string.pref_Account_Password_Default);

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
			updateDescription(preferences, key);
		}
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
		PreferenceManager.getDefaultSharedPreferences(this).unregisterOnSharedPreferenceChangeListener(this);
	}

	@Override
	public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key)
	{
		Log.v(TAG, String.format("Preference '%s' changed", key));
		updateDescription(sharedPreferences, key);
		Storage.applyPreference(sharedPreferences, key);
	}

	private void updateDescription(SharedPreferences sharedPreferences, String key)
	{
		Log.v(TAG, String.format("Updating description for preference '%s'", key));

		Preference p = findPreference(key);
		if (p instanceof EditTextPreference)
		{
			if (!PREF_ACCOUNT_PASSWORD_KEY.equals(key))
			{
				findPreference(key).setSummary(sharedPreferences.getString(key, ""));
			}
		}
	}
}
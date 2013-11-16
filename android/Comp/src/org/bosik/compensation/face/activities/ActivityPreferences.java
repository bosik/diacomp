package org.bosik.compensation.face.activities;

import org.bosik.compensation.face.R;
import android.os.Bundle;
import android.preference.PreferenceActivity;

public class ActivityPreferences extends PreferenceActivity
{
	/* =========================== КОНСТАНТЫ ================================ */

	@SuppressWarnings("unused")
	private static final String	TAG	= "ActivityPreferences";

	// public static SharedPreferences pref = null;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		addPreferencesFromResource(R.xml.preferences);
		// pref = PreferenceManager.getDefaultSharedPreferences(this);
		// pref.registerOnSharedPreferenceChangeListener(spChanged);

		// Get the custom preference
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

	/*
	 * OnSharedPreferenceChangeListener spChanged = new OnSharedPreferenceChangeListener() { public
	 * void onSharedPreferenceChanged(SharedPreferences pref, String key) { //Log.d(TAG, "Key " +
	 * key + " has been modified, new value is " + pref.getString(key, "hmmm"));
	 * applyPreference(key); } };
	 */
}
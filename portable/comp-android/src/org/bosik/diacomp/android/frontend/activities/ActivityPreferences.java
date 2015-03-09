package org.bosik.diacomp.android.frontend.activities;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.preferences.PreferencesLocalService;
import org.bosik.diacomp.core.services.preferences.Preference;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.os.Bundle;
import android.preference.EditTextPreference;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.util.Log;

public class ActivityPreferences extends PreferenceActivity implements OnSharedPreferenceChangeListener
{
	private static final String		TAG	= ActivityPreferences.class.getSimpleName();

	// Services
	private SharedPreferences		systemPreferences;
	private PreferencesTypedService	syncablePreferences;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		// Initializing services
		syncablePreferences = new PreferencesLocalService(getContentResolver());
		systemPreferences = PreferenceManager.getDefaultSharedPreferences(this);

		// Filling values
		Editor editor = systemPreferences.edit();
		for (Preference preference : Preference.values())
		{
			editor.putString(preference.getKey(), syncablePreferences.getStringValue(preference));
		}
		editor.apply();

		// Inflating layout
		addPreferencesFromResource(R.xml.preferences);
		for (String key : systemPreferences.getAll().keySet())
		{
			updateDescription(systemPreferences, key);
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
	public void onSharedPreferenceChanged(SharedPreferences preferences, String key)
	{
		Log.v(TAG, String.format("Preference '%s' changed", key));
		updateDescription(preferences, key);
		// Storage.applyPreference(sharedPreferences, key);

		for (Preference preference : Preference.values())
		{
			if (preference.getKey().equals(key))
			{
				PreferenceEntry<String> entry = syncablePreferences.getString(preference);

				if (entry == null)
				{
					entry = new PreferenceEntry<String>();
					entry.setType(preference);
					entry.setVersion(1);
				}
				else
				{
					entry.setVersion(entry.getVersion() + 1);
				}
				entry.setValue(preferences.getString(key, preference.getDefaultValue()));
				syncablePreferences.setString(entry);

				break;
			}
		}
	}

	private void updateDescription(SharedPreferences sharedPreferences, String key)
	{
		Log.v(TAG, String.format("Updating description for preference '%s'", key));

		android.preference.Preference p = findPreference(key);
		if (p instanceof EditTextPreference)
		{
			p.setSummary(sharedPreferences.getString(key, ""));
		}
	}
}
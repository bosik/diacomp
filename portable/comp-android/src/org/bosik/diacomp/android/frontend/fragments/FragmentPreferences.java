/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package org.bosik.diacomp.android.frontend.fragments;

import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.EditTextPreference;
import android.preference.PreferenceFragment;
import android.preference.PreferenceManager;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.notifications.NotificationService;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;

public class FragmentPreferences extends PreferenceFragment implements SharedPreferences.OnSharedPreferenceChangeListener
{
	// Services
	private SharedPreferences       devicePreferences;
	private PreferencesTypedService syncablePreferences;

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		// Initializing services
		syncablePreferences = new PreferencesTypedService(new PreferencesLocalService(getActivity()));
		devicePreferences = PreferenceManager.getDefaultSharedPreferences(getActivity());

		// Copy syncable preferences to local ones
		SharedPreferences.Editor editor = devicePreferences.edit();
		for (PreferenceID id : PreferenceID.values())
		{
			switch (id.getType())
			{
				case STRING:
				{
					editor.putString(id.getKey(), syncablePreferences.getStringValue(id));
					break;
				}
				case FLOAT:
				{
					// EditTextPreference supposed preference is String
					editor.putString(id.getKey(), Utils.compactDecimal(syncablePreferences.getStringValue(id)));
					break;
				}
				case BOOLEAN:
				{
					editor.putBoolean(id.getKey(), syncablePreferences.getBooleanValue(id));
					break;
				}
				default:
				{
					throw new IllegalArgumentException("Unsupported preference type: " + id.getType());
				}
			}
		}
		editor.apply();

		// display (only after preferences are filled!)
		addPreferencesFromResource(R.xml.preferences);

		for (String key : devicePreferences.getAll().keySet())
		{
			updateDescription(devicePreferences, key);
		}
	}

	@Override
	public void onResume()
	{
		super.onResume();
		PreferenceManager.getDefaultSharedPreferences(getActivity()).registerOnSharedPreferenceChangeListener(this);
	}

	@Override
	public void onPause()
	{
		super.onPause();
		PreferenceManager.getDefaultSharedPreferences(getActivity()).unregisterOnSharedPreferenceChangeListener(this);
	}

	@Override
	public void onSharedPreferenceChanged(SharedPreferences preferences, String key)
	{
		updateDescription(preferences, key);

		PreferenceID id = PreferenceID.parseSafely(key);

		if (id != null) // if it is a syncable preference
		{
			PreferenceEntry<String> entry = syncablePreferences.getString(id);

			if (entry == null)
			{
				entry = new PreferenceEntry<>();
				entry.setId(id);
				entry.setVersion(1);
			}
			else
			{
				entry.setVersion(entry.getVersion() + 1);
			}

			switch (id.getType())
			{
				case STRING:
				{
					entry.setValue(preferences.getString(id.getKey(), id.getDefaultValue()));
					break;
				}
				case FLOAT: // EditTextPreference supposed preference is String
				{
					entry.setValue(Utils.compactDecimal(preferences.getString(id.getKey(), id.getDefaultValue())));
					break;
				}
				case BOOLEAN:
				{
					entry.setValue(String.valueOf(preferences.getBoolean(id.getKey(), Boolean.parseBoolean(id.getDefaultValue()))));
					break;
				}
				default:
				{
					throw new IllegalArgumentException("Unsupported type: " + id.getType());
				}
			}

			syncablePreferences.setString(entry);
		}

		if (PreferenceID.ANDROID_SHOW_TIME_AFTER.getKey().equals(key))
		{
			if (preferences.getBoolean(key, true))
			{
				getActivity().startService(new Intent(getActivity(), NotificationService.class));
			}
			else
			{
				getActivity().stopService(new Intent(getActivity(), NotificationService.class));
			}
		}
	}

	private void updateDescription(SharedPreferences sharedPreferences, String key)
	{
		android.preference.Preference p = findPreference(key);
		Object value = sharedPreferences.getAll().get(key);

		if (value == null)
		{
			p.setSummary("");
		}
		else
		{
			if (p instanceof EditTextPreference)
			{
				if (value instanceof String)
				{
					p.setSummary((String) value);
				}
				else
				{
					throw new IllegalArgumentException("Unsupported type: " + value.getClass().getName());
				}
			}
		}
	}
}

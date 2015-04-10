/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.services.preferences;

import java.util.List;
import java.util.Set;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerSet;

/**
 * Adapter with casting methods
 */
public class PreferencesTypedService extends PreferencesService
{
	private PreferencesService	service;

	public PreferencesTypedService(PreferencesService service)
	{
		if (service == null)
		{
			throw new IllegalArgumentException("Service is null");
		}

		this.service = service;
	}

	@Override
	public List<PreferenceEntry<String>> getAll()
	{
		return service.getAll();
	}

	@Override
	public PreferenceEntry<String> getString(Preference preference)
	{
		return service.getString(preference);
	}

	@Override
	public void setString(PreferenceEntry<String> entry)
	{
		service.setString(entry);
	}

	@Override
	public void update(List<PreferenceEntry<String>> entries)
	{
		service.update(entries);
	}

	private int getNextVersion(Preference preference)
	{
		PreferenceEntry<String> oldPreferenceEntry = service.getString(preference);
		if (oldPreferenceEntry == null)
		{
			return 1;
		}
		else
		{
			return oldPreferenceEntry.getVersion() + 1;
		}
	}

	private PreferenceEntry<String> buildEntry(Preference preference, String value)
	{
		PreferenceEntry<String> entry = new PreferenceEntry<String>();
		entry.setType(preference);
		entry.setValue(value);
		entry.setVersion(getNextVersion(preference));
		return entry;
	}

	/**
	 * Returns preference as string value
	 * 
	 * @param preference
	 * @return Value if found, default otherwise
	 */
	public String getStringValue(Preference preference)
	{
		PreferenceEntry<String> entry = service.getString(preference);

		if (entry != null)
		{
			return entry.getValue();
		}
		else
		{
			return preference.getDefaultValue();
		}
	}

	/**
	 * Updates string preference. Version is incremented automatically.
	 * 
	 * @param preference
	 * @param set
	 */
	public void setStringValue(Preference preference, String value)
	{
		service.setString(buildEntry(preference, value));
	}

	/**
	 * Returns preference as float value
	 * 
	 * @param preference
	 * @return Value if found, default otherwise
	 */
	public Float getFloatValue(Preference preference)
	{
		return Float.parseFloat(getStringValue(preference));
	}

	/**
	 * Updates float preference. Version is incremented automatically.
	 * 
	 * @param preference
	 * @param set
	 */
	public void setFloatValue(Preference preference, Float value)
	{
		service.setString(buildEntry(preference, String.valueOf(value)));
	}

	/**
	 * Returns preference as double value
	 * 
	 * @param preference
	 * @return Value if found, default otherwise
	 */
	public Double getDoubleValue(Preference preference)
	{
		return Double.parseDouble(getStringValue(preference));
	}

	/**
	 * Updates double preference. Version is incremented automatically.
	 * 
	 * @param preference
	 * @param set
	 */
	public void setDoubleValue(Preference preference, Float value)
	{
		service.setString(buildEntry(preference, String.valueOf(value)));
	}

	/**
	 * Returns preference as string set
	 * 
	 * @param preference
	 * @return Value if found, default otherwise
	 */
	public Set<String> getStringSet(Preference preference)
	{
		Serializer<Set<String>> serializer = new SerializerSet();
		String value = getStringValue(preference);
		return serializer.read(value);
	}

	/**
	 * Updates string set preference. Version is incremented automatically.
	 * 
	 * @param preference
	 * @param set
	 */
	public void setStringSet(Preference preference, Set<String> set)
	{
		Serializer<Set<String>> serializer = new SerializerSet();
		String value = serializer.write(set);
		service.setString(buildEntry(preference, value));
	}
}

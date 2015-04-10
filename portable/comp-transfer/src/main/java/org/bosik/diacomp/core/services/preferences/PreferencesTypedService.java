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

import java.util.Set;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerSet;

public abstract class PreferencesTypedService implements PreferencesService
{
	@Override
	public String getHash()
	{
		final int prime = 31;
		int hash = 1;

		for (PreferenceEntry<String> entity : getAll())
		{
			hash = prime * hash + entity.getVersion();
		}

		return String.valueOf(hash);
	}

	private int getNextVersion(Preference preference)
	{
		PreferenceEntry<String> oldPreferenceEntry = getString(preference);
		if (oldPreferenceEntry == null)
		{
			return 1;
		}
		else
		{
			return oldPreferenceEntry.getVersion() + 1;
		}
	}

	/**
	 * Returns preference as string value
	 * 
	 * @param preference
	 * @return Value if found, default otherwise
	 */
	public String getStringValue(Preference preference)
	{
		PreferenceEntry<String> entry = getString(preference);

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

		PreferenceEntry<String> entry = new PreferenceEntry<String>();
		entry.setType(preference);
		entry.setValue(value);
		entry.setVersion(getNextVersion(preference));

		setString(entry);
	}
}

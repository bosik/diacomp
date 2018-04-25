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

import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerSet;
import org.bosik.diacomp.core.utils.Utils;

import java.text.ParseException;
import java.util.List;
import java.util.Set;

/**
 * Decorator with casting methods
 */
public class PreferencesTypedService implements PreferencesService
{
	private PreferencesService service;

	public PreferencesTypedService(PreferencesService service)
	{
		if (service == null)
		{
			throw new IllegalArgumentException("Service is null");
		}

		this.service = service;
	}

	@Override
	public String getHash()
	{
		return service.getHash();
	}

	@Override
	public List<PreferenceEntry<String>> getAll()
	{
		return service.getAll();
	}

	@Override
	public PreferenceEntry<String> getString(PreferenceID id)
	{
		return service.getString(id);
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

	private int getNextVersion(PreferenceID preferenceID)
	{
		PreferenceEntry<String> oldPreferenceEntry = service.getString(preferenceID);
		if (oldPreferenceEntry == null)
		{
			return 1;
		}
		else
		{
			return oldPreferenceEntry.getVersion() + 1;
		}
	}

	private PreferenceEntry<String> buildEntry(PreferenceID preferenceID, String value)
	{
		PreferenceEntry<String> entry = new PreferenceEntry<>();
		entry.setId(preferenceID);
		entry.setValue(value);
		entry.setVersion(getNextVersion(preferenceID));
		return entry;
	}

	/**
	 * Returns preference as string value
	 *
	 * @param preferenceID
	 * @return Value if found, default otherwise
	 */
	public String getStringValue(PreferenceID preferenceID)
	{
		PreferenceEntry<String> entry = service.getString(preferenceID);

		if (entry != null)
		{
			return entry.getValue();
		}
		else
		{
			return preferenceID.getDefaultValue();
		}
	}

	/**
	 * Updates string preference. Version is incremented automatically.
	 *
	 * @param preferenceID
	 * @param value
	 */
	public void setStringValue(PreferenceID preferenceID, String value)
	{
		service.setString(buildEntry(preferenceID, value));
	}

	/**
	 * Returns preference as boolean value
	 *
	 * @param preferenceID
	 * @return Value if found, default otherwise
	 */
	public Boolean getBooleanValue(PreferenceID preferenceID)
	{
		return Boolean.parseBoolean(getStringValue(preferenceID));
	}

	/**
	 * Updates boolean preference. Version is incremented automatically.
	 *
	 * @param preferenceID
	 * @param value
	 */
	public void setBooleanValue(PreferenceID preferenceID, Boolean value)
	{
		service.setString(buildEntry(preferenceID, String.valueOf(value)));
	}

	/**
	 * Returns preference as float value
	 *
	 * @param preferenceID
	 * @return Value if found, default otherwise
	 */
	public Float getFloatValue(PreferenceID preferenceID) throws ParseException
	{
		String value = getStringValue(preferenceID);
		return (float) Utils.parseDouble(value);
	}

	/**
	 * Updates float preference. Version is incremented automatically.
	 *
	 * @param preferenceID
	 * @param value
	 */
	public void setFloatValue(PreferenceID preferenceID, Float value)
	{
		service.setString(buildEntry(preferenceID, String.valueOf(value)));
	}

	/**
	 * Returns preference as double value
	 *
	 * @param preferenceID
	 * @return Value if found, default otherwise
	 */
	public Double getDoubleValue(PreferenceID preferenceID) throws ParseException
	{
		String value = getStringValue(preferenceID);
		return Utils.parseDouble(value);
	}

	/**
	 * Updates double preference. Version is incremented automatically.
	 *
	 * @param preferenceID
	 * @param value
	 */
	public void setDoubleValue(PreferenceID preferenceID, Float value)
	{
		service.setString(buildEntry(preferenceID, String.valueOf(value)));
	}

	/**
	 * Returns preference as string set
	 *
	 * @param preferenceID
	 * @return Value if found, default otherwise
	 */
	public Set<String> getStringSet(PreferenceID preferenceID)
	{
		Serializer<Set<String>> serializer = new SerializerSet();
		String value = getStringValue(preferenceID);
		return serializer.read(value);
	}

	/**
	 * Updates string set preference. Version is incremented automatically.
	 *
	 * @param preferenceID
	 * @param set
	 */
	public void setStringSet(PreferenceID preferenceID, Set<String> set)
	{
		Serializer<Set<String>> serializer = new SerializerSet();
		String value = serializer.write(set);
		service.setString(buildEntry(preferenceID, value));
	}
}

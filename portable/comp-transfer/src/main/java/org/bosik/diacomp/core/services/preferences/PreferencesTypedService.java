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

	/**
	 * Returns null if preference not found
	 * 
	 * @param preference
	 * 
	 * @return
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
			return null;
		}
	}

	/**
	 * Returns null if preference not found
	 * 
	 * @param preference
	 * 
	 * @return
	 */
	public Float getFloatValue(Preference preference)
	{
		PreferenceEntry<String> entry = getString(preference);

		if (entry != null)
		{
			return Float.parseFloat(entry.getValue());
		}
		else
		{
			return null;
		}
	}

	/**
	 * Returns null if preference not found
	 * 
	 * @param preference
	 * 
	 * @return
	 */
	public Double getDoubleValue(Preference preference)
	{
		PreferenceEntry<String> entry = getString(preference);

		if (entry != null)
		{
			return Double.parseDouble(entry.getValue());
		}
		else
		{
			return null;
		}
	}

	//	public void setDouble(PreferenceEntry<String> entry)
	//	{
	//		PreferenceEntry<String> result = new PreferenceEntry<String>();
	//		result.setType(entry.getType());
	//		result.setValue(String.valueOf(entry.getValue()));
	//		result.setVersion(entry.getVersion());
	//
	//		setString(result);
	//	}
}

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

/**
 * Contains all available account preferences (syncable)
 */
public enum PreferenceID
{
	/**
	 * Target blood sugar, in mmol/l
	 */
	TARGET_BS("e6681282aa724d3fa4cd6ac5735a163f", "5.5"),
	/**
	 * List of preferred food sets
	 */
	FOOD_SETS("1a25c92eaa3148219da83b1e66275052", "[]"),

	ANDROID_FIRST_START("8b6575e476d64becae68468500f1bc1c", "true"),

	ANDROID_SHOW_TIME_AFTER("d5c1a902e83b4d05a51085e344bee953", "true");

	private String  key;
	private String  defaultValue;

	PreferenceID(String key, String defaultValue)
	{
		this.key = key;
		this.defaultValue = defaultValue;
	}

	public String getKey()
	{
		return key;
	}

	public String getDefaultValue()
	{
		return defaultValue;
	}

	public static PreferenceID parse(String key)
	{
		for (PreferenceID value : values())
		{
			if (value.getKey().equals(key))
			{
				return value;
			}
		}

		throw new IllegalArgumentException("Unknown key: " + key);
	}
}

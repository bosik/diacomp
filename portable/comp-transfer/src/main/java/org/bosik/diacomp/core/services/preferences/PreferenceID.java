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
	TARGET_BS("e6681282aa724d3fa4cd6ac5735a163f", Type.FLOAT, "5.5", true),
	/**
	 * List of preferred food sets
	 */
	FOOD_SETS("1a25c92eaa3148219da83b1e66275052", Type.STRING, "[]", true),
	/**
	 * Calculate rates automatically
	 */
	RATES_AUTO("e92b955f48fa434d960fdc4a541490de", Type.BOOLEAN, "true", true),

	ANDROID_FIRST_START("8b6575e476d64becae68468500f1bc1c", Type.BOOLEAN, "true", false),

	ANDROID_SHOW_TIME_AFTER("d5c1a902e83b4d05a51085e344bee953", Type.BOOLEAN, "true", true);

	private String  key;
	private Type    type;
	private String  defaultValue;
	private boolean syncable;

	PreferenceID(String key, Type type, String defaultValue, boolean syncable)
	{
		this.key = key;
		this.type = type;
		this.defaultValue = defaultValue;
		this.syncable = syncable;
	}

	public String getKey()
	{
		return key;
	}

	public Type getType()
	{
		return type;
	}

	public String getDefaultValue()
	{
		return defaultValue;
	}

	public boolean isSyncable()
	{
		return syncable;
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

	public static PreferenceID parseSafely(String key)
	{
		for (PreferenceID value : values())
		{
			if (value.getKey().equals(key))
			{
				return value;
			}
		}

		return null;
	}
}

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
 * Contains all available user preferences
 */
public enum Preference {
	/**
	 * Target blood sugar, in mmol/l
	 */
	TARGET_BS("e6681282aa724d3fa4cd6ac5735a163f", "5.5");

	private String	key;
	private String	defaultValue;

	private Preference(String key, String defaultValue)
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

	public static Preference parse(String key)
	{
		for (Preference value : values())
		{
			if (value.getKey().equals(key))
			{
				return value;
			}
		}

		throw new IllegalArgumentException("Unknown key: " + key);
	}
}

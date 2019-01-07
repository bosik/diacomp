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

import org.bosik.diacomp.core.entities.business.Units;
import org.bosik.diacomp.core.entities.tech.Coded;
import org.bosik.diacomp.core.services.diary.MealFormat;

/**
 * Contains all available account preferences
 */
public enum PreferenceID implements Coded
{
	/**
	 * Target blood sugar, in mmol/l
	 */
	TARGET_BS("e6681282aa724d3fa4cd6ac5735a163f", Type.FLOAT, "5.5", true),
	/**
	 * List of preferred food sets
	 */
	@Deprecated FOOD_SETS("1a25c92eaa3148219da83b1e66275052", Type.STRING, "[]", true),
	/**
	 * Calculate rates automatically
	 */
	RATES_AUTO("e92b955f48fa434d960fdc4a541490de", Type.BOOLEAN, "true", true),
	/**
	 * Manually specified rates (as JSON array)
	 */
	RATES_DATA("7648a35d3fbe4b4ca8fab12876abb1b6", Type.STRING, "[]", true),
	/**
	 * Units of mass to use in manual rates, options are {@link Units.Mass}'s codes
	 */
	RATES_MASS_UNITS("589985b443c243d6844209964b2b1e8e", Type.STRING, Units.Mass.G.getCode(), true),
	/**
	 * If it's first start on Android device. TODO: move it?
	 */
	ANDROID_FIRST_START("8b6575e476d64becae68468500f1bc1c", Type.BOOLEAN, "true", false),
	/**
	 * Show notification about time elapsed after meal/injection
	 */
	ANDROID_SHOW_TIME_AFTER("d5c1a902e83b4d05a51085e344bee953", Type.BOOLEAN, "true", true),
	/**
	 * How to display meals in diary
	 */
	ANDROID_MEAL_FORMAT("f3f54f8f02a3411faf48f90aadf0ca2d", Type.STRING, MealFormat.SINGLE_MOST_CARBS.getCode(), true),
	/**
	 * Display records separator in diary
	 */
	ANDROID_DIARY_USE_SEPARATOR("ba9c6c26873e4bc19b96b8838901ed7a", Type.BOOLEAN, "true", true);

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

	@Override
	public String getCode()
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
			if (value.getCode().equals(key))
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
			if (value.getCode().equals(key))
			{
				return value;
			}
		}

		return null;
	}
}

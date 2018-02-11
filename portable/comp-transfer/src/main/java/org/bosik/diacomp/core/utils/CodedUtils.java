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
package org.bosik.diacomp.core.utils;

import org.bosik.diacomp.core.entities.tech.Coded;

public class CodedUtils
{
	/**
	 * Parses entity by code
	 *
	 * @param entityClass Class to parse to
	 * @param code        Code to parse
	 * @return Entity having specified code if found
	 * @throws IllegalArgumentException If code is unknown
	 */
	public static <T extends Enum<T> & Coded> T parse(Class<T> entityClass, String code) throws IllegalArgumentException
	{
		T[] values = entityClass.getEnumConstants();
		if (values == null)
		{
			// should never actually happen
			throw new IllegalArgumentException("Class " + entityClass.getName() + " is not enum");
		}

		for (T entity : values)
		{
			if (entity.getCode().equals(code))
			{
				return entity;
			}
		}

		throw new IllegalArgumentException("Failed to parse " + entityClass.getName() + ": unknown code " + code);
	}

	/**
	 * Parses entity by code
	 *
	 * @param entityClass  Class to parse to
	 * @param code         Code to parse
	 * @param defaultValue Default value
	 * @return Entity having specified code if found, {@code defaultValue} otherwise
	 */
	public static <T extends Enum<T> & Coded> T parse(Class<T> entityClass, String code, T defaultValue)
	{
		T[] values = entityClass.getEnumConstants();
		if (values == null)
		{
			// should never actually happen
			throw new IllegalArgumentException("Class " + entityClass.getName() + " is not enum");
		}

		for (T unit : values)
		{
			if (unit.getCode().equals(code))
			{
				return unit;
			}
		}

		return defaultValue;
	}
}

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
package org.bosik.diacomp.android.backend.common.db;

public class Column
{
	public static final String TYPE_INTEGER = "INTEGER";
	public static final String TYPE_REAL    = "REAL";
	public static final String TYPE_TEXT    = "TEXT";
	public static final String TYPE_BLOB    = "BLOB";

	private final String  name;
	private final String  type;
	private final boolean nullable;
	private final boolean primary;

	public Column(String name, String type, boolean primary, boolean nullable)
	{
		this.name = name;
		this.type = type;
		this.primary = primary;
		this.nullable = nullable;
	}

	public String getName()
	{
		return name;
	}

	public String getType()
	{
		return type;
	}

	public boolean isPrimary()
	{
		return primary;
	}

	public boolean isNullable()
	{
		return nullable;
	}
}

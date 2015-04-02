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
 * The base class for preferences
 * 
 * @param <T>
 *            Type of preference value
 */
public class PreferenceEntry<T>
{
	private Preference	type;
	private T			value;
	private int			version;

	/**
	 * Return preference type
	 * 
	 * @return
	 */
	public Preference getType()
	{
		return type;
	}

	public void setType(Preference type)
	{
		this.type = type;
	}

	/**
	 * Returns preference value
	 * 
	 * @return
	 */
	public T getValue()
	{
		return value;
	}

	/**
	 * Sets preference value
	 * 
	 * @param value
	 */
	public void setValue(T value)
	{
		this.value = value;
	}

	/**
	 * Returns preference version, which is increased by one every time the preference is changed
	 * 
	 * @return
	 */
	public int getVersion()
	{
		return version;
	}

	/**
	 * Sets the preference version
	 * 
	 * @param timestamp
	 */
	public void setVersion(int timestamp)
	{
		this.version = timestamp;
	}
}

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

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The base class for preferences
 *
 * @param <T> Type of preference value
 */
public class PreferenceEntry<T>
{
	@JsonIgnore
	private PreferenceID id;

	@JsonProperty("value")
	private T value;

	@JsonProperty("version")
	private int version;

	/**
	 * Return preference type
	 *
	 * @return
	 */
	public PreferenceID getId()
	{
		return id;
	}

	public void setId(PreferenceID id)
	{
		this.id = id;
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
	 * @param version
	 */
	public void setVersion(int version)
	{
		this.version = version;
	}

	@JsonProperty("key")
	public String getKey()
	{
		return getId().getCode();
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
			return true;
		if (!(o instanceof PreferenceEntry))
			return false;

		PreferenceEntry<?> that = (PreferenceEntry<?>) o;

		if (version != that.version)
			return false;
		if (id != that.id)
			return false;
		return value != null ? value.equals(that.value) : that.value == null;
	}

	@Override
	public int hashCode()
	{
		int result = id != null ? id.hashCode() : 0;
		result = 31 * result + (value != null ? value.hashCode() : 0);
		result = 31 * result + version;
		return result;
	}
}

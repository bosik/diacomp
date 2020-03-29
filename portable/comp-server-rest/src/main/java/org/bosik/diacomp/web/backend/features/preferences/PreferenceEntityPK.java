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
package org.bosik.diacomp.web.backend.features.preferences;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.io.Serializable;

@Embeddable
public class PreferenceEntityPK implements Serializable
{
	@Column(name = "user_id")
	private int userId;

	@Column(name = "key", columnDefinition = "CHAR(32)")
	private String key;

	public PreferenceEntityPK()
	{
	}

	public PreferenceEntityPK(int userId, String key)
	{
		this.userId = userId;
		this.key = key;
	}

	public int getUserId()
	{
		return userId;
	}

	public void setUserId(int userId)
	{
		this.userId = userId;
	}

	public String getKey()
	{
		return key;
	}

	public void setKey(String key)
	{
		this.key = key;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
			return true;
		if (!(o instanceof PreferenceEntityPK))
			return false;

		PreferenceEntityPK that = (PreferenceEntityPK) o;

		if (userId != that.userId)
			return false;
		return key.equals(that.key);
	}

	@Override
	public int hashCode()
	{
		int result = userId;
		result = 31 * result + key.hashCode();
		return result;
	}
}

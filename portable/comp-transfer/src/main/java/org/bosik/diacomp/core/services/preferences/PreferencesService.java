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

import java.util.List;

public abstract class PreferencesService
{
	/**
	 * Returns hash
	 *
	 * @return
	 */
	public String getHash()
	{
		final int prime = 31;
		int hash = 1;

		for (PreferenceEntry<String> entity : getAll())
		{
			hash = prime * hash + entity.getVersion();

			// It's a public API and can't be changed
			// hash = prime * hash + entity.hashCode();
		}

		return String.valueOf(hash);
	}

	/**
	 * Returns all preferences
	 *
	 * @return
	 */
	public abstract List<PreferenceEntry<String>> getAll();

	/**
	 * Returns string preference
	 *
	 * @param id
	 * @return Entry if preference found, null otherwise
	 */
	public abstract PreferenceEntry<String> getString(PreferenceID id);

	/**
	 * Updates single string entry
	 *
	 * @param entry
	 */
	public abstract void setString(PreferenceEntry<String> entry);

	/**
	 * Updates multiple string entries
	 *
	 * @param entries
	 */
	public abstract void update(List<PreferenceEntry<String>> entries);
}

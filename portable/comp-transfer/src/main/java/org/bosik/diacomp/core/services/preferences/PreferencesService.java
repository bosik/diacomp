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

public interface PreferencesService
{
	/**
	 * Returns hash
	 * 
	 * @return
	 */
	String getHash();

	/**
	 * Returns all preferences
	 * 
	 * @return
	 */
	List<PreferenceEntry<String>> getAll();

	/**
	 * Updates specified entries
	 * 
	 * @param entries
	 */
	void update(List<PreferenceEntry<String>> entries);

	/**
	 * Returns null if preference not found
	 * 
	 * @param preference
	 * @return
	 */
	PreferenceEntry<String> getString(Preference preference);

	void setString(PreferenceEntry<String> entry);
}

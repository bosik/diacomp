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

import org.bosik.diacomp.core.utils.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PreferencesSync
{
	/**
	 * Synchronizes two preference providers
	 *
	 * @param preferences1
	 * @param preferences2
	 * @return True if any data was transferred, false otherwise (i.e. if services already had been
	 * synchronized)
	 */
	public static boolean synchronizePreferences(PreferencesService preferences1, PreferencesService preferences2)
	{
		String hash1 = preferences1.getHash();
		String hash2 = preferences2.getHash();

		if (hash1 != null && hash1.equals(hash2))
		{
			return false;
		}

		// build maps

		Map<PreferenceID, PreferenceEntry<String>> map1 = indexate(preferences1.getAll());
		Map<PreferenceID, PreferenceEntry<String>> map2 = indexate(preferences2.getAll());

		// build diff lists

		List<PreferenceEntry<String>> newer1 = new ArrayList<PreferenceEntry<String>>();
		List<PreferenceEntry<String>> newer2 = new ArrayList<PreferenceEntry<String>>();

		for (PreferenceID key : Utils.difference(map1.keySet(), map2.keySet()))
		{
			newer1.add(map1.get(key));
		}

		for (PreferenceID key : Utils.difference(map2.keySet(), map1.keySet()))
		{
			newer2.add(map2.get(key));
		}

		for (PreferenceID key : Utils.intersection(map1.keySet(), map2.keySet()))
		{
			PreferenceEntry<String> e1 = map1.get(key);
			PreferenceEntry<String> e2 = map2.get(key);

			if (e1.getVersion() > e2.getVersion())
			{
				newer1.add(e1);
			}
			else if (e1.getVersion() < e2.getVersion())
			{
				newer2.add(e2);
			}
		}

		// perform data transfer

		if (!newer2.isEmpty())
		{
			preferences1.update(newer2);
		}

		if (!newer1.isEmpty())
		{
			preferences2.update(newer1);
		}

		if (!newer1.isEmpty() || !newer2.isEmpty())
		{
			return true;
		}
		else
		{
			// Warning: this means some data remains unsynced
			return false;
		}
	}

	private static Map<PreferenceID, PreferenceEntry<String>> indexate(List<PreferenceEntry<String>> entries)
	{
		Map<PreferenceID, PreferenceEntry<String>> map = new HashMap<PreferenceID, PreferenceEntry<String>>();

		for (PreferenceEntry<String> entry : entries)
		{
			if (entry.getId().isSyncable())
			{
				map.put(entry.getId(), entry);
			}
		}

		return map;
	}
}

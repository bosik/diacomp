package org.bosik.diacomp.core.services.preferences;

import java.util.ArrayList;
import java.util.List;

public class PreferencesSync
{
	/**
	 * Synchronizes only items presented in both services
	 * 
	 * @param preferences1
	 * @param preferences2
	 * @return True if any data was transferred, false otherwise (i.e. if services already had been synchronized)
	 */
	public static boolean synchronizePreferences(PreferencesService preferences1, PreferencesService preferences2)
	{
		String hash1 = preferences1.getHash();
		String hash2 = preferences2.getHash();

		if (hash1 != null && hash1.equals(hash2))
		{
			return false;
		}

		List<PreferenceEntry<String>> entries1 = preferences1.getAll();
		List<PreferenceEntry<String>> entries2 = preferences2.getAll();
		List<PreferenceEntry<String>> newer1 = new ArrayList<PreferenceEntry<String>>();
		List<PreferenceEntry<String>> newer2 = new ArrayList<PreferenceEntry<String>>();

		for (PreferenceEntry<String> e1 : entries1)
		{
			boolean found = false;
			for (PreferenceEntry<String> e2 : entries2)
			{
				if (e1.getType() == e2.getType())
				{
					found = true;
					if (e1.getVersion() > e2.getVersion())
					{
						newer1.add(e1);
					}
					else if (e1.getVersion() < e2.getVersion())
					{
						newer2.add(e2);
					}
					break;
				}
			}

			if (!found)
			{
				newer1.add(e1);
			}
		}

		for (PreferenceEntry<String> e2 : entries2)
		{
			boolean found = false;
			for (PreferenceEntry<String> e1 : entries1)
			{
				if (e1.getType() == e2.getType())
				{
					found = true;
					break;
				}
			}

			if (!found)
			{
				newer2.add(e2);
			}
		}

		preferences1.update(newer2);
		preferences2.update(newer1);

		return !newer1.isEmpty() || !newer2.isEmpty();
	}

}

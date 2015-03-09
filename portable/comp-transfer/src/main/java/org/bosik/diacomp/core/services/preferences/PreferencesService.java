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

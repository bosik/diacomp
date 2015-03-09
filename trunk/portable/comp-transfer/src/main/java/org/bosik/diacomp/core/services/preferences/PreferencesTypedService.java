package org.bosik.diacomp.core.services.preferences;

public abstract class PreferencesTypedService implements PreferencesService
{
	@Override
	public String getHash()
	{
		final int prime = 31;
		int hash = 1;

		for (PreferenceEntry<String> entity : getAll())
		{
			hash = prime * hash + entity.getVersion();
		}

		return String.valueOf(hash);
	}

	/**
	 * Returns null if preference not found
	 * 
	 * @param preference
	 * 
	 * @return
	 */
	public String getStringValue(Preference preference)
	{
		PreferenceEntry<String> entry = getString(preference);

		if (entry != null)
		{
			return entry.getValue();
		}
		else
		{
			return null;
		}
	}

	/**
	 * Returns null if preference not found
	 * 
	 * @param preference
	 * 
	 * @return
	 */
	public Float getFloatValue(Preference preference)
	{
		PreferenceEntry<String> entry = getString(preference);

		if (entry != null)
		{
			return Float.parseFloat(entry.getValue());
		}
		else
		{
			return null;
		}
	}

	/**
	 * Returns null if preference not found
	 * 
	 * @param preference
	 * 
	 * @return
	 */
	public Double getDoubleValue(Preference preference)
	{
		PreferenceEntry<String> entry = getString(preference);

		if (entry != null)
		{
			return Double.parseDouble(entry.getValue());
		}
		else
		{
			return null;
		}
	}

	//	public void setDouble(PreferenceEntry<String> entry)
	//	{
	//		PreferenceEntry<String> result = new PreferenceEntry<String>();
	//		result.setType(entry.getType());
	//		result.setValue(String.valueOf(entry.getValue()));
	//		result.setVersion(entry.getVersion());
	//
	//		setString(result);
	//	}
}

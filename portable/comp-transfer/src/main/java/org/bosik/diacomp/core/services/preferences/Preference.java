package org.bosik.diacomp.core.services.preferences;

/**
 * Contains all available user preferences
 * 
 * @author Bosik
 * 
 */
public enum Preference {
	/**
	 * Target blood sugar, in mmol/l
	 */
	TARGET_BS("e6681282aa724d3fa4cd6ac5735a163f", "5.5");

	private String	key;
	private String	defaultValue;

	private Preference(String key, String defaultValue)
	{
		this.key = key;
		this.defaultValue = defaultValue;
	}

	public String getKey()
	{
		return key;
	}

	public String getDefaultValue()
	{
		return defaultValue;
	}

	public static Preference parse(String key)
	{
		for (Preference value : values())
		{
			if (value.getKey().equals(key))
			{
				return value;
			}
		}

		throw new IllegalArgumentException("Unknown key: " + key);
	}
}

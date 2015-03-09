package org.bosik.diacomp.core.services.preferences;

/**
 * The base class for preferences
 * 
 * @author Bosik
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

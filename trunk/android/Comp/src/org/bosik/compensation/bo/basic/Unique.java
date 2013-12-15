package org.bosik.compensation.bo.basic;

import java.io.Serializable;
import java.util.Date;
import java.util.UUID;
import org.bosik.compensation.utils.Utils;

/**
 * Has ID field (random; useful for comparing)
 * 
 * @author Bosik
 */
public class Unique implements Cloneable, Serializable, Versioned
{
	private static final long	serialVersionUID	= 6063993499772711799L;

	private String				id;
	private Date				timeStamp;
	private int					version;

	public Unique()
	{
		id = UUID.randomUUID().toString();
		version = 1;
	}

	@Override
	public String getId()
	{
		return id;
	}

	public void setId(String id)
	{
		if (id == null)
		{
			throw new NullPointerException("ID can't be null");
		}

		this.id = id;
	}

	@Override
	public Date getTimeStamp()
	{
		return timeStamp;
	}

	public void setTimeStamp(Date timeStamp)
	{
		this.timeStamp = timeStamp;
	}

	@Override
	public int getVersion()
	{
		return version;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	public void updateTimeStamp()
	{
		version++;
		timeStamp = Utils.now();
	}

	// ================================ CLONE ================================

	@Override
	public Unique clone() throws CloneNotSupportedException
	{
		Unique result = (Unique) super.clone();
		result.id = id;
		return result;
	}

	// ================================ OTHER ================================

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = (prime * result) + ((id == null) ? 0 : id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Unique other = (Unique) obj;
		if (id == null)
		{
			if (other.id != null)
				return false;
		}
		else if (!id.equals(other.id))
			return false;
		return true;
	}
}
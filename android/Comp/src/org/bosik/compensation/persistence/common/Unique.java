package org.bosik.compensation.persistence.common;

import java.io.Serializable;
import java.util.UUID;

/**
 * Has ID field (random; useful for comparing)
 * 
 * @author Bosik
 */
public class Unique implements Cloneable, Serializable
{
	private static final long	serialVersionUID	= 6063993499772711799L;

	private String				id;

	public Unique()
	{
		id = UUID.randomUUID().toString();
	}

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
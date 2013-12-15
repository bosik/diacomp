package org.bosik.compensation.persistence.common;

import java.io.Serializable;
import java.util.Date;
import java.util.UUID;
import org.bosik.compensation.bo.basic.TrueCloneable;
import org.bosik.compensation.utils.Utils;

/**
 * Has ID field (random; useful for comparing)
 * 
 * @author Bosik
 */

// FIXME: remove Cloneable
public class Versioned<T /* extends TrueCloneable */> implements Serializable, TrueCloneable
{
	private static final long	serialVersionUID	= 6063993499772711799L;

	private String				id;
	private Date				timeStamp;
	private int					version;
	private T					data;

	// ================================ MAIN ================================

	public Versioned(T data)
	{
		id = UUID.randomUUID().toString();
		version = 0;
		updateTimeStamp();
		this.data = data;
	}

	public void updateTimeStamp()
	{
		version++;
		timeStamp = Utils.now();
	}

	// ================================ GET / SET ================================

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

	public Date getTimeStamp()
	{
		return timeStamp;
	}

	public void setTimeStamp(Date timeStamp)
	{
		this.timeStamp = timeStamp;
	}

	public int getVersion()
	{
		return version;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	public T getData()
	{
		return data;
	}

	public void setData(T data)
	{
		this.data = data;
	}

	// ================================ CLONE ================================

	@SuppressWarnings("unchecked")
	@Override
	public Versioned<T> clone() throws CloneNotSupportedException
	{
		throw new RuntimeException("Do not use clone method");

		// Versioned<T> result = (Versioned<T>) super.clone();
		// result.id = id;
		// result.timeStamp = timeStamp;
		// result.version = version;
		// result.data = (T) data.clone();
		// return result;
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
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Versioned<T> other = (Versioned<T>) obj;
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
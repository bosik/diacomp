package org.bosik.diacomp.core.entities.tech;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Date;
import org.bosik.diacomp.core.utils.Utils;

/**
 * Has ID field (random; useful for comparing)
 * 
 * @author Bosik
 */

public class Versioned<T> implements Serializable
{
	private static final long	serialVersionUID	= 6063993499772711799L;

	private String				id;
	private Date				timeStamp;
	private String				hash;
	private int					version;
	private boolean				deleted;

	private T					data;

	// ================================ MAIN ================================

	public Versioned()
	{
		this((T)null);
	}

	public Versioned(T data)
	{
		this.version = 0;
		this.data = data;
		this.deleted = false;
		updateTimeStamp();
	}

	public Versioned(Versioned<? extends T> object)
	{
		setId(object.getId());
		setTimeStamp(object.getTimeStamp());
		setHash(object.getHash());
		setVersion(object.getVersion());
		setDeleted(object.isDeleted());
		setData(object.getData());
	}

	// TODO: rename to 'modified'
	public void updateTimeStamp()
	{
		version++;
		hash = Utils.generateGuid();
		timeStamp = new Date();
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

	public String getHash()
	{
		return hash;
	}

	public void setHash(String hash)
	{
		this.hash = hash;
	}

	public int getVersion()
	{
		return version;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	public boolean isDeleted()
	{
		return deleted;
	}

	public void setDeleted(boolean deleted)
	{
		this.deleted = deleted;
	}

	public T getData()
	{
		return data;
	}

	public void setData(T data)
	{
		this.data = data;
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
		if (this == obj) return true;
		if (obj == null) return false;
		if (getClass() != obj.getClass()) return false;
		Versioned<T> other = (Versioned<T>)obj;
		if (id == null)
		{
			if (other.id != null) return false;
		}
		else if (!id.equals(other.id)) return false;
		return true;
	}

	@Override
	public String toString()
	{
		return String.format("(ID=%s, Hash=%s, Version=%d, Timestamp=%s)", getId(), getHash(), getVersion(),
				getTimeStamp().toString());
	}

	public static final Comparator<Versioned<?>>	COMPARATOR_GUID	= new Comparator<Versioned<?>>()
																	{
																		@Override
																		public int compare(Versioned<?> lhs,
																				Versioned<?> rhs)
																		{
																			return lhs.getId().compareTo(rhs.getId());
																		}
																	};
}

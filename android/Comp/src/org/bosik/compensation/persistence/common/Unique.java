package org.bosik.compensation.persistence.common;

import java.util.UUID;

/**
 * 1. Has ID field (useful for comparing).<br/>
 * 2. Implements {@link Cloneable}.
 * 
 * @author Bosik
 * 
 */
public class Unique implements Cloneable
{
	private String	id;
	private String	name;

	public Unique()
	{
		id = UUID.randomUUID().toString();
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

	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		if (name == null)
		{
			throw new NullPointerException("Name can't be null");
		}
		if (name.trim().equals(""))
		{
			throw new IllegalArgumentException("Name must contain non-whitespace characters");
		}

		this.name = name;
	}

	// ================================ CLONE ================================

	@Override
	public Unique clone() throws CloneNotSupportedException
	{
		Unique result = (Unique) super.clone();

		result.id = getId();

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
		else
			if (!id.equals(other.id))
				return false;
		return true;
	}
}
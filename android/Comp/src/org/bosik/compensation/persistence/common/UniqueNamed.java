package org.bosik.compensation.persistence.common;

public class UniqueNamed extends Unique
{
	private String	name;

	// ================================ GET / SET ================================

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
	public UniqueNamed clone() throws CloneNotSupportedException
	{
		UniqueNamed result = (UniqueNamed) super.clone();
		result.name = name;
		return result;
	}
}
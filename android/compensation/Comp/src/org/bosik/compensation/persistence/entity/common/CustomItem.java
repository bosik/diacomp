package org.bosik.compensation.persistence.entity.common;

/**
 * 1. Имеет поле ID (потребуется в методах get() / set() контейнеров).<br/>
 * 2. Реализует {@link Cloneable}.
 * 
 * @author Bosik
 * 
 */
public class CustomItem implements Cloneable
{
	private int id;

	// ================================ GET / SET ================================

	public int getId()
	{
		return id;
	}

	public void setId(int id)
	{
		this.id = id;
	}

	// ================================ CLONE ================================

	@Override
	public CustomItem clone() throws CloneNotSupportedException
	{
		CustomItem result = (CustomItem) super.clone();

		result.setId(getId());

		return result;
	}

	/*
	 * public CustomItem() {
	 * 
	 * }
	 * 
	 * public CustomItem(CustomItem copy) { setId(copy.getId()); }
	 */

	// ================================ OTHER ================================

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
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
		CustomItem other = (CustomItem) obj;
		if (id != other.id)
			return false;
		return true;
	}
}
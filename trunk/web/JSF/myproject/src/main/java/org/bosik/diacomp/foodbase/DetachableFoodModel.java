package org.bosik.diacomp.foodbase;

import org.apache.wicket.model.LoadableDetachableModel;

public class DetachableFoodModel extends LoadableDetachableModel<Food>
{
	private static final long	serialVersionUID	= 1L;

	private final String		id;

	/**
	 * @param c
	 */
	public DetachableFoodModel(Food c)
	{
		this(c.getId());
	}

	/**
	 * @param id
	 */
	public DetachableFoodModel(String id)
	{
		if (id == null)
		{
			throw new IllegalArgumentException();
		}
		this.id = id;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode()
	{
		return Long.valueOf(id).hashCode();
	}

	/**
	 * used for dataview with ReuseIfModelsEqualStrategy item reuse strategy
	 * 
	 * @see org.apache.wicket.markup.repeater.ReuseIfModelsEqualStrategy
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj)
	{
		if (obj == this)
		{
			return true;
		}
		else if (obj == null)
		{
			return false;
		}
		else if (obj instanceof DetachableFoodModel)
		{
			DetachableFoodModel other = (DetachableFoodModel) obj;
			return other.id == id;
		}
		return false;
	}

	/**
	 * @see org.apache.wicket.model.LoadableDetachableModel#load()
	 */
	@Override
	protected Food load()
	{
		// loads contact from the database
		// return getContactsDB().get(id);
		return new Food(id, "Food #" + id);
	}
}
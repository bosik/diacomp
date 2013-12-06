package org.bosik.compensation.bo.dishbase;

import org.bosik.compensation.bo.basic.UniqueNamed;

public class DishItem extends UniqueNamed
{
	private static final long	serialVersionUID	= 1L;

	public DishItem(String name)
	{
		super(name);
	}

	// ================================ GET / SET ================================

	// ================================ CLONE ================================

	@Override
	public UniqueNamed clone() throws CloneNotSupportedException
	{
		DishItem result = (DishItem) super.clone();

		// TODO: implement

		return result;
	}
}

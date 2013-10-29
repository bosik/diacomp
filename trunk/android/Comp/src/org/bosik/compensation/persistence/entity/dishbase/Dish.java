package org.bosik.compensation.persistence.entity.dishbase;

import org.bosik.compensation.persistence.entity.common.Item;

public class Dish extends Item
{

	// ================================ GET / SET ================================

	// ================================ CLONE ================================

	@Override
	public Item clone() throws CloneNotSupportedException
	{
		Dish result = (Dish) super.clone();

		// TODO: implement

		return result;
	}
}

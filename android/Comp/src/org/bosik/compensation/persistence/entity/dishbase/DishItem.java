package org.bosik.compensation.persistence.entity.dishbase;

import org.bosik.compensation.persistence.entity.common.Item;

public class DishItem extends Item
{

	// ================================ GET / SET ================================

	// ================================ CLONE ================================

	@Override
	public Item clone() throws CloneNotSupportedException
	{
		DishItem result = (DishItem) super.clone();

		// TODO: implement

		return result;
	}
}

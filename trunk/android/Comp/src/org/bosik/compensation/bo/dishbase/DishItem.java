package org.bosik.compensation.bo.dishbase;

import org.bosik.compensation.persistence.common.Unique;

public class DishItem extends Unique
{

	// ================================ GET / SET ================================

	// ================================ CLONE ================================

	@Override
	public Unique clone() throws CloneNotSupportedException
	{
		DishItem result = (DishItem) super.clone();

		// TODO: implement

		return result;
	}
}

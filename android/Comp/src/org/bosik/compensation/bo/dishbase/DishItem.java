package org.bosik.compensation.bo.dishbase;

import org.bosik.compensation.persistence.common.UniqueNamed;

public class DishItem extends UniqueNamed
{

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

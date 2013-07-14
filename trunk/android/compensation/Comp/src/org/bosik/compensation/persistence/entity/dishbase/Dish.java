package org.bosik.compensation.persistence.entity.dishbase;

import org.bosik.compensation.persistence.entity.common.CustomItem;

public class Dish extends CustomItem
{
	
	// ================================ GET / SET ================================
	
	// ================================ CLONE ================================
	
	@Override
	public CustomItem clone() throws CloneNotSupportedException
	{
		Dish result = (Dish) super.clone();
		
		// TODO: implement

		return result;
	}
}

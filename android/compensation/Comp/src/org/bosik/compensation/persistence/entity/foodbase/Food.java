package org.bosik.compensation.persistence.entity.foodbase;

import org.bosik.compensation.persistence.entity.CustomItem;
import org.bosik.compensation.persistence.entity.FoodData;

/**
 * Продукт в базе продуктов
 * 
 * @author Bosik
 * 
 */
public class Food extends FoodData
{
	private boolean fromTable;
	private int tag;

	// ================================ GET / SET ================================
	
	public boolean getFromTable()
	{
		return fromTable;
	}

	public void setFromTable(boolean fromTable)
	{
		this.fromTable = fromTable;
	}

	public int getTag()
	{
		return tag;
	}

	public void setTag(int tag)
	{
		this.tag = tag;
	}
	
	// ================================ CLONE ================================
	
	@Override
	public CustomItem clone() throws CloneNotSupportedException
	{
		Food result = (Food) super.clone();
		
		result.setFromTable(getFromTable());
		result.setTag(getTag());

		return result;
	}
}

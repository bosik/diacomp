package org.bosik.compensation.bo.foodbase;

import org.bosik.compensation.bo.common.Food;
import org.bosik.compensation.bo.common.Item;

/**
 * Продукт
 * 
 * @author Bosik
 * 
 */
public class FoodItem extends Food implements Cloneable
{
	private boolean	fromTable;
	private int		tag;

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
	public Item clone() throws CloneNotSupportedException
	{
		FoodItem result = (FoodItem) super.clone();

		result.setFromTable(getFromTable());
		result.setTag(getTag());

		return result;
	}
}

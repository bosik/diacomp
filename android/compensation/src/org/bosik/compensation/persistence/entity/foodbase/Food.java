package org.bosik.compensation.persistence.entity.foodbase;

import org.bosik.compensation.persistence.entity.FoodData;

/**
 * Продукт в базе продуктов
 * 
 * @author Bosik
 * 
 */
public class Food extends FoodData, CustomItem
{
	private boolean fromTable;
	private int tag;

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
}

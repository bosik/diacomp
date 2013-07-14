package org.bosik.compensation.persistence.entity.foodbase;

import java.util.ArrayList;
import java.util.List;

public class FoodBase
{
	private int version = 0;
	private final List<Food> items = new ArrayList<Food>();

	public int getVersion()
	{
		return version;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	public void add(Food food)
	{
		if (null == food)
			throw new NullPointerException("Food can't be null");
		items.add(food);
	}

	public void remove(int index)
	{
		items.remove(index);
	}

	public Food get(int index)
	{
		return items.get(index);
	}

	public int count()
	{
		return items.size();
	}
}

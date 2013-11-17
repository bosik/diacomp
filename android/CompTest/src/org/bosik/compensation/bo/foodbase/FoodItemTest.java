package org.bosik.compensation.bo.foodbase;

import org.bosik.compensation.bo.foodbase.FoodItem;
import junit.framework.TestCase;

public class FoodItemTest extends TestCase
{
	private FoodItem	food	= new FoodItem();

	public void testClone() throws CloneNotSupportedException
	{
		food.setName("Колбаса");
		food.setRelProts(12.7);
		food.setRelFats(19.1);
		food.setRelCarbs(0.1);
		food.setRelValue(270);
		food.setFromTable(true);
		food.setTag(42);

		FoodItem copy = (FoodItem) food.clone();
		assertEquals(copy, food);
		assertEquals(copy.getId(), food.getId());
		assertEquals(copy.getName(), food.getName());
		assertEquals(copy.getRelProts(), food.getRelProts());
		assertEquals(copy.getRelFats(), food.getRelFats());
		assertEquals(copy.getRelCarbs(), food.getRelCarbs());
		assertEquals(copy.getRelValue(), food.getRelValue());
		assertEquals(copy.getFromTable(), food.getFromTable());
		assertEquals(copy.getTag(), food.getTag());
	}
}

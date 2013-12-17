package org.bosik.compensation.fakes.mocks;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import junit.framework.TestCase;
import org.bosik.compensation.bo.Food;
import org.bosik.compensation.bo.foodbase.FoodItem;

public class MockFoodItem extends TestCase implements Mock<FoodItem>
{
	private static final Mock<Food>	mockFood	= new MockFood();

	public List<FoodItem> getSamples()
	{
		List<Food> foods = mockFood.getSamples();
		Random r = new Random();

		List<FoodItem> samples = new ArrayList<FoodItem>();

		for (Food f : foods)
		{
			FoodItem item = new FoodItem();

			item.setName(f.getName());
			item.setRelProts(f.getRelProts());
			item.setRelFats(f.getRelFats());
			item.setRelCarbs(f.getRelCarbs());
			item.setRelValue(f.getRelValue());

			item.setFromTable(r.nextBoolean());
			item.setTag(r.nextInt(100000));

			samples.add(item);
		}

		return samples;
	}

	public void compare(FoodItem exp, FoodItem act)
	{
		mockFood.compare(exp, act);

		assertEquals(exp.getFromTable(), act.getFromTable());
		assertEquals(exp.getTag(), act.getTag());
	}
}

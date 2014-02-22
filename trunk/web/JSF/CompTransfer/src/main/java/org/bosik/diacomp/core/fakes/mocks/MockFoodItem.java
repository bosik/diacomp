package org.bosik.diacomp.core.fakes.mocks;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import junit.framework.TestCase;
import org.bosik.diacomp.core.bo.Food;
import org.bosik.diacomp.core.bo.foodbase.FoodItem;

public class MockFoodItem implements Mock<FoodItem>
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

		TestCase.assertEquals(exp.getFromTable(), act.getFromTable());
		TestCase.assertEquals(exp.getTag(), act.getTag());
	}
}

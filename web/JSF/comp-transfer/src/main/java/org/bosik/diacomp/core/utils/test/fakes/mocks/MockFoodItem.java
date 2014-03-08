package org.bosik.diacomp.core.utils.test.fakes.mocks;

import static junit.framework.TestCase.assertEquals;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;

public class MockFoodItem implements Mock<FoodItem>
{
	private static final Mock<Food>	mockFood	= new MockFood();

	@Override
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

	@Override
	public void compare(FoodItem exp, FoodItem act)
	{
		mockFood.compare(exp, act);

		assertEquals(exp.getFromTable(), act.getFromTable());
		assertEquals(exp.getTag(), act.getTag());
	}
}

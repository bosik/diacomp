package org.bosik.diacomp.core.test.fakes.mocks;

import static junit.framework.TestCase.assertEquals;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;

public class MockFoodItem implements Mock<FoodItem>
{
	private static final Mock<Food>	mockFood	= new MockFood();
	private Random					r			= new Random();

	@Override
	public List<FoodItem> getSamples()
	{
		List<FoodItem> samples = new ArrayList<FoodItem>();

		for (int i = 0; i < 10; i++)
		{
			samples.add(getSample());
		}

		return samples;
	}

	@Override
	public FoodItem getSample()
	{
		FoodItem item = new FoodItem();

		Food f = mockFood.getSample();
		item.setName(f.getName());
		item.setRelProts(f.getRelProts());
		item.setRelFats(f.getRelFats());
		item.setRelCarbs(f.getRelCarbs());
		item.setRelValue(f.getRelValue());

		item.setFromTable(r.nextBoolean());
		item.setTag(r.nextInt(100000));

		return item;
	}

	@Override
	public void compare(FoodItem exp, FoodItem act)
	{
		mockFood.compare(exp, act);

		assertEquals(exp.getFromTable(), act.getFromTable());
		assertEquals(exp.getTag(), act.getTag());
	}
}

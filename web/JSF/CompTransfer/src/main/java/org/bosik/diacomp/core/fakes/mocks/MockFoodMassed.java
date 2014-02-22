package org.bosik.diacomp.core.fakes.mocks;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import junit.framework.TestCase;
import org.bosik.diacomp.core.bo.Food;
import org.bosik.diacomp.core.bo.FoodMassed;
import org.bosik.diacomp.core.utils.TestUtils;

public class MockFoodMassed implements Mock<FoodMassed>
{
	private static final Mock<Food>	mockFood	= new MockFood();

	public List<FoodMassed> getSamples()
	{
		List<Food> foods = mockFood.getSamples();
		Random r = new Random();

		List<FoodMassed> samples = new ArrayList<FoodMassed>();

		for (Food f : foods)
		{
			FoodMassed item = new FoodMassed();

			item.setName(f.getName());
			item.setRelProts(f.getRelProts());
			item.setRelFats(f.getRelFats());
			item.setRelCarbs(f.getRelCarbs());
			item.setRelValue(f.getRelValue());

			item.setMass(r.nextInt(5000) * 0.1);

			samples.add(item);
		}

		return samples;
	}

	public void compare(FoodMassed exp, FoodMassed act)
	{
		mockFood.compare(exp, act);

		TestCase.assertEquals(exp.getMass(), act.getMass(), TestUtils.EPS);
	}
}

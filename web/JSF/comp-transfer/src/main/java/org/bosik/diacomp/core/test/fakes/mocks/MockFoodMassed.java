package org.bosik.diacomp.core.test.fakes.mocks;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.utils.Utils;

public class MockFoodMassed implements Mock<FoodMassed>
{
	private static final Mock<Food>	mockFood	= new MockFood();
	private Random					r			= new Random();

	@Override
	public List<FoodMassed> getSamples()
	{
		List<FoodMassed> samples = new ArrayList<FoodMassed>();

		for (int i = 0; i < 10; i++)
		{
			samples.add(getSample());
		}

		return samples;
	}

	@Override
	public FoodMassed getSample()
	{
		FoodMassed item = new FoodMassed();

		Food f = mockFood.getSample();
		item.setName(f.getName());
		item.setRelProts(f.getRelProts());
		item.setRelFats(f.getRelFats());
		item.setRelCarbs(f.getRelCarbs());
		item.setRelValue(f.getRelValue());

		item.setMass(r.nextInt(5000) / 10);

		return item;
	}

	@Override
	public void compare(FoodMassed exp, FoodMassed act)
	{
		mockFood.compare(exp, act);

		TestCase.assertEquals(exp.getMass(), act.getMass(), Utils.EPS);
	}
}

package org.bosik.diacomp.core.entities.business.diary.records;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;
import java.util.Date;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockFoodMassed;
import org.junit.Test;

public class TestMealRecord
{
	private static final Mock<FoodMassed>	mock	= new MockFoodMassed();
	private final MealRecord				meal	= new MealRecord(new Date(), false);

	@Test
	public void addGet_normal_ok()
	{
		for (int k = 1; k <= 10; k++)
		{
			FoodMassed food = mock.getSample();
			int n = meal.add(food);
			assertEquals(food, meal.get(n));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void addGet_null_exception()
	{
		meal.add(null);
	}

	@Test
	public void clearAddSize()
	{
		meal.clear();
		assertEquals(0, meal.count());
		meal.add(new FoodMassed());
		assertEquals(1, meal.count());
		meal.add(new FoodMassed());
		assertEquals(2, meal.count());
	}

	@Test
	public void PFCV()
	{
		FoodMassed food = mock.getSample();
		meal.clear();
		meal.add(food);
		meal.add(food);

		assertEquals(2 * food.getProts(), meal.getProts());
		assertEquals(2 * food.getFats(), meal.getFats());
		assertEquals(2 * food.getCarbs(), meal.getCarbs());
		assertEquals(2 * food.getValue(), meal.getValue());
		assertEquals(2 * food.getMass(), meal.getMass());
	}

	@Test
	public void shortMeal()
	{
		meal.setShortMeal(true);
		assertTrue(meal.getShortMeal());
		meal.setShortMeal(false);
		assertFalse(meal.getShortMeal());
	}
}

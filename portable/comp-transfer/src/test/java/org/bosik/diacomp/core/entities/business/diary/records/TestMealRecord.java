/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.entities.business.diary.records;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;
import java.util.Date;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.mocks.Mock;
import org.bosik.diacomp.core.mocks.MockFoodMassed;
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

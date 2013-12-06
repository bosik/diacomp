package org.bosik.compensation.bo.diary.records;

import junit.framework.TestCase;
import org.bosik.compensation.bo.common.FoodMassed;

public class MealRecordTest extends TestCase
{
	private MealRecord	meal	= new MealRecord(620, false);

	// THINK: ��������� ����� - ����� ��� ���?

	/**
	 * ������ ����-��������� ��� ������������
	 * 
	 * @return
	 */
	private static FoodMassed createDemoFood()
	{
		FoodMassed food = new FoodMassed("�������");
		food.setMass(78);
		food.setRelProts(12.2);
		food.setRelFats(18.9);
		food.setRelCarbs(0);
		food.setRelValue(272);
		return food;
	}

	public void testAddGet()
	{
		// ���������� ����
		for (int k = 1; k <= 10; k++)
		{
			FoodMassed food = createDemoFood();
			int n = meal.add(food);
			assertEquals(food, meal.get(n));
		}

		// ����-����
		try
		{
			meal.add(null);
			fail();
		}
		catch (Exception e)
		{
		}
	}

	public void testClearAddSize()
	{
		meal.clear();
		assertEquals(0, meal.count());
		meal.add(new FoodMassed("Item1"));
		assertEquals(1, meal.count());
		meal.add(new FoodMassed("Item2"));
		assertEquals(2, meal.count());
	}

	public void testPFCV()
	{
		FoodMassed food = createDemoFood();
		meal.clear();
		meal.add(food);
		meal.add(food);

		assertEquals(2 * food.getProts(), meal.getProts());
		assertEquals(2 * food.getFats(), meal.getFats());
		assertEquals(2 * food.getCarbs(), meal.getCarbs());
		assertEquals(2 * food.getValue(), meal.getValue());
		assertEquals(2 * food.getMass(), meal.getMass());
	}

	public void testShortMeal()
	{
		meal.setShortMeal(true);
		assertTrue(meal.getShortMeal());
		meal.setShortMeal(false);
		assertFalse(meal.getShortMeal());
	}
}

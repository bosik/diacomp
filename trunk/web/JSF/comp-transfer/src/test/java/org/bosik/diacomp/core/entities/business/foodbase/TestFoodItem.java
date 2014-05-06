package org.bosik.diacomp.core.entities.business.foodbase;

import junit.framework.TestCase;
import org.junit.Test;

public class TestFoodItem extends TestCase
{
	@Test
	public void test_constructor_normal1_ok()
	{
		FoodItem food = new FoodItem("Test 1", 0.1, 0.2, 0.3, 0.4, 42, true);
		assertEquals("Test 1", food.getName());
		assertEquals(0.1, food.getRelProts());
		assertEquals(0.2, food.getRelFats());
		assertEquals(0.3, food.getRelCarbs());
		assertEquals(0.4, food.getRelValue());
		assertEquals(42, food.getTag());
		assertEquals(true, food.getFromTable());
	}

	@Test
	public void test_constructor_normal2_ok()
	{
		FoodItem food = new FoodItem("Test 2", 0.0, 0.2, 0.3, 180, 42, false);
		assertEquals("Test 2", food.getName());
		assertEquals(0.0, food.getRelProts());
		assertEquals(0.2, food.getRelFats());
		assertEquals(0.3, food.getRelCarbs());
		assertEquals(180.0, food.getRelValue());
		assertEquals(42, food.getTag());
		assertEquals(false, food.getFromTable());
	}
}

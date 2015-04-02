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

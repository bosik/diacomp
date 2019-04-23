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
package org.bosik.diacomp.core.entities.business;

import org.bosik.diacomp.core.utils.Utils;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class TestFoodMassed
{
	private final FoodMassed food = new FoodMassed();

	@Test
	public void testMass()
	{
		// normal test
		food.setMass(0.0);
		assertEquals(0.0, food.getMass(), Utils.EPS);
		food.setMass(0.01);
		assertEquals(0.01, food.getMass(), Utils.EPS);
		food.setMass(100.0);
		assertEquals(100.0, food.getMass(), Utils.EPS);
		food.setMass(999.5);
		assertEquals(999.5, food.getMass(), Utils.EPS);

		// crash-test
		try
		{
			food.setMass(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setMass(-100);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	@Test
	public void testGetAbs()
	{
		// normal test
		food.setMass(10);

		food.setRelProts(1);
		assertEquals(0.1, food.getProts(), Utils.EPS);
		food.setRelFats(2);
		assertEquals(0.2, food.getFats(), Utils.EPS);
		food.setRelCarbs(3);
		assertEquals(0.3, food.getCarbs(), Utils.EPS);
		food.setRelValue(4);
		assertEquals(0.4, food.getValue(), Utils.EPS);

		food.setMass(1000);

		food.setRelProts(2);
		assertEquals(20.0, food.getProts(), Utils.EPS);
		food.setRelFats(4);
		assertEquals(40.0, food.getFats(), Utils.EPS);
		food.setRelCarbs(6);
		assertEquals(60.0, food.getCarbs(), Utils.EPS);
		food.setRelValue(8);
		assertEquals(80.0, food.getValue(), Utils.EPS);

		food.setMass(0);

		assertEquals(0.0, food.getProts(), Utils.EPS);
		assertEquals(0.0, food.getFats(), Utils.EPS);
		assertEquals(0.0, food.getCarbs(), Utils.EPS);
		assertEquals(0.0, food.getValue(), Utils.EPS);

		// crash-test
		try
		{
			food.setRelProts(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelProts(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelFats(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelFats(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelCarbs(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelCarbs(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelValue(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	@Test
	public void equal()
	{
		FoodMassed food1 = new FoodMassed()
		{{
			setName("Apple");
			setRelProts(0.2);
			setRelFats(0.1);
			setRelCarbs(11.2);
			setRelValue(40);
			setMass(2000);
		}};
		FoodMassed food2 = new FoodMassed("Apple", 0.2, 0.1, 11.2, 40, 2000);

		assertEquals(food1, food2);
		assertEquals(food2, food1);
	}
}

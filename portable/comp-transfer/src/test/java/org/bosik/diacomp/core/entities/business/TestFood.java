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

public class TestFood
{
	private Food	food	= new Food();

	@Test
	public void testName()
	{
		// нормальный тест
		food.setName("name");
		assertEquals("name", food.getName());

		// краш-тест
		//		try
		//		{
		//			food.setName(null);
		//			fail();
		//		}
		//		catch (IllegalArgumentException e)
		//		{
		//		}
		//		try
		//		{
		//			food.setName("  ");
		//			fail();
		//		}
		//		catch (IllegalArgumentException e)
		//		{
		//		}
		//		try
		//		{
		//			food.setName("");
		//			fail();
		//		}
		//		catch (IllegalArgumentException e)
		//		{
		//		}
	}

	@Test
	public void testRelProts()
	{
		// нормальный тест
		food.setRelProts(0.0);
		assertEquals(0.0, food.getRelProts(), Utils.EPS);
		food.setRelProts(12.8);
		assertEquals(12.8, food.getRelProts(), Utils.EPS);
		food.setRelProts(100.0);
		assertEquals(100.0, food.getRelProts(), Utils.EPS);

		// краш-тест
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
	}

	@Test
	public void testRelFats()
	{
		// нормальный тест
		food.setRelFats(0.0);
		assertEquals(0.0, food.getRelFats(), Utils.EPS);
		food.setRelFats(12.8);
		assertEquals(12.8, food.getRelFats(), Utils.EPS);
		food.setRelFats(100.0);
		assertEquals(100.0, food.getRelFats(), Utils.EPS);

		// краш-тест
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
	}

	@Test
	public void testRelCarbs()
	{
		// нормальный тест
		food.setRelCarbs(0.0);
		assertEquals(0.0, food.getRelCarbs(), Utils.EPS);
		food.setRelCarbs(12.8);
		assertEquals(12.8, food.getRelCarbs(), Utils.EPS);
		food.setRelCarbs(100.0);
		assertEquals(100.0, food.getRelCarbs(), Utils.EPS);

		// краш-тест
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
	}

	@Test
	public void testRelValue()
	{
		// нормальный тест
		food.setRelValue(0.0);
		assertEquals(0.0, food.getRelValue(), Utils.EPS);
		food.setRelValue(12.8);
		assertEquals(12.8, food.getRelValue(), Utils.EPS);
		food.setRelValue(102.8);
		assertEquals(102.8, food.getRelValue(), Utils.EPS);

		// краш-тест
		try
		{
			food.setRelValue(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}
}

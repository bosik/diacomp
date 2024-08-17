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
package org.bosik.diacomp.core.entities.business.dishbase;

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.utils.Utils;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TestDishItem
{
	private DishItem dish = new DishItem();

	@Test
	public void test_setMass_null_ok()
	{
		dish.setMass(null);
		assertEquals(null, dish.getMass());
	}

	@Test
	public void test_getRels_withoutMass_ok()
	{
		dish.add(new FoodMassed("Item", 10.0, 20.0, 30.0, 40.0, 100));
		assertEquals(10.0, dish.getRelProts(), Utils.EPS);
		assertEquals(20.0, dish.getRelFats(), Utils.EPS);
		assertEquals(30.0, dish.getRelCarbs(), Utils.EPS);
		assertEquals(40.0, dish.getRelValue(), Utils.EPS);
	}

	@Test
	public void test_getRels_withMass_ok()
	{
		dish.add(new FoodMassed("Item", 10.0, 20.0, 30.0, 40.0, 100));
		dish.setMass(200);
		assertEquals(5.0, dish.getRelProts(), Utils.EPS);
		assertEquals(10.0, dish.getRelFats(), Utils.EPS);
		assertEquals(15.0, dish.getRelCarbs(), Utils.EPS);
		assertEquals(20.0, dish.getRelValue(), Utils.EPS);
	}

	@Test(expected = IllegalArgumentException.class)
	public void test_add_null_exceptionThrown()
	{
		dish.add(null);
	}

	@Test
	public void test_remove_normal_ok()
	{
		dish.add(new FoodMassed("Item", 10.0, 20.0, 30.0, 40.0, 100));
		dish.remove(0);
		assertEquals(0, dish.count());
	}
}

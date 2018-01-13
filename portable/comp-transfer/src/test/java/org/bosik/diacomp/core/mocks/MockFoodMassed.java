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
package org.bosik.diacomp.core.mocks;

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

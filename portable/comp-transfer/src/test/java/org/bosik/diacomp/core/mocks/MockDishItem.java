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

import static junit.framework.TestCase.assertEquals;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;

public class MockDishItem implements Mock<DishItem>
{
	private static final Mock<FoodMassed>	mockFood	= new MockFoodMassed();
	private Random							r			= new Random();

	@Override
	public List<DishItem> getSamples()
	{
		List<DishItem> samples = new ArrayList<>();

		DishItem dishEmpty = new DishItem();
		dishEmpty.setName("[Test] Empty dish");
		dishEmpty.setTag(100);
		samples.add(dishEmpty);

		DishItem dishChocolateBiscuit = new DishItem();
		dishChocolateBiscuit.setName("[Test] Бисквит шоколадный");
		dishChocolateBiscuit.setTag(1);
		dishChocolateBiscuit.setMass(413);
		dishChocolateBiscuit.add(new FoodMassed("Яйцо", 12.7, 11.5, 0.7, 157, 200));
		dishChocolateBiscuit.add(new FoodMassed("Сахар", 0, 0, 99.8, 379, 50));
		dishChocolateBiscuit.add(new FoodMassed("Масло \"Вкуснотеево\" жёлтое", 0.8, 72.5, 1.3, 661, 95));
		dishChocolateBiscuit.add(new FoodMassed("Шоколад \"Бабаевский\" элитный 75%", 10.8, 38.6, 37.1, 545, 110));
		dishChocolateBiscuit.add(new FoodMassed("Мука \"Старооскольская\" пшеничная ВС", 10.3, 1.1, 70.6, 334, 60));
		samples.add(dishChocolateBiscuit);

		DishItem dishTiramisuDessert = new DishItem();
		dishTiramisuDessert.setName("[Test] Десерт \"Тирамису\" (порция)");
		dishTiramisuDessert.setTag(1);
		dishTiramisuDessert.setMass(null);
		dishTiramisuDessert.add(new FoodMassed("Крем для торта \"Тирамису\"", 2.8, 25.7, 27.1, 353.6, 124));
		dishTiramisuDessert.add(new FoodMassed("Бисквит шоколадный", 9.6, 39.8, 27.3, 504.2, 46));
		dishTiramisuDessert.add(new FoodMassed("Груша", 0.4, 0.3, 9.5, 42, 62));
		samples.add(dishTiramisuDessert);

		DishItem dishToastBatter = new DishItem();
		dishToastBatter.setName("[Test] Кляр для гренок");
		dishToastBatter.setTag(42);
		dishToastBatter.add(new FoodMassed("Яйцо", 12.7, 11.5, 0.7, 157, 54));
		dishToastBatter.add(new FoodMassed("Молоко \"Вкуснотеево\", 3,2%", 2.8, 3.2, 4.7, 58, 172));
		samples.add(dishToastBatter);

		DishItem dishMegaSalad = new DishItem();
		dishMegaSalad.setName("[Test] Megasalad");
		dishMegaSalad.setTag(100500);
		for (FoodMassed food : mockFood.getSamples())
		{
			dishMegaSalad.add(food);
		}
		samples.add(dishMegaSalad);

		return samples;
	}

	@Override
	public void compare(DishItem exp, DishItem act)
	{
		assertEquals(exp.getName(), act.getName());
		assertEquals(exp.getTag(), act.getTag());
		assertEquals(exp.getMass(), act.getMass());

		assertEquals(exp.count(), act.count());
		for (int i = 0; i < exp.count(); i++)
		{
			mockFood.compare(exp.get(i), act.get(i));
		}
	}

	@Override
	public DishItem getSample()
	{
		// TODO: make it actually random
		List<DishItem> samples = getSamples();
		return samples.get(r.nextInt(samples.size()));
	}
}

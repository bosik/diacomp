/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2025 Nikita Bosik
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
package org.bosik.diacomp.android.backend.features.search;

import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.Named;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class UsageIndexDishbase
{
	/**
	 * Updates index of food & dish usage in dish base
	 *
	 * @param foodBaseService
	 * @param dishBaseService
	 */
	public static void update(FoodBaseService foodBaseService, DishBaseService dishBaseService)
	{
		final List<Versioned<DishItem>> records = dishBaseService.findAll(false);
		final Map<String, Long> lastUsages = new HashMap<>();

		for (Versioned<DishItem> dish : records)
		{
			final DishItem data = dish.getData();
			for (int j = 0; j < data.count(); j++)
			{
				lastUsages.merge(data.get(j).getName(), dish.getTimeStamp().getTime(), Utils::max);
			}
		}

		lastUsages.forEach((name, lastUsage) -> {
			final Versioned<FoodItem> food = foodBaseService.findOne(name);
			if (food != null)
			{
				if (!Objects.equals(lastUsage, food.getData().getLastUsedInDishBase()))
				{
					food.getData().setLastUsedInDishBase(lastUsage);
					foodBaseService.save(Collections.singletonList(food)); // TODO: optimize
				}
			}
			else
			{
				final Versioned<DishItem> dish = dishBaseService.findOne(name);
				if (dish != null)
				{
					if (!Objects.equals(lastUsage, dish.getData().getLastUsedInDishBase()))
					{
						dish.getData().setLastUsedInDishBase(lastUsage);
						dishBaseService.save(Collections.singletonList(dish)); // TODO: optimize
					}
				}
			}
		});
	}

	public static <T extends Named> void sort(List<Versioned<? extends T>> items)
	{
		SortUtil.sort(items, data -> {
			if (data instanceof FoodItem)
			{
				return ((FoodItem) data).getLastUsedInDishBase();
			}
			else if (data instanceof DishItem)
			{
				return ((DishItem) data).getLastUsedInDishBase();
			}
			else
			{
				throw new IllegalArgumentException("Unsupported type: " + data.getClass().getCanonicalName());
			}
		});
	}
}

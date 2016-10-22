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
package org.bosik.diacomp.core.services.search;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

public class RelevantIndexator
{
	public static void indexate(DiaryService diary, FoodBaseService foodBase, DishBaseService dishBase)
	{
		//		Profiler p = new Profiler();

		// constructing dates range
		final long PERIOD = 30; // days
		final Date max = new Date(new Date().getTime() + Utils.MsecPerDay);
		final Date min = new Date(max.getTime() - (PERIOD * Utils.MsecPerDay));

		// analyze diary; collect names & relevance

		List<Versioned<DiaryRecord>> records = diary.findPeriod(min, max, false);
		//		System.out.printf("Fetching diary records (%d total items): %.1f ms%n", records.size(),
		//				0.000001 * p.sinceLastCheck());

		//		int meals = 0;

		Map<String, Integer> names = new HashMap<String, Integer>();
		for (Versioned<DiaryRecord> item : records)
		{
			DiaryRecord rec = item.getData();
			if (rec instanceof MealRecord)
			{
				//				meals++;
				MealRecord meal = (MealRecord)rec;
				for (int j = 0; j < meal.count(); j++)
				{
					FoodMassed food = meal.get(j);
					int delta = f(rec.getTime(), min, max);

					Integer value = names.get(food.getName());

					if (value == null)
					{
						value = 0;
					}

					names.put(food.getName(), value + delta);
				}
			}
		}

		//		System.out.printf("Diary analyzing (%d meals, %d names): %.1f ms%n", meals, names.size(),
		//				0.000001 * p.sinceLastCheck());

		// update tags

		List<Versioned<FoodItem>> foods = updateItems(foodBase.findAll(false), names);
		//		System.out.printf("Updating foods (%d): %.1f ms%n", foods.size(), 0.000001 * p.sinceLastCheck());

		foodBase.save(foods);
		//		System.out.printf("Saving foods: %.1f ms%n", 0.000001 * p.sinceLastCheck());

		List<Versioned<DishItem>> dishes = updateItems(dishBase.findAll(false), names);
		//		System.out.printf("Updating dishes (%d): %.1f ms%n", foods.size(), 0.000001 * p.sinceLastCheck());

		dishBase.save(dishes);
		//		System.out.printf("Saving dishes: %.1f ms%n", 0.000001 * p.sinceLastCheck());

		//		System.out.printf("Total indexing time: %.1f ms%n", 0.000001 * p.sinceStart());
	}

	private static <T extends NamedRelativeTagged> List<Versioned<T>> updateItems(List<Versioned<T>> items,
			Map<String, Integer> names)
	{
		List<Versioned<T>> changed = new ArrayList<Versioned<T>>();

		for (Versioned<T> item : items)
		{
			Integer value = names.get(item.getData().getName());

			if (value == null)
			{
				value = 0;
			}

			if (item.getData().getTag() != value)
			{
				item.getData().setTag(value);
				changed.add(item);
			}
		}

		return changed;
	}

	/**
	 * Time complexity: O(1); NOTE: float operations
	 * 
	 * @param curDate
	 * @param minDate
	 * @param maxDate
	 * @return
	 */
	private static int f(Date curDate, Date minDate, Date maxDate)
	{
		int delta = (int)((100 * (curDate.getTime() - minDate.getTime())) / (maxDate.getTime() - minDate.getTime()));
		return delta * delta;
	}
}

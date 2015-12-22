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

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Profiler;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

public class RelevantIndexator
{
	public static void indexate(TagService tagService, DiaryService diary, FoodBaseService foodBase,
			DishBaseService dishBase)
	{
		Profiler p = new Profiler();

		// constructing dates range
		final long PERIOD = 30; // days
		final Date max = new Date(new Date().getTime() + Utils.MsecPerDay);
		final Date min = new Date(max.getTime() - (PERIOD * Utils.MsecPerDay));

		//		System.out.println("Inits: " + p.sinceLastCheck());

		// search for base items

		// T.C.: O(|foodItems|) FIXME
		List<Versioned<FoodItem>> foodItems = foodBase.findAll(false);
		// T.C.: O(|dishItems|) FIXME
		List<Versioned<DishItem>> dishItems = dishBase.findAll(false);

		//		System.out.println("Search for base items: " + p.sinceLastCheck());

		// clear tags
		tagService.reset();

		// T.C.: O(|foodItems|)
		//clearTags(foodItems);
		// T.C.: O(|dishItems|)
		//clearTags(dishItems);

		//		System.out.println("Clearing tags: " + p.sinceLastCheck());

		// search for diary items

		// T.C.: const (let M)
		List<Versioned<DiaryRecord>> items = diary.findPeriod(min, max, false);

		//		System.out.println("Search for diary items: " + p.sinceLastCheck());

		// T.C.: O(M * (|foodbase| + |dishbase|))

		for (Versioned<DiaryRecord> item : items)
		{
			DiaryRecord rec = item.getData();
			if (rec.getClass().equals(MealRecord.class))
			{
				MealRecord meal = (MealRecord)rec;
				for (int j = 0; j < meal.count(); j++)
				{
					FoodMassed food = meal.get(j);
					int delta = f(rec.getTime(), min, max);
					process(tagService, food.getName(), delta, foodItems, dishItems);
				}
			}
		}
		//		System.out.println("Processing items: " + p.sinceLastCheck());

		// T.C.: O(|foodItems|), NOTE: very slow JSON serialization
		//foodBase.save(foodItems);

		// T.C.: O(|dishItems|), NOTE: very slow JSON serialization
		//dishBase.save(dishItems);

		//		System.out.println("Saving tags: " + p.sinceLastCheck());
		//		System.out.println("Total indexing time: " + p.sinceStart());
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

	//	/**
	//	 * Time complexity: O(|list|)
	//	 *
	//	 * @param list
	//	 */
	//	private static <T extends NamedRelativeTagged> void clearTags(List<Versioned<T>> list)
	//	{
	//		// List<Versioned<T>> list = base.findAll(false);
	//		// for (Versioned<T> item : list)
	//		// {
	//		// item.getData().setTag(0);
	//		// }
	//		// base.save(list);
	//
	//		for (Versioned<T> item : list)
	//		{
	//			item.getData().setTag(0);
	//		}
	//	}

	/**
	 * Time complexity: O(|foodbase| + |dishbase|)
	 * 
	 * @param name
	 * @param delta
	 * @param foodBase
	 * @param dishBase
	 */
	private static <T extends NamedRelativeTagged> void process(TagService tagService, String name, int delta,
			List<Versioned<FoodItem>> foodBase, List<Versioned<DishItem>> dishBase)
	{
		if (process(tagService, name, delta, foodBase)) return;
		if (process(tagService, name, delta, dishBase)) return;
	}

	/**
	 * Time complexity: O(|items|)
	 * 
	 * @param name
	 * @param delta
	 * @param items
	 * @return
	 */
	private static <T extends NamedRelativeTagged> boolean process(TagService tagService, String name, int delta,
			List<Versioned<T>> items)
	{
		// TODO: binary search?

		for (Versioned<T> x : items)
		{
			if (x.getData().getName().equals(name))
			{
				//x.getData().setTag(x.getData().getTag() + delta);
				tagService.incTag(x.getId(), delta);
				return true;
			}
		}

		return false;
	}
}

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

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.interfaces.Named;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RelevantIndexator
{
	private static volatile Map<String, Integer> usagesMap;

	public static void index(DiaryService diary)
	{
		final Map<String, Integer> temp = findUsages(diary, 30);
		usagesMap = temp;
	}

	public static synchronized Map<String, Integer> getUsages(DiaryService diary)
	{
		if (usagesMap == null)
		{
			index(diary);
		}

		return usagesMap;
	}

	/**
	 * @param diary  Diary source to analyze
	 * @param period Period to scan, days
	 * @return
	 */
	private static Map<String, Integer> findUsages(DiaryService diary, long period)
	{
		// constructing dates range

		final Date max = new Date(new Date().getTime() + Utils.MsecPerDay);
		final Date min = new Date(max.getTime() - (period * Utils.MsecPerDay));

		// analyze diary; collect names & relevance

		List<Versioned<DiaryRecord>> records = diary.findPeriod(min, max, false);

		Map<String, Integer> names = new HashMap<>();
		for (Versioned<DiaryRecord> item : records)
		{
			DiaryRecord rec = item.getData();
			if (rec instanceof MealRecord)
			{
				MealRecord meal = (MealRecord) rec;
				for (int j = 0; j < meal.count(); j++)
				{
					FoodMassed food = meal.get(j);
					int delta = f(rec.getTime(), min, max);
					int value = Utils.nullToZero(names.get(food.getName()));

					names.put(food.getName(), value + delta);
				}
			}
		}
		return names;
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
		int delta = (int) ((100 * (curDate.getTime() - minDate.getTime())) / (maxDate.getTime() - minDate.getTime()));
		return delta * delta;
	}

	public static <T extends Named> void sort(List<Versioned<? extends T>> items, DiaryService diaryService)
	{
		final Map<String, Integer> usages = getUsages(diaryService);

		items.sort((Comparator<Versioned<? extends Named>>) (lhs, rhs) -> {
			final String name1 = lhs.getData().getName();
			final String name2 = rhs.getData().getName();

			int tag1 = Utils.nullToZero(usages.get(name1));
			int tag2 = Utils.nullToZero(usages.get(name2));

			if (tag1 != tag2)
			{
				return tag2 - tag1;
			}

			return name1.compareTo(name2);
		});
	}
}

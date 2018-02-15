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

					Integer value = names.get(food.getName());

					if (value == null)
					{
						value = 0;
					}

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

		Collections.sort(items, new Comparator<Versioned<? extends Named>>()
		{
			@Override
			public int compare(Versioned<? extends Named> lhs, Versioned<? extends Named> rhs)
			{
				Named data1 = lhs.getData();
				Named data2 = rhs.getData();

				Integer tag1 = usages.get(data1.getName());
				if (tag1 == null)
				{
					tag1 = 0;
				}

				Integer tag2 = usages.get(data2.getName());
				if (tag2 == null)
				{
					tag2 = 0;
				}

				if (tag1.equals(tag2))
				{
					return data1.getName().compareTo(data2.getName());
				}
				else
				{
					return tag2 - tag1;
				}
			}
		});
	}
}

/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.backend.features.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.services.search.Sorter;
import org.bosik.diacomp.core.services.search.Sorter.Sort;
import org.bosik.diacomp.core.services.search.TagService;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.utils.Profiler;
import org.bosik.merklesync.Versioned;
import android.test.AndroidTestCase;

public class TestTagLocalService extends AndroidTestCase
{
	private TagService												tagService;
	private final Mock<FoodItem>									mockFood		= new MockFoodItem();
	private final Mock<Versioned<FoodItem>>							mockVersioned	= new MockVersionedConverter<FoodItem>(
			mockFood);
	private final Sorter											sorterFood		= new Sorter();
	/**
	 * Size of (food+dish) base
	 */
	private static final int										N_TOTAL			= 10000;
	/**
	 * Count of unique items in the diary
	 */
	private static final int										N_USED			= 291;
	/**
	 * Average count of each unique item in the diary
	 */
	private static final int										N_DUP			= 24;
	private static List<Versioned<? extends NamedRelativeTagged>>	foodBase;
	private static Map<String, Versioned<FoodItem>>					foodBaseIndex;

	{
		synchronized (this)
		{
			if (foodBase == null)
			{
				Profiler p = new Profiler();

				foodBase = new ArrayList<Versioned<? extends NamedRelativeTagged>>();
				foodBaseIndex = new HashMap<String, Versioned<FoodItem>>();

				for (int i = 0; i < N_TOTAL; i++)
				{
					final Versioned<FoodItem> item = mockVersioned.getSample();
					item.getData().setTag(0);
					foodBase.add(item);
					foodBaseIndex.put(item.getId(), item);
				}

				sorterFood.sort(foodBase, Sort.ALPHABET);

				System.err.println("Foodbase inited in " + (p.sinceStart() / 1000000) + " ms");
			}
		}
	}

	@Override
	public void setUp() throws Exception
	{
		super.setUp();
		tagService = new TagLocalService();
	}

	public void testInsertPerfomance()
	{
		final long limitTime = 500000000; // 500 ms

		// play

		Profiler p = new Profiler();

		tagService.reset();
		for (int j = 0; j < N_DUP; j++)
		{
			for (int i = 0; i < N_USED; i++)
			{
				tagService.incTag(foodBase.get(i).getId(), 1);
			}
		}

		// check

		final long actualTime = p.sinceStart();
		if (actualTime > limitTime)
		{
			fail("Took " + actualTime + " ns (" + ((100 * actualTime) / limitTime) + "% of limit)");
		}
	}

	public void testSelectPerfomance()
	{
		final long limitTime = 100000000; // 100 ms

		// prepare DB

		tagService.reset();
		for (int i = 0; i < N_USED; i++)
		{
			tagService.incTag(foodBase.get(i).getId(), 1);
		}

		// play

		Profiler p = new Profiler();

		List<Versioned<? extends NamedRelativeTagged>> favourite = new ArrayList<Versioned<? extends NamedRelativeTagged>>();

		for (Entry<String, Versioned<FoodItem>> entry : foodBaseIndex.entrySet())
		{
			Integer tag = tagService.getTag(entry.getKey());
			Versioned<FoodItem> item = entry.getValue();
			item.getData().setTag(tag != null ? tag : 0);
			favourite.add(item);
		}

		sorterFood.sort(favourite, Sort.RELEVANT);

		final int LIMIT = 100;

		if (favourite.size() > LIMIT)
		{
			favourite = favourite.subList(0, LIMIT);
		}

		// check
		final long actualTime = p.sinceStart();
		if (actualTime > limitTime)
		{
			fail("Took " + actualTime + " ns (" + ((100 * actualTime) / limitTime) + "% of limit)");
		}
	}
}

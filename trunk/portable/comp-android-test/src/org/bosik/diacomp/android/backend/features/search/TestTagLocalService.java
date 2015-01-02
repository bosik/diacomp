package org.bosik.diacomp.android.backend.features.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.search.Sorter;
import org.bosik.diacomp.core.services.search.Sorter.Sort;
import org.bosik.diacomp.core.services.search.TagService;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.core.utils.Profiler;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestTagLocalService extends AndroidTestCase
{
	private TagService								tagService;
	private final Mock<FoodItem>					mockFood		= new MockFoodItem();
	private final Mock<Versioned<FoodItem>>			mockVersioned	= new MockVersionedConverter<FoodItem>(mockFood);
	private static final Sorter<FoodItem>			sorterFood		= new Sorter<FoodItem>();
	/**
	 * Size of (food+dish) base
	 */
	private static final int						N_TOTAL			= 10000;
	/**
	 * Count of unique items in the diary
	 */
	private static final int						N_USED			= 291;
	/**
	 * Average count of each unique item in the diary
	 */
	private static final int						N_DUP			= 24;
	private static List<Versioned<FoodItem>>		foodBase;
	private static Map<String, Versioned<FoodItem>>	foodBaseIndex;

	{
		synchronized (this)
		{
			if (foodBase == null)
			{
				Profiler p = new Profiler();

				foodBase = new ArrayList<Versioned<FoodItem>>();
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
		ContentResolver resolver = getContext().getContentResolver();
		tagService = new TagLocalService(resolver);
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
		Map<String, Integer> tagInfo = tagService.getTags();

		List<Versioned<FoodItem>> favourite = new ArrayList<Versioned<FoodItem>>();

		for (Entry<String, Integer> tag : tagInfo.entrySet())
		{
			final Versioned<FoodItem> item = foodBaseIndex.get(tag.getKey());
			if (item != null)
			{
				item.getData().setTag(tag.getValue());
				favourite.add(item);
			}
		}

		// Iterator<Versioned<FoodItem>> iterator = foodBase.iterator();
		// while (iterator.hasNext())
		// {
		// if (tagInfo.containsKey(iterator.next().getId()))
		// {
		// iterator.remove();
		// }
		// }

		sorterFood.sort(favourite, Sort.RELEVANT);
		// sorterFood.sort(foodBase, Sort.ALPHABET);

		final int LIMIT = 100;

		if (favourite.size() > LIMIT)
		{
			favourite = favourite.subList(0, LIMIT);
		}
		else if (favourite.size() < LIMIT)
		{
			for (Versioned<FoodItem> item : foodBase)
			{
				if (tagInfo.get(item.getId()) == null)
				{
					favourite.add(item);
					if (favourite.size() == LIMIT)
					{
						break;
					}
				}
			}
		}

		// check

		final long actualTime = p.sinceStart();
		if (actualTime > limitTime)
		{
			fail("Took " + actualTime + " ns (" + ((100 * actualTime) / limitTime) + "% of limit)");
		}
	}
}

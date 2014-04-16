package org.bosik.diacomp.android.backend.features.search;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import android.util.Log;

@SuppressWarnings("unchecked")
public class RelevantIndexator
{
	private static final String	TAG	= RelevantIndexator.class.getSimpleName();

	public static void indexate(DiaryService diary, FoodBaseService foodBase, DishBaseService dishBase)
	{
		/**/long time = System.currentTimeMillis();

		// constructing dates list
		final long PERIOD = 30; // days
		final long MSEC_PER_DAY = 86400000;
		final Date max = new Date(new Date().getTime() + MSEC_PER_DAY);
		final Date min = new Date(max.getTime() - (PERIOD * MSEC_PER_DAY));

		List<Versioned<FoodItem>> foodItems = foodBase.findAll(false);
		List<Versioned<DishItem>> dishItems = dishBase.findAll(false);

		// clear tags
		clearTags(foodItems);
		clearTags(dishItems);

		// process
		List<Versioned<DiaryRecord>> items = diary.findBetween(min, max, false);
		for (Versioned<DiaryRecord> item : items)
		{
			DiaryRecord rec = item.getData();
			if (rec.getClass().equals(MealRecord.class))
			{
				MealRecord meal = (MealRecord) rec;
				for (int j = 0; j < meal.count(); j++)
				{
					FoodMassed food = meal.get(j);
					int delta = f(rec.getTime(), min, max);
					process(food.getName(), delta, foodItems, dishItems);
				}
			}
		}

		foodBase.save(foodItems);
		dishBase.save(dishItems);

		/**/Log.v(TAG, String.format("Indexated in %d msec", System.currentTimeMillis() - time));
	}

	private static int f(Date curDate, Date minDate, Date maxDate)
	{
		int delta = (int) ((100 * (curDate.getTime() - minDate.getTime())) / (maxDate.getTime() - minDate.getTime()));
		return delta * delta;
	}

	private static <T extends NamedRelativeTagged> void clearTags(List<Versioned<T>> list)
	{
		// List<Versioned<T>> list = base.findAll(false);
		// for (Versioned<T> item : list)
		// {
		// item.getData().setTag(0);
		// }
		// base.save(list);

		for (Versioned<T> item : list)
		{
			item.getData().setTag(0);
		}
	}

	private static <T extends NamedRelativeTagged> void process(String name, int delta,
			List<Versioned<FoodItem>> foodBase, List<Versioned<DishItem>> dishBase)
	{
		if (process(name, delta, foodBase))
			return;
		if (process(name, delta, dishBase))
			return;
	}

	private static <T extends NamedRelativeTagged> boolean process(String name, int delta, List<Versioned<T>> items)
	{
		for (Versioned<T> x : items)
		{
			if (x.getData().getName().equals(name))
			{
				x.getData().setTag(x.getData().getTag() + delta);
				return true;
			}
		}

		return false;
	}
}

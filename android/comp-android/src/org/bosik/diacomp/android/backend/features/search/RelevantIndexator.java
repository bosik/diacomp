package org.bosik.diacomp.android.backend.features.search;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.interfaces.Tagged;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.BaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import android.util.Log;

public class RelevantIndexator
{
	private static final String	TAG	= RelevantIndexator.class.getSimpleName();

	public static void indexate(DiaryService diary, FoodBaseService foodBase, DishBaseService dishBase)
	{
		/**/long time = System.currentTimeMillis();

		// constructing dates list
		final int PERIOD = 30; // days
		final Date max = new Date();
		final Date min = new Date(max.getTime() - (PERIOD * 86400000));

		// final List<Date> dates = Utils.getPeriodDates(max, PERIOD);

		// clear tags
		clearTags(foodBase);
		clearTags(dishBase);

		// process
		List<Versioned<DiaryRecord>> items = diary.getRecords(min, max, false);
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
					process(food.getName(), delta, foodBase, dishBase);
				}
			}
		}

		/**/Log.v(TAG, String.format("Indexated in %d msec", System.currentTimeMillis() - time));
	}

	private static int f(Date curDate, Date minDate, Date maxDate)
	{
		int delta = (int) ((100 * (curDate.getTime() - minDate.getTime())) / (maxDate.getTime() - minDate.getTime()));
		return delta * delta;
	}

	private static <T extends Tagged> void clearTags(BaseService<T> base)
	{
		List<Versioned<T>> list = base.findAll(false);
		for (Versioned<T> item : list)
		{
			item.getData().setTag(0);
		}
		base.save(list);
	}

	private static void process(String name, int delta, FoodBaseService foodBase, DishBaseService dishBase)
	{
		if (process(name, delta, foodBase))
			return;
		if (process(name, delta, dishBase))
			return;
	}

	private static <T extends Tagged> boolean process(String name, int delta, BaseService<T> base)
	{
		Versioned<T> item = base.findOne(name);
		if (null != item)
		{
			item.getData().setTag(item.getData().getTag() + delta);
			base.save(Arrays.asList(item));
			return true;
		}
		else
		{
			return false;
		}
	}
}

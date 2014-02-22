package org.bosik.diacomp.android.services;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.bo.FoodMassed;
import org.bosik.diacomp.core.bo.diary.DiaryRecord;
import org.bosik.diacomp.core.bo.diary.records.MealRecord;
import org.bosik.diacomp.core.bo.interfaces.Tagged;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.core.services.BaseService;
import org.bosik.diacomp.core.services.DiaryService;
import org.bosik.diacomp.core.services.DishBaseService;
import org.bosik.diacomp.core.services.FoodBaseService;
import org.bosik.diacomp.core.utils.Utils;
import android.util.Log;

public class RelevantIndexator
{
	private static final String	TAG	= RelevantIndexator.class.getSimpleName();

	public static void indexate(DiaryService diary, FoodBaseService foodBase, DishBaseService dishBase)
	{
		/**/long time = System.currentTimeMillis();

		// constructing dates list
		final int PERIOD = 30; // days
		final Date max = Utils.now();
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
		List<Versioned<T>> list = base.findAll();
		for (Versioned<T> item : list)
		{
			item.getData().setTag(0);
			base.update(item);
		}
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
			base.update(item);
			return true;
		}
		else
		{
			return false;
		}
	}
}

package org.bosik.diacomp.services;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.bo.FoodMassed;
import org.bosik.diacomp.bo.basic.RelativeTagged;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.bo.diary.records.MealRecord;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.dao.BaseDAO;
import org.bosik.diacomp.persistence.dao.DiaryDAO;
import org.bosik.diacomp.persistence.dao.DishBaseDAO;
import org.bosik.diacomp.persistence.dao.FoodBaseDAO;
import org.bosik.diacomp.utils.Utils;
import android.util.Log;

public class RelevantIndexator
{
	private static final String	TAG	= RelevantIndexator.class.getSimpleName();

	public static void indexate(DiaryDAO diary, FoodBaseDAO foodBase, DishBaseDAO dishBase)
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
		List<Versioned<DiaryRecord>> items = diary.getRecords(min, max);
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

	private static <T extends RelativeTagged> void clearTags(BaseDAO<T> base)
	{
		List<Versioned<T>> list = base.findAll();
		for (Versioned<T> item : list)
		{
			item.getData().setTag(0);
			base.update(item);
		}
	}

	private static void process(String name, int delta, FoodBaseDAO foodBase, DishBaseDAO dishBase)
	{
		if (process(name, delta, foodBase))
			return;
		if (process(name, delta, dishBase))
			return;
	}

	private static <T extends RelativeTagged> boolean process(String name, int delta, BaseDAO<T> base)
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

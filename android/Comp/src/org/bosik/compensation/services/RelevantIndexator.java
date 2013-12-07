package org.bosik.compensation.services;

import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.FoodMassed;
import org.bosik.compensation.bo.RelativeTagged;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.bo.diary.records.DiaryRecord;
import org.bosik.compensation.bo.diary.records.MealRecord;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.DishBaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class RelevantIndexator
{
	private static final String	TAG	= RelevantIndexator.class.getSimpleName();

	public static void indexate(DiaryDAO diary, FoodBaseDAO foodBase, DishBaseDAO dishBase)
	{
		/**/long time = System.currentTimeMillis();

		// constructing dates list
		final Date now = Utils.now();
		final int PERIOD = 30; // days
		final List<Date> dates = Utils.getPeriodDates(now, PERIOD);

		// clear tags
		clearTags(foodBase);
		clearTags(dishBase);

		// process
		List<DiaryPage> pages = diary.getPages(dates);
		for (DiaryPage page : pages)
		{
			for (int i = 0; i < page.count(); i++)
			{
				DiaryRecord rec = page.get(i);
				if (rec.getClass().equals(MealRecord.class))
				{
					MealRecord meal = (MealRecord) rec;
					for (int j = 0; j < meal.count(); j++)
					{
						FoodMassed food = meal.get(j);
						process(food.getName(), foodBase, dishBase);
					}
				}
			}
		}

		/**/Log.v(TAG, String.format("Indexated in %d msec", System.currentTimeMillis() - time));
	}

	private static <T extends RelativeTagged> void clearTags(BaseDAO<T> base)
	{
		List<T> list = base.findAll();
		for (RelativeTagged item : list)
		{
			item.setTag(0);
		}
		base.replaceAll(list, base.getVersion());
	}

	private static void process(String name, FoodBaseDAO foodBase, DishBaseDAO dishBase)
	{
		if (process(name, foodBase))
			return;
		if (process(name, dishBase))
			return;
	}

	@SuppressWarnings("unchecked")
	private static <T extends RelativeTagged> boolean process(String name, BaseDAO<T> base)
	{
		RelativeTagged item = base.findOne(name);
		if (null != item)
		{
			item.setTag(item.getTag() + 1);
			base.update((T) item);
			return true;
		}
		else
		{
			return false;
		}
	}
}

package org.bosik.compensation.persistence.repository.diary;

import java.util.Date;
import junit.framework.TestCase;
import org.bosik.compensation.bo.common.FoodMassed;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.bo.diary.records.BloodRecord;
import org.bosik.compensation.bo.diary.records.InsRecord;
import org.bosik.compensation.bo.diary.records.MealRecord;
import org.bosik.compensation.bo.diary.records.NoteRecord;

public class DiaryPageUtils extends TestCase
{
	public static DiaryPage demoPageA()
	{
		DiaryPage page = new DiaryPage();
		page.setDate(new Date(2002, 03, 15));
		page.add(new BloodRecord(127, 4.9, 3));
		page.add(new InsRecord(135, 12));

		MealRecord meal = new MealRecord(201, true);
		meal.add(new FoodMassed("Карбонат \"Восточный\" (Черн)", 9.9, 26.3, 0, 276, 90));
		meal.add(new FoodMassed("Хлеб чёрный \"Премиум\"", 5.5, 0.9, 44.1, 206.3, 42));

		page.add(meal);
		page.add(new NoteRecord(230, "Демо"));

		return page;
	}

	public static DiaryPage demoPageB()
	{
		DiaryPage page = new DiaryPage();
		page.setDate(new Date(2002, 03, 16));
		page.add(new BloodRecord(820, 6.8, 7));
		page.add(new InsRecord(829, 16));
		page.add(new MealRecord(850, false));
		page.add(new NoteRecord(1439, "DTest Again"));

		return page;
	}

	public static void comparePages(DiaryPage expPage, DiaryPage actPage)
	{
		// check the header
		assertEquals(expPage.getDate().getTime(), actPage.getDate().getTime(), 5 * 1000);
		assertEquals(expPage.getTimeStamp().getTime(), actPage.getTimeStamp().getTime(), 5 * 1000);
		assertEquals(expPage.getVersion(), actPage.getVersion());

		// check body
		assertEquals(expPage.count(), actPage.count());
		for (int i = 0; i < expPage.count(); i++)
		{
			assertEquals(expPage.get(i).getTime(), actPage.get(i).getTime());

			if (expPage.get(i).getClass() == BloodRecord.class)
			{
				assertEquals(((BloodRecord) expPage.get(i)).getValue(), ((BloodRecord) actPage.get(i)).getValue());
				assertEquals(((BloodRecord) expPage.get(i)).getFinger(), ((BloodRecord) actPage.get(i)).getFinger());
			}
			else
				if (expPage.get(i).getClass() == InsRecord.class)
				{
					assertEquals(((InsRecord) expPage.get(i)).getValue(), ((InsRecord) actPage.get(i)).getValue());
				}
				else
					if (expPage.get(i).getClass() == MealRecord.class)
					{
						MealRecord expMeal = (MealRecord) expPage.get(i);
						MealRecord actMeal = (MealRecord) actPage.get(i);

						assertEquals(expMeal.getShortMeal(), actMeal.getShortMeal());
						assertEquals(expMeal.count(), actMeal.count());

						for (int j = 0; j < expMeal.count(); j++)
						{
							assertEquals(expMeal.get(j).getName(), actMeal.get(j).getName());
							assertEquals(expMeal.get(j).getRelProts(), actMeal.get(j).getRelProts());
							assertEquals(expMeal.get(j).getRelFats(), actMeal.get(j).getRelFats());
							assertEquals(expMeal.get(j).getRelCarbs(), actMeal.get(j).getRelCarbs());
							assertEquals(expMeal.get(j).getRelValue(), actMeal.get(j).getRelValue());
							assertEquals(expMeal.get(j).getMass(), actMeal.get(j).getMass());
						}
					}

			if (expPage.get(i).getClass() == NoteRecord.class)
			{
				assertEquals(((NoteRecord) expPage.get(i)).getText(), ((NoteRecord) actPage.get(i)).getText());
			}
		}
	}

}

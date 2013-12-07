package org.bosik.compensation.fakes.mocks;

import java.util.Date;
import junit.framework.TestCase;
import org.bosik.compensation.bo.FoodMassed;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.bo.diary.records.BloodRecord;
import org.bosik.compensation.bo.diary.records.DiaryRecord;
import org.bosik.compensation.bo.diary.records.InsRecord;
import org.bosik.compensation.bo.diary.records.MealRecord;
import org.bosik.compensation.bo.diary.records.NoteRecord;

public class DiaryPageUtils extends TestCase
{
	private static final double	EPS	= 0.00001;

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

	public static void compareRecords(DiaryRecord expRecord, DiaryRecord actRecord)
	{
		assertEquals(expRecord.getTime(), actRecord.getTime());
	}

	private static void compareBloodRecords(BloodRecord expRecord, BloodRecord actRecord)
	{
		compareRecords(expRecord, actRecord);

		assertEquals(expRecord.getValue(), actRecord.getValue(), EPS);
		assertEquals(expRecord.getFinger(), actRecord.getFinger());
	}

	private static void compareInsRecords(InsRecord expRecord, InsRecord actRecord)
	{
		compareRecords(expRecord, actRecord);

		assertEquals(expRecord.getValue(), actRecord.getValue(), EPS);
	}

	private static void compareMealRecords(MealRecord expRecord, MealRecord actRecord)
	{
		compareRecords(expRecord, actRecord);

		assertEquals(expRecord.getTime(), actRecord.getTime());
		assertEquals(expRecord.getShortMeal(), actRecord.getShortMeal());
		assertEquals(expRecord.count(), actRecord.count());

		for (int j = 0; j < expRecord.count(); j++)
		{
			compareFoodMassed(expRecord.get(j), actRecord.get(j));
		}
	}

	private static void compareFoodMassed(FoodMassed expItem, FoodMassed actItem)
	{
		assertEquals(expItem.getName(), actItem.getName());
		assertEquals(expItem.getRelProts(), actItem.getRelProts(), EPS);
		assertEquals(expItem.getRelFats(), actItem.getRelFats(), EPS);
		assertEquals(expItem.getRelCarbs(), actItem.getRelCarbs(), EPS);
		assertEquals(expItem.getRelValue(), actItem.getRelValue(), EPS);
		assertEquals(expItem.getMass(), actItem.getMass(), EPS);
	}

	private static void compareNoteRecords(NoteRecord expRecord, NoteRecord actRecord)
	{
		compareRecords(expRecord, actRecord);

		assertEquals(expRecord.getText(), actRecord.getText());
	}

	public static void comparePages(DiaryPage expPage, DiaryPage actPage)
	{
		// check the header
		final int EPS_TIME = 5 * 1000;
		assertEquals(expPage.getDate().getTime(), actPage.getDate().getTime(), EPS_TIME);
		assertEquals(expPage.getTimeStamp().getTime(), actPage.getTimeStamp().getTime(), EPS_TIME);
		assertEquals(expPage.getVersion(), actPage.getVersion());

		// check body
		assertEquals(expPage.count(), actPage.count());
		for (int i = 0; i < expPage.count(); i++)
		{
			DiaryRecord expRecord = expPage.get(i);
			DiaryRecord actRecord = actPage.get(i);

			// @formatter:off
			if (expRecord.getClass() == BloodRecord.class)	compareBloodRecords((BloodRecord) expRecord, (BloodRecord) actRecord); else
			if (expRecord.getClass() == InsRecord.class)	compareInsRecords((InsRecord) expRecord, (InsRecord) actRecord); else
			if (expRecord.getClass() == MealRecord.class)	compareMealRecords((MealRecord) expRecord, (MealRecord) actRecord); else
			if (expRecord.getClass() == NoteRecord.class)	compareNoteRecords((NoteRecord) expRecord, (NoteRecord) actRecord);
			// @formatter:on
		}
	}
}

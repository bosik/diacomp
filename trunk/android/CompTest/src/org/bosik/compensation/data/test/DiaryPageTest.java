package org.bosik.compensation.data.test;

import java.util.Calendar;
import junit.framework.TestCase;
import org.bosik.compensation.persistence.entity.diary.DiaryPage;
import org.bosik.compensation.persistence.entity.diary.records.BloodRecord;
import org.bosik.compensation.persistence.entity.diary.records.DiaryRecord;
import org.bosik.compensation.persistence.entity.diary.records.InsRecord;
import org.bosik.compensation.persistence.entity.diary.records.MealRecord;

public class DiaryPageTest extends TestCase
{
	public void testReadingBlood()
	{
		DiaryPage d = new DiaryPage(Calendar.getInstance().getTime(), Calendar.getInstance().getTime(), 1, "*09:05 4,8");

		assertEquals(1, d.count());
		DiaryRecord r = d.get(0);
		assertTrue(r.getClass() == BloodRecord.class);
		assertEquals(9 * 60 + 5, ((BloodRecord) r).getTime());
		assertEquals(4.8, ((BloodRecord) r).getValue());
	}

	public void testReadingIns()
	{
		DiaryPage d = new DiaryPage(Calendar.getInstance().getTime(), Calendar.getInstance().getTime(), 1, "-09:05 7.9");

		assertEquals(1, d.count());
		DiaryRecord r = d.get(0);
		assertTrue(r.getClass() == InsRecord.class);
		assertEquals(9 * 60 + 5, ((InsRecord) r).getTime());
		assertEquals(7.9, ((InsRecord) r).getValue());
	}

	public void testReadingShortMeal()
	{
		DiaryPage d = new DiaryPage(Calendar.getInstance().getTime(), Calendar.getInstance().getTime(), 1,
				" 02.48s\n#Сахар[10|10|99,8|379]:2,5");

		assertEquals(1, d.count());
		DiaryRecord r = d.get(0);
		assertTrue(r.getClass() == MealRecord.class);
		assertEquals(true, ((MealRecord) r).getShortMeal());
		assertEquals(2 * 60 + 48, ((MealRecord) r).getTime());
		assertEquals(0.25, ((MealRecord) r).getProts());
		assertEquals(0.25, ((MealRecord) r).getFats());
		assertEquals(2.495, ((MealRecord) r).getCarbs());
		assertEquals(9.475, ((MealRecord) r).getValue());

		assertEquals(1, ((MealRecord) r).size());
		assertEquals("Сахар", ((MealRecord) r).get(0).getName());
		assertEquals(10.0, ((MealRecord) r).get(0).getRelProts());
		assertEquals(10.0, ((MealRecord) r).get(0).getRelFats());
		assertEquals(99.8, ((MealRecord) r).get(0).getRelCarbs());
		assertEquals(379.0, ((MealRecord) r).get(0).getRelValue());
		assertEquals(2.5, ((MealRecord) r).get(0).getMass());

	}

	public void testReadingMeal()
	{
		DiaryPage d = new DiaryPage(Calendar.getInstance().getTime(), Calendar.getInstance().getTime(), 1,
				" 02.48\n#Сахар[10|10|99,8|379]:2,5");

		assertEquals(1, d.count());
		DiaryRecord r = d.get(0);
		assertTrue(r.getClass() == MealRecord.class);
		assertEquals(false, ((MealRecord) r).getShortMeal());
		assertEquals(2 * 60 + 48, ((MealRecord) r).getTime());
		assertEquals(0.25, ((MealRecord) r).getProts());
		assertEquals(0.25, ((MealRecord) r).getFats());
		assertEquals(2.495, ((MealRecord) r).getCarbs());
		assertEquals(9.475, ((MealRecord) r).getValue());

		assertEquals(1, ((MealRecord) r).size());
		assertEquals("Сахар", ((MealRecord) r).get(0).getName());
		assertEquals(10.0, ((MealRecord) r).get(0).getRelProts());
		assertEquals(10.0, ((MealRecord) r).get(0).getRelFats());
		assertEquals(99.8, ((MealRecord) r).get(0).getRelCarbs());
		assertEquals(379.0, ((MealRecord) r).get(0).getRelValue());
		assertEquals(2.5, ((MealRecord) r).get(0).getMass());
	}

	public void testWriteReadMeal()
	{
		// DiaryPage d = new DiaryPage(Calendar.getInstance().getTime(),
		// Calendar.getInstance().getTime(), 1, "");

		// TODO: Написать тест

		/*
		 * MealRecord
		 * 
		 * assertEquals(1, d.count()); DiaryRecord r = d.get(0); assertTrue(r.getClass() ==
		 * MealRecord.class); assertEquals(false, ((MealRecord)r).getShortMeal()); assertEquals(2*60
		 * + 48, ((MealRecord)r).getTime()); assertEquals(0.25, ((MealRecord)r).getProts());
		 * assertEquals(0.25, ((MealRecord)r).getFats()); assertEquals(2.495,
		 * ((MealRecord)r).getCarbs()); assertEquals(9.475, ((MealRecord)r).getValue());
		 * 
		 * assertEquals(1, ((MealRecord)r).size()); assertEquals("Сахар",
		 * ((MealRecord)r).get(0).getName()); assertEquals(10.0,
		 * ((MealRecord)r).get(0).getRelProts()); assertEquals(10.0,
		 * ((MealRecord)r).get(0).getRelFats()); assertEquals(99.8,
		 * ((MealRecord)r).get(0).getRelCarbs()); assertEquals(379.0,
		 * ((MealRecord)r).get(0).getRelValue()); assertEquals(2.5,
		 * ((MealRecord)r).get(0).getMass());
		 */

	}
}

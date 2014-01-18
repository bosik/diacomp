package org.bosik.compensation.fakes.mocks;

import java.util.ArrayList;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.compensation.utills.TestUtils;
import org.bosik.diacomp.bo.FoodMassed;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.bo.diary.records.BloodRecord;
import org.bosik.diacomp.bo.diary.records.InsRecord;
import org.bosik.diacomp.bo.diary.records.MealRecord;
import org.bosik.diacomp.bo.diary.records.NoteRecord;

public class MockDiaryRecord extends TestCase implements Mock<DiaryRecord>
{
	private Mock<FoodMassed>	mockFoodMassed	= new MockFoodMassed();

	public List<DiaryRecord> getSamples()
	{
		List<DiaryRecord> samples = new ArrayList<DiaryRecord>();

		samples.add(new BloodRecord(TestUtils.time(2012, 04, 18, 16, 42, 05), 7.1, 0));
		samples.add(new BloodRecord(TestUtils.time(2012, 04, 18, 16, 50, 28), 7.0, 1));
		samples.add(new BloodRecord(TestUtils.time(2012, 04, 18, 17, 00, 00), 5.2, 2));

		samples.add(new InsRecord(TestUtils.time(2012, 04, 18, 17, 20, 00), 16.0));

		MealRecord meal1 = new MealRecord(TestUtils.time(2012, 04, 18, 17, 25, 59), false);
		meal1.add(new FoodMassed("�������� \"���������\" (����)", 9.9, 26.3, 0, 276, 90));
		meal1.add(new FoodMassed("���� ������ \"�������\"", 5.5, 0.9, 44.1, 206.3, 42));
		samples.add(meal1);

		MealRecord meal2 = new MealRecord(TestUtils.time(2012, 04, 18, 23, 59, 59), true);
		meal2.add(new FoodMassed("�����", 0.0, 0.0, 99.8, 379.0, 6.0));
		samples.add(meal2);

		MealRecord meal3 = new MealRecord(TestUtils.time(2012, 04, 18, 23, 59, 59), true);
		for (FoodMassed f : mockFoodMassed.getSamples())
		{
			meal3.add(f);
		}
		samples.add(meal3);

		samples.add(new MealRecord(TestUtils.time(2012, 04, 18, 0, 0, 0), true));

		samples.add(new NoteRecord(TestUtils.time(2014, 01, 01, 12, 00, 00), "Just a �������� record with \"quotes\""));

		samples.add(new NoteRecord(TestUtils.time(2013, 12, 31, 23, 59, 59), ""));

		return samples;
	}

	public void compare(DiaryRecord exp, DiaryRecord act)
	{
		assertEquals(exp.getTime(), act.getTime());
		assertEquals(exp.getClass(), act.getClass());

		// @formatter:off
		if (exp.getClass() == BloodRecord.class)	compareBloodRecords((BloodRecord) exp, (BloodRecord) act); else
		if (exp.getClass() == InsRecord.class)		compareInsRecords((InsRecord) exp, (InsRecord) act); else
		if (exp.getClass() == MealRecord.class)		compareMealRecords((MealRecord) exp, (MealRecord) act); else
		if (exp.getClass() == NoteRecord.class)		compareNoteRecords((NoteRecord) exp, (NoteRecord) act);
		// @formatter:on
	}

	private static void compareBloodRecords(BloodRecord exp, BloodRecord act)
	{
		assertEquals(exp.getValue(), act.getValue(), TestUtils.EPS);
		assertEquals(exp.getFinger(), act.getFinger());
	}

	private static void compareInsRecords(InsRecord exp, InsRecord act)
	{
		assertEquals(exp.getValue(), act.getValue(), TestUtils.EPS);
	}

	private void compareMealRecords(MealRecord exp, MealRecord act)
	{
		assertEquals(exp.getTime(), act.getTime());
		assertEquals(exp.getShortMeal(), act.getShortMeal());
		assertEquals(exp.count(), act.count());

		for (int j = 0; j < exp.count(); j++)
		{
			mockFoodMassed.compare(exp.get(j), act.get(j));
		}
	}

	private static void compareNoteRecords(NoteRecord expRecord, NoteRecord actRecord)
	{
		assertEquals(expRecord.getText(), actRecord.getText());
	}
}
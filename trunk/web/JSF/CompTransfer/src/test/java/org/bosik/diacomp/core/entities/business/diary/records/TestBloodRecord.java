package org.bosik.diacomp.core.entities.business.diary.records;

import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import junit.framework.TestCase;

public class TestBloodRecord extends TestCase
{
	public void testCheckValue()
	{
		assertTrue(BloodRecord.checkValue(0.1));
		assertTrue(BloodRecord.checkValue(5.5));
		assertFalse(BloodRecord.checkValue(-2));
		assertFalse(BloodRecord.checkValue(0));
	}

	public void testCheckFinger()
	{
		for (int i = -1; i < 10; i++)
		{
			assertTrue(BloodRecord.checkFinger(0));
		}
		assertFalse(BloodRecord.checkFinger(-2));
		assertFalse(BloodRecord.checkFinger(10));
		assertFalse(BloodRecord.checkFinger(10500));
	}
}
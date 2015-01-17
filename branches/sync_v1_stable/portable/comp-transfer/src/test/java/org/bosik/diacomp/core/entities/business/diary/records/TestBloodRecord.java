package org.bosik.diacomp.core.entities.business.diary.records;

import junit.framework.TestCase;

@SuppressWarnings("static-method")
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

	public void test_setTime_null_exceptionThrown()
	{
		BloodRecord rec = new BloodRecord();
		try
		{
			rec.setTime(null);
			fail("Exception was not thrown");
		}
		catch (IllegalArgumentException e)
		{
			// just as planned
		}
	}
}

package org.bosik.compensation.bo.diary.records;

import junit.framework.TestCase;

public class BloodRecordTest extends TestCase
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

	public void testClone()
	{
		BloodRecord a = new BloodRecord(890, 5.1, 2);
		BloodRecord b = (BloodRecord) a.clone();

		assertEquals(a, b);
		assertNotSame(a, b);
		assertEquals(a.getId(), b.getId());
		assertEquals(a.getTime(), b.getTime());
		assertEquals(a.getValue(), b.getValue(), 0.01);
		assertEquals(a.getFinger(), b.getFinger());

		a.setTime(1003);
		assertEquals(890, b.getTime());
	}
}
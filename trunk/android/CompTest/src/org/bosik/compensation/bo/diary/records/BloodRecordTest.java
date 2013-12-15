package org.bosik.compensation.bo.diary.records;

import junit.framework.TestCase;
import org.bosik.compensation.bo.basic.Versioned;

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

	public void testClone() throws CloneNotSupportedException
	{
		Versioned<BloodRecord> a = new Versioned<BloodRecord>(new BloodRecord(890, 5.1, 2));
		Versioned<BloodRecord> b = a.clone();

		assertEquals(a, b);
		assertNotSame(a, b);
		assertEquals(a.getData().getTime(), b.getData().getTime());
		assertEquals(a.getData().getValue(), b.getData().getValue(), 0.01);
		assertEquals(a.getData().getFinger(), b.getData().getFinger());

		a.getData().setTime(1003);
		assertEquals(890, b.getData().getTime());
	}
}
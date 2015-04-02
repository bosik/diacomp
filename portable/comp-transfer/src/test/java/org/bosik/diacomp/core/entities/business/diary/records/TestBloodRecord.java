/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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

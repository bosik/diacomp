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
package org.bosik.diacomp.core.utils;

import java.text.ParseException;
import java.util.Date;
import java.util.List;
import junit.framework.TestCase;
import org.junit.Test;

@SuppressWarnings("static-method")
public class TestUtils extends TestCase
{
	// private static String formatArray(byte array[])
	// {
	// StringBuilder sb = new StringBuilder("{");
	//
	// for (int i = 0; i < array.length; i++)
	// {
	// sb.append(array[i]);
	// if (i < (array.length - 1))
	// {
	// sb.append(", ");
	// }
	// }
	//
	// sb.append("}");
	// return sb.toString();
	// }

	@Test
	public void testIntTo00()
	{
		assertEquals("-10", Utils.intTo00(-10));
		assertEquals("-1", Utils.intTo00(-1));
		assertEquals("00", Utils.intTo00(0));
		assertEquals("01", Utils.intTo00(1));
		assertEquals("09", Utils.intTo00(9));
		assertEquals("10", Utils.intTo00(10));
	}

	public void test_formatDoubleShort()
	{
		assertEquals("0", Utils.formatDoubleShort(0.0));
		assertEquals("1", Utils.formatDoubleShort(1.0));
		assertEquals("-1", Utils.formatDoubleShort(-1.0));
		assertEquals("10", Utils.formatDoubleShort(10.0));
		assertEquals("10.1", Utils.formatDoubleShort(10.1));
	}

	public void testCheckTime()
	{
		// корректное время
		assertTrue(Utils.checkTime(0, 0));
		assertTrue(Utils.checkTime(20, 59));
		assertTrue(Utils.checkTime(23, 30));
		assertTrue(Utils.checkTime(23, 59));

		// некорректное время
		assertFalse(Utils.checkTime(0, -1));
		assertFalse(Utils.checkTime(-1, 0));
		assertFalse(Utils.checkTime(24, 0));
		assertFalse(Utils.checkTime(0, 60));
		assertFalse(Utils.checkTime(24, 70));
		assertFalse(Utils.checkTime(-100, 100));
	}

	public void testStrToTime()
	{
		// нормальный тест
		assertEquals(0, Utils.parseMinuteTime("00:00"));
		assertEquals(1, Utils.parseMinuteTime("00:01"));
		assertEquals(59, Utils.parseMinuteTime("00:59"));
		assertEquals(60, Utils.parseMinuteTime("01:00"));
		assertEquals(630, Utils.parseMinuteTime("10:30"));
		assertEquals(1439, Utils.parseMinuteTime("23:59"));

		// краш-тест
		try
		{
			Utils.parseMinuteTime(null);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.parseMinuteTime("");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.parseMinuteTime("gArBAgE");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.parseMinuteTime(":");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.parseMinuteTime("xx:yy");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.parseMinuteTime("10:60");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.parseMinuteTime("24:00");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.parseMinuteTime("00:-1");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.parseMinuteTime("-1:00");
			fail();
		}
		catch (Exception e)
		{
		}
	}

	// public void testTimeToStr()
	// {
	// // нормальный тест
	// assertEquals("00:00", Utils.timeToStr(0));
	// assertEquals("00:01", Utils.timeToStr(1));
	// assertEquals("00:59", Utils.timeToStr(59));
	// assertEquals("01:00", Utils.timeToStr(60));
	// assertEquals("10:30", Utils.timeToStr(630));
	// assertEquals("23:59", Utils.timeToStr(1439));
	//
	// // краш-тест
	// try
	// {
	// Utils.timeToStr(1440);
	// fail();
	// }
	// catch (Exception e)
	// {
	// }
	// try
	// {
	// Utils.timeToStr(-1);
	// fail();
	// }
	// catch (Exception e)
	// {
	// }
	// }

	public void testTimeToMin()
	{
		assertEquals(0, Utils.timeToMin(Utils.time(2013, 01, 01, 00, 00, 00)));
		assertEquals(0, Utils.timeToMin(Utils.time(2013, 01, 01, 00, 00, 01)));
		assertEquals(1, Utils.timeToMin(Utils.time(2013, 01, 01, 00, 01, 00)));
		assertEquals(60, Utils.timeToMin(Utils.time(2013, 01, 01, 01, 00, 00)));
		assertEquals(121, Utils.timeToMin(Utils.time(2013, 01, 01, 02, 01, 00)));
		assertEquals(1439, Utils.timeToMin(Utils.time(2013, 01, 01, 23, 59, 00)));
	}

	public void testFormatDate()
	{
		assertEquals("2012-04-02", Utils.formatDateUTC(Utils.date(2012, 04, 02)));
		assertEquals("2012-05-01", Utils.formatDateUTC(Utils.date(2012, 05, 01)));
	}

	public void testFormatTime()
	{
		assertEquals("2012-05-01 09:45:17", Utils.formatTimeUTC(Utils.time(2012, 05, 01, 9, 45, 17)));
		assertEquals("2012-05-01 21:45:17", Utils.formatTimeUTC(Utils.time(2012, 05, 01, 21, 45, 17)));
		assertEquals("2012-04-02 00:00:00", Utils.formatTimeUTC(Utils.time(2012, 04, 02, 00, 00, 00)));
	}

	public void testParseTime()
	{
		assertEquals(Utils.time(2012, 04, 02, 00, 00, 00), Utils.parseTimeUTC("2012-04-02 00:00:00"));
		assertEquals(Utils.time(2012, 05, 01, 9, 45, 17), Utils.parseTimeUTC("2012-05-01 09:45:17"));
		assertEquals(Utils.time(2012, 05, 01, 22, 30, 17), Utils.parseTimeUTC("2012-05-01 22:30:17"));
	}

	public void testParseTime_performance()
	{
		long time = System.currentTimeMillis();
		final int count = 1000000;

		for (int i = 0; i < count; i++)
		{
			Utils.parseTimeUTC("2012-04-02 00:00:00");
		}

		time = System.currentTimeMillis() - time;
		final double itemsPerSec = (double)count / (double)time * 1000.0;
		if (itemsPerSec < 500000)
		{
			fail(String.format("Speed: %.0f items/sec", itemsPerSec));
		}
	}

	public void testParseDate() throws ParseException
	{
		assertEquals(Utils.date(2012, 04, 02), Utils.parseDateUTC("2012-04-02"));
		assertEquals(Utils.date(2012, 05, 01), Utils.parseDateUTC("2012-05-01"));
	}

	public void testGetPrevDay()
	{
		assertEquals(Utils.date(2011, 12, 31), Utils.getPrevDay(Utils.date(2012, 01, 01)));
		assertEquals(Utils.date(2012, 04, 01), Utils.getPrevDay(Utils.date(2012, 04, 02)));
		assertEquals(Utils.date(2012, 02, 29), Utils.getPrevDay(Utils.date(2012, 03, 01))); // leap
	}

	public void testGetNextDay()
	{
		assertEquals(Utils.date(2012, 01, 01), Utils.getNextDay(Utils.date(2011, 12, 31)));
		assertEquals(Utils.date(2012, 04, 02), Utils.getNextDay(Utils.date(2012, 04, 01)));
		assertEquals(Utils.date(2012, 02, 29), Utils.getNextDay(Utils.date(2012, 02, 28))); // leap
	}

	public void testGetPeriodDates_empty()
	{
		List<Date> dates = Utils.getPeriodDates(Utils.date(2013, 8, 4), 0);

		assertEquals(0, dates.size());
	}

	public void testGetPeriodDates_one()
	{
		List<Date> dates = Utils.getPeriodDates(Utils.date(2013, 8, 4), 1);

		assertEquals(1, dates.size());
		assertEquals(Utils.date(2013, 8, 4).getTime(), dates.get(0).getTime());
	}

	public void testGetPeriodDates_many()
	{
		List<Date> dates = Utils.getPeriodDates(Utils.date(2013, 8, 4), 4);

		assertEquals(4, dates.size());
		assertEquals(Utils.date(2013, 8, 1).getTime(), dates.get(0).getTime());
		assertEquals(Utils.date(2013, 8, 2).getTime(), dates.get(1).getTime());
		assertEquals(Utils.date(2013, 8, 3).getTime(), dates.get(2).getTime());
		assertEquals(Utils.date(2013, 8, 4).getTime(), dates.get(3).getTime());
	}

	public void test_Calculate()
	{
		assertEquals(0.0, Utils.parseExpression(""), Utils.EPS);
		assertEquals(1.0, Utils.parseExpression("1"), Utils.EPS);
		assertEquals(2.0, Utils.parseExpression("2.0"), Utils.EPS);
		assertEquals(3.0, Utils.parseExpression("3,0"), Utils.EPS);

		assertEquals(9.0, Utils.parseExpression("4+5"), Utils.EPS);
		assertEquals(4.0, Utils.parseExpression("4+"), Utils.EPS);
		assertEquals(4.0, Utils.parseExpression("+4"), Utils.EPS);
		assertEquals(13.0, Utils.parseExpression("6.0+7.0"), Utils.EPS);
		assertEquals(17.3, Utils.parseExpression("  8.1 + 9.2 "), Utils.EPS);
		assertEquals(15.0, Utils.parseExpression("1+2+3+4+5"), Utils.EPS);

		assertEquals(11.0, Utils.parseExpression("11-"), Utils.EPS);
		assertEquals(1.0, Utils.parseExpression("11-10"), Utils.EPS);
		assertEquals(9.0, Utils.parseExpression("12-1-2"), Utils.EPS);
		assertEquals(14.0, Utils.parseExpression("13-14+15"), Utils.EPS);

		assertEquals(14.0, Utils.parseExpression("2+3*4"), Utils.EPS);
		assertEquals(10.0, Utils.parseExpression("2*3+4"), Utils.EPS);
		assertEquals(2.0, Utils.parseExpression("2*3-4"), Utils.EPS);
		assertEquals(-10.0, Utils.parseExpression("2-3*4"), Utils.EPS);
		assertEquals(120.0, Utils.parseExpression("2*3*4*5"), Utils.EPS);
		assertEquals(234.0, Utils.parseExpression("2*3*4+5*6*7"), Utils.EPS);
		assertEquals(-186.0, Utils.parseExpression("2*3*4-5*6*7"), Utils.EPS);

		assertEquals(0.5, Utils.parseExpression("1 / 2"), Utils.EPS);
		assertEquals(-0.5, Utils.parseExpression("-1/2"), Utils.EPS);
		//		assertEquals(0.5, Utils.calculate("-1/-2"), Utils.EPS);
		assertEquals(3.0, Utils.parseExpression("1+12/6"), Utils.EPS);
		assertEquals(3.5, Utils.parseExpression("3/2+8/4"), Utils.EPS);
		assertEquals(-16.0, Utils.parseExpression("2*3+4*5-6*7"), Utils.EPS);
	}

	public void test_hasWordStartedWith()
	{
		assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", ""));
		assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "в"));
		assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "вкус"));
		assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "вкусНоТеЕвО"));
		assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "моло"));
		assertFalse(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "сыр"));
	}

	public void test_formatTimePeriod()
	{
		assertEquals("Fail", "00:00", Utils.formatTimePeriod(0));
		assertEquals("Fail", "00:00", Utils.formatTimePeriod(1));
		assertEquals("Fail", "00:01", Utils.formatTimePeriod(60));
		assertEquals("Fail", "01:00", Utils.formatTimePeriod(3600));
		assertEquals("Fail", "23:59", Utils.formatTimePeriod(86399));
	}

	// public void testTimeToStr()
	// {
	// // нормальный тест
	// assertEquals("00:00", AndroidUtils.timeToStr(0));
	// assertEquals("00:01", AndroidUtils.timeToStr(1));
	// assertEquals("00:59", AndroidUtils.timeToStr(59));
	// assertEquals("01:00", AndroidUtils.timeToStr(60));
	// assertEquals("10:30", AndroidUtils.timeToStr(630));
	// assertEquals("23:59", AndroidUtils.timeToStr(1439));
	//
	// // краш-тест
	// try
	// {
	// AndroidUtils.timeToStr(1440);
	// fail();
	// }
	// catch (Exception e)
	// {
	// }
	// try
	// {
	// AndroidUtils.timeToStr(-1);
	// fail();
	// }
	// catch (Exception e)
	// {
	// }
	// }

}

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
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;
import junit.framework.TestCase;

@SuppressWarnings("static-method")
public class TestUtils extends TestCase
{
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
		Locale.setDefault(Locale.US);

		assertEquals("0", Utils.formatDoubleShort(0.0));
		assertEquals("1", Utils.formatDoubleShort(1.0));
		assertEquals("-1", Utils.formatDoubleShort(-1.0));
		assertEquals("10", Utils.formatDoubleShort(10.0));

		assertEquals("10.1", Utils.formatDoubleShort(10.1));

		assertEquals("10", Utils.formatDoubleShort(10.01));
		assertEquals("1.4", Utils.formatDoubleShort(1.41));
		assertEquals("1.4", Utils.formatDoubleShort(1.4000000000000004));
		assertEquals("1.4", Utils.formatDoubleShort(10.5 - 9.1));
	}

	public void test_formatDoubleShort_locales()
	{
		Locale.setDefault(Locale.US);

		assertEquals("0", Utils.formatDoubleShort(0.0));
		assertEquals("1", Utils.formatDoubleShort(1.0));
		assertEquals("-1", Utils.formatDoubleShort(-1.0));
		assertEquals("10", Utils.formatDoubleShort(10.0));

		Locale.setDefault(new Locale("ru"));

		assertEquals("10,1", Utils.formatDoubleShort(10.1));

		assertEquals("10", Utils.formatDoubleShort(10.01));
		assertEquals("1,4", Utils.formatDoubleShort(1.41));
		assertEquals("1,4", Utils.formatDoubleShort(1.4000000000000004));
		assertEquals("1,4", Utils.formatDoubleShort(10.5 - 9.1));
	}

	public void testCheckTime()
	{
		// good cases
		assertTrue(Utils.checkTime(0, 0));
		assertTrue(Utils.checkTime(20, 59));
		assertTrue(Utils.checkTime(23, 30));
		assertTrue(Utils.checkTime(23, 59));

		// bad cases
		assertFalse(Utils.checkTime(0, -1));
		assertFalse(Utils.checkTime(-1, 0));
		assertFalse(Utils.checkTime(24, 0));
		assertFalse(Utils.checkTime(0, 60));
		assertFalse(Utils.checkTime(24, 70));
		assertFalse(Utils.checkTime(-100, 100));
	}

	public void testStrToTime()
	{
		// good cases
		assertEquals(0, Utils.parseMinuteTime("00:00"));
		assertEquals(1, Utils.parseMinuteTime("00:01"));
		assertEquals(59, Utils.parseMinuteTime("00:59"));
		assertEquals(60, Utils.parseMinuteTime("01:00"));
		assertEquals(630, Utils.parseMinuteTime("10:30"));
		assertEquals(1439, Utils.parseMinuteTime("23:59"));

		// bad cases
		parseMinuteTimeMustFail(null);
		parseMinuteTimeMustFail("");
		parseMinuteTimeMustFail("gArBAgE");
		parseMinuteTimeMustFail(":");
		parseMinuteTimeMustFail("xx:yy");
		parseMinuteTimeMustFail("10:60");
		parseMinuteTimeMustFail("24:00");
		parseMinuteTimeMustFail("00:-1");
		parseMinuteTimeMustFail("-1:00");
	}

	private static void parseMinuteTimeMustFail(String s)
	{
		try
		{
			Utils.parseMinuteTime(s);
			fail();
		}
		catch (Exception e)
		{
			// ok, that's expected
		}
	}

	public void testTimeToMin()
	{
		assertEquals(0, Utils.getDayMinutesUTC(Utils.time(2013, 01, 01, 00, 00, 00)));
		assertEquals(0, Utils.getDayMinutesUTC(Utils.time(2013, 01, 01, 00, 00, 01)));
		assertEquals(1, Utils.getDayMinutesUTC(Utils.time(2013, 01, 01, 00, 01, 00)));
		assertEquals(60, Utils.getDayMinutesUTC(Utils.time(2013, 01, 01, 01, 00, 00)));
		assertEquals(121, Utils.getDayMinutesUTC(Utils.time(2013, 01, 01, 02, 01, 00)));
		assertEquals(1439, Utils.getDayMinutesUTC(Utils.time(2013, 01, 01, 23, 59, 00)));
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

	public void testFormatDoubleSigned()
	{
		Locale.setDefault(Locale.US);

		assertEquals("-365.3", Utils.formatDoubleSigned(-365.25));
		assertEquals("-1.0", Utils.formatDoubleSigned(-1.01));
		assertEquals("-0.0", Utils.formatDoubleSigned(-0.01));
		assertEquals("+0.0", Utils.formatDoubleSigned(0.01));
		assertEquals("+1.0", Utils.formatDoubleSigned(1.0));
		assertEquals("+1.1", Utils.formatDoubleSigned(1.1));
		assertEquals("+18.4", Utils.formatDoubleSigned(18.379));
		assertEquals("+18.5", Utils.formatDoubleSigned(18.479));
		assertEquals("+18.6", Utils.formatDoubleSigned(18.579));
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
		final double itemsPerSec = (double) count / (double) time * 1000.0;
		if (itemsPerSec < 400000)
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
		// assertEquals(0.5, Utils.calculate("-1/-2"), Utils.EPS);
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

	public void test_sameDay()
	{
		Date time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 13, 15, 29);
		Date time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 13, 15, 29);
		assertTrue(Utils.sameDay(time1, time2));

		time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 00, 00, 00);
		time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 23, 59, 59);
		assertTrue(Utils.sameDay(time1, time2));

		time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 00, 00, 00);
		time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 5, 00, 00, 00);
		assertFalse(Utils.sameDay(time1, time2));

		time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 00, 00, 00);
		time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 3, 23, 59, 59);
		assertFalse(Utils.sameDay(time1, time2));
	}

	public void test_sameDay_performance()
	{
		Date time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 00, 00, 00);
		Date time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 23, 59, 59);

		final int count = 1000000;
		long time = System.currentTimeMillis();

		for (int i = 0; i < count; i++)
		{
			Utils.sameDay(time1, time2);
		}

		time = System.currentTimeMillis() - time;
		final double itemsPerSec = (double) count / (double) time * 1000.0;
		if (itemsPerSec < 500000)
		{
			fail(String.format("Speed: %.0f items/sec", itemsPerSec));
		}
	}

	private static <T> Set<T> set(T... values)
	{
		return new HashSet<T>(Arrays.asList(values));
	}

	private static <T> void assertEquals(Set<T> expected, Set<T> actual)
	{
		if (!expected.containsAll(actual) || !actual.containsAll(expected))
		{
			failNotEquals("Sets are not equal", expected, actual);
		}
	}

	public void testIntersection()
	{
		assertEquals(set(), Utils.intersection(new HashSet<Integer>(), new HashSet<Integer>()));
		assertEquals(set(), Utils.intersection(set(1), new HashSet<Integer>()));
		assertEquals(set(), Utils.intersection(new HashSet<Integer>(), set(1)));

		assertEquals(set(), Utils.intersection(set(1), set(2)));
		assertEquals(set(2), Utils.intersection(set(1, 2), set(2, 3)));
		assertEquals(set(2, 3), Utils.intersection(set(1, 2, 3), set(2, 3)));
		assertEquals(set(1, 2), Utils.intersection(set(1, 2), set(1, 2, 3)));
		assertEquals(set(1, 2, 3), Utils.intersection(set(1, 2, 3), set(1, 2, 3)));
	}

	public void testDifference()
	{
		assertEquals(set(), Utils.difference(new HashSet<Integer>(), new HashSet<Integer>()));
		assertEquals(set(1), Utils.difference(set(1), new HashSet<Integer>()));
		assertEquals(set(), Utils.difference(new HashSet<Integer>(), set(1)));

		assertEquals(set(1), Utils.difference(set(1), set(2)));
		assertEquals(set(1), Utils.difference(set(1, 2), set(2, 3)));
		assertEquals(set(1), Utils.difference(set(1, 2, 3), set(2, 3)));
		assertEquals(set(), Utils.difference(set(1, 2), set(1, 2, 3)));
		assertEquals(set(), Utils.difference(set(1, 2, 3), set(1, 2, 3)));
	}
}

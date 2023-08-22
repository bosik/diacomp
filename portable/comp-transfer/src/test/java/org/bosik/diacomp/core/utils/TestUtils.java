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

import org.bosik.merklesync.HashUtils;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.text.ParseException;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;

public class TestUtils
{
	@Test
	public void test_removeNonUtf8()
	{
		Assert.assertEquals(null, Utils.removeNonUtf8(null));
		Assert.assertEquals("", Utils.removeNonUtf8(""));

		Assert.assertEquals("Normal string", Utils.removeNonUtf8("Normal string"));
		Assert.assertEquals("\n\r\t\f\b", Utils.removeNonUtf8("\n\r\t\f\b"));
		Assert.assertEquals("Ümläötẞß", Utils.removeNonUtf8("Ümläötẞß"));

		Assert.assertEquals("Goodchars", Utils.removeNonUtf8("Good\uD83D\uDC7D\uD83D\uDC94chars"));
		Assert.assertEquals("Goodchars", Utils.removeNonUtf8("Good\uD800\uD800\uDC00\uDC00chars"));
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void test_performance_removeNonUtf8()
	{
		StringBuilder sb = new StringBuilder();
		while (sb.length() < 1024)
		{
			sb.append(HashUtils.generateGuid());
		}
		final String s = sb.toString();

		System.out.printf(Locale.US, "%.6f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				Utils.removeNonUtf8(s);
			}
		}, 1000000));
	}

	@Test
	public void test_checkSize_normal()
	{
		Utils.checkSize(null, 5);
		Utils.checkSize(null, -1);
		Utils.checkSize("", 5);
		Utils.checkSize("12345", 5);
	}

	@Test
	public void test_checkSize_long()
	{
		String longString = HashUtils.generateGuid();
		while (longString.length() < 128 * 1024 * 1024)
		{
			longString += longString;
		}

		try
		{
			Utils.checkSize(longString, 5);
			Assert.fail(IllegalArgumentException.class.getSimpleName() + " must be thrown");
		}
		catch (IllegalArgumentException e)
		{
			Assert.assertTrue(e.getMessage().length() < longString.length());
		}
	}

	@Test
	public void test_checkNotNull()
	{
		Utils.checkNotNull("", "Message");
		Utils.checkNotNull("text", "Message");

		try
		{
			Utils.checkNotNull(null, "Message");
			Assert.fail(IllegalArgumentException.class.getSimpleName() + " must be thrown");
		}
		catch (IllegalArgumentException e)
		{
			Assert.assertEquals("Message", e.getMessage());
		}
	}

	@Test
	public void test_intTo00()
	{
		Assert.assertEquals("-10", Utils.intTo00(-10));
		Assert.assertEquals("-1", Utils.intTo00(-1));
		Assert.assertEquals("00", Utils.intTo00(0));
		Assert.assertEquals("01", Utils.intTo00(1));
		Assert.assertEquals("09", Utils.intTo00(9));
		Assert.assertEquals("10", Utils.intTo00(10));
	}

	@Test
	public void test_formatDoubleShort()
	{
		Locale.setDefault(Locale.US);

		Assert.assertEquals("0", Utils.formatDoubleShort(0.0));
		Assert.assertEquals("1", Utils.formatDoubleShort(1.0));
		Assert.assertEquals("-1", Utils.formatDoubleShort(-1.0));
		Assert.assertEquals("10", Utils.formatDoubleShort(10.0));

		Assert.assertEquals("10.1", Utils.formatDoubleShort(10.1));

		Assert.assertEquals("10", Utils.formatDoubleShort(10.01));
		Assert.assertEquals("1.4", Utils.formatDoubleShort(1.41));
		Assert.assertEquals("1.4", Utils.formatDoubleShort(1.4000000000000004));
		Assert.assertEquals("1.4", Utils.formatDoubleShort(10.5 - 9.1));
	}

	@Test
	public void test_formatDoubleShort_locales()
	{
		Locale.setDefault(Locale.US);

		Assert.assertEquals("0", Utils.formatDoubleShort(0.0));
		Assert.assertEquals("1", Utils.formatDoubleShort(1.0));
		Assert.assertEquals("-1", Utils.formatDoubleShort(-1.0));
		Assert.assertEquals("10", Utils.formatDoubleShort(10.0));

		Locale.setDefault(new Locale("ru"));

		Assert.assertEquals("10.1", Utils.formatDoubleShort(10.1));

		Assert.assertEquals("10", Utils.formatDoubleShort(10.01));
		Assert.assertEquals("1.4", Utils.formatDoubleShort(1.41));
		Assert.assertEquals("1.4", Utils.formatDoubleShort(1.4000000000000004));
		Assert.assertEquals("1.4", Utils.formatDoubleShort(10.5 - 9.1));
	}

	@Test
	public void test_checkTime()
	{
		// good cases
		Assert.assertTrue(Utils.checkTime(0, 0));
		Assert.assertTrue(Utils.checkTime(20, 59));
		Assert.assertTrue(Utils.checkTime(23, 30));
		Assert.assertTrue(Utils.checkTime(23, 59));

		// bad cases
		Assert.assertFalse(Utils.checkTime(0, -1));
		Assert.assertFalse(Utils.checkTime(-1, 0));
		Assert.assertFalse(Utils.checkTime(24, 0));
		Assert.assertFalse(Utils.checkTime(0, 60));
		Assert.assertFalse(Utils.checkTime(24, 70));
		Assert.assertFalse(Utils.checkTime(-100, 100));
	}

	@Test
	public void test_parseMinuteTime()
	{
		// good cases
		Assert.assertEquals(0, Utils.parseMinuteTime("00:00"));
		Assert.assertEquals(1, Utils.parseMinuteTime("00:01"));
		Assert.assertEquals(59, Utils.parseMinuteTime("00:59"));
		Assert.assertEquals(60, Utils.parseMinuteTime("01:00"));
		Assert.assertEquals(630, Utils.parseMinuteTime("10:30"));
		Assert.assertEquals(1439, Utils.parseMinuteTime("23:59"));

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
			Assert.fail();
		}
		catch (Exception e)
		{
			// ok, that's expected
		}
	}

	@Test
	public void test_getDayMinutesUTC()
	{
		Assert.assertEquals(0, Utils.getDayMinutesUTC(Utils.time(2013, 1, 1, 0, 0, 0)));
		Assert.assertEquals(0, Utils.getDayMinutesUTC(Utils.time(2013, 1, 1, 0, 0, 1)));
		Assert.assertEquals(1, Utils.getDayMinutesUTC(Utils.time(2013, 1, 1, 0, 1, 0)));
		Assert.assertEquals(60, Utils.getDayMinutesUTC(Utils.time(2013, 1, 1, 1, 0, 0)));
		Assert.assertEquals(121, Utils.getDayMinutesUTC(Utils.time(2013, 1, 1, 2, 1, 0)));
		Assert.assertEquals(1439, Utils.getDayMinutesUTC(Utils.time(2013, 1, 1, 23, 59, 0)));
	}

	@Test
	public void test_formatDateUTC()
	{
		Assert.assertEquals("2012-04-02", Utils.formatDateUTC(Utils.date(2012, 4, 2)));
		Assert.assertEquals("2012-05-01", Utils.formatDateUTC(Utils.date(2012, 5, 1)));
	}

	@Test
	public void test_formatTimeUTC()
	{
		Assert.assertEquals("2012-05-01 09:45:17", Utils.formatTimeUTC(Utils.time(2012, 5, 1, 9, 45, 17)));
		Assert.assertEquals("2012-05-01 21:45:17", Utils.formatTimeUTC(Utils.time(2012, 5, 1, 21, 45, 17)));
		Assert.assertEquals("2012-04-02 00:00:00", Utils.formatTimeUTC(Utils.time(2012, 4, 2, 0, 0, 0)));
	}

	@Test
	public void test_formatDoubleSigned()
	{
		Locale.setDefault(Locale.US);

		Assert.assertEquals("-365.3", Utils.formatDoubleSigned(-365.25));
		Assert.assertEquals("-1.0", Utils.formatDoubleSigned(-1.01));
		Assert.assertEquals("-0.0", Utils.formatDoubleSigned(-0.01));
		Assert.assertEquals("+0.0", Utils.formatDoubleSigned(0.01));
		Assert.assertEquals("+1.0", Utils.formatDoubleSigned(1.0));
		Assert.assertEquals("+1.1", Utils.formatDoubleSigned(1.1));
		Assert.assertEquals("+18.4", Utils.formatDoubleSigned(18.379));
		Assert.assertEquals("+18.5", Utils.formatDoubleSigned(18.479));
		Assert.assertEquals("+18.6", Utils.formatDoubleSigned(18.579));
	}

	@Test
	public void test_compactDecimal()
	{
		Assert.assertEquals(null, Utils.compactDecimal(null));
		Assert.assertEquals("", Utils.compactDecimal(""));

		Assert.assertEquals("5.0", Utils.compactDecimal("5.0"));
		Assert.assertEquals("5.0", Utils.compactDecimal("5.00"));
		Assert.assertEquals("5.0", Utils.compactDecimal("5"));
		Assert.assertEquals("5.0", Utils.compactDecimal("05"));
		Assert.assertEquals("5.0", Utils.compactDecimal("05.0"));

		Assert.assertEquals("5.0", Utils.compactDecimal("5,0"));
		Assert.assertEquals("5.0", Utils.compactDecimal("5,00"));
		Assert.assertEquals("5.0", Utils.compactDecimal("05,0"));
	}

	@Test
	public void test_parseTimeUTC()
	{
		Assert.assertEquals(Utils.time(2012, 4, 2, 0, 0, 0), Utils.parseTimeUTC("2012-04-02 00:00:00"));
		Assert.assertEquals(Utils.time(2012, 5, 1, 9, 45, 17), Utils.parseTimeUTC("2012-05-01 09:45:17"));
		Assert.assertEquals(Utils.time(2012, 5, 1, 22, 30, 17), Utils.parseTimeUTC("2012-05-01 22:30:17"));
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void test_performance_parseTimeUTC()
	{
		System.out.printf(Locale.US, "%.6f ms/item%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				Utils.parseTimeUTC("2012-04-02 00:00:00");
			}
		}, 1000000));
	}

	@Test
	public void test_parseDateUTC() throws ParseException
	{
		Assert.assertEquals(Utils.date(2012, 4, 2), Utils.parseDateUTC("2012-04-02"));
		Assert.assertEquals(Utils.date(2012, 5, 1), Utils.parseDateUTC("2012-05-01"));
	}

	@Test
	public void test_getPrevDay()
	{
		Assert.assertEquals(Utils.date(2011, 12, 31), Utils.getPrevDay(Utils.date(2012, 1, 1)));
		Assert.assertEquals(Utils.date(2012, 4, 1), Utils.getPrevDay(Utils.date(2012, 4, 2)));
		Assert.assertEquals(Utils.date(2012, 2, 29), Utils.getPrevDay(Utils.date(2012, 3, 1))); // leap
	}

	@Test
	public void test_getNextDay()
	{
		Assert.assertEquals(Utils.date(2012, 1, 1), Utils.getNextDay(Utils.date(2011, 12, 31)));
		Assert.assertEquals(Utils.date(2012, 4, 2), Utils.getNextDay(Utils.date(2012, 4, 1)));
		Assert.assertEquals(Utils.date(2012, 2, 29), Utils.getNextDay(Utils.date(2012, 2, 28))); // leap
	}

	@Test
	public void test_getPeriodDates_empty()
	{
		List<Date> dates = Utils.getPeriodDates(Utils.date(2013, 8, 4), 0);

		Assert.assertEquals(0, dates.size());
	}

	@Test
	public void test_getPeriodDates_one()
	{
		List<Date> dates = Utils.getPeriodDates(Utils.date(2013, 8, 4), 1);

		Assert.assertEquals(1, dates.size());
		Assert.assertEquals(Utils.date(2013, 8, 4).getTime(), dates.get(0).getTime());
	}

	@Test
	public void test_getPeriodDates_many()
	{
		List<Date> dates = Utils.getPeriodDates(Utils.date(2013, 8, 4), 4);

		Assert.assertEquals(4, dates.size());
		Assert.assertEquals(Utils.date(2013, 8, 1).getTime(), dates.get(0).getTime());
		Assert.assertEquals(Utils.date(2013, 8, 2).getTime(), dates.get(1).getTime());
		Assert.assertEquals(Utils.date(2013, 8, 3).getTime(), dates.get(2).getTime());
		Assert.assertEquals(Utils.date(2013, 8, 4).getTime(), dates.get(3).getTime());
	}

	@Test
	public void test_parseExpression()
	{
		Assert.assertEquals(0.0, Utils.parseExpression(""), Utils.EPS);
		Assert.assertEquals(1.0, Utils.parseExpression("1"), Utils.EPS);
		Assert.assertEquals(2.0, Utils.parseExpression("2.0"), Utils.EPS);
		Assert.assertEquals(3.0, Utils.parseExpression("3,0"), Utils.EPS);

		Assert.assertEquals(9.0, Utils.parseExpression("4+5"), Utils.EPS);
		Assert.assertEquals(4.0, Utils.parseExpression("4+"), Utils.EPS);
		Assert.assertEquals(4.0, Utils.parseExpression("+4"), Utils.EPS);
		Assert.assertEquals(13.0, Utils.parseExpression("6.0+7.0"), Utils.EPS);
		Assert.assertEquals(17.3, Utils.parseExpression("  8.1 + 9.2 "), Utils.EPS);
		Assert.assertEquals(15.0, Utils.parseExpression("1+2+3+4+5"), Utils.EPS);

		Assert.assertEquals(11.0, Utils.parseExpression("11-"), Utils.EPS);
		Assert.assertEquals(1.0, Utils.parseExpression("11-10"), Utils.EPS);
		Assert.assertEquals(9.0, Utils.parseExpression("12-1-2"), Utils.EPS);
		Assert.assertEquals(14.0, Utils.parseExpression("13-14+15"), Utils.EPS);

		Assert.assertEquals(14.0, Utils.parseExpression("2+3*4"), Utils.EPS);
		Assert.assertEquals(10.0, Utils.parseExpression("2*3+4"), Utils.EPS);
		Assert.assertEquals(2.0, Utils.parseExpression("2*3-4"), Utils.EPS);
		Assert.assertEquals(-10.0, Utils.parseExpression("2-3*4"), Utils.EPS);
		Assert.assertEquals(120.0, Utils.parseExpression("2*3*4*5"), Utils.EPS);
		Assert.assertEquals(234.0, Utils.parseExpression("2*3*4+5*6*7"), Utils.EPS);
		Assert.assertEquals(-186.0, Utils.parseExpression("2*3*4-5*6*7"), Utils.EPS);

		Assert.assertEquals(0.5, Utils.parseExpression("1 / 2"), Utils.EPS);
		Assert.assertEquals(-0.5, Utils.parseExpression("-1/2"), Utils.EPS);
		// Assert.assertEquals(0.5, Utils.calculate("-1/-2"), Utils.EPS);
		Assert.assertEquals(3.0, Utils.parseExpression("1+12/6"), Utils.EPS);
		Assert.assertEquals(3.5, Utils.parseExpression("3/2+8/4"), Utils.EPS);
		Assert.assertEquals(-16.0, Utils.parseExpression("2*3+4*5-6*7"), Utils.EPS);
	}

	@Test
	public void test_hasWordStartedWith()
	{
		Assert.assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", ""));
		Assert.assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "в"));
		Assert.assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "вкус"));
		Assert.assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "вкусНоТеЕвО"));
		Assert.assertTrue(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "моло"));
		Assert.assertFalse(Utils.hasWordStartedWith("Молоко \"Вкуснотеево\" 3.2%", "сыр"));
	}

	@Test
	public void test_formatTimePeriod()
	{
		Assert.assertEquals("Fail", "00:00", Utils.formatTimePeriod(0));
		Assert.assertEquals("Fail", "00:00", Utils.formatTimePeriod(1));
		Assert.assertEquals("Fail", "00:01", Utils.formatTimePeriod(60));
		Assert.assertEquals("Fail", "01:00", Utils.formatTimePeriod(3600));
		Assert.assertEquals("Fail", "23:59", Utils.formatTimePeriod(86399));
	}

	@Test
	public void test_sameDay()
	{
		Date time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 13, 15, 29);
		Date time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 13, 15, 29);
		Assert.assertTrue(Utils.sameDay(time1, time2));

		time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 0, 0, 0);
		time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 23, 59, 59);
		Assert.assertTrue(Utils.sameDay(time1, time2));

		time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 0, 0, 0);
		time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 5, 0, 0, 0);
		Assert.assertFalse(Utils.sameDay(time1, time2));

		time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 0, 0, 0);
		time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 3, 23, 59, 59);
		Assert.assertFalse(Utils.sameDay(time1, time2));
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void test_performance_sameDay()
	{
		final Date time1 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 0, 0, 0);
		final Date time2 = Utils.timeLocal(TimeZone.getDefault(), 2013, 8, 4, 23, 59, 59);

		System.out.printf(Locale.US, "%.6f ms/item%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				Utils.sameDay(time1, time2);
			}
		}, 1000000));
	}

	private static <T> Set<T> set(T... values)
	{
		return new HashSet<>(Arrays.asList(values));
	}

	@Test
	public void test_intersection()
	{
		Assert.assertEquals(set(), Utils.intersection(new HashSet<>(), new HashSet<>()));
		Assert.assertEquals(set(), Utils.intersection(set(1), new HashSet<>()));
		Assert.assertEquals(set(), Utils.intersection(new HashSet<>(), set(1)));

		Assert.assertEquals(set(), Utils.intersection(set(1), set(2)));
		Assert.assertEquals(set(2), Utils.intersection(set(1, 2), set(2, 3)));
		Assert.assertEquals(set(2, 3), Utils.intersection(set(1, 2, 3), set(2, 3)));
		Assert.assertEquals(set(1, 2), Utils.intersection(set(1, 2), set(1, 2, 3)));
		Assert.assertEquals(set(1, 2, 3), Utils.intersection(set(1, 2, 3), set(1, 2, 3)));
	}

	@Test
	public void test_difference()
	{
		Assert.assertEquals(set(), Utils.difference(new HashSet<>(), new HashSet<>()));
		Assert.assertEquals(set(1), Utils.difference(set(1), new HashSet<>()));
		Assert.assertEquals(set(), Utils.difference(new HashSet<>(), set(1)));

		Assert.assertEquals(set(1), Utils.difference(set(1), set(2)));
		Assert.assertEquals(set(1), Utils.difference(set(1, 2), set(2, 3)));
		Assert.assertEquals(set(1), Utils.difference(set(1, 2, 3), set(2, 3)));
		Assert.assertEquals(set(), Utils.difference(set(1, 2), set(1, 2, 3)));
		Assert.assertEquals(set(), Utils.difference(set(1, 2, 3), set(1, 2, 3)));
	}

	@Test
	public void test_uppercaseFirstLetter()
	{
		// empty
		Assert.assertEquals(null, Utils.uppercaseFirstLetter(null));
		Assert.assertEquals("", Utils.uppercaseFirstLetter(""));

		// normal
		Assert.assertEquals("A", Utils.uppercaseFirstLetter("a"));
		Assert.assertEquals("Test", Utils.uppercaseFirstLetter("test"));
		Assert.assertEquals("Русский", Utils.uppercaseFirstLetter("русский"));
		Assert.assertEquals("Ärztin", Utils.uppercaseFirstLetter("ärztin"));

		// nothing to uppercase
		Assert.assertEquals(" ", Utils.uppercaseFirstLetter(" "));
		Assert.assertEquals(" another", Utils.uppercaseFirstLetter(" another"));
		Assert.assertEquals("☺", Utils.uppercaseFirstLetter("☺"));
	}

	@Test
	public void test_lowercaseFirstLetter()
	{
		// empty
		Assert.assertEquals(null, Utils.lowercaseFirstLetter(null));
		Assert.assertEquals("", Utils.lowercaseFirstLetter(""));

		// normal
		Assert.assertEquals("a", Utils.lowercaseFirstLetter("A"));
		Assert.assertEquals("test", Utils.lowercaseFirstLetter("Test"));
		Assert.assertEquals("русский", Utils.lowercaseFirstLetter("Русский"));
		Assert.assertEquals("ärztin", Utils.lowercaseFirstLetter("Ärztin"));

		// nothing to lowercase
		Assert.assertEquals(" ", Utils.lowercaseFirstLetter(" "));
		Assert.assertEquals(" another", Utils.lowercaseFirstLetter(" another"));
		Assert.assertEquals("☺", Utils.lowercaseFirstLetter("☺"));
	}
}

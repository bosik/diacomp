package org.bosik.compensation.utills;

import java.io.UnsupportedEncodingException;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import junit.framework.TestCase;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

@SuppressWarnings("static-method")
public class TestUtils extends TestCase
{
	private static final String	TAG			= TestUtils.class.getSimpleName();
	public static final double	EPS			= 0.00001;
	public static final long	EPS_TIME	= 5000;							// ms

	public static Date date(int year, int month, int day)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.set(year, month - 1, day);
		return c.getTime();
	}

	public static Date time(int year, int month, int day, int hour, int min, int sec)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.setTimeZone(TimeZone.getTimeZone("UTC"));
		c.set(year, month - 1, day, hour, min, sec);
		return c.getTime();
	}

	private static String formatArray(byte array[])
	{
		StringBuilder sb = new StringBuilder("{");

		for (int i = 0; i < array.length; i++)
		{
			sb.append(array[i]);
			if (i < (array.length - 1))
			{
				sb.append(", ");
			}
		}

		sb.append("}");
		return sb.toString();
	}

	public void testIntTo00()
	{
		assertEquals("-10", Utils.intTo00(-10));
		assertEquals("-1", Utils.intTo00(-1));
		assertEquals("00", Utils.intTo00(0));
		assertEquals("01", Utils.intTo00(1));
		assertEquals("09", Utils.intTo00(9));
		assertEquals("10", Utils.intTo00(10));
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
		assertEquals(0, Utils.strToTime("00:00"));
		assertEquals(1, Utils.strToTime("00:01"));
		assertEquals(59, Utils.strToTime("00:59"));
		assertEquals(60, Utils.strToTime("01:00"));
		assertEquals(630, Utils.strToTime("10:30"));
		assertEquals(1439, Utils.strToTime("23:59"));

		// краш-тест
		try
		{
			Utils.strToTime(null);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.strToTime("");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.strToTime("gArBAgE");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.strToTime(":");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.strToTime("xx:yy");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.strToTime("10:60");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.strToTime("24:00");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.strToTime("00:-1");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			Utils.strToTime("-1:00");
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
		assertEquals(0, Utils.timeToMin(time(2013, 01, 01, 00, 00, 00)));
		assertEquals(0, Utils.timeToMin(time(2013, 01, 01, 00, 00, 01)));
		assertEquals(1, Utils.timeToMin(time(2013, 01, 01, 00, 01, 00)));
		assertEquals(60, Utils.timeToMin(time(2013, 01, 01, 01, 00, 00)));
		assertEquals(121, Utils.timeToMin(time(2013, 01, 01, 02, 01, 00)));
		assertEquals(1439, Utils.timeToMin(time(2013, 01, 01, 23, 59, 00)));
	}

	public void testFormatDate()
	{
		assertEquals("2012-04-02", Utils.formatDate(date(2012, 04, 02)));
		assertEquals("2012-05-01", Utils.formatDate(date(2012, 05, 01)));
	}

	public void testFormatTime()
	{
		assertEquals("2012-05-01 09:45:17", Utils.formatTimeUTC(time(2012, 05, 01, 9, 45, 17)));
		assertEquals("2012-05-01 21:45:17", Utils.formatTimeUTC(time(2012, 05, 01, 21, 45, 17)));
		assertEquals("2012-04-02 00:00:00", Utils.formatTimeUTC(time(2012, 04, 02, 00, 00, 00)));
	}

	public void testParseTime()
	{
		try
		{
			assertEquals(time(2012, 04, 02, 00, 00, 00), Utils.parseTimeUTC("2012-04-02 00:00:00"));
			assertEquals(time(2012, 05, 01, 9, 45, 17), Utils.parseTimeUTC("2012-05-01 09:45:17"));
			assertEquals(time(2012, 05, 01, 22, 30, 17), Utils.parseTimeUTC("2012-05-01 22:30:17"));
		}
		catch (Exception e)
		{
			fail();
		}
	}

	public void testParseDate()
	{
		try
		{
			assertEquals(date(2012, 04, 02), Utils.parseDate("2012-04-02"));
			assertEquals(date(2012, 05, 01), Utils.parseDate("2012-05-01"));
		}
		catch (Exception e)
		{
			fail();
		}
	}

	public void testGetPrevDay()
	{
		assertEquals(date(2011, 12, 31), Utils.getPrevDay(date(2012, 01, 01)));
		assertEquals(date(2012, 04, 01), Utils.getPrevDay(date(2012, 04, 02)));
		assertEquals(date(2012, 02, 29), Utils.getPrevDay(date(2012, 03, 01))); // leap
	}

	public void testGetNextDay()
	{
		assertEquals(date(2012, 01, 01), Utils.getNextDay(date(2011, 12, 31)));
		assertEquals(date(2012, 04, 02), Utils.getNextDay(date(2012, 04, 01)));
		assertEquals(date(2012, 02, 29), Utils.getNextDay(date(2012, 02, 28))); // leap
	}

	public void testGetPeriodDates_empty()
	{
		List<Date> dates = Utils.getPeriodDates(date(2013, 8, 4), 0);

		assertEquals(0, dates.size());
	}

	public void testGetPeriodDates_one()
	{
		List<Date> dates = Utils.getPeriodDates(date(2013, 8, 4), 1);

		assertEquals(1, dates.size());
		assertEquals(date(2013, 8, 4), dates.get(0));
	}

	public void testGetPeriodDates_many()
	{
		List<Date> dates = Utils.getPeriodDates(date(2013, 8, 4), 4);

		assertEquals(4, dates.size());
		assertEquals(date(2013, 8, 1), dates.get(0));
		assertEquals(date(2013, 8, 2), dates.get(1));
		assertEquals(date(2013, 8, 3), dates.get(2));
		assertEquals(date(2013, 8, 4), dates.get(3));
	}

	public void testEncodingUtf8Cp1251Utf8Persistance() throws UnsupportedEncodingException
	{
		String utf8 = "моя строка";
		String cp1251 = Utils.Utf8ToCp1251(utf8);
		String utf8_again = Utils.Cp1251ToUtf8(cp1251);

		Log.d(TAG, "Pers. test: utf8 = " + utf8);
		Log.d(TAG, "Pers. test: utf8 = " + formatArray(utf8.getBytes("UTF-8")));
		Log.d(TAG, "Pers. test: cp1251 = " + cp1251);
		Log.d(TAG, "Pers. test: cp1251 = " + formatArray(cp1251.getBytes("Cp1251")));
		Log.d(TAG, "Pers. test: utf8_again = " + utf8_again);
		Log.d(TAG, "Pers. test: utf8_again = " + formatArray(utf8_again.getBytes("UTF-8")));

		assertEquals(utf8, utf8_again);
	}

	public void testEncodingUtf8ToCp1251() throws UnsupportedEncodingException
	{
		String utf8 = "моя строка";
		String cp1251 = Utils.Utf8ToCp1251(utf8);

		Log.d(TAG, "8->1251 test: utf8 = " + utf8);
		Log.d(TAG, "8->1251 test: utf8 = " + formatArray(utf8.getBytes("UTF-8")));
		Log.d(TAG, "8->1251 test: cp1251 = " + cp1251);
		Log.d(TAG, "8->1251 test: cp1251 = " + formatArray(cp1251.getBytes("Cp1251")));

		byte cp1251_bytes[] = cp1251.getBytes("Cp1251");
		int expected_bytes[] = new int[] { 0xEC, 0xEE, 0xFF, 0x20, 0xF1, 0xF2, 0xF0, 0xEE, 0xEA, 0xE0 };

		// correcting
		for (int i = 0; i < expected_bytes.length; i++)
		{
			if (expected_bytes[i] > 127)
			{
				expected_bytes[i] -= 256;
			}
		}

		// assert
		assertEquals(expected_bytes.length, cp1251_bytes.length);
		for (int i = 0; i < expected_bytes.length; i++)
		{
			assertEquals(expected_bytes[i], cp1251_bytes[i]);
		}
	}

	public void testEncodingCp1251TotUtf8()
	{
		int cp1251_int[] = new int[] { 0xEC, 0xEE, 0xFF, 0x20, 0xF1, 0xF2, 0xF0, 0xEE, 0xEA, 0xE0 };

		byte cp1251_bytes[] = new byte[cp1251_int.length];
		for (int i = 0; i < cp1251_bytes.length; i++)
		{
			cp1251_bytes[i] = (byte) cp1251_int[i];
		}

		String cp1251;
		String utf8 = "";

		try
		{
			cp1251 = new String(cp1251_bytes, "Cp1251");
			utf8 = Utils.Cp1251ToUtf8(cp1251);

			Log.d(TAG, "1251->8 test: cp1251 = " + cp1251);
			Log.d(TAG, "1251->8 test: cp1251 = " + formatArray(cp1251.getBytes("Cp1251")));
			Log.d(TAG, "1251->8 test: utf8 = " + utf8);
			Log.d(TAG, "1251->8 test: utf8 = " + formatArray(utf8.getBytes("UTF-8")));

		}
		catch (UnsupportedEncodingException e)
		{
			e.printStackTrace();
		}

		// assert
		assertEquals("моя строка", utf8);
	}
}

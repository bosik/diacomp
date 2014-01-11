package org.bosik.compensation.utills;

import java.io.UnsupportedEncodingException;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import junit.framework.TestCase;
import org.bosik.compensation.utils.AndroidUtils;

@SuppressWarnings("static-method")
public class TestUtils extends TestCase
{
	// private static final String TAG = TestUtils.class.getSimpleName();
	public static final double	EPS			= 0.00001;
	public static final long	EPS_TIME	= 5000;	// ms

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

	public void testEncodingUtf8Cp1251Utf8Persistance()
	{
		String utf8 = "моя строка";
		String cp1251 = AndroidUtils.Utf8ToCp1251(utf8);
		String utf8_again = AndroidUtils.Cp1251ToUtf8(cp1251);

		// Log.d(TAG, "Pers. test: utf8 = " + utf8);
		// Log.d(TAG, "Pers. test: utf8 = " + formatArray(utf8.getBytes("UTF-8")));
		// Log.d(TAG, "Pers. test: cp1251 = " + cp1251);
		// Log.d(TAG, "Pers. test: cp1251 = " + formatArray(cp1251.getBytes("Cp1251")));
		// Log.d(TAG, "Pers. test: utf8_again = " + utf8_again);
		// Log.d(TAG, "Pers. test: utf8_again = " + formatArray(utf8_again.getBytes("UTF-8")));

		assertEquals(utf8, utf8_again);
	}

	public void testEncodingUtf8ToCp1251() throws UnsupportedEncodingException
	{
		String utf8 = "моя строка";
		String cp1251 = AndroidUtils.Utf8ToCp1251(utf8);

		// Log.d(TAG, "8->1251 test: utf8 = " + utf8);
		// Log.d(TAG, "8->1251 test: utf8 = " + formatArray(utf8.getBytes("UTF-8")));
		// Log.d(TAG, "8->1251 test: cp1251 = " + cp1251);
		// Log.d(TAG, "8->1251 test: cp1251 = " + formatArray(cp1251.getBytes("Cp1251")));

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
			utf8 = AndroidUtils.Cp1251ToUtf8(cp1251);

			// Log.d(TAG, "1251->8 test: cp1251 = " + cp1251);
			// Log.d(TAG, "1251->8 test: cp1251 = " + formatArray(cp1251.getBytes("Cp1251")));
			// Log.d(TAG, "1251->8 test: utf8 = " + utf8);
			// Log.d(TAG, "1251->8 test: utf8 = " + formatArray(utf8.getBytes("UTF-8")));

		}
		catch (UnsupportedEncodingException e)
		{
			e.printStackTrace();
		}

		// assert
		assertEquals("моя строка", utf8);
	}
}

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

import org.bosik.diacomp.core.entities.business.Units;
import org.bosik.diacomp.core.services.analyze.entities.Rate;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.json.JSONArray;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import java.util.Set;
import java.util.TimeZone;

public class Utils
{
	/* =========================================================================================
	 * CONSTANTS
	 * =========================================================================================/

	// Energy values

	/**
	 * Value of proteins, kcal/g
	 */
	public static final double     KCAL_PER_PROTS    = 3.8;
	/**
	 * Value of fats, kcal/g
	 */
	public static final double     KCAL_PER_FATS     = 9.3;
	/**
	 * Value of carbohydrates, kcal/g
	 */
	public static final double     KCAL_PER_CARBS    = 4.1;
	public static final int        CARB_PER_BU       = 12; // g/BU
	public static final Units.Mass DEFAULT_MASS_UNIT = CodedUtils.parse(Units.Mass.class, PreferenceID.RATES_MASS_UNITS.getDefaultValue());
	public static final Rate       STD_COEFFICIENT   = new Rate(0.25, 2.5, 0.0);

	// Time

	public static final int  NsecPerMsec   = 1000000;
	public static final int  MsecPerSec    = 1000;
	public static final int  SecPerMin     = 60;
	public static final int  MinPerHour    = 60;
	public static final int  HourPerDay    = 24;
	public static final int  SecPerHour    = SecPerMin * MinPerHour;
	public static final int  SecPerDay     = SecPerMin * MinPerHour * HourPerDay;
	public static final int  MinPerDay     = MinPerHour * HourPerDay;
	public static final int  HalfMinPerDay = (MinPerHour * HourPerDay) / 2;
	public static final long MsecPerMin    = MsecPerSec * SecPerMin;
	public static final long MsecPerHour   = MsecPerSec * SecPerMin * MinPerHour;
	public static final long MsecPerDay    = MsecPerSec * SecPerMin * MinPerHour * HourPerDay;

	// Epsilon values

	public static final double EPS = 0.00000001;

	// Format settings

	private static char          DECIMAL_DOT;
	private static DecimalFormat DF;

	private static Random r = new Random();

	// Formatters

	public static final String FORMAT_DATE_TIME  = "yyyy-MM-dd HH:mm:ss";
	public static final String FORMAT_DATE       = "yyyy-MM-dd";
	public static final String FORMAT_TIME_SHORT = "HH:mm";

	private static final TimeZone TIMEZONE_UTC = TimeZone.getTimeZone("UTC");

	private static final ThreadLocal<SimpleDateFormat> FORMATTER_DATE_UTC = new ThreadLocal<SimpleDateFormat>()
	{
		@Override
		protected SimpleDateFormat initialValue()
		{
			SimpleDateFormat format = new SimpleDateFormat(FORMAT_DATE, Locale.US);
			format.setTimeZone(TIMEZONE_UTC);
			return format;
		}
	};

	private static final ThreadLocal<SimpleDateFormat> FORMATTER_TIME_UTC = new ThreadLocal<SimpleDateFormat>()
	{
		@Override
		protected SimpleDateFormat initialValue()
		{
			SimpleDateFormat format = new SimpleDateFormat(FORMAT_DATE_TIME, Locale.US);
			format.setTimeZone(TIMEZONE_UTC);
			return format;
		}
	};

	public static final String ALPHANUMERIC = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ";

	static
	{
		NumberFormat f = NumberFormat.getInstance(Locale.US);
		if (f instanceof DecimalFormat)
		{
			DF = (DecimalFormat) f;
			DECIMAL_DOT = DF.getDecimalFormatSymbols().getDecimalSeparator();
		}
		else
		{
			throw new RuntimeException("Number format is not a decimal format");
		}
	}

	private static SimpleDateFormat getFormatDateLocal(TimeZone timeZone)
	{
		SimpleDateFormat format = new SimpleDateFormat(FORMAT_DATE, Locale.getDefault());
		format.setTimeZone(timeZone);
		return format;
	}

	private static SimpleDateFormat getFormatTimeLocal(TimeZone timeZone)
	{
		SimpleDateFormat format = new SimpleDateFormat(FORMAT_DATE_TIME, Locale.getDefault());
		format.setTimeZone(timeZone);
		return format;
	}

	private static SimpleDateFormat getFormatTimeLocalShort(TimeZone timeZone)
	{
		SimpleDateFormat format = new SimpleDateFormat(FORMAT_TIME_SHORT, Locale.getDefault());
		format.setTimeZone(timeZone);
		return format;
	}

	/* =========================================================================================
	 * PARSERS
	 * =========================================================================================/

	/**
	 * Replaces all . and , with actual locale decimal separator (DECIMAL_DOT)
	 *
	 * @param s
	 * @return
	 */
	public static String checkDot(String s)
	{
		return s.replace('.', DECIMAL_DOT).replace(',', DECIMAL_DOT);
	}

	/**
	 * Parses double value, replacing ./, decimal separator if need
	 *
	 * @param s
	 * @return
	 * @throws ParseException
	 */
	public static double parseDouble(String s) throws ParseException
	{
		return DF.parse(checkDot(s)).doubleValue();
	}

	/**
	 * [tested] Parses minute-time
	 *
	 * @param S Time in format "hh:mm"
	 * @return Minute time (number of minutes since midnight)
	 */
	public static int parseMinuteTime(String S)
	{
		int hour = Integer.parseInt(S.substring(0, 2));
		int min = Integer.parseInt(S.substring(3, 5));

		if (checkTime(hour, min))
		{
			return (hour * MinPerHour) + min;
		}
		else
		{
			throw new IllegalArgumentException("Incorrect time (" + S + ")");
		}
	}

	public static Date parseDateUTC(String date) throws ParseException
	{
		return FORMATTER_DATE_UTC.get().parse(date);
	}

	public static Date parseDateLocal(TimeZone timeZone, String date) throws ParseException
	{
		return getFormatDateLocal(timeZone).parse(date);
	}

	/**
	 * Parses time using STD_FORMAT_TIME_UTC format
	 *
	 * @param time String to parse
	 * @return Date
	 */
	public static Date parseTimeUTC(String time)
	{
		try
		{
			return FORMATTER_TIME_UTC.get().parse(time);
		}
		catch (ParseException e)
		{
			try
			{
				return FORMATTER_DATE_UTC.get().parse(time);
			}
			catch (ParseException e2)
			{
				// TODO: don't wrap (or wrap time parser too)
				throw new IllegalArgumentException(e2);
			}
		}
	}

	/**
	 * Calculates value of simple math expression. Four operations supported: +, -, *, /. Correct
	 * processing of negative values is not guaranteed (f.e., calculate(-1/-2) = -3 instead of
	 * expected 0.5)
	 *
	 * @param s String to calculate (f.e., "2+3*4", "-10*2")
	 */
	public static double parseExpression(String s) throws NumberFormatException
	{
		s = s.trim();

		if (s.isEmpty())
		{
			return 0;
		}

		try
		{
			s = checkDot(s);
			return Double.parseDouble(s);
		}
		catch (NumberFormatException e)
		{
			// well, try next...
		}

		if (s.contains("+"))
		{
			int k = s.lastIndexOf("+");
			String op1 = s.substring(0, k).trim();
			String op2 = s.substring(k + 1).trim();

			if (!op1.isEmpty() && op2.isEmpty())
			{
				return parseExpression(op1);
			}
			if (op1.isEmpty() && !op2.isEmpty())
			{
				return parseExpression(op2);
			}
			if (!op1.isEmpty() && !op2.isEmpty())
			{
				return parseExpression(op1) + parseExpression(op2);
			}
		}

		if (s.contains("-"))
		{
			int k = s.lastIndexOf("-");
			String op1 = s.substring(0, k).trim();
			String op2 = s.substring(k + 1).trim();

			if (!op1.isEmpty() && op2.isEmpty())
			{
				return parseExpression(op1);
			}
			if (op1.isEmpty() && !op2.isEmpty())
			{
				return -parseExpression(op2);
			}
			if (!op1.isEmpty() && !op2.isEmpty())
			{
				return parseExpression(op1) - parseExpression(op2);
			}
		}

		if (s.contains("*"))
		{
			int k = s.lastIndexOf("*");
			String op1 = s.substring(0, k).trim();
			String op2 = s.substring(k + 1).trim();

			if (!op1.isEmpty() && op2.isEmpty())
			{
				return parseExpression(op1);
			}
			if (op1.isEmpty() && !op2.isEmpty())
			{
				return parseExpression(op2);
			}
			if (!op1.isEmpty() && !op2.isEmpty())
			{
				return parseExpression(op1) * parseExpression(op2);
			}
		}

		if (s.contains("/"))
		{
			int k = s.lastIndexOf("/");
			String op1 = s.substring(0, k).trim();
			String op2 = s.substring(k + 1).trim();

			if (!op1.isEmpty() && op2.isEmpty())
			{
				return parseExpression(op1);
			}
			if (op1.isEmpty() && !op2.isEmpty())
			{
				return parseExpression(op2);
			}
			if (!op1.isEmpty() && !op2.isEmpty())
			{
				return parseExpression(op1) / parseExpression(op2);
			}
		}

		throw new NumberFormatException("Can't parse expression: " + s);

		// if (s.isEmpty())
		// {
		// return 0.0;
		// }
		//
		// s = s.replaceAll("\\.", String.valueOf(DECIMAL_DOT));
		// s = s.replaceAll("\\,", String.valueOf(DECIMAL_DOT));
		//
		// try
		// {
		// return (Double)engine.eval(s);
		// }
		// catch (ScriptException e)
		// {
		// throw new RuntimeException(e);
		// }
	}

	/* =========================================================================================
	 * FORMATTERS
	 * =========================================================================================/

	/**
	 * Converts integer into string; non-negative single-digit numbers get one leading
	 * zero. Negative values are returning as-is.
	 *
	 * @param n number
	 * @return
	 */
	public static String intTo00(int n)
	{
		return String.format(Locale.US, "%02d", n);
	}

	/**
	 * Converts double value into string dismissing ".0" part if the value is integer.
	 *
	 * @param x
	 * @return
	 */
	public static String formatDoubleShort(double x)
	{
		String s = String.format("%.1f", x);

		if (s.endsWith(".0") || s.endsWith(",0"))
		{
			s = s.substring(0, s.length() - 2);
		}

		return s;
	}

	public static String formatDoubleSigned(double x)
	{
		return String.format(Locale.US, "%+.1f", x);
	}

	public static String formatDateUTC(Date date)
	{
		if (date == null)
		{
			throw new IllegalArgumentException("date is null");
		}

		return FORMATTER_DATE_UTC.get().format(date);
	}

	public static String formatDateLocal(TimeZone timeZone, Date date)
	{
		if (date == null)
		{
			throw new IllegalArgumentException("date is null");
		}

		return getFormatDateLocal(timeZone).format(date);
	}

	public static String formatBooleanStr(boolean x)
	{
		return x ? "true" : "false";
	}

	public static String formatBooleanInt(boolean x)
	{
		return x ? "1" : "0";
	}

	public static String formatJSONArray(List<String> list)
	{
		JSONArray json = new JSONArray();

		for (String item : list)
		{
			json.put(item);
		}

		return json.toString();
	}

	/**
	 * Formats time as STD_FORMAT_TIME_UTC
	 *
	 * @param time Time
	 * @return Time string
	 */
	public static String formatTimeUTC(Date time)
	{
		if (time == null)
		{
			throw new IllegalArgumentException("time is null");
		}

		return FORMATTER_TIME_UTC.get().format(time);
	}

	public static String formatTimeLocal(TimeZone timeZone, Date time)
	{
		if (time == null)
		{
			throw new IllegalArgumentException("time is null");
		}

		return getFormatTimeLocal(timeZone).format(time);
	}

	public static String formatTimeLocalShort(TimeZone timeZone, Date time)
	{
		if (time == null)
		{
			throw new IllegalArgumentException("time is null");
		}

		return getFormatTimeLocalShort(timeZone).format(time);
	}

	/**
	 * Formats time as hh:mm
	 *
	 * @param time Time in seconds
	 * @return Time string
	 */
	public static String formatTimePeriod(int time)
	{
		int h = time / SecPerHour;
		time %= SecPerHour;
		int m = time / SecPerMin;
		return String.format(Locale.US, "%02d:%02d", h, m);
	}

	/**
	 * Formats time as hh:mm
	 *
	 * @param time Time in minutes
	 * @return Time string
	 */
	public static String formatTimeMin(int time)
	{
		int h = time / MinPerHour;
		int m = time % MinPerHour;
		return String.format(Locale.US, "%02d:%02d", h, m);
	}

	public static String formatK(double k, Units.Mass unit)
	{
		switch (unit)
		{
			case G:
			{
				return String.format(Locale.US, "%.2f", k);
			}

			case BU:
			{
				return String.format(Locale.US, "%.1f", k * CARB_PER_BU);
			}

			default:
			{
				throw new IllegalArgumentException("Unsupported unit of mass: " + unit);
			}
		}
	}

	public static String formatQ(double q)
	{
		return String.format(Locale.US, "%.1f", q);
	}

	public static String formatX(double x, Units.Mass unit)
	{
		switch (unit)
		{
			case G:
			{
				return String.format(Locale.US, "%.3f", x);
			}

			case BU:
			{
				return String.format(Locale.US, "%.2f", x * CARB_PER_BU);
			}

			default:
			{
				throw new IllegalArgumentException("Unsupported unit of mass: " + unit);
			}
		}
	}

	public static String compactDecimal(String s)
	{
		if (s == null || s.length() == 0)
		{
			return s;
		}

		DecimalFormat f = new DecimalFormat();
		DecimalFormatSymbols symbols = new DecimalFormatSymbols();
		symbols.setDecimalSeparator('.');
		f.setDecimalFormatSymbols(symbols);
		f.setMinimumFractionDigits(1);
		f.setMaximumFractionDigits(1);

		try
		{
			Number number = f.parse(s.replace(',', '.'));
			s = f.format(number);
		}
		catch (ParseException e)
		{
			e.printStackTrace();
		}

		return s;
	}

	public static String uppercaseFirstLetter(final String s)
	{
		if (s == null || s.isEmpty())
		{
			return s;
		}
		else
		{
			return s.substring(0, 1).toUpperCase() + s.substring(1);
		}
	}

	public static String lowercaseFirstLetter(final String s)
	{
		if (s == null || s.isEmpty())
		{
			return s;
		}
		else
		{
			return s.substring(0, 1).toLowerCase() + s.substring(1);
		}
	}

	/* =========================================================================================
	 * VALIDATORS
	 * =========================================================================================/

	/**
	 * [tested] Validates the (hour,minute) pair
	 *
	 * @param hour
	 * @param min
	 * @return True if pair is correct, false otherwise
	 */
	public static boolean checkTime(int hour, int min)
	{
		return (hour >= 0) && (hour < HourPerDay) && (min >= 0) && (min < MinPerHour);
	}

	/**
	 * Checks if the string value fits the maximum size. Null-safe.
	 *
	 * @param s       String to check
	 * @param maxSize Max size allowed
	 * @throws IllegalArgumentException If max size exceed
	 */
	public static void checkSize(String s, int maxSize) throws IllegalArgumentException
	{
		if (s != null && s.length() > maxSize)
		{
			throw new IllegalArgumentException(
					String.format(Locale.US, "String too long: %d chars passed, but at most %d are allowed", s.length(), maxSize));
		}
	}

	/**
	 * If string is {@code null}, IllegalArgumentException with specified message will be thrown
	 *
	 * @param s       String to check
	 * @param message Message to throw if the string is {@code null}
	 * @throws IllegalArgumentException If the string is {@code null}
	 */
	public static void checkNotNull(String s, String message) throws IllegalArgumentException
	{
		if (s == null)
		{
			throw new IllegalArgumentException(message);
		}
	}

	/**
	 * Removes all non-BMP (Basic Multilingual Plane) chars
	 *
	 * @param s String to process
	 * @return Cleared string
	 */
	public static String removeNonUtf8(String s)
	{
		// do twice to handle surrogate artifacts
		final String regex = "[^\u0000-\uFFFF]";
		return (s != null) ? s.replaceAll(regex, "").replaceAll(regex, "") : null;
	}

	/* =========================================================================================
	 * CONVERTERS
	 * =========================================================================================/

	/**
	 * Rounds up to specified number of digits after dot
	 *
	 * @param x
	 * @param digits
	 * @return
	 */
	public static double round(double x, int digits)
	{
		// TODO: seems bad approach
		double factor;

		switch (digits)
		{
			case 1:
			{
				factor = 10;
				break;
			}
			case 2:
			{
				factor = 100;
				break;
			}
			case 3:
			{
				factor = 1000;
				break;
			}
			default:
			{
				factor = Math.pow(10, digits);
			}
		}

		return (Math.round(x * factor)) / factor;
	}

	/**
	 * Rounds up to 2 digits after dot
	 *
	 * @param x
	 * @return
	 */
	public static double round2(double x)
	{
		return round(x, 2);
	}

	/**
	 * Calculates sum
	 *
	 * @param values Values to process
	 * @return Sum
	 */
	public static double getSum(Iterable<Double> values)
	{
		double sum = 0.0;

		for (Double x : values)
		{
			sum += x;
		}

		return sum;
	}

	/**
	 * Calculates mean value
	 *
	 * @param values Values to process
	 * @return Mean value
	 */
	public static double getMean(Collection<Double> values)
	{
		if (values.size() > 0)
		{
			return getSum(values) / values.size();
		}
		else
		{
			return 0.0;
		}
	}

	/**
	 * Calculates standard deviation using pre-calculated mean
	 *
	 * @param values Values to process
	 * @param mean   Mean values
	 * @return Standard deviation
	 */
	public static double getDeviation(Collection<Double> values, double mean)
	{
		double s = 0.0;

		if (values.size() > 0)
		{
			for (Double x : values)
			{
				s += (x - mean) * (x - mean);
			}
			s /= values.size();
		}

		return Math.sqrt(s);
	}

	/* =========================================================================================
	 * RANDOM
	 * =========================================================================================/

	/**
	 * Returns random string from supplied string array
	 *
	 * @param strings
	 * @return Random string
	 */
	public static String randomString(String... strings)
	{
		return strings[r.nextInt(strings.length)];
	}

	/**
	 * Returns random date of period [2000-01-01 00:00:00, 2029-12-28 23:59:59]. Day of month is
	 * always in [1, 28] interval.
	 */
	public static Date randomTime()
	{
		final int year = 2000 + r.nextInt(30);
		final int month = 1 + r.nextInt(12);
		final int day = 1 + r.nextInt(28);
		final int hour = r.nextInt(24);
		final int min = r.nextInt(60);
		final int sec = r.nextInt(60);

		return time(year, month, day, hour, min, sec);
	}

	/* =========================================================================================
	 * DATE UTILS
	 * =========================================================================================/

	/**
	 * Calculates date the day before specified one
	 *
	 * @param date Date
	 * @return One day before
	 */
	public static Date getPrevDay(Date date)
	{
		return shiftDate(date, -1);
	}

	/**
	 * Calculates date the day after specified one
	 *
	 * @param date Date
	 * @return One day later
	 */
	public static Date getNextDay(Date date)
	{
		return shiftDate(date, +1);
	}

	public static Date getNextMonth(Date date)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(date);
		c.add(Calendar.MONTH, 1);
		return c.getTime();
	}

	public static Date getPrevMonth(Date date)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(date);
		c.add(Calendar.MONTH, -1);
		return c.getTime();
	}

	/**
	 * Adds specified amount of days to the date
	 *
	 * @param date
	 * @param days
	 * @return
	 */
	public static Date shiftDate(Date date, int days)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(date);
		c.add(Calendar.DATE, days);
		return c.getTime();
	}

	public static Date setDate(Date dateTime, int year, int monthOfYear, int dayOfMonth)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(dateTime);
		c.set(Calendar.YEAR, year);
		c.set(Calendar.MONTH, monthOfYear);
		c.set(Calendar.DAY_OF_MONTH, dayOfMonth);

		return c.getTime();
	}

	public static Date setTime(Date dateTime, int hourOfDay, int minute)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(dateTime);
		c.set(Calendar.HOUR_OF_DAY, hourOfDay);
		c.set(Calendar.MINUTE, minute);
		c.set(Calendar.SECOND, 0);
		c.set(Calendar.MILLISECOND, 0);

		return c.getTime();
	}

	/**
	 * [tested] Returns sorted dates list (lastDate-period+1 ... lastDate)
	 *
	 * @param lastDate Current date
	 * @param period   Days
	 * @return
	 */
	public static List<Date> getPeriodDates(Date lastDate, int period)
	{
		List<Date> dates = new ArrayList<Date>();

		Calendar c = Calendar.getInstance();
		c.setTime(lastDate);

		c.add(Calendar.DATE, -period);

		for (int i = 0; i < period; i++)
		{
			c.add(Calendar.DATE, +1);
			dates.add(c.getTime());
		}

		return dates;
	}

	/**
	 * Constructs date (UTC)
	 *
	 * @param year
	 * @param month
	 * @param day
	 * @return
	 */
	public static Date date(int year, int month, int day)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.setTimeZone(TIMEZONE_UTC);
		c.set(year, month - 1, day);
		return c.getTime();
	}

	/**
	 * Constructs time (UTC)
	 *
	 * @param year
	 * @param month
	 * @param day
	 * @param hour
	 * @param min
	 * @param sec
	 * @return
	 */
	public static Date time(int year, int month, int day, int hour, int min, int sec)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.setTimeZone(TIMEZONE_UTC);
		c.set(year, month - 1, day, hour, min, sec);
		return c.getTime();
	}

	/**
	 * Constructs date (local)
	 *
	 * @param timeZone
	 * @param year
	 * @param month
	 * @param day
	 * @return
	 */
	public static Date dateLocal(TimeZone timeZone, int year, int month, int day)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.setTimeZone(timeZone);
		c.set(year, month - 1, day);
		return c.getTime();
	}

	/**
	 * Constructs time (local)
	 *
	 * @param timeZone
	 * @param year
	 * @param month
	 * @param day
	 * @param hour
	 * @param min
	 * @param sec
	 * @return
	 */
	public static Date timeLocal(TimeZone timeZone, int year, int month, int day, int hour, int min, int sec)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.setTimeZone(timeZone);
		c.set(year, month - 1, day, hour, min, sec);
		return c.getTime();
	}

	/**
	 * Constructs today's midnight date (local)
	 *
	 * @param timeZone
	 * @return
	 */
	public static Date today(TimeZone timeZone)
	{
		Calendar c = Calendar.getInstance(timeZone);
		int year = c.get(Calendar.YEAR);
		int month = c.get(Calendar.MONTH);
		int day = c.get(Calendar.DATE);

		c.clear();
		c.setTimeZone(timeZone);
		c.set(year, month, day);
		return c.getTime();
	}

	/**
	 * Returns number of minutes since day's beginning
	 *
	 * @param time     Time
	 * @param timezone Timezone
	 * @return
	 */
	private static int getDayMinutes(Date time, TimeZone timezone)
	{
		Calendar c = Calendar.getInstance(timezone);
		c.setTime(time);
		return (c.get(Calendar.HOUR_OF_DAY) * MinPerHour) + c.get(Calendar.MINUTE);
	}

	/**
	 * Returns number of minutes since day's beginning (timezone: local)
	 *
	 * @param time     Time
	 * @return
	 */
	public static int getDayMinutesLocal(Date time)
	{
		return getDayMinutes(time, TimeZone.getDefault());
	}

	/**
	 * Returns number of minutes since day's beginning (timezone: UTC)
	 *
	 * @param time     Time
	 * @return
	 */
	public static int getDayMinutesUTC(Date time)
	{
		return getDayMinutes(time, TIMEZONE_UTC);
	}

	/**
	 * Checks if two dates belongs to the same day (local timezone)
	 *
	 * @param date1
	 * @param date2
	 * @return
	 */
	public static boolean sameDay(Date date1, Date date2)
	{
		Calendar c1 = Calendar.getInstance();
		c1.setTime(date1);

		Calendar c2 = Calendar.getInstance();
		c2.setTime(date2);

		return (c1.get(Calendar.DATE) == c2.get(Calendar.DATE)) && (c1.get(Calendar.MONTH) == c2.get(Calendar.MONTH)) && (
				c1.get(Calendar.YEAR) == c2.get(Calendar.YEAR));
	}

	/**
	 * MISC
	 */

	public static String removeTabs(String s)
	{
		return s.replaceAll("\t", "");
	}

	public static void sleep(long time)
	{
		try
		{
			Thread.sleep(time);
		}
		catch (InterruptedException e)
		{
		}
	}

	public static boolean hasWordStartedWith(String s, String editText)
	{
		if (editText.isEmpty())
		{
			return true;
		}

		s = s.toUpperCase();
		editText = editText.toUpperCase();

		boolean ignore = false;
		int j = 0;
		for (int i = 0; i < s.length(); i++)
		{
			if (ALPHANUMERIC.indexOf(s.charAt(i)) != -1)
			{
				if (!ignore)
				{
					if (s.charAt(i) == editText.charAt(j))
					{
						if (++j == editText.length())
						{
							return true;
						}
					}
					else
					{
						j = 0;
						ignore = true;
					}
				}
			}
			else
			{
				j = 0;
				ignore = false;
			}
		}

		return false;
	}

	public static boolean isNullOrEmpty(String s)
	{
		return (s == null) || (s.isEmpty());
	}

	/**
	 * Determines proper name for numeral
	 *
	 * @param value Any integer value
	 * @param s0    Noun for zero things
	 * @param s1    Noun for one thing
	 * @param s2    Noun for two things
	 * @return Proper noun
	 */
	public static String getNumberName(int value, String s0, String s1, String s2)
	{
		value = Math.abs(value);

		switch (value % 10)
		{
			case 0:
			case 5:
			case 6:
			case 7:
			case 8:
			case 9:
			{
				return s0;
			}
			case 1:
			{
				if (value > 10 && value < 15)
				{
					return s0;
				}
				else
				{
					return s1;
				}
			}
			default:
				// case 2:
				// case 3:
				// case 4:
			{
				if (value > 10 && value < 20)
				{
					return s0;
				}
				else
				{
					return s2;
				}
			}
		}
	}

	public static void saveStreamToFile(InputStream inputStream, String fileName) throws IOException
	{
		try (OutputStream outputStream = new FileOutputStream(new File(fileName)))
		{
			copy(inputStream, outputStream);
		}
	}

	public static void saveStringToFile(String data, String fileName) throws IOException
	{
		try (Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName), "UTF-8")))
		{
			out.write(data);
		}
	}

	public static void copy(InputStream input, OutputStream output) throws IOException
	{
		int read = 0;
		byte[] bytes = new byte[16 * 1024];

		while ((read = input.read(bytes)) != -1)
		{
			output.write(bytes, 0, read);
		}
	}

	public static String readStream(InputStream stream) throws IOException
	{
		// Scanner s = new Scanner(stream);
		// try
		// {
		// s.useDelimiter("\\A");
		// return s.hasNext() ? s.next() : "";
		// }
		// finally
		// {
		// s.close();
		// }

		ByteArrayOutputStream result = new ByteArrayOutputStream();
		byte[] buffer = new byte[1024];
		int length;
		while ((length = stream.read(buffer)) != -1)
		{
			result.write(buffer, 0, length);
		}

		return result.toString("UTF-8");
	}

	/**
	 * @param a Set
	 * @param b Set
	 * @return Intersections of two sets: A ∩ B
	 */
	public static <T> Set<T> intersection(Set<T> a, Set<T> b)
	{
		Set<T> result = new HashSet<T>(a);
		result.retainAll(b);
		return result;
	}

	/**
	 * @param a Set
	 * @param b Set
	 * @return Difference A\B
	 */
	public static <T> Set<T> difference(Set<T> a, Set<T> b)
	{
		Set<T> result = new HashSet<T>(a);
		result.removeAll(b);
		return result;
	}

	/**
	 * Checks if string ends with slash. For {@code null} input, {@code null} returned
	 *
	 * @param s String to handle
	 * @return String with appended slash if required
	 */
	public static String makeSureEndsWithSlash(String s)
	{
		return (s == null || s.endsWith("/")) ? s : (s + "/");
	}

	public static String nullToEmpty(String s)
	{
		return (s == null) ? "" : s;
	}

	public static String buildString(int size)
	{
		final StringBuilder s = new StringBuilder(size);
		for (int i = 0; i < size; i++)
		{
			s.append("*");
		}
		return s.toString();
	}
}

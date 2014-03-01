package org.bosik.diacomp.core.utils;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;
import java.util.UUID;
import org.json.JSONArray;

public class Utils
{
	// отладочная печать
	private static final String				TAG					= "Utils";

	// константы
	public static final int					MsecPerSec			= 1000;
	public static final int					SecPerMin			= 60;
	public static final int					MinPerHour			= 60;
	public static final int					HourPerDay			= 24;
	public static final int					SecPerDay			= SecPerMin * MinPerHour * HourPerDay;
	public static final int					MinPerDay			= MinPerHour * HourPerDay;
	public static final int					HalfMinPerDay		= (MinPerHour * HourPerDay) / 2;
	public static final long				MsecPerMin			= MsecPerSec * SecPerMin;
	public static final long				MsecPerDay			= MsecPerSec * SecPerMin * MinPerHour * HourPerDay;

	public static final double				EPS					= 0.0000001;

	private static char						DECIMAL_DOT;
	private static DecimalFormat			DF;

	/*
	 * public static final String[] MONTH_NAMES = new String[] { "Январь", "Февраль", "Март",
	 * "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь"};
	 */

	// форматы
	public static final SimpleDateFormat	STD_FORMAT_TIME_UTC	= new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.US);
	public static final SimpleDateFormat	STD_FORMAT_TIME_LOC	= new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",
																		Locale.getDefault());
	public static final SimpleDateFormat	STD_DATE_FORMAT		= new SimpleDateFormat("yyyy-MM-dd", Locale.US);

	// статическая инициализация
	static
	{
		STD_FORMAT_TIME_UTC.setTimeZone(TimeZone.getTimeZone("UTC"));
		STD_FORMAT_TIME_LOC.setTimeZone(TimeZone.getDefault());

		NumberFormat f = NumberFormat.getInstance(Locale.US);
		if (f instanceof DecimalFormat)
		{
			DF = (DecimalFormat)f;
			DECIMAL_DOT = DF.getDecimalFormatSymbols().getDecimalSeparator();
		}
		else
		{
			throw new RuntimeException("Number format is not a decimal format");
		}
	}

	// функции

	/**
	 * [tested] Преобразует число в строку. Для однозначных неотрицательных чисел строка дополняется
	 * лидирующим нулём. Отрицательные числа возвращаются как есть.
	 * 
	 * @param Число
	 * @return Строка
	 */
	public static String intTo00(int n)
	{
		return (n >= 0) && (n < 10) ? "0" + String.valueOf(n) : String.valueOf(n);
	}

	/**
	 * [tested] Проверяет корректность пары часы:минуты.
	 * 
	 * @param hour
	 *            Часы
	 * @param min
	 *            Минуты
	 * @return Корректна ли пара
	 */
	public static boolean checkTime(int hour, int min)
	{
		return (hour >= 0) && (hour < HourPerDay) && (min >= 0) && (min < MinPerHour);
	}

	/**
	 * [tested] Преобразует текстовое время во время дневника
	 * 
	 * @param S
	 *            Строка вида "dd:dd"
	 * @return Время дневника (измеряется как число минут после полуночи)
	 */
	public static int strToTime(String S)
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

	/**
	 * [tested] Преобразует время во время дневника.
	 * 
	 * @param time
	 *            Время
	 * @return Время дневника
	 */
	public static int timeToMin(Date time)
	{
		Calendar c = Calendar.getInstance();
		c.setTimeZone(TimeZone.getTimeZone("UTC"));
		c.setTime(time);
		return (c.get(Calendar.HOUR_OF_DAY) * MinPerHour) + c.get(Calendar.MINUTE);
	}

	/**
	 * [tested] Преобразует дату в формат сервера STD_DATE_FORMAT
	 * 
	 * @param date
	 *            Дата
	 * @return Строка
	 */
	public static String formatDate(Date date)
	{
		return STD_DATE_FORMAT.format(date);
	}

	/**
	 * [tested] Преобразует время в формат сервера STD_FORMAT_TIME_UTC
	 * 
	 * @param time
	 *            Время
	 * @return Строка
	 */
	public static String formatTimeUTC(Date time)
	{
		return STD_FORMAT_TIME_UTC.format(time);
	}

	public static String formatTimeLocal(Date time)
	{
		return STD_FORMAT_TIME_LOC.format(time);
	}

	/**
	 * [tested] Читает время из строки формата STD_FORMAT_TIME_UTC
	 * 
	 * @param time
	 *            Строка, хранящая время
	 * @return Время
	 * @throws ParseException
	 */
	public static Date parseTimeUTC(String time)
	{
		try
		{
			return STD_FORMAT_TIME_UTC.parse(time);
		}
		catch (ParseException e)
		{
			throw new RuntimeException(e);
		}
	}

	/**
	 * [tested] Читает дату из строки формата STD_DATE_FORMAT
	 * 
	 * @param date
	 *            Строка, хранящая дату
	 * @return Дата
	 * @throws ParseException
	 */
	public static Date parseDate(String date) throws ParseException
	{
		return STD_DATE_FORMAT.parse(date);
	}

	/**
	 * [tested] Получает предыдущую дату по отношению к указанной
	 * 
	 * @param date
	 *            Дата
	 * @return Предыдущая дата
	 */
	public static Date getPrevDay(Date date)
	{
		return new Date(date.getTime() - MsecPerDay);
	}

	/**
	 * [tested] Получает следующую дату по отношению к указанной
	 * 
	 * @param date
	 *            Дата
	 * @return Следующая дата
	 */
	public static Date getNextDay(Date date)
	{
		return new Date(date.getTime() + MsecPerDay);
	}

	/**
	 * Заменяет в строке все точки и запятые на DECIMAL_DOT
	 * 
	 * @param s
	 *            Строка
	 * @return Исправленная строка
	 */
	public static String checkDot(String s)
	{
		return s.replace('.', DECIMAL_DOT).replace(',', DECIMAL_DOT);
	}

	public static double parseDouble(String s) throws ParseException
	{
		return DF.parse(checkDot(s)).doubleValue();
	}

	/**
	 * Returns current date-time
	 * 
	 * @return
	 */
	public static Date now()
	{
		// return Calendar.getInstance().getTime();
		return new Date();
	}

	// /**
	// * Returns current date-time in UTC
	// *
	// * @return
	// */
	// public static Date utc()
	// {
	// Calendar c = Calendar.getInstance();
	// int utcOffset = c.get(Calendar.ZONE_OFFSET) + c.get(Calendar.DST_OFFSET);
	// long utcMilliseconds = c.getTimeInMillis() - utcOffset;
	// return new Date(utcMilliseconds);

	// return new Date();

	// Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
	// cal.set(year + 1900, month, day, hour, minute, second);
	// cal.getTime().getTime();
	// }

	/**
	 * [tested] Returns sorted dates list (lastDate-period+1 ... lastDate)
	 * 
	 * @param lastDate
	 *            Current date
	 * @param period
	 *            Days
	 * @return
	 */
	public static List<Date> getPeriodDates(Date lastDate, int period)
	{
		List<Date> dates = new LinkedList<Date>();

		Calendar c = Calendar.getInstance();
		c.setTime(lastDate);
		c.set(Calendar.HOUR_OF_DAY, 0);
		c.set(Calendar.MINUTE, 0);
		c.set(Calendar.SECOND, 0);
		c.set(Calendar.MILLISECOND, 0);

		c.add(Calendar.DATE, -period);

		for (int i = 0; i < period; i++)
		{
			c.add(Calendar.DATE, +1);
			dates.add(c.getTime());
		}

		return dates;
	}

	/**
	 * Получает текущее время в минутах после полуночи
	 * 
	 * @return Время в минутах
	 */
	public static int curMinutes()
	{
		// TODO: check if it is used anywhere
		Calendar c = Calendar.getInstance();
		int h = c.get(Calendar.HOUR) + (c.get(Calendar.AM_PM) == Calendar.AM ? 0 : 12);
		int m = c.get(Calendar.MINUTE);
		return (h * 60) + m;
	}

	public static void sleep(long time)
	{
		try
		{
			Thread.sleep(time);
		}
		catch (Exception e)
		{
		}
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

	public static String getGUID()
	{
		return UUID.randomUUID().toString().replace("-", "");
	}
}

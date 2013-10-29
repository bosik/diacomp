package org.bosik.compensation.utils;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import android.util.Log;

public class Utils
{
	// отладочная печать
	@SuppressWarnings("unused")
	private static final String TAG = "Utils";

	// константы
	public static final int MsecPerSec = 1000;
	public static final int SecPerMin = 60;
	public static final int MinPerHour = 60;
	public static final int HourPerDay = 24;
	public static final int SecPerDay = SecPerMin * MinPerHour * HourPerDay;
	public static final int MinPerDay = MinPerHour * HourPerDay;
	public static final long MsecPerDay = MsecPerSec * SecPerMin * MinPerHour * HourPerDay;

	private static char DECIMAL_DOT;
	private static DecimalFormat DF;

	/*
	 * public static final String[] MONTH_NAMES = new String[] { "Январь", "Февраль", "Март",
	 * "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь"};
	 */

	// форматы
	public static final SimpleDateFormat STD_TIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
	public static final SimpleDateFormat STD_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");

	// статическая инициализация
	static
	{
		NumberFormat f = NumberFormat.getInstance(Locale.US);

		if (f instanceof DecimalFormat)
		{
			DF = (DecimalFormat) f;
			DECIMAL_DOT = DF.getDecimalFormatSymbols().getDecimalSeparator();
		} else
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
			return hour * MinPerHour + min;
		else
			throw new IllegalArgumentException("Incorrect time (" + S + ")");
	}

	/**
	 * [tested] Преобразует время дневника в текстовое время
	 * 
	 * @param Time
	 *            Время дневника (измеряется как число минут после полуночи)
	 * @return Текстовое время
	 */
	public static String timeToStr(int Time)
	{
		int hour = Time / MinPerHour;
		int min = Time % MinPerHour;
		if (checkTime(hour, min))
			return intTo00(hour) + ":" + intTo00(min);
		else
			throw new IllegalArgumentException("Incorrect time (" + Time + ")");
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
		return time.getHours() * MinPerHour + time.getMinutes();
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
	 * [tested] Преобразует время в формат сервера STD_TIME_FORMAT
	 * 
	 * @param time
	 *            Время
	 * @return Строка
	 */
	public static String formatTime(Date time)
	{
		return STD_TIME_FORMAT.format(time);
	}

	/**
	 * [tested] Читает время из строки формата STD_TIME_FORMAT
	 * 
	 * @param time
	 *            Строка, хранящая время
	 * @return Время
	 * @throws ParseException
	 */
	public static Date parseTime(String time) throws ParseException
	{
		return STD_TIME_FORMAT.parse(time);
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
	 * Получает текущую дату и время
	 * 
	 * @return Текущие дата и время
	 */
	public static Date now()
	{
		return Calendar.getInstance().getTime();
	}

	/**
	 * Получает текущее время в минутах после полуночи
	 * 
	 * @return Время в минутах
	 */
	public static int curMinutes()
	{
		Calendar c = Calendar.getInstance();
		int h = c.get(Calendar.HOUR) + (c.get(Calendar.AM_PM) == Calendar.AM ? 0 : 12);
		int m = c.get(Calendar.MINUTE);
		return h * 60 + m;
	}

	/*
	 * public static void logTimer(String tag, String msg) { Log.w(tag,
	 * String.valueOf(System.currentTimeMillis()) + " " + msg); }
	 */

	public static String Cp1251ToUtf8(String s)
	{
		// got no idea why it works
		return s;

		/*
		 * try { //return new String(s.getBytes(), "UTF-8"); } catch (Exception e) {
		 * e.printStackTrace(); throw new RuntimeException("Unsupported code page", e); }
		 */

		/*
		 * Charset charset = Charset.forName("Cp1251"); CharsetDecoder decoder =
		 * charset.newDecoder(); CharsetEncoder encoder = charset.newEncoder();
		 * 
		 * try { ByteBuffer bbuf = decoder.decode(CharBuffer.wrap(s)); CharBuffer cbuf =
		 * encoder.encode(bbuf); return cbuf.toString(); } catch (CharacterCodingException e) {
		 * throw new RuntimeException(e); }
		 */
	}

	public static String Utf8ToCp1251(String s)
	{

		/*
		 * try { return new String(s.getBytes(), "Cp1251"); } catch (UnsupportedEncodingException e)
		 * { e.printStackTrace(); throw new RuntimeException("Unsupported code page", e); }
		 */

		Log.d(TAG, "Utf8ToCp1251: " + s);

		Charset charset = Charset.forName("Cp1251");
		CharsetDecoder decoder = charset.newDecoder();
		CharsetEncoder encoder = charset.newEncoder();

		decoder.onMalformedInput(CodingErrorAction.IGNORE);
		encoder.onMalformedInput(CodingErrorAction.IGNORE);
		try
		{
			ByteBuffer bbuf = encoder.encode(CharBuffer.wrap(s));
			CharBuffer cbuf = decoder.decode(bbuf);
			return cbuf.toString();
		} catch (CharacterCodingException e)
		{
			throw new RuntimeException(e);
		}
	}
}

package org.bosik.diacomp.core.testutils;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

public class MiscUtils
{
	public static final double	EPS			= 0.00001;
	public static final long	EPS_TIME	= 5000;	// ms

	public static Date date(int year, int month, int day)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.setTimeZone(TimeZone.getTimeZone("UTC"));
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
}

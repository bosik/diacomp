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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

public class TimeUtils
{
	/**
	 * Calculates best time zone offset for the given server-local time pair
	 * 
	 * @param serverTime
	 * @param localTime
	 * @return Offset (in milliseconds) if found, <code>null</code> otherwise
	 */
	public static Long guessTimeZoneOffset(Date serverTime, Date localTime)
	{
		long error = localTime.getTime() - serverTime.getTime();
		long properOffset = TimeZone.getDefault().getOffset(System.currentTimeMillis()) + error;
		long min = Long.MAX_VALUE;
		Long bestOffset = null;

		for (String id : TimeZone.getAvailableIDs())
		{
			TimeZone timeZone = TimeZone.getTimeZone(id);
			long offset = timeZone.getOffset(System.currentTimeMillis()); // ms

			long difference = Math.abs(offset - properOffset);
			if (difference < min)
			{
				min = difference;
				bestOffset = offset;
			}
		}

		return bestOffset;
	}

	public static TimeZone[] getTimeZones(int offset)
	{
		final String[] availableIDs = TimeZone.getAvailableIDs(offset);
		TimeZone[] result = new TimeZone[availableIDs.length];

		for (int i = 0; i < result.length; i++)
		{
			result[i] = TimeZone.getTimeZone(availableIDs[i]);
		}

		return result;
	}

	public static void main(String... args)
	{
		final Date serverTime = new Date(System.currentTimeMillis());
		final Date localTime = new Date(System.currentTimeMillis() + 4 * 60 * 60 * 1000);
		Long offset = guessTimeZoneOffset(serverTime, localTime);

		if (offset != null)
		{
			long offsetMin = offset / (1000 * 60);
			System.out.printf("%+02d:%02d %n", offsetMin / 60, offsetMin % 60);

			//			for (String id : TimeZone.getAvailableIDs(offset.intValue()))
			//			{
			//				TimeZone zone = TimeZone.getTimeZone(id);
			//				System.out.printf("%s/%s %n", zone.getDisplayName(), id);
			//			}

			List<String> zones = new ArrayList<>();

			for (TimeZone zone : getTimeZones(offset.intValue()))
			{
				zones.add(zone.getID() + " (" + zone.getDisplayName() + ")");
			}

			Collections.sort(zones);
			for (String zone : zones)
			{
				System.out.println(zone);
			}
		}
	}
}

/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package org.bosik.diacomp.android.backend.features.sync;

import android.os.SystemClock;

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Date;

public class ServerTimeService
{
	// REST methods
	private static final String API_PREFERENCES       = "api/system/time";
	private static final long   CACHE_EXPIRATION_TIME = Utils.MsecPerHour;    // ms

	private final WebClient webClient;
	private       Date      cachedServerTime;
	private       long      cachedDeviceOffset;                                // ms

	public ServerTimeService(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new IllegalArgumentException("WebClient is null");
		}

		this.webClient = webClient;
	}

	private Date getServerTime()
	{
		// time-variant actions
		final long before = SystemClock.elapsedRealtime();
		final String resp = webClient.get(API_PREFERENCES);
		final long after = SystemClock.elapsedRealtime();

		// time-invariant actions
		final Date serverTime = new Date(Utils.parseTimeUTC(resp).getTime() + (after - before) / 2);

		cachedServerTime = serverTime;
		cachedDeviceOffset = after;

		return serverTime;
	}

	/**
	 * <b>Thread-blocking</b>. Fetches current server's time <i>at the moment method returns</i>.
	 * Fail-soft.
	 *
	 * @param cacheAllowed If <code>true</code>, the time might be calculated using previously fetched server
	 *                     time; otherwise the server will be contacted
	 * @return Server's time if available, <code>null</code> otherwise.
	 */
	public synchronized Date getServerTime(boolean cacheAllowed)
	{
		try
		{
			if (cacheAllowed && cachedServerTime != null)
			{
				long elapsedTime = SystemClock.elapsedRealtime() - cachedDeviceOffset;
				if (elapsedTime < CACHE_EXPIRATION_TIME)
				{
					return new Date(cachedServerTime.getTime() + elapsedTime);
				}
			}

			return getServerTime();
		}
		catch (Exception e)
		{
			return null;
		}
	}
}

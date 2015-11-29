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

import java.util.Date;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.utils.Utils;

public class ServerTimeService
{
	// REST methods
	private static final String	API_PREFERENCES	= "api/system/time";

	private final WebClient		webClient;

	public ServerTimeService(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new IllegalArgumentException("WebClient is null");
		}

		this.webClient = webClient;
	}

	/**
	 * <b>Thread-blocking</b>. Fetches current server's time <i>at the moment method returns</i>.
	 * Fail-soft.
	 *
	 * @return Server's time if available, <code>null</code> otherwise.
	 */
	public Date getServerTime()
	{
		try
		{
			// time-variant actions
			long before = System.nanoTime();
			String resp = webClient.get(API_PREFERENCES);
			long after = System.nanoTime();

			// time-invariant actions
			Date serverTime = Utils.parseTimeUTC(resp);
			long shift = (after - before) / (1000000 * 2);
			return new Date(serverTime.getTime() + shift);
		}
		catch (Exception e)
		{
			return null;
		}
	}
}

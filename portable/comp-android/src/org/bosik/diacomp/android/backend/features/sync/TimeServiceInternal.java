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

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.common.webclient.WebClientInternal;
import android.content.Context;
import android.util.Log;

public class TimeServiceInternal
{
	private static final String			TAG	= TimeServiceInternal.class.getSimpleName();
	private static ServerTimeService	instance;

	public static synchronized ServerTimeService getInstance(Context context)
	{
		if (null == instance)
		{
			Log.i(TAG, "Time service initialization...");
			WebClient webClient = WebClientInternal.getInstance(context);
			instance = new ServerTimeService(webClient);
		}
		return instance;
	}
}
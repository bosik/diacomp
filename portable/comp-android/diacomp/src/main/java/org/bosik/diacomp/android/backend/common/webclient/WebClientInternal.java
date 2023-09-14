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
package org.bosik.diacomp.android.backend.common.webclient;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.Context;
import android.util.Log;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.AccountUtils;

public class WebClientInternal
{
	private static final String	TAG	= WebClientInternal.class.getSimpleName();
	private static WebClient	instance;

	public static synchronized WebClient getInstance(Context context, String username, String password)
	{
		if (instance == null)
		{
			Log.i(TAG, "Web client initialization...");
			final String serverURL = context.getString(R.string.server_url);
			final int connectionTimeout = Integer.parseInt(context.getString(R.string.server_timeout));

			instance = new WebClient(serverURL, connectionTimeout);
		}

		instance.setUsername(username);
		instance.setPassword(password);

		return instance;
	}

	public static synchronized WebClient getInstance(Context context)
	{
		String username = null;
		String password = null;

		Account account = AccountUtils.getAccount(context);
		if (account != null)
		{
			username = account.name;
			password = AccountManager.get(context).getPassword(account);
		}

		return getInstance(context, username, password);
	}
}
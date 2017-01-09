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

import org.bosik.diacomp.android.backend.common.webclient.exceptions.WebClientException;
import org.bosik.diacomp.android.test.R;
import android.content.Context;
import android.test.AndroidTestCase;

public class TestWebClient extends AndroidTestCase
{
	// private static final String TAG = TestWebClient.class.getSimpleName();

	private static WebClient webClient;

	/**
	 * Constructs authorized WebClient
	 * 
	 * @return
	 */
	public static synchronized WebClient getWebClient(Context context)
	{
		if (webClient == null)
		{
			String SERVER = context.getString(R.string.test_url);
			String USERNAME = context.getString(R.string.test_username);
			String PASSWORD = context.getString(R.string.test_password);

			webClient = WebClientInternal.getInstance(context, USERNAME, PASSWORD);
			webClient.setServer(SERVER);
			try
			{
				webClient.login();
			}
			catch (WebClientException e)
			{
				throw new RuntimeException("Failed to initialize WebClient\n" + "Server: " + SERVER + "\nUsername: "
						+ USERNAME + "\nPassword: " + PASSWORD, e);
			}
		}
		return webClient;
	}

	public void testLogin()
	{
		getWebClient(getContext());
	}
}
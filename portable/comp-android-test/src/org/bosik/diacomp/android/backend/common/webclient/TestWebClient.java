package org.bosik.diacomp.android.backend.common.webclient;

import org.bosik.diacomp.android.backend.common.webclient.exceptions.WebClientException;
import org.bosik.diacomp.android.v1.test.R;
import android.test.AndroidTestCase;

public class TestWebClient extends AndroidTestCase
{
	// private static final String TAG = TestWebClient.class.getSimpleName();

	private static WebClient	webClient;

	/**
	 * Constructs authorized WebClient
	 * 
	 * @return
	 */
	public static synchronized WebClient getWebClient()
	{
		if (webClient == null)
		{
			TestWebClient test = new TestWebClient();
			String SERVER = test.getContext().getString(R.string.test_url);
			String USERNAME = test.getContext().getString(R.string.test_username);
			String PASSWORD = test.getContext().getString(R.string.test_password);
			int TIMEOUT = 3000;

			webClient = new WebClient(TIMEOUT);
			try
			{
				webClient.setUsername(USERNAME);
				webClient.setPassword(PASSWORD);
				webClient.setServer(SERVER);
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
		getWebClient();
	}
}
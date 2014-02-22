package org.bosik.diacomp.persistence.services.web.utils.client;

import junit.framework.TestCase;
import org.bosik.diacomp.android.persistence.services.web.utils.client.WebClient;
import org.bosik.diacomp.android.persistence.services.web.utils.client.exceptions.WebClientException;
import android.util.Log;

public class TestWebClient extends TestCase
{
	private static final String	TAG	= TestWebClient.class.getSimpleName();

	private static WebClient	webClient;

	public static WebClient getWebClient()
	{
		if (webClient == null)
		{
			// final String SERVER = "http://diacomp.16mb.com/";
			final String SERVER = "http://192.168.0.104/";
			final String USERNAME = "bosik-007@narod.ru";
			final String PASSWORD = "devel0pment";
			final int TIMEOUT = 3000;

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
				Log.e(TAG, "Failed to initialize WebClient: " + e.getLocalizedMessage() + "\n" + "Server: " + SERVER
						+ "\n" + "Username: " + USERNAME + "\n" + "Password: " + PASSWORD);

			}
		}
		return webClient;
	}

	// TODO: implement

	// @Override
	// protected void setUp() throws Exception
	// {
	// super.setUp();
	//
	// webClient = new WebClient(TIMEOUT);
	// webClient.setUsername(USERNAME);
	// webClient.setPassword(PASSWORD);
	// webClient.setServer(SERVER);
	//
	// webClient.login();
	// }

	// public void testLocalToServer()
	// {
	// Date localTime = Calendar.getInstance().getTime();
	// Date serverTime = webClient.localToServer(localTime);
	// Date localTimeAgain = webClient.serverToLocal(serverTime);
	//
	// assertEquals(localTime.getTime(), localTimeAgain.getTime());
	// }

	// public void testIsOnlineBoolean()
	// {
	// assertTrue(webClient.isOnline());
	// assertTrue(webClient.isOnline(true));
	// }
}
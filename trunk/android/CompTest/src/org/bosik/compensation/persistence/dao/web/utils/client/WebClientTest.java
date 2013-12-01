package org.bosik.compensation.persistence.dao.web.utils.client;

import junit.framework.TestCase;

public class WebClientTest extends TestCase
{
	// private static final String TAG = WebClientTest.class.getSimpleName();

	public static final WebClient	webClient;

	static
	{
		final String SERVER = "http://diacomp.16mb.com/"; // "http://127.0.0.1";
		final String USERNAME = "bosik-007@narod.ru";
		final String PASSWORD = "devel0pment";
		final int TIMEOUT = 3000;

		webClient = new WebClient(TIMEOUT);
		webClient.setUsername(USERNAME);
		webClient.setPassword(PASSWORD);
		webClient.setServer(SERVER);
		webClient.login();
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

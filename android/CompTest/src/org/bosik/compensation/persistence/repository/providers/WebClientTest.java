package org.bosik.compensation.persistence.repository.providers;

import junit.framework.TestCase;

public class WebClientTest extends TestCase
{
	private static final String	TAG	= WebClientTest.class.getSimpleName();

	private WebClient			webClient;

	public static WebClient getWebClient()
	{
		final String SERVER = "http://diacomp.16mb.com/";
		final String USERNAME = "bosik-007@narod.ru";
		final String PASSWORD = "devel0pment";
		final int TIMEOUT = 3000;

		WebClient client = new WebClient(TIMEOUT);
		client.setUsername(USERNAME);
		client.setPassword(PASSWORD);
		client.setServer(SERVER);

		return client;
	}

	// TODO: implements

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

package org.bosik.diacomp.android.backend.common.webclient;

import junit.framework.TestCase;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.WebClientException;

public class TestWebClient extends TestCase
{
	// private static final String TAG = TestWebClient.class.getSimpleName();
	// private static final String SERVER = "http://diacomp.16mb.com/";
	private static final String	SERVER		= "http://192.168.0.104:8090/comp-server/";
	private static final String	USERNAME	= "bosik-007@narod.ru";
	private static final String	PASSWORD	= "devel0pment";
	private static final int	TIMEOUT		= 3000;

	private static WebClient	webClient;

	/**
	 * Constructs logged-in WebClient
	 * 
	 * @return
	 */
	public static WebClient getWebClient()
	{
		if (webClient == null)
		{
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
		WebClient client = getWebClient();
		assertEquals(true, client.isOnline());
	}
}
package org.bosik.compensation.persistence.repository.providers;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import junit.framework.TestCase;
import android.util.Log;

public class WebClientTest extends TestCase
{
	private static final String	TAG			= WebClientTest.class.getSimpleName();

	private static final String	SERVER		= "http://diacomp.16mb.com/";
	private static final String	USERNAME	= "bosik-007@narod.ru";
	private static final String	PASSWORD	= "devel0pment";
	private static final int	TIMEOUT		= 3000;

	private WebClient			webClient;

	@Override
	protected void setUp() throws Exception
	{
		super.setUp();

		webClient = new WebClient(TIMEOUT);
		webClient.setUsername(USERNAME);
		webClient.setPassword(PASSWORD);
		webClient.setServer(SERVER);

		webClient.login();
	}

	@Override
	protected void tearDown() throws Exception
	{
		super.tearDown();
	}

	public void testLocalToServer()
	{
		Date localTime = Calendar.getInstance().getTime();
		Date serverTime = webClient.localToServer(localTime);
		Date localTimeAgain = webClient.serverToLocal(serverTime);

		assertEquals(localTime.getTime(), localTimeAgain.getTime());
	}

	public void testIsOnlineBoolean()
	{
		assertTrue(webClient.isOnline());
		assertTrue(webClient.isOnline(true));
	}

	public void testGetPages()
	{
		List<Date> dates = new ArrayList<Date>();

		GregorianCalendar gc = new GregorianCalendar(2013, 04 - 1, 02);

		// dates.add(new Date(2013, 04 - 1, 02));
		dates.add(gc.getTime());

		String resp = webClient.getPages(dates);

		Log.v(TAG, "Page is " + resp);
	}

	public void testGetFoodBase()
	{
		String resp = webClient.getFoodBase();

		Log.v(TAG, "FoodBase is " + resp);
	}
}

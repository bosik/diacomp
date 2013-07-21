package org.bosik.compensation.api.test;

import junit.framework.TestCase;

public class DiaryWebSourceTest extends TestCase
{
	private static final String TAG = "DiaryWebSourceTest";
	private static final String USERNAME = "bosiknk@rambler.ru";
	private static final String PASSWORD = "diacomp1440";
	private static final String SERVER = "http://diacomp.16mb.com/";

	/*
	 * public void testLoginGetTime() { final int TIMEOUT = 3000; final int MAX_DIFF = 10*1000; //
	 * in msec final WebDiaryRepository web = new WebDiaryRepository(TIMEOUT); web.username =
	 * USERNAME; web.password = PASSWORD; web.server = SERVER; web.login();
	 * 
	 * Date localTime1 = Calendar.getInstance().getTime(); long serverTime1 = web.timeShift;
	 * 
	 * try { Thread.sleep(3000); } catch (InterruptedException e) {}
	 * 
	 * Date localTime2 = Calendar.getInstance().getTime(); long serverTime2 = web.timeShift;
	 * 
	 * long d1 = localTime1.getTime() - serverTime1; long d2 = localTime2.getTime() - serverTime2;
	 * long diff = Math.abs(d1-d2);
	 * 
	 * Log.d(TAG, "diff: " + String.valueOf(diff));
	 * 
	 * assertTrue(diff < MAX_DIFF); }
	 */
}

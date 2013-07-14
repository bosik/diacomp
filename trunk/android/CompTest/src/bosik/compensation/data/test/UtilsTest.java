package bosik.compensation.data.test;

import java.util.Calendar;
import java.util.Date;
import org.bosik.compensation.utils.Utils;


import junit.framework.TestCase;

public class UtilsTest extends TestCase
{
	private Date date(int year, int month, int day)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.set(year, month-1, day);
		return c.getTime();
	}
	private Date time(int year, int month, int day, int hour, int min, int sec)
	{
		Calendar c = Calendar.getInstance();
		c.clear();
		c.set(year, month-1, day, hour, min, sec);
		return c.getTime();
	}
	
	public void testIntTo00()
	{		
		assertEquals("-10",	Utils.intTo00(-10));
		assertEquals("-1",	Utils.intTo00(-1));
		assertEquals("00",	Utils.intTo00(0));
		assertEquals("01",	Utils.intTo00(1));
		assertEquals("09",	Utils.intTo00(9));
		assertEquals("10",	Utils.intTo00(10));
	}
	
	public void testCheckTime()
	{
		// корректное время
		assertTrue(Utils.checkTime(0, 0));
		assertTrue(Utils.checkTime(20, 59));
		assertTrue(Utils.checkTime(23, 30));
		assertTrue(Utils.checkTime(23, 59));
		
		// некорректное время
		assertFalse(Utils.checkTime(0, -1));
		assertFalse(Utils.checkTime(-1, 0));
		assertFalse(Utils.checkTime(24, 0));
		assertFalse(Utils.checkTime(0, 60));
		assertFalse(Utils.checkTime(24, 70));
		assertFalse(Utils.checkTime(-100, 100));
	}
	
	public void testStrToTime()
	{
		// нормальный тест
		assertEquals(0,		Utils.strToTime("00:00"));
		assertEquals(1,		Utils.strToTime("00:01"));
		assertEquals(59,	Utils.strToTime("00:59"));
		assertEquals(60,	Utils.strToTime("01:00"));
		assertEquals(630,	Utils.strToTime("10:30"));
		assertEquals(1439, 	Utils.strToTime("23:59"));
		
		// краш-тест		
		try { Utils.strToTime(null); 		fail(); } catch (Exception e) {}
		try { Utils.strToTime(""); 			fail(); } catch (Exception e) {}
		try { Utils.strToTime("gArBAgE");	fail(); } catch (Exception e) {}
		try { Utils.strToTime(":"); 		fail(); } catch (Exception e) {}
		try { Utils.strToTime("xx:yy"); 	fail(); } catch (Exception e) {}
		try { Utils.strToTime("10:60"); 	fail(); } catch (Exception e) {}
		try { Utils.strToTime("24:00"); 	fail(); } catch (Exception e) {}
		try { Utils.strToTime("00:-1"); 	fail(); } catch (Exception e) {}
		try { Utils.strToTime("-1:00"); 	fail(); } catch (Exception e) {}
	}

	public void testTimeToStr()
	{
		// нормальный тест
		assertEquals("00:00",	Utils.timeToStr(0));
		assertEquals("00:01",	Utils.timeToStr(1));
		assertEquals("00:59",	Utils.timeToStr(59));
		assertEquals("01:00",	Utils.timeToStr(60));
		assertEquals("10:30",	Utils.timeToStr(630));
		assertEquals("23:59",	Utils.timeToStr(1439));
		
		// краш-тест		
		try { Utils.timeToStr(1440); 	fail(); } catch (Exception e) {}
		try { Utils.timeToStr(-1); 		fail(); } catch (Exception e) {}
	}

	public void testTimeToMin()
	{
		assertEquals(0,	Utils.timeToMin(time(2013, 01, 01, 00, 00, 00)));
		assertEquals(0,	Utils.timeToMin(time(2013, 01, 01, 00, 00, 01)));
		assertEquals(1,	Utils.timeToMin(time(2013, 01, 01, 00, 01, 00)));
		assertEquals(60,	Utils.timeToMin(time(2013, 01, 01, 01, 00, 00)));
		assertEquals(121,	Utils.timeToMin(time(2013, 01, 01, 02, 01, 00)));
		assertEquals(1439,	Utils.timeToMin(time(2013, 01, 01, 23, 59, 00)));
	}
	
	public void testFormatDate()
	{		
		assertEquals("2012-04-02", Utils.formatDate(date(2012, 04, 02)));
		assertEquals("2012-05-01", Utils.formatDate(date(2012, 05, 01)));
	}

	public void testFormatTime()
	{
		assertEquals("2012-05-01 09:45:17", Utils.formatTime(time(2012, 05, 01, 9,  45, 17)));
		assertEquals("2012-05-01 21:45:17", Utils.formatTime(time(2012, 05, 01, 21, 45, 17)));
		assertEquals("2012-04-02 00:00:00", Utils.formatTime(time(2012, 04, 02, 00, 00, 00)));		
	}

	public void testParseTime()
	{
		try
		{
			assertEquals(time(2012, 04, 02, 00, 00, 00),	Utils.parseTime("2012-04-02 00:00:00"));
			assertEquals(time(2012, 05, 01,  9, 45, 17),	Utils.parseTime("2012-05-01 09:45:17"));
			assertEquals(time(2012, 05, 01, 22, 30, 17),	Utils.parseTime("2012-05-01 22:30:17"));
		}
		catch(Exception e)
		{
			fail();
		}
	}

	public void testParseDate()
	{
		try
		{
			assertEquals(date(2012, 04, 02),	Utils.parseDate("2012-04-02"));
			assertEquals(date(2012, 05, 01),	Utils.parseDate("2012-05-01"));
		}
		catch(Exception e)
		{
			fail();
		}
	}

	public void testGetPrevDay()
	{		
		assertEquals(date(2011, 12, 31),	Utils.getPrevDay(date(2012, 01, 01)));
		assertEquals(date(2012, 04, 01),	Utils.getPrevDay(date(2012, 04, 02)));
	}

	public void testGetNextDay()
	{
		assertEquals(date(2012, 01, 01),	Utils.getNextDay(date(2011, 12, 31)));  
		assertEquals(date(2012, 04, 02),	Utils.getNextDay(date(2012, 04, 01)));  
	}
}

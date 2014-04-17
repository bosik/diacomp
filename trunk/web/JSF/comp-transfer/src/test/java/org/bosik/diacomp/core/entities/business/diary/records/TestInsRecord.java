package org.bosik.diacomp.core.entities.business.diary.records;

import java.util.Date;
import junit.framework.TestCase;
import org.bosik.diacomp.core.utils.Utils;

public class TestInsRecord extends TestCase
{
	public void testInsRecord()
	{
		Date time = new Date();
		double value = 5.2;

		InsRecord note = new InsRecord(time, value);
		assertEquals(time, note.getTime());
		assertEquals(value, note.getValue(), Utils.EPS);
	}
}

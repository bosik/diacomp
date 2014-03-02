package org.bosik.diacomp.core.bo.diary.records;

import java.util.Date;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import junit.framework.TestCase;

public class TestNoteRecord extends TestCase
{
	public void testNoteRecord()
	{
		Date time = new Date();
		String text = "This is note";

		NoteRecord note = new NoteRecord(time, text);
		assertEquals(time, note.getTime());
		assertEquals(text, note.getText());
	}
}

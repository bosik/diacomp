package org.bosik.compensation.bo.diary.records;

import org.bosik.compensation.bo.diary.records.NoteRecord;
import junit.framework.TestCase;

public class TestNoteRecord extends TestCase
{
	public void testNoteRecord()
	{
		int time = 600;
		String text = "This is note";

		NoteRecord note = new NoteRecord(time, text);
		assertEquals(time, note.getTime());
		assertEquals(text, note.getText());
	}
}

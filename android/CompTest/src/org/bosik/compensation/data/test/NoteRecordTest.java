package org.bosik.compensation.data.test;

import junit.framework.TestCase;
import org.bosik.compensation.persistence.entity.diary.records.NoteRecord;

public class NoteRecordTest extends TestCase
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

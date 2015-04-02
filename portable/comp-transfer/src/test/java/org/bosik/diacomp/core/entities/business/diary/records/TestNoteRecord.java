/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.entities.business.diary.records;

import java.util.Date;
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

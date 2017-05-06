/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.backend.features.diary;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import org.bosik.diacomp.android.backend.common.stream.StreamReader;
import org.bosik.diacomp.android.backend.common.stream.versioned.DiaryRecordVersionedReader;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.merklesync.Versioned;
import android.util.JsonReader;

public class DiaryImportService
{
	public static void importDiary(byte[] data, final DiaryService service) throws IOException
	{
		importDiary(new ByteArrayInputStream(data), service);
	}

	public static void importDiary(InputStream stream, final DiaryService service) throws IOException
	{
		StreamReader<Versioned<DiaryRecord>> reader = new DiaryRecordVersionedReader();

		JsonReader json = new JsonReader(new InputStreamReader(stream, "UTF-8"));
		try
		{
			// Solution 1: slower, consumes less memory

			json.beginArray();
			while (json.hasNext())
			{
				Versioned<DiaryRecord> item = reader.read(json);
				service.add(item);
			}
			json.endArray();

			// Solution 2: faster (when done in single transaction), but consumes more memory

			// List<Versioned<DiaryRecord>> items = versionedDiaryRecordReader.readAll(reader);
			// service.save(items);
		}
		finally
		{
			json.close();
		}
	}
}

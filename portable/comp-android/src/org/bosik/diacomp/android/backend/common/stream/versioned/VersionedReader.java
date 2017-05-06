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
package org.bosik.diacomp.android.backend.common.stream.versioned;

import java.io.IOException;
import org.bosik.diacomp.android.backend.common.stream.StreamReader;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import android.util.JsonReader;

public class VersionedReader<T> extends StreamReader<Versioned<T>>
{
	private final StreamReader<T> dataReader;

	public VersionedReader(StreamReader<T> dataReader)
	{
		this.dataReader = dataReader;
	}

	@Override
	public Versioned<T> read(JsonReader json) throws IOException
	{
		Versioned<T> versioned = new Versioned<T>();

		json.beginObject();
		while (json.hasNext())
		{
			switch (json.nextName())
			{
				case "id":
				{
					versioned.setId(json.nextString());
					break;
				}
				case "hash":
				{
					versioned.setHash(json.nextString());
					break;
				}
				case "stamp":
				{
					versioned.setTimeStamp(Utils.parseTimeUTC(json.nextString()));
					break;
				}
				case "version":
				{
					versioned.setVersion(json.nextInt());
					break;
				}
				case "deleted":
				{
					versioned.setDeleted(json.nextBoolean());
					break;
				}
				case "data":
				{
					versioned.setData(dataReader.read(json));
					break;
				}
				default:
				{
					json.skipValue();
				}
			}
		}

		json.endObject();
		return versioned;
	}
}
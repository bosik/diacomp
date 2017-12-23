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
package org.bosik.diacomp.android.backend.common.stream;

import java.io.IOException;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import android.util.JsonReader;

public class PreferenceReader extends StreamReader<PreferenceEntry<String>>
{
	@Override
	public PreferenceEntry<String> read(JsonReader json) throws IOException
	{
		PreferenceEntry<String> item = new PreferenceEntry<String>();

		json.beginObject();
		while (json.hasNext())
		{
			String name = json.nextName();

			switch (name)
			{
				case "key":
				{
					item.setId(PreferenceID.parse(json.nextString()));
					break;
				}
				case "value":
				{
					item.setValue(json.nextString());
					break;
				}
				case "version":
				{
					item.setVersion(json.nextInt());
					break;
				}
				default:
				{
					throw new IllegalArgumentException("Unexpected property: " + name);
				}
			}
		}
		json.endObject();

		return item;
	}
}
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
package org.bosik.diacomp.core.persistence.serializers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserMapEntry;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;

public class SerializerMap implements Serializer<Map<String, String>>
{
	private Parser<Entry<String, String>>		parserEntry		= new ParserMapEntry();
	private Serializer<Entry<String, String>>	serializerEntry	= new SerializerAdapter<Entry<String, String>>(
			parserEntry);

	@Override
	public Map<String, String> read(String s)
	{
		Map<String, String> map = new HashMap<String, String>();

		List<Entry<String, String>> pairs = serializerEntry.readAll(s);
		for (Entry<String, String> pair : pairs)
		{
			map.put(pair.getKey(), pair.getValue());
		}

		return map;
	}

	@Override
	public String write(Map<String, String> object)
	{
		return serializerEntry.writeAll(object.entrySet());
	}

	@Override
	public List<Map<String, String>> readAll(String s)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public String writeAll(Iterable<Map<String, String>> objects)
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}

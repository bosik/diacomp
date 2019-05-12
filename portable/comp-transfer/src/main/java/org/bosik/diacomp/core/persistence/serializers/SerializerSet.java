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

import org.json.JSONArray;
import org.json.JSONException;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class SerializerSet implements Serializer<Set<String>>
{
	@Override
	public Set<String> read(String s)
	{
		try
		{
			JSONArray json = new JSONArray(s);

			Set<String> result = new HashSet<>();

			for (int i = 0; i < json.length(); i++)
			{
				String value = json.getString(i);
				result.add(value);
			}

			return result;
		}
		catch (JSONException e)
		{
			throw new IllegalArgumentException("Failed to parse JSON: " + s, e);
		}
	}

	@Override
	public String write(Set<String> object)
	{
		JSONArray json = new JSONArray();

		for (String value : object)
		{
			json.put(value);
		}

		return json.toString();
	}

	@Override
	public List<Set<String>> readAll(String s)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public String writeAll(Iterable<Set<String>> objects)
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}

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
package org.bosik.diacomp.core.persistence.utils;

import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

public class SerializerAdapter<T> implements Serializer<T>
{
	private final Parser<T> parser;

	public SerializerAdapter(Parser<T> parser)
	{
		this.parser = parser;
	}

	@Override
	public T read(String s)
	{
		try
		{
			JSONObject json = new JSONObject(s);
			return parser.read(json);
		}
		catch (JSONException e)
		{
			throw new IllegalArgumentException("Failed to parse JSON: '" + s + "'", e);
		}
	}

	@Override
	public List<T> readAll(String s)
	{
		try
		{
			JSONArray json = new JSONArray(s);
			return parser.readAll(json);
		}
		catch (JSONException e)
		{
			throw new IllegalArgumentException("Failed to parse JSON: " + s, e);
		}
	}

	@Override
	public String write(T object)
	{
		try
		{
			return parser.write(object).toString();
		}
		catch (JSONException e)
		{
			throw new RuntimeException("Failed to encode JSON", e);
		}
	}

	@Override
	public String writeAll(Iterable<T> objects)
	{
		try
		{
			return parser.writeAll(objects).toString();
		}
		catch (JSONException e)
		{
			throw new RuntimeException("Failed to encode JSON", e);
		}
	}
}

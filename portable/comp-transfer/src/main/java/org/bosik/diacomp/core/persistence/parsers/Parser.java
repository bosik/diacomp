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
package org.bosik.diacomp.core.persistence.parsers;

import java.util.ArrayList;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Converts <b>object(s) <—> JSON(s)</b>
 * 
 * @param <T>
 */
public abstract class Parser<T>
{
	/**
	 * Deserializes object from JSON
	 * 
	 * @param json
	 *            JSON with serialized object
	 * @return Deserialized object
	 * @throws JSONException
	 */
	public abstract T read(JSONObject json) throws JSONException;

	/**
	 * Deserializes arbitrary amount of objects from JSON array
	 * 
	 * @param json
	 *            JSON with serialized objects
	 * @return List of deserialized objects
	 * @throws JSONException
	 */
	public List<T> readAll(JSONArray json) throws JSONException
	{
		List<T> list = new ArrayList<T>();

		for (int i = 0; i < json.length(); i++)
		{
			list.add(read(json.getJSONObject(i)));
		}

		return list;
	}

	/**
	 * Serializes object into JSON
	 * 
	 * @param object
	 *            The object to be serialized
	 * @return JSON containing serialized object
	 * @throws JSONException
	 */
	public abstract JSONObject write(T object) throws JSONException;

	/**
	 * Serializes arbitrary amount of objects into JSON array
	 * 
	 * @param objects
	 *            List of objects to be serialized
	 * @return JSON array containing serialized objects
	 * @throws JSONException
	 */
	public JSONArray writeAll(List<T> objects) throws JSONException
	{
		JSONArray array = new JSONArray();

		for (T object : objects)
		{
			array.put(write(object));
		}

		return array;
	}
}

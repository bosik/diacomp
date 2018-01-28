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

import java.util.AbstractMap;
import java.util.Map.Entry;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserMapEntry extends Parser<Entry<String, String>>
{
	private static final String	FIELD_KEY	= "key";
	private static final String	FIELD_VALUE	= "value";

	@Override
	public Entry<String, String> read(JSONObject json) throws JSONException
	{
		String key = json.getString(FIELD_KEY);
		String value = json.getString(FIELD_VALUE);
		return new AbstractMap.SimpleEntry<>(key, value);
	}

	@Override
	public JSONObject write(Entry<String, String> object) throws JSONException
	{
		JSONObject json = new JSONObject();
		json.put(FIELD_KEY, object.getKey());
		json.put(FIELD_VALUE, object.getValue());
		return json;
	}
}

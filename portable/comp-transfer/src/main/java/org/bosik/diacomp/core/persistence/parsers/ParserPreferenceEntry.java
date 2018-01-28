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

import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserPreferenceEntry extends Parser<PreferenceEntry<String>>
{
	private static final String	FIELD_KEY		= "key";
	private static final String	FIELD_VALUE		= "value";
	private static final String	FIELD_VERSION	= "version";

	@Override
	public PreferenceEntry<String> read(JSONObject json) throws JSONException
	{
		String key = json.getString(FIELD_KEY);
		String value = json.getString(FIELD_VALUE);
		int version = json.getInt(FIELD_VERSION);

		PreferenceEntry<String> entry = new PreferenceEntry<>();
		entry.setId(PreferenceID.parse(key));
		entry.setValue(value);
		entry.setVersion(version);

		return entry;
	}

	@Override
	public JSONObject write(PreferenceEntry<String> object) throws JSONException
	{
		JSONObject json = new JSONObject();
		json.put(FIELD_KEY, object.getId().getKey());
		json.put(FIELD_VALUE, object.getValue());
		json.put(FIELD_VERSION, object.getVersion());
		return json;
	}
}

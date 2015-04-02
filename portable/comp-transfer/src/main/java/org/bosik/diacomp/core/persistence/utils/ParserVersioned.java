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
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserVersioned<T> extends Parser<Versioned<T>>
{
	// private static final String TAG = ParserVersioned.class.getSimpleName();

	private static final String	FIELD_ID		= "id";
	private static final String	FIELD_TIMESTAMP	= "stamp";
	private static final String	FIELD_HASH		= "hash";
	private static final String	FIELD_VERSION	= "version";
	private static final String	FIELD_DELETED	= "deleted";
	private static final String	FIELD_DATA		= "data";

	private final Parser<T>		parser;

	public ParserVersioned(Parser<T> parser)
	{
		this.parser = parser;
	}

	@Override
	public Versioned<T> read(JSONObject json) throws JSONException
	{
		Versioned<T> item = new Versioned<T>();

		item.setId(json.getString(FIELD_ID));
		item.setTimeStamp(Utils.parseTimeUTC(json.getString(FIELD_TIMESTAMP)));
		// TODO: make the Hash field mandatory again after data migration
		item.setHash(json.has(FIELD_HASH) ? json.getString(FIELD_HASH) : HashUtils.generateGuid());
		item.setVersion(json.getInt(FIELD_VERSION));
		item.setDeleted(json.getBoolean(FIELD_DELETED));
		item.setData(parser.read(json.getJSONObject(FIELD_DATA)));

		return item;
	}

	@Override
	public JSONObject write(Versioned<T> object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put(FIELD_ID, object.getId());
		json.put(FIELD_TIMESTAMP, Utils.formatTimeUTC(object.getTimeStamp()));
		json.put(FIELD_HASH, object.getHash());
		json.put(FIELD_VERSION, object.getVersion());
		json.put(FIELD_DELETED, object.isDeleted());
		final JSONObject obj = parser.write(object.getData());
		json.put(FIELD_DATA, obj); // do not inline
		return json;
	}
}

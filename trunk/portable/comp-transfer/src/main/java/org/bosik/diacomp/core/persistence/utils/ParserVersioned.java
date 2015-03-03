package org.bosik.diacomp.core.persistence.utils;

import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.utils.Utils;
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
		item.setHash(json.getString(FIELD_HASH));
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

package org.bosik.diacomp.core.persistence.serializers.utils;

import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Parser;
import org.bosik.diacomp.core.utils.Utils;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserVersioned<T> extends Parser<Versioned<T>>
{
	// private static final String TAG = ParserVersioned.class.getSimpleName();

	private Parser<T>	parser;

	public ParserVersioned(Parser<T> parser)
	{
		this.parser = parser;
	}

	@Override
	public Versioned<T> read(JSONObject json) throws JSONException
	{
		Versioned<T> item = new Versioned<T>();

		item.setId(json.getString("id"));
		item.setTimeStamp(Utils.parseTimeUTC(json.getString("stamp")));
		item.setVersion(json.getInt("version"));
		item.setDeleted(json.getBoolean("deleted"));
		item.setData(parser.read(json.getJSONObject("data")));

		return item;
	}

	@Override
	public JSONObject write(Versioned<T> object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("id", object.getId());
		json.put("stamp", Utils.formatTimeUTC(object.getTimeStamp()));
		json.put("version", object.getVersion());
		json.put("deleted", object.isDeleted());
		final JSONObject obj = parser.write(object.getData());
		json.put("data", obj); // do not inline
		return json;
	}
}
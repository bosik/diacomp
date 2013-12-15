package org.bosik.compensation.persistence.serializers;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.utils.Utils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserVersioned<T> implements Parser<Versioned<T>>
{
	private Parser<T>	parser;

	public ParserVersioned(Parser<T> parser)
	{
		this.parser = parser;
	}

	@Override
	public Versioned<T> read(JSONObject json) throws JSONException
	{
		try
		{
			Versioned<T> item = new Versioned<T>(parser.read(json.getJSONObject("data")));

			item.setId(json.getString("id"));
			item.setTimeStamp(Utils.parseTimeUTC(json.getString("stamp")));
			item.setVersion(json.getInt("version"));

			return item;
		}
		catch (ParseException e)
		{
			throw new JSONException(e.getLocalizedMessage());
		}
	}

	@Override
	public List<Versioned<T>> readAll(JSONArray jsonArray) throws JSONException
	{
		List<Versioned<T>> list = new ArrayList<Versioned<T>>();

		for (int i = 0; i < jsonArray.length(); i++)
		{
			JSONObject json = jsonArray.getJSONObject(i);
			list.add(read(json));
		}

		return list;
	}

	@Override
	public JSONObject write(Versioned<T> object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("id", object.getId());
		json.put("stamp", Utils.formatTimeUTC(object.getTimeStamp()));
		json.put("version", object.getVersion());
		json.put("data", parser.write(object.getData()));

		return json;
	}

	@Override
	public JSONArray writeAll(List<Versioned<T>> objects) throws JSONException
	{
		JSONArray jsonArray = new JSONArray();
		for (Versioned<T> item : objects)
		{
			jsonArray.put(write(item));
		}
		return jsonArray;
	}
}

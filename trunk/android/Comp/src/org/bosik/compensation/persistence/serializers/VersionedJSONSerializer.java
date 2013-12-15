package org.bosik.compensation.persistence.serializers;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.bo.basic.TrueCloneable;
import org.bosik.compensation.bo.basic.Unique;
import org.bosik.compensation.utils.Utils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class VersionedJSONSerializer<T extends TrueCloneable> implements JSONSerializer<Unique<T>>
{
	private JSONSerializer<T>	serializer;

	public VersionedJSONSerializer(JSONSerializer<T> serializer)
	{
		this.serializer = serializer;
	}

	@Override
	public Unique<T> read(JSONObject json) throws JSONException
	{
		try
		{
			Unique<T> item = new Unique<T>(serializer.read(json.getJSONObject("data")));

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
	public List<Unique<T>> readAll(JSONArray jsonArray) throws JSONException
	{
		List<Unique<T>> list = new ArrayList<Unique<T>>();

		for (int i = 0; i < jsonArray.length(); i++)
		{
			JSONObject json = jsonArray.getJSONObject(i);
			list.add(read(json));
		}

		return list;
	}

	@Override
	public JSONObject write(Unique<T> object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("id", object.getId());
		json.put("stamp", Utils.formatTimeUTC(object.getTimeStamp()));
		json.put("version", object.getVersion());
		json.put("data", serializer.write(object.getData()));

		return json;
	}

	@Override
	public JSONArray writeAll(List<Unique<T>> objects) throws JSONException
	{
		JSONArray jsonArray = new JSONArray();
		for (Unique<T> item : objects)
		{
			jsonArray.put(write(item));
		}
		return jsonArray;
	}
}

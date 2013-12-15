package org.bosik.compensation.persistence.serializers;

import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class SerializerJSONAdapter<T> implements Serializer<T>
{
	private JSONParser<T>	parser;

	public SerializerJSONAdapter(JSONParser<T> parser)
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
			throw new IllegalArgumentException("Failed to parse JSON: " + s, e);
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
	public String writeAll(List<T> objects)
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
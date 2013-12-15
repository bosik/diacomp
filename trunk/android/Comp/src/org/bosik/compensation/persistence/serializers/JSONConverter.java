package org.bosik.compensation.persistence.serializers;

import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

// TODO: rename to JSONSerializer
public class JSONConverter<T> implements Serializer<T>
{
	private JSONSerializer<T>	serializer;

	public JSONConverter(JSONSerializer<T> serializer)
	{
		this.serializer = serializer;
	}

	@Override
	public T read(String s)
	{
		try
		{
			JSONObject json = new JSONObject(s);
			return serializer.read(json);
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
			return serializer.readAll(json);
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
			return serializer.write(object).toString();
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
			return serializer.writeAll(objects).toString();
		}
		catch (JSONException e)
		{
			throw new RuntimeException("Failed to encode JSON", e);
		}
	}
}
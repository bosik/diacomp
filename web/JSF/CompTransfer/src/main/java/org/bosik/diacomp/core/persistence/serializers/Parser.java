package org.bosik.diacomp.core.persistence.serializers;

import java.util.ArrayList;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Converts <b>object(s) <—> JSON(s)<b/>
 * 
 * @author Bosik
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

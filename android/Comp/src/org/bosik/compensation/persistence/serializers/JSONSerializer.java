package org.bosik.compensation.persistence.serializers;

import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

// TODO: rename to JSONParser
public interface JSONSerializer<T>
{
	/**
	 * Deserializes object from JSON
	 * 
	 * @param json
	 *            JSON with serialized object
	 * @return Deserialized object
	 * @throws JSONException
	 */
	public T read(JSONObject json) throws JSONException;

	/**
	 * Deserializes arbitrary amount of objects from JSON array
	 * 
	 * @param json
	 *            JSON with serialized objects
	 * @return List of deserialized objects
	 * @throws JSONException
	 */
	public List<T> readAll(JSONArray json) throws JSONException;

	/**
	 * Serializes object into JSON
	 * 
	 * @param object
	 *            The object to be serialized
	 * @return JSON containing serialized object
	 * @throws JSONException
	 */
	public JSONObject write(T object) throws JSONException;

	/**
	 * Serializes arbitrary amount of objects into JSON array
	 * 
	 * @param objects
	 *            List of objects to be serialized
	 * @return JSON array containing serialized objects
	 * @throws JSONException
	 */
	public JSONArray writeAll(List<T> objects) throws JSONException;
}
